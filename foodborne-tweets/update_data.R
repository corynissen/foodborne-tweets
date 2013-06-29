
library(RMongo)
library(lubridate)
library(stringr)

update.df <- function(dataframe){
  max.date <- max(dataframe$epoch, na.rm=T) * 1000
  new.df <-  dbGetQueryForKeys(mongo, "fp_tweets_chi_01", query=paste0('{"jDate":{"$gt":{"$date":', max.date, '}}}'), "{'text':1, 'jDate':1, 'created_at':1, 'rClass':1}", 0, 1000000)  
  if(nrow(new.df) > 0){
    new.df <- subset(new.df, created_at != "")
    new.df <- add.cols(new.df)
    add_these_cols <- names(dataframe)[!names(dataframe) %in% names(new.df)]
    for(col in add_these_cols){
      new.df[,col] <- rep(NA, nrow(new.df))
    }
    new.df <- as.data.frame(rbind(dataframe, new.df))
  }else{
    new.df <- dataframe
  }
  return(new.df)
}

add.cols <- function(dataframe){
  dataframe$text <- iconv(dataframe$text, "", "UTF-8", sub="")
  #dataframe$created_at2 <- as.Date(dataframe$created_at, "%a, %d %m %Y %H:%M:%S +0000")
  dataframe$is.rt <- grepl("^RT| RT @", dataframe$text)
  comma.date <- grepl(",", dataframe$created_at)
  date1 <- subset(dataframe, comma.date)
  date2 <- subset(dataframe, !comma.date)
  if(nrow(date1) > 0){
    date1$created_at3 <- gsub(" \\+0000", "", date1$created_at)
    date1$created_at3 <- parse_date_time(substring(date1$created_at3, 6, nchar(date1$created_at3)), "%d %b %Y %H:%M:%S")
  }
  if(nrow(date2) > 0){
    date2$created_at3 <- gsub("\\+0000 ", "", date2$created_at)
    date2$created_at3 <- parse_date_time(substring(date2$created_at3, 5, nchar(date2$created_at3)), "%b %d %H:%M:%S %Y")
  }
  dataframe <- as.data.frame(rbind(date1, date2))
  dataframe$epoch <- seconds(dataframe$created_at3)
  dataframe$created_at2 <- as.Date(dataframe$created_at3)
  dataframe$category <- ifelse(dataframe$rClass=="food poisoning tweet", "Good", "Junk")
  return(dataframe)
}
# must have ssh tunnel up and running
mongo <- mongoDbConnect("chihs")
source("pw.R")
authenticated <- dbAuthenticate(mongo, username, password)

if(file.exists("data.Rdata")){
  load("data.Rdata")
  df <- update.df(df)
}else{
  # just get the whole data set
  #df <- dbGetQuery(mongo, "fp_tweets_chi_01", '{}', 0, 1000000)
  df <- dbGetQueryForKeys(mongo, "fp_tweets_chi_01", '{}', "{'text':1, 'jDate':1, 'created_at':1, 'rClass':1}", 0, 10000000)
  df <- subset(df, created_at != "")
  df <- add.cols(df)
}

save(list=c("df"), file="data.Rdata")

# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongo_tunnel.sh ubuntu@54.227.2.128:/src/keys
# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongodb1.pem ubuntu@54.227.2.128:/src/keys
