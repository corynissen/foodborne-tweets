
library(shiny)
library(lubridate)

source("update_data.R")  # df is all the data
load("data.Rdata")

shinyServer(function(input, output) {

  data <- reactive({    
    if(input$refresh > 0){
      isolate({
        source("update_data.R")
        load("data.Rdata")
      })
    }
    df <- df[order(df$epoch, decreasing=TRUE),]
    if(!input$rt){
      df <- subset(df, !is.rt)
    }

    if(input$category != "All"){
      df <- subset(df, category==input$category)
    }
    
    df
  })

  # had to subset the data after the slider was created
  subset.data <- reactive({
    df <- data()
    max.date <- max(df$created_at3)
    df <- subset(df, created_at3 >= max.date-days(input$day.slider.reactive))
    df
  })

  # create the slider here because I need input from the df dataframe
  output$day.slider <- renderUI({
    df <- data()
    min.date <- min(df$created_at3)
    max.date <- max(df$created_at3)
    max.value <- ceiling(as.numeric((max.date - min.date)))
    #browser()
    return(sliderInput("day.slider.reactive", "Date range (back from present)",
                       min=1, max=max.value, value=7))
  })  

  output$caption <- renderText({
    "Foodborne Chicago Tweets by Time"
  })

  output$plot <- renderPlot({
    df <- subset.data()
    tmp <- as.data.frame(table(df$created_at2, df$category))
    tmp$Var1 <- as.character(tmp$Var1)
    tmp$Var2 <- as.character(tmp$Var2)
    overall.len <- length(unique(tmp$Var1))
    ticks <- c(seq(1, overall.len, ceiling(overall.len / 10)), overall.len)
    
    p <- ggplot(tmp) + geom_point(aes(x=Var1, y=Freq, colour=Var2)) +
         geom_line(aes(x=Var1, y=Freq, colour=Var2, group=Var2)) +
         xlab("Date") + ylab("Count of Tweets") +
         scale_colour_discrete(name="Tweet Label") +
         scale_x_discrete(breaks=tmp[ticks, "Var1"])
    print(p)
  })

  output$tweet.table <- renderTable({
      df <- subset.data()
      tab <- subset(df, select=c("text", "category", "created_at"))      
  },include.rownames=FALSE)
      
})
