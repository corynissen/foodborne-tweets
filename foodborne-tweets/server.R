
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
    
  output$caption <- renderText({
    "Foodborne Chicago Tweets by Time"
  })

  output$plot <- renderPlot({
    df <- data()
    tmp <- as.data.frame(table(df$created_at2, df$category))
    tmp$Var1 <- as.character(tmp$Var1)
    tmp$Var2 <- as.character(tmp$Var2)    
    
    p <- ggplot(tmp) + geom_point(aes(x=Var1, y=Freq, colour=Var2)) +
         geom_line(aes(x=Var1, y=Freq, colour=Var2, group=Var2))
    print(p)
  })

  output$tweet.table <- renderTable({
      df <- data()
      tab <- subset(df, select=c("text", "category", "created_at"))      
  },include.rownames=FALSE)
      
})