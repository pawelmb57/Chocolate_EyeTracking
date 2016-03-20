
data <- as.data.frame(read.csv("http://goo.gl/XfsrPI"))

shinyServer(function(input, output) {


  output$heatPlot <- renderPlotly({

    


  data <- data[which(data$Ind==input$bins),]

  heatdata <- data.matrix(aggregate( data[c("pos1","pos2","pos3")] , by=list(Fix_Location=data$variable),sum)[,c(2:4)])



  p <- plot_ly(
          z = heatdata
          , x = c("pos1","pos2","pos3")
          , y = c("Brand","Type","Price")
          , type = "heatmap"
  )


  })
})