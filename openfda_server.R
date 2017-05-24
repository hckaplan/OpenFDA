
#Server file to create dynamic cross tab bubble charts of AEs from Open FDA database in R shiny
#updated by Hannah K. 5/23/17

library(shiny)
load("openfda_example.rds")
countries.AEs.cln$logtotal <- log(countries.AEs.cln$total)

shinyServer(function(input, output) {
  plotdata <- reactive({
    #subset input data for just records of chosen events
    plotdata.tmp <- subset(countries.AEs.cln, term==input$ae1 | term == input$ae2)
    
    #transform from long to wide data for plotting
    plotdata <- reshape(plotdata.tmp, idvar = c("Name", "total", "country", "logtotal"), timevar = "term", direction = "wide")
    
    #create new variable names for plotting dynamically
    names(plotdata)[names(plotdata) == paste("index_aes.", input$ae1, sep="")] <- "var1"
    if (input$ae1 != input$ae2) {names(plotdata)[names(plotdata) == paste("index_aes.", input$ae2, sep="")] <- "var2"}
    else {plotdata$var2 <- plotdata$var1}
    return (plotdata)
  })
  output$bubblePlot <- renderPlot({
  #create bubble chart of relative indices of the chosen AEs by country where bubble size is proportional to total AEs reported by that country
  ggplot(plotdata(), aes(x = var1 , y = var2, size=logtotal)) + geom_point(shape=21) + 
      geom_text(aes(label=Name), size=4, hjust=0, vjust=0)+
      xlab(paste(input$ae1, "(% of Total Country AEs)", sep=" ")) + 
      ylab(paste(input$ae2, "(% of Total Country AEs)", sep=" ")) +
      labs(size= "Natural Log Total Reported Events") +
      theme(panel.background = element_rect(fill="white"))
  })

})
