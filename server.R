library("dplyr")

count_by_month <- readRDS("data/count_by_month.RDS")

shinyServer(function(input, output) {
  
  output$main_plot <- renderPlot({
    id <- courts$id[courts$name == input$court]
    counts <- select(filter(count_by_month, court_id == id), month, count)
    if (nrow(counts) == 0) {
      plot.new()
      text(0.5, 0.5, "Brak danych")
    } else {
      plot(counts, type = "l", main = input$court)
    }
    
  })
})
