library("saos")
data(courts)

shinyUI(fluidPage(
  titlePanel("Obciążenie sądów powszechnych"),
  
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "court",
                             label = "Sąd:",
                             choices = courts$name,
                             selected = courts$name[1])),
    mainPanel(plotOutput(outputId = "main_plot", height = "300px"))
  )
  
))
