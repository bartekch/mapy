
shinyUI(
  navbarPage(
    # page title
    "Trendy",
    
    # tab with common courts -> another tabs are nested
    tabPanel(
      # title for common courts tab
      "Sądy powszechne",
      
      tabsetPanel(
        # tab for showing maps
        tabPanel(
          "Mapy",
          sidebarLayout(
            sidebarPanel(
              # choose level of map
              selectInput("map_level",
                          "Poziom mapy:",
                          c("Apelacje" = "appeal", "Regiony" = "region"),
                          "appeal"),
              
              # if region is chosen, choose which appeal to show
              conditionalPanel(
                "input.map_level == 'region'",
                selectInput("map_level2",
                            "Apelacja:",
                            c("Wszystkie", dplyr::filter(courts_h, type == "APPEAL")$appeal_name),
                            "Wszystkie")
              ),
              
              "Na poziomie regionów sądy apelacyjne nie są uwzględnione"
            ),
            
            mainPanel(plotOutput(outputId = "map_plot"),
                      plotOutput(outputId = "map_ggplot"),
                      "potencjalne użycie leaflet")
          )
        ),
        
        # tab for showing trends in court
        tabPanel(
          "Stats",
          sidebarLayout(
            # quite complicated because it dynamically creates list of courts
            sidebarPanel(
              # choose appeal, or all
              selectInput("cc_stats_appeal", 
                          "Wybór apelacji", 
                          c("Wszystkie", courts_h[courts_h$type == "APPEAL", "appeal_name"]),
                          "Wszystkie"),
              
              # if appeal is chosen, list available regions
              conditionalPanel(
                "input.cc_stats_appeal != 'Wszystkie'",
                uiOutput("cc_stats_region")
              ),
              
              # list available courts
              uiOutput("cc_stats_court")
            ),
            
            mainPanel(
              plotOutput("court_trends")
            )
          )
        )
      )
    ),
    
    # tab for supreme court
    tabPanel("Sąd Najwyższy", "coś tu będzie"),
    
    # tab for constitutional tribune
    tabPanel("Trybunał Konstytucyjny", "coś tu będzie")
  )
)

