
shinyUI(
  navbarPage(
    # page title
    "Trendy",
    
    ## tab with common courts -> another tabs are nested
    tabPanel(
      # title for common courts tab
      "Sądy powszechne",
      
      tabsetPanel(
        # tab for showing maps
        tabPanel(
          "Mapy",
          sidebarLayout(
            sidebarPanel(
              # choose variable to be shown
              selectInput("map_variable", "Zmienna:", 
                          c("Liczba orzeczeń" = "count",
                            "Obciążenie sędziów" = "burden")),
              
              # choose type of graph
              radioButtons("map_type", "Rodzaj wykresu:", 
                           c("statyczny" = "static", "interaktywny" = "interactive"),
                           selected = "interactive",
                           inline = TRUE),
              
              # choose time unit
              radioButtons("map_time_unit", "Rozdzielczość czasowa:",
                          c("roczna" = "year", "miesięczna" = "month"),
                          inline = TRUE),
              
              # choose year for year plot
              conditionalPanel(
                "input.map_time_unit == 'year'",
                sliderInput("map_year", "Rok", min = 2000, max = 2015,
                            value = 2014, step = 1, round = TRUE, sep = "", 
                            ticks = FALSE, animate = TRUE)
              ),
              
              # choose months for month plot
              conditionalPanel(
                "input.map_time_unit != 'year'",
                uiOutput("map_month_range_ui"),
                uiOutput("map_month_ui")
              ),
              
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
              #actionButton("map_goButton", "Pokaż mapę"),
              tags$br(),
              "Na poziomie regionów sądy apelacyjne nie są uwzględnione"
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Mapa",
                  conditionalPanel("input.map_type == 'static'",
                                   imageOutput(outputId = "map_plot_static_svg")),
                  conditionalPanel("input.map_type == 'interactive'",
                                   leafletOutput(outputId = "map_plot_interactive",
                                                 height = 700))
                ),
             
                tabPanel(
                  "Dane",
                  dataTableOutput("map_data_table")
                )
              )
            )
          )
        ),
        
        # tab for showing trends in court
        tabPanel(
          "Statystyki",
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
              uiOutput("cc_stats_court"),
              
              tags$hr(),
              # list available divisions
              uiOutput("cc_stats_division")
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Statystyki",
                  tableOutput("cc_stats_info")
                ),
                tabPanel(
                  "Wykres",
                  #plotOutput("court_trends"),
                  dygraphOutput("court_trends_dygraph")
                )
              )
            )
          )
        )
      )
    ),
    
    
    ## tab for supreme court
    tabPanel(
      "Sąd Najwyższy", 
      
      # tab for showing trends in supreme court
      
      sidebarLayout(
        sidebarPanel(
          uiOutput("sc_chamber_out"),
          
          conditionalPanel(
            "input.sc_chamber_in != 'Wszystkie'",
            uiOutput("sc_division_out")
          )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Wykresy",
              dygraphOutput("sc_trends_dygraph")
            ),
            
            tabPanel(
              "Dane",
              dataTableOutput("sc_data_table")
            )
          )
        )
      )
    ),
    
    
    ## tab for constitutional tribune
    tabPanel("Trybunał Konstytucyjny", "coś tu będzie")
  )
)

