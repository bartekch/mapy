library("dplyr")

courts_h <- readRDS("data//courts_hierarchy.RDS")

shinyUI(
  navbarPage("Trendy",
             tabPanel("Sądy powszechne",
                      tabsetPanel(
                        tabPanel("Mapy", 
                                 sidebarLayout(
                                   sidebarPanel(selectInput("map_level",
                                                            "Poziom mapy:",
                                                            c("Apelacje" = "appeal",
                                                              "Regiony" = "region")),
                                                conditionalPanel("input.map_level == 'region'",
                                                                 selectInput("map_level2",
                                                                             "Apelacja:",
                                                                             c("Wszystkie", filter(courts_h, type == "APPEAL")$appeal_name))
                                                                 ),
                                                "Na poziomie regionów sądy apelacyjne nie są uwzględnione"
                                   ),
                                   mainPanel(plotOutput(outputId = "map_plot"),
                                             plotOutput(outputId = "map_ggplot"))
                                 )
                        ),
                        tabPanel("Stats",
                                 sidebarLayout(
                                   sidebarPanel(selectInput("stats_court",
                                                            "Wybór apelacji",
                                                            c("Wszystkie", filter(courts_h, type == "APPEAL")$appeal_name),
                                                            "Wszystkie"),
                                                
                                                conditionalPanel("input.stats_court != 'Wszystkie'",
                                                                 uiOutput("regions_in_appeal"),
                                                                 
                                                                 conditionalPanel("input.stats_court2 != 'Wszystkie'",
                                                                                  uiOutput("courts_in_region")),
                                                                 
                                                                 conditionalPanel("input.stats_court2 == 'Wszystkie'",
                                                                                  uiOutput("all_courts2"))
                                                ),
                                                
                                                conditionalPanel("input.stats_court == 'Wszystkie'",
                                                                 uiOutput("all_courts"))
                                   ),
                                   
                                   mainPanel(plotOutput("court_trends"))
                                 )
                        )
                      )
             ),
             tabPanel("Sąd Najwyższy", "coś tu będzie"),
             tabPanel("Trybunał Konstytucyjny", "coś tu będzie")
  )
)

