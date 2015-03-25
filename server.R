library("sp")
library("rgeos")
library("ggplot2")
library("saos")
data(courts)
courts_h <- readRDS("data//courts_hierarchy.RDS")

count_by_month <- readRDS("data/count_by_month.RDS")
shp_appeal <- readRDS("data//cc_sp_appeal_simple.RDS")
shp_region <- readRDS("data//cc_sp_region_simple.RDS")
appeal_f <- fortify(shp_appeal)
region_f <- fortify(shp_region)
# appeal_f <- inner_join(appeal_f, shp_appeal@data, by = c("id" = "appeal"))

theme_own <- theme_minimal() +
  theme(axis.ticks = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

# join count data with hierarchy data
count_by_month <- inner_join(count_by_month, 
                             select(courts_h, id, appeal, region),
                             by = c("court_id" = "id"))


shinyServer(function(input, output) {
  count_appeal <- reactive({
    count_by_month %>%
      group_by(appeal) %>%
      summarise(count = sum(count))
  })

  count_region <- reactive({
    count_by_month %>%
      group_by(region) %>%
      summarise(count = sum(count))
  })
    
  output$map_plot <- renderPlot({
    if (input$map_level == "appeal"){
      tmp <- count_appeal()
      
      shp_appeal@data$count <- tmp$count
      spplot(shp_appeal, zcol = "count")
    } else {
      tmp <- count_region()
      shp_region@data <- inner_join(shp_region@data, tmp, by = "region")
      if (input$map_level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == input$map_level2)$region)
        shp_region <- shp_region[shp_region@data$region %in% regions,]
      }
      
      spplot(shp_region, zcol = "count")
    }
  })
  
  output$map_ggplot <- renderPlot({
    if (input$map_level == "appeal"){
      tmp <- count_appeal()
      appeal_f <- inner_join(appeal_f, tmp, by = c("id" = "appeal"))
      ggplot(appeal_f, aes(long, lat, group = group, fill = count)) +
        geom_polygon() +
        geom_path(color = "white", lwd = 0.2) +
        theme_own +
        coord_map()
    } else {
      tmp <- count_region()
      region_f <- inner_join(region_f, tmp, by = c("id" = "region"))
      if (input$map_level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == input$map_level2)$region)
        region_f <- filter(region_f, id %in% regions)
      }
      
      ggplot(region_f, aes(long, lat, group = group, fill = count)) +
        geom_polygon() +
        geom_path(color = "white", lwd = 0.5) +
        theme_own +
        coord_map()
    }
  })
  
  output$regions_in_appeal <- renderUI({
    tmp <- filter(courts_h, appeal_name == input$stats_court)$region_name
    tmp <- unique(na.omit(tmp))
    selectInput("stats_court2",
                "Wybór okręgu",
                c("Wszystkie", tmp),
                "Wszystkie")
  })
  
  output$courts_in_region <- renderUI({
    #if (!is.null(input$stats_court2)){
    #  tmp <- filter(courts_h, region_name == input$stats_court2)$name
    #}
    tmp <- courts_h[ courts_h$region_name == input$stats_court2, "name"]
    tmp <- tmp[!is.na(tmp)]
    selectInput("stats_court3",
                "Wybór sądu",
                tmp,
                tmp[1])
  })
  
  output$all_courts <- renderUI({
    tmp <- courts_h$name
    selectInput("stats_court4",
                "Wybór sądu",
                tmp,
                tmp[1])
  })
  
  output$all_courts2 <- renderUI({
    tmp <- filter(courts_h, appeal_name == input$stats_court)$name
    selectInput("stats_court5",
                "Wybór sądu",
                tmp,
                tmp[1])
  })
  
  output$court_trends <- renderPlot({
    # find court id
    name <- NULL
    if (input$stats_court == "Wszystkie") {
      name <- input$stats_court4
    } else if (!is.null(input$stats_court2)) {
      if (input$stats_court2 == "Wszystkie") {
        name <- input$stats_court5
      } else {
        name <- input$stats_court3
      }
    }
    id <- courts$id[courts$name == name]
    counts <- count_by_month[count_by_month$court_id == id, c("month", "count")]
    #counts <- select(filter(count_by_month, court_id == id), month, count)
    if (nrow(counts) == 0) {
      plot.new()
      text(0.5, 0.5, "Brak danych")
    } else {
      plot(counts, type = "l", main = name)
    }
  })
    
})