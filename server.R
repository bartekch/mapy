library("sp")
library("rgeos")
library("ggplot2")
library("saos")
library("dplyr")

data(courts)
#courts_h <- readRDS("data//courts_hierarchy.RDS")

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

count_by_month %>%
  group_by(appeal) %>%
  summarise(count = sum(count)) -> count_appeal

count_by_month %>%
  group_by(region) %>%
  summarise(count = sum(count)) -> count_region



shinyServer(function(input, output) {
  
  output$map_plot <- renderPlot({
    if (input$map_level == "appeal"){
      tmp <- count_appeal
      
      shp_appeal@data$count <- tmp$count
      spplot(shp_appeal, zcol = "count")
    } else {
      tmp <- count_region
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
      tmp <- count_appeal
      appeal_f <- inner_join(appeal_f, tmp, by = c("id" = "appeal"))
      ggplot(appeal_f, aes(long, lat, group = group, fill = count)) +
        geom_polygon() +
        geom_path(color = "white", lwd = 0.2) +
        theme_own +
        coord_map()
    } else {
      tmp <- count_region
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
  
  ## STATS TAB
  
  # conditional slider - if an particular appeal is chosen, choose between all 
  # all regions or particular region, within chosen appeal
  output$cc_stats_region <- renderUI({
    tmp <- courts_h[courts_h$appeal_name == input$cc_stats_appeal, "region_name"]
    tmp <- unique(na.omit(tmp))
    selectInput("cc_stats_region",
                "Wybór okręgu",
                c("Wszystkie", tmp),
                "Wszystkie")
  })

  # slider for selecting particular courts
  output$cc_stats_court <- renderUI({
    tmp <- NULL
    if (input$cc_stats_appeal == "Wszystkie") {
      tmp <- courts_h$name
    } else if (length(input$cc_stats_region) > 0) {
      if (input$cc_stats_region == "Wszystkie") {
        tmp <- courts_h[courts_h$appeal_name == input$cc_stats_appeal, "name"]
      } else {
        tmp <- courts_h[courts_h$region_name == input$cc_stats_region, "name"]
      }
    }
    if (!is.null(tmp)) tmp <- unique(tmp[!is.na(tmp)])
    
    selectInput("cc_stats_court",
                "Sąd",
                tmp,
                tmp[1])
  })
  
  # list of divisions
  output$cc_stats_division <- renderUI({
    tmp <- courts[courts$name == input$cc_stats_court, "divisions"]
    if (length(tmp) > 0) tmp <- tmp[[1]]$name
    selectInput("cc_stats_division",
                "Wydziały",
                tmp,
                tmp[1])
  })
  
  # plot stats for an active court
  output$court_trends <- renderPlot({
    name <- input$cc_stats_court
    id <- courts$id[courts$name == name]
    counts <- count_by_month[count_by_month$court_id == id, c("month", "count")]
    if (nrow(counts) == 0) {
      plot.new()
      text(0.5, 0.5, "Brak danych")
    } else {
      plot(counts, type = "l", main = name)
    }
  })
  
})