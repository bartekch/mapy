library("sp")
library("rgeos")
library("ggplot2")
library("saos")
library("dplyr")

## load essential data
data(courts)
#courts_h <- readRDS("data//courts_hierarchy.RDS") # moved to "global.R"
count_by_month <- readRDS("data/count_by_month.RDS")
shp_appeal <- readRDS("data//cc_sp_appeal_simple.RDS")
shp_region <- readRDS("data//cc_sp_region_simple.RDS")

##  prepare data 
# join count data with hierarchy data
count_by_month <- inner_join(count_by_month, 
                             select(courts_h, id, appeal, region),
                             by = c("court_id" = "id"))
# create count by appeal
count_by_month %>%
  group_by(appeal) %>%
  summarise(count = sum(count)) -> count_appeal

# create coutn by region
count_by_month %>%
  group_by(region) %>%
  summarise(count = sum(count)) -> count_region



## prepare data and utilities for maps

# ggplots datasets
appeal_f <- fortify(shp_appeal)
appeal_f <- inner_join(appeal_f, count_appeal, by = c("id" = "appeal"))
region_f <- fortify(shp_region)
region_f <- inner_join(region_f, count_region, by = c("id" = "region"))

# spatial data
shp_appeal@data <- inner_join(shp_appeal@data, count_appeal, by = "appeal")
shp_region@data <- inner_join(shp_region@data, count_region, by = "region")

# own ggplot theme for maps
theme_own <- theme_minimal() +
  theme(axis.ticks = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

# function for plotting ggplot map
plot_ggplotmap <- function(data) {
  ggplot(data, aes(long, lat, group = group, fill = count)) +
    geom_polygon() +
    geom_path(color = "black", lwd = 0.5) +
    theme_own +
    coord_map()
}




shinyServer(function(input, output) {
  
  output$map_plot <- renderPlot({
    if (input$map_goButton == 0)
      return()
    
    if (isolate(input$map_level) == "appeal"){
      spplot(shp_appeal, zcol = "count")
    } else {
      level2 <- isolate(input$map_level2)
      if (level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == level2)$region)
        shp_region <- shp_region[shp_region@data$region %in% regions,]
      }
      
      spplot(shp_region, zcol = "count")
    }
  })
  
  output$map_ggplot <- renderPlot({
    if (input$map_goButton == 0)
      return()
    
    if (isolate(input$map_level) == "appeal"){
      plot_ggplotmap(appeal_f)
    } else {
      level2 <- isolate(input$map_level2)
      if (level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == level2)$region)
        region_f <- filter(region_f, id %in% regions)
      }
      plot_ggplotmap(region_f)
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