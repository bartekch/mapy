library("sp")
library("rgeos")
library("RColorBrewer")
library("scales")
library("ggplot2")
library("ggvis")
library("lubridate")
library("xts")
library("saos")
library("saosTools")
library("RSQLite")
library("dplyr")

## connect to database
con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")

## load essential data
data(courts)
#courts_h <- readRDS("data//courts_hierarchy.RDS") # moved to "global.R"
count_by_month <- dbReadTable(con, "count_by_month")
data(cc_sp_appeal)
shp_appeal <- cc_sp_appeal
data(cc_sp_region)
shp_region <- cc_sp_region

##  prepare data 
# join count data with hierarchy data
count_by_month <- inner_join(count_by_month, 
                             select(courts_h, id, appeal, region),
                             by = c("court_id" = "id"))
count_by_month$month <- as.Date(count_by_month$month)

# month count by appeal
count_by_month_appeal <- summarise(group_by(count_by_month, appeal, month), count = sum(count))
# month count by year
count_by_month_region <- summarise(group_by(count_by_month, region, month), count = sum(count))

# create count by appeal
count_by_year_appeal <- dbReadTable(con, "count_by_year_appeal")
count_appeal <- summarise(group_by(count_by_year_appeal, appeal), count = sum(count))
count_by_year_appeal$year <- as.Date(count_by_year_appeal$year)

# create count by region
count_by_year_region <- dbReadTable(con, "count_by_year_region")
count_region <- summarise(group_by(count_by_year_region, region), count = sum(count))
count_by_year_region$year <- as.Date(count_by_year_region$year)


## prepare data and utilities for maps

# ggplots datasets
appeal_f <- fortify(shp_appeal)
#appeal_f <- inner_join(appeal_f, count_appeal, by = c("id" = "appeal"))
region_f <- fortify(shp_region)
region_f <- inner_join(region_f, count_region, by = c("id" = "region"))

# spatial data
#shp_appeal@data <- inner_join(shp_appeal@data, count_appeal, by = "appeal")
#shp_region@data <- inner_join(shp_region@data, count_region, by = "region")

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

## leaflet
# palette and helpers
myPalette <- scales::gradient_n_pal(brewer.pal(9, "Blues")[9:4])
normalise <- function(vector) {
  (vector - min(vector)) / (max(vector) - min(vector))
}
# function for creating leaflet
plot_leaflet <- function(shp) {
  #popups <- paste(shp@data[name][,1], shp@data$count, sep = ", ")
  leaflet(data = shp) %>% addTiles() %>%
    addPolygons(fillColor = myPalette(normalise(shp@data$variable)),
                fillOpacity = 0.7,
                stroke = TRUE, color = "black", weight = 3)
                #popup = popups)
}


## prepare data about judges
cc_judges_count <- dbReadTable(con, "judges_count")
dbReadTable(con, "judges_burden") %>%
  group_by(court_id) %>%
  summarise(burden = mean(count)) -> cc_judges_burden


## prepare global year-month values
year_month <- expand.grid(formatC(1:12, width = 2, flag = "0"), 2000:2015)
year_month <- head(year_month, -10)
year_month <- apply(year_month[, c(2,1)], 1, paste, collapse = "-")






# Main server function ----------------------------------------------------

shinyServer(function(input, output) {
  
  #### I. COMMON COURTS TAB
  
  ### map section
  
  ## UI
  # sliders for months
  output$map_month_range_ui <- renderUI({
    slider <- sliderInput("map_month_range", "Zakres:", min = 1, 
                          max = length(year_month),
                          value = c(length(year_month) - 12, length(year_month)),
                          step = 1, ticks = FALSE, animate = FALSE)
    
    slider$children[[2]]$attribs[['data-values']] <- paste0(year_month, collapse = ',')
    slider
  })

  output$map_month_ui <- renderUI({
    if (is.null(input$map_month_range)) return()
    range <- input$map_month_range + 1
    
    slider <- sliderInput("map_month", "Miesiąc:", min = 0, max = 1 ,
                          value = 0, step = .1, ticks = FALSE, animate = TRUE)
    tmp <- year_month[range[1]:range[2]]
    slider$children[[2]]$attribs[['data-values']] <- paste0(tmp, collapse = ",") 
    slider
  })
  
  ## OUTPUT
  # static plot
  output$map_plot_static <- renderPlot({
    map_data <- prepare_map_data()
    if (is.null(map_data)) return()
    spplot(map_data, zcol = "variable")
  })
  
  # svg static plot
  output$map_plot_static_svg <- renderImage({
    map_data <- prepare_map_data()
    if (is.null(map_data)) return()

    outfile <- tempfile(fileext = ".svg")
    svg(outfile)
    print(spplot(map_data, zcol = "variable"))
    dev.off()
    
    filename <- normalizePath(file.path(outfile))
    list(src = filename, alt = "not working", width = 500, height = 500)
  }, deleteFile = TRUE)

  # interactive plot
  output$map_plot_interactive <- renderLeaflet({
    map_data <- prepare_map_data()
    if (is.null(map_data)) return()
    plot_leaflet(map_data)
  })
  
  # prepare shapefile for plotting
  prepare_map_data <- reactive({
    if (input$map_level == "appeal") {
      if (input$map_time_unit == "year") {
        inp_year <- input$map_year
        tmp <- filter(count_by_year_appeal, lubridate::year(year) == inp_year)
      } else {
        if (is.null(input$map_month)) return()
        range <- input$map_month_range
        inp_month <- as.Date(paste0(year_month[input$map_month+range[1]+1], "-01"), 
                           format = "%Y-%m-%d")
        tmp <- filter(count_by_month_appeal, month == inp_month)
      }
      shp_appeal@data <- left_join(shp_appeal@data, tmp, by = "appeal")
      shp_appeal@data$variable <- ifelse(is.na(shp_appeal@data$count), 0, shp_appeal@data$count)
      shp_appeal
    } else {
      level2 <- input$map_level2
      if (input$map_time_unit == "year") {
        inp_year <- input$map_year
        tmp <- filter(count_by_year_region, lubridate::year(year) == inp_year)
      } else {
        if (is.null(input$map_month)) return()
        range <- input$map_month_range
        inp_month <- as.Date(paste0(year_month[input$map_month+range[1]+1], "-01"), 
                             format = "%Y-%m-%d")
        tmp <- filter(count_by_month_region, month == inp_month)
      }
      shp_region@data <- left_join(shp_region@data, tmp, by = "region")
      shp_region@data$variable <- ifelse(is.na(shp_region@data$count), 0, shp_region@data$count)
      if (level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == level2)$region)
        shp_region <- shp_region[shp_region@data$region %in% regions,]
      }
      shp_region
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
  
  # basic info for an active court
  output$cc_stats_info <- renderTable({
    if (is.null(input$cc_stats_court)) {
      return()
    }
    name <- input$cc_stats_court
    id <- courts$id[courts$name == name]
    data.frame("Co"=c("Liczba sędziów", "Średnia liczba orzeczeń na sędziego"), 
               "Ile"=c(cc_judges_count$count[cc_judges_count$court_id == id],
                 cc_judges_burden$burden[cc_judges_burden$court_id == id]))
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
      ggplot(counts, aes(x = month, y = count)) +
        geom_line() +
        labs(title = name, x = "Miesiąc", y = "Liczba orzeczeń") +
        theme_bw()
    }
  })
  
  # interactive plot for an active court
  output$court_trends_dygraph <- renderDygraph({
    name <- input$cc_stats_court
    id <- courts$id[courts$name == name]
    counts <- count_by_month[count_by_month$court_id == id, c("month", "count")]
    if (nrow(counts) == 0) {
      return()
    } else {
      counts <- xts(counts$count, order.by = counts$month)
      dygraph(counts, main = name, xlab = "", ylab = "Liczba orzeczeń") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2, includeZero = TRUE) %>%
        dyRangeSelector()
    }
  })
  
})