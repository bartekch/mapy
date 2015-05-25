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

# connect to database
con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")

## load essential data
data(courts)
count_by_month <- dbReadTable(con, "count_by_month")
data(cc_sp_appeal)
data(cc_sp_region)

### data structure for convenient map plotting
map_data <- list(count = list(year = list(appeal = dbReadTable(con, "count_by_year_appeal"),
                                          region = dbReadTable(con, "count_by_year_region")),
                              month = list(appeal = dbReadTable(con, "count_by_month_appeal"),
                                           region = dbReadTable(con, "count_by_month_region"))),
                 burden = list(year = list(appeal = dbReadTable(con, "burden_by_year_appeal"),
                                           region = dbReadTable(con, "burden_by_year_region")),
                               month = list(appeal = dbReadTable(con, "burden_by_month_appeal"),
                                            region = dbReadTable(con, "burden_by_month_region")))
                 )


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


### load data for supreme courts
conS <- dbConnect(RSQLite::SQLite(), "data/supreme_court.db")
data(scchambers, package = "saos")
sc_data <- list(count = list(year = list(whole = dbReadTable(conS, "count_by_year"),
                                         chamber = dbReadTable(conS, "count_by_year_by_chamber"),
                                         div = dbReadTable(conS, "count_by_year_div")),
                             month = list(whole = dbReadTable(conS, "count_by_month"),
                                          chamber = dbReadTable(conS, "count_by_month_by_chamber"),
                                          div = dbReadTable(conS, "count_by_month_div")))
)
for (i in 1:3){
  sc_data$count$month[[i]]$time <- as.yearmon(sc_data$count$month[[i]]$time)
}




### load data for constitutional tribunal
conT <- dbConnect(RSQLite::SQLite(), "data/constitutional_tribunal.db")
ct_data <- list(year = dbReadTable(conT, "count_by_year"),
                month = dbReadTable(conT, "count_by_month"))
ct_data$month$time <- as.yearmon(ct_data$month$time)



# Main server function ----------------------------------------------------

shinyServer(function(input, output) {
  
  #### I. COMMON COURTS TAB
  
  ### Map section
  
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
  
  
  ## Reactive expressions for processing input
  # filter map data
  prepare_map_data <- reactive({
    tmp <- map_data[[input$map_variable]][[input$map_time_unit]][[input$map_level]]
    if (input$map_time_unit == "year") {
      inp_time <- input$map_year
    } else {
      if (is.null(input$map_month)) return()
      range <- input$map_month_range
      inp_time <- year_month[input$map_month+range[1]+1]
    }
    tmp <- filter(tmp, time == inp_time)
    
    if (input$map_level == "region" & input$map_level2 != "Wszystkie") {
      regions <- unique(filter(courts_h, appeal_name == input$map_level2)$region)
      tmp <- tmp[tmp$region %in% regions,]
    }
    tmp
  })
  
  # prepare shapefile for plotting
  prepare_map_shp <- reactive({
    tmp <- prepare_map_data()
    if (is.null(tmp)) return()
    if (input$map_level == "appeal") {
      shp <- cc_sp_appeal
    } else {
      shp <- cc_sp_region
      level2 <- input$map_level2
      if (level2 != "Wszystkie") {
        regions <- unique(filter(courts_h, appeal_name == level2)$region)
        shp <- cc_sp_region[cc_sp_region@data$region %in% regions,]
      }
    }
    shp@data <- left_join(shp@data, as.data.frame(tmp), by = input$map_level)
    shp
  })

  ## Output
  # svg static plot
  output$map_plot_static_svg <- renderImage({
    map_data <- prepare_map_shp()
    outfile <- tempfile(fileext = ".svg")
    svg(outfile)
    if (is.null(map_data)) {
      plot.new()
    } else if (all(is.na(map_data@data$variable))) {
      plot(map_data)
    } else {
      print(spplot(map_data, zcol = "variable"))
    }
    dev.off()
    filename <- normalizePath(file.path(outfile))
    list(src = filename, alt = "", width = 500, height = 500)
  }, deleteFile = TRUE)

  # interactive plot
  output$map_plot_interactive <- renderLeaflet({
    map_data <- prepare_map_shp()
    if (is.null(map_data)) return()
    map_data@data$variable[is.na(map_data@data$variable)] <- 0
    plot_leaflet(map_data)
  })
  
  # data table
  output$map_data_table <- renderDataTable({
    prepare_map_data()
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
    HTML(paste(paste0("<h3>", input$cc_stats_court, "</h3>"), 
               "<b>Lista wydziałów:</b><br/>", 
               paste(tmp, collapse = "<br/>"), 
               collapse = "<br/>"))
    #selectInput("cc_stats_division",
    #            "Wydziały",
    #            tmp,
    #            tmp[1])
  })
  
  # basic info for an active court
  output$cc_stats_info <- renderTable({
    if (is.null(input$cc_stats_court)) {
      return()
    }
    name <- input$cc_stats_court
    id <- courts$id[courts$name == name]
    data.frame("Zmienna"=c("Liczba sędziów", "Średnia liczba orzeczeń na sędziego"), 
               "Wartość"=c(cc_judges_count$count[cc_judges_count$court_id == id],
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
      counts <- xts(counts$count, order.by = as.yearmon(counts$month))
      dygraph(counts, main = name, xlab = "", ylab = "Liczba orzeczeń") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2, includeZero = TRUE) %>%
        dyRangeSelector()
    }
  })
  
  
  
  
  
  #### II. SUPREME COURT TAB
  
  output$sc_chamber_out <- renderUI({
    selectInput("sc_chamber_in", "Wybór izby Sądu Najwyższego",
                choices = c("Wszystkie", scchambers$name), "Wszystkie")
  })
  
  output$sc_division_out <- renderUI({
    if (is.null(input$sc_chamber_in)) return()
    if (input$sc_chamber_in == "Wszystkie") return()
    tmp <- scchambers$divisions[[which(scchambers$name == input$sc_chamber_in)]]$name
    selectInput("sc_division_in", "Wybór wydziału",
                c("Wszystkie", sort(tmp)), "Wszystkie")
  })
  
  
  prepare_sc_data <- reactive({
    if (is.null(input$sc_chamber_in)) return()
    
    tmp <- sc_data[["count"]][[input$sc_time_unit]]
    
    if (input$sc_chamber_in == "Wszystkie") {
      res <- tmp$whole
      attr(res, "name") <- "Sąd Najwyższy"
      return(res)
    }
    
    if (is.null(input$sc_division_in)) return()
    if (input$sc_division_in == "Wszystkie") {
      n <- scchambers$id[which(scchambers$name == input$sc_chamber_in)]
      res <- tmp$chamber[tmp$chamber$chamber == n,]
      attr(res, "name") <- input$sc_chamber_in
      return(res)
    }
    
    divs <- scchambers$divisions[[which(scchambers$name == input$sc_chamber_in)]]
    div <- divs$id[which(divs$name == input$sc_division_in)]
    res <- tmp$div[tmp$div$division.id == div, ]
    attr(res, "name") <- divs$fullName[which(divs$name == input$sc_division_in)]
    return(res)
  })
  
  output$sc_trends_dygraph <- renderDygraph({
    data <- prepare_sc_data()
    if (is.null(data)) return()
    
    name <- attr(data, "name")
    data <- mutate(data,
                  time = if (input$sc_time_unit == "year") {
                    as.Date(time, format = "%Y")
                  } else {
                    as.yearmon(time)
                    })
    
    if (input$sc_plot_type) {
      data <- select(data, -total)
      data <- rename(data,
                     "cała izba" = ALL_CHAMBER,
                     "pięcioosobowy" = FIVE_PERSON,
                     "jednoosobowy" = ONE_PERSON,
                     "siedmioosobowy" = SEVEN_PERSON,
                     "trzyosobowy" = THREE_PERSON,
                     "nieznany" = unknown)
    } else {
      data <- select(data, time, total)
    }
    if (nrow(data) == 0) {
      return()
    } else {
      data <- xts(select(data, -one_of("time", "division.id", "chamber")), order.by = data$time)
      dygraph(data, main = name, xlab = "", ylab = "Liczba orzeczeń") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2, includeZero = TRUE) %>%
        dyRangeSelector()
    }
  })
  
  output$sc_data_table <- renderDataTable({
    prepare_sc_data()
  })


  
  
  
  #### III. CONSTITUTIONAL TRIBUNAL TAB
  
  prepare_ct_data <- reactive({
    if (is.null(input$ct_time_unit)) return()
    
    res <- ct_data[[input$ct_time_unit]]
    return(res)
  })
  
  output$ct_trends_dygraph <- renderDygraph({
    data <- prepare_ct_data()
    if (is.null(data)) return()
    
    data <- mutate(data,
                   time = if (input$ct_time_unit == "year") {
                     as.Date(time, format = "%Y")
                   } else {
                     as.yearmon(time)
                   })
    
    if (nrow(data) == 0) {
      return()
    } else {
      data <- xts(select(data, count), order.by = data$time)
      dygraph(data, main = "Trybunał Konstytucyjny", xlab = "", ylab = "Liczba orzeczeń") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2, includeZero = TRUE) %>%
        dyRangeSelector()
    }
  })
  
  output$ct_data_table <- renderDataTable({
    prepare_ct_data()
  })
  

})