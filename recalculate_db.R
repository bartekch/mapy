recalculate_db <- function() {
  library("dplyr")
  library("RSQLite")
  
  con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")
  
  
  judgments <- dbReadTable(con, "judgments")
  
  # summarise number of judgments in each division by month
  judgments %>%
    group_by(division_id, month) %>%
    summarise(count = n())  %>% 
    ungroup() -> count_by_month_by_division
  
  dbWriteTable(con, "count_by_month_div", as.data.frame(count_by_month_by_division), overwrite = TRUE)

  # summarise number of judgments in each division by year
  judgments %>%
    group_by(division_id, year) %>%
    summarise(count = n())  %>% 
    ungroup() -> count_by_year_by_division
  
  dbWriteTable(con, "count_by_year_div", as.data.frame(count_by_year_by_division), overwrite = TRUE)
  
  # add courts
  divs <- dbReadTable(con, "divisions")
  count_by_month_by_division  %>%  
    inner_join(select(divs, id, court_id), by = c("division_id" = "id")) %>%
    group_by(court_id, month) %>%
    summarise(count = sum(count))  %>% 
    ungroup() -> count_by_month
  
  dbWriteTable(con, "count_by_month", as.data.frame(count_by_month), overwrite = TRUE)
  
  count_by_year_by_division  %>%  
    inner_join(select(divs, id, court_id), by = c("division_id" = "id")) %>%
    group_by(court_id, year) %>%
    summarise(count = sum(count))  %>% 
    ungroup() -> count_by_year
  
  dbWriteTable(con, "count_by_year", as.data.frame(count_by_year), overwrite = TRUE)
  
  # add court data
  courts <- dbReadTable(con, "courts")
  count_by_month <- inner_join(count_by_month,
                               select(courts, id, appeal, region), 
                               by = c("court_id" = "id"))
  count_by_year <- inner_join(count_by_year,
                               select(courts, id, appeal, region), 
                               by = c("court_id" = "id"))
  
  # create count by appeal
  count_by_month %>%
    group_by(appeal, month) %>%
    summarise(count = sum(count)) -> count_appeal
  
  dbWriteTable(con, "count_by_month_appeal", as.data.frame(count_appeal), overwrite = TRUE)
  
  count_by_year %>%
    group_by(appeal, year) %>%
    summarise(count = sum(count)) -> count_appeal
  
  dbWriteTable(con, "count_by_year_appeal", as.data.frame(count_appeal), overwrite = TRUE)
  
  
  # create count by region
  count_by_month %>%
    group_by(region, month) %>%
    summarise(count = sum(count)) -> count_region
  
  dbWriteTable(con, "count_by_month_region", as.data.frame(count_region), overwrite = TRUE)
  
  count_by_year %>%
    group_by(region, year) %>%
    summarise(count = sum(count)) -> count_region
  
  dbWriteTable(con, "count_by_year_region", as.data.frame(count_region), overwrite = TRUE)
  
  ## judges data
  judges <- dbReadTable(con, "judges")
  # add division and court info
  judges <- inner_join(judges,
                       select(judgments, id, division_id),
                       by = "id")
  judges <- inner_join(judges,
                       select(divs, id, court_id),
                       by = c("division_id" = "id"))
  
  # count number of distinct judges in each court
  judges %>%
    select(name, court_id) %>%
    distinct() %>%
    group_by(court_id) %>%
    summarise(count = n()) -> judges_count
  
  dbWriteTable(con, "judges_count", as.data.frame(judges_count), overwrite = TRUE)
  
  # calculate how many cases ruled each judge (in single court)
  judges %>%
    group_by(court_id, name) %>%
    summarise(count = n()) -> judges_burden
  dbWriteTable(con, "judges_burden", as.data.frame(judges_burden), overwrite = TRUE)
  
  # clean up
  dbDisconnect(con)
}