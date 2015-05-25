recalculate_db <- function() {
  library("reshape2")
  library("dplyr")
  library("RSQLite")
  library("lubridate")
  library("tidyr")
  library("saos")
  
  ## COMMON COURTS 
  con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")
  
  
  judgments <- dbReadTable(con, "judgments")
  judgments$month <- substr(judgments$month, 1, 7)
  judgments$year <- substr(judgments$year, 1, 4)
  
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
    summarise(count = sum(count)) %>%
    rename(time = month, variable = count) -> count_appeal
  tmp <- expand(count_appeal, appeal, time)
  count_appeal <- left_join(tmp, count_appeal, by = c("appeal", "time"))
  dbWriteTable(con, "count_by_month_appeal", as.data.frame(count_appeal), overwrite = TRUE)
  
  count_by_year %>%
    group_by(appeal, year) %>%
    summarise(count = sum(count))  %>% 
    rename(time = year, variable = count) -> count_appeal
  tmp <- expand(count_appeal, appeal, time)
  count_appeal <- left_join(tmp, count_appeal, by = c("appeal", "time"))
  dbWriteTable(con, "count_by_year_appeal", as.data.frame(count_appeal), overwrite = TRUE)
  
  
  # create count by region
  count_by_month %>%
    group_by(region, month) %>%
    summarise(count = sum(count)) %>%
    rename(time = month, variable = count) -> count_region
  tmp <- expand(count_region, region, time)
  count_region <- left_join(tmp, count_region, by = c("region", "time"))
  dbWriteTable(con, "count_by_month_region", as.data.frame(count_region), overwrite = TRUE)
  
  count_by_year %>%
    group_by(region, year) %>%
    summarise(count = sum(count)) %>% 
    rename(time = year, variable = count) -> count_region
  tmp <- expand(count_region, region, time)
  count_region <- left_join(tmp, count_region, by = c("region", "time"))
  dbWriteTable(con, "count_by_year_region", as.data.frame(count_region), overwrite = TRUE)
  
  ## judges data
  judges <- dbReadTable(con, "judges")
  # add division and court info
  judges <- inner_join(judges,
                       select(judgments, id, division_id, month, year),
                       by = "id")
  judges <- inner_join(judges,
                       select(divs, id, court_id),
                       by = c("division_id" = "id"))
  judges <- inner_join(judges,
                       select(courts, id, appeal, region),
                       by = c("court_id" = "id"))
  
  # prepare datasets with burden
  judges %>%
    group_by(appeal, month, name) %>%
    summarise(count = n()) %>%
    summarise(variable = mean(count)) %>%
    rename(time = month) -> burden_appeal
  tmp <- expand(burden_appeal, appeal, time)
  burden_appeal <- left_join(tmp, burden_appeal, by = c("appeal", "time"))
  dbWriteTable(con, "burden_by_month_appeal", as.data.frame(burden_appeal), overwrite = TRUE)
  
  judges %>%
    group_by(appeal, year, name) %>%
    summarise(count = n()) %>%
    summarise(variable = mean(count)) %>%
    rename(time = year) -> burden_appeal
  tmp <- expand(burden_appeal, appeal, time)
  burden_appeal <- left_join(tmp, burden_appeal, by = c("appeal", "time"))
  dbWriteTable(con, "burden_by_year_appeal", as.data.frame(burden_appeal), overwrite = TRUE)
  
  judges %>%
    group_by(region, month, name) %>%
    summarise(count = n()) %>%
    summarise(variable = mean(count)) %>%
    rename(time = month) -> burden_region
  tmp <- expand(burden_region, region, time)
  burden_region <- left_join(tmp, burden_region, by = c("region", "time"))
  dbWriteTable(con, "burden_by_month_region", as.data.frame(burden_region), overwrite = TRUE)
  
  judges %>%
    group_by(region, year, name) %>%
    summarise(count = n()) %>%
    summarise(variable = mean(count)) %>%
    rename(time = year) -> burden_region
  tmp <- expand(burden_region, region, time)
  burden_region <- left_join(tmp, burden_region, by = c("region", "time"))
  dbWriteTable(con, "burden_by_year_region", as.data.frame(burden_region), overwrite = TRUE)
  
  
  
  
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
  
  
  
  
  
  ## SUPREME COURT 
  
  con <- dbConnect(RSQLite::SQLite(), "data/supreme_court.db")
  
  judgments <- dbReadTable(con, "judgments")
  judgments %>%
    mutate(month = substr(judgmentDate, 1, 7),
           year = substr(judgmentDate, 1, 4),
           personnel = personnelType) -> judgments
  
  # summarise number of judgments in each division by month
  judgments %>%
    group_by(division.id, month, personnel) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = month) -> count_by_month_by_division
  tmp <- expand(count_by_month_by_division, division.id, time)
  tmp %>%
    left_join(count_by_month_by_division, by = c("division.id", "time")) %>%
    mutate(personnel = ifelse(is.na(personnel), "unknown", personnel)) %>%
    dcast(division.id + time ~ personnel, value.var = "count", fill = NA) ->
    count_by_month_by_division
  count_by_month_by_division$total <- rowSums(count_by_month_by_division[,3:8], na.rm = TRUE)
  dbWriteTable(con, "count_by_month_div", as.data.frame(count_by_month_by_division), overwrite = TRUE)
  
  # summarise number of judgments in each division by year
  judgments %>%
    group_by(division.id, year, personnel) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = year) -> count_by_year_by_division
  tmp <- expand(count_by_year_by_division, division.id, time)
  tmp %>%
    left_join(count_by_year_by_division, by = c("division.id", "time")) %>%
    mutate(personnel = ifelse(is.na(personnel), "unknown", personnel)) %>%
    dcast(division.id + time ~ personnel, value.var = "count", fill = NA) ->
    count_by_year_by_division
  count_by_year_by_division$total <- rowSums(count_by_year_by_division[,3:8], na.rm = TRUE)
  dbWriteTable(con, "count_by_year_div", as.data.frame(count_by_year_by_division), overwrite = TRUE)
  
  # add chambers
  data(scchambers)
  divs <- data.frame(chamber = rep(scchambers$id, times = sapply(scchambers$divisions, nrow)),
                     division = unlist(sapply(scchambers$divisions, `[[`, "id")))
  
  count_by_month_by_division  %>%  
    left_join(divs, by = c("division.id" = "division")) %>%
    group_by(chamber, time) %>%
    summarise(ALL_CHAMBER = sum(ALL_CHAMBER, na.rm = TRUE),
              FIVE_PERSON = sum(FIVE_PERSON, na.rm = TRUE),
              ONE_PERSON = sum(ONE_PERSON, na.rm = TRUE),
              SEVEN_PERSON = sum(SEVEN_PERSON, na.rm = TRUE),
              THREE_PERSON = sum(THREE_PERSON, na.rm = TRUE),
              unknown = sum(unknown, na.rm = TRUE),
              total = sum(total, na.rm = TRUE))  %>% 
    ungroup() -> count_by_month
  
  dbWriteTable(con, "count_by_month_by_chamber", as.data.frame(count_by_month), overwrite = TRUE)
  
  count_by_year_by_division  %>%  
    inner_join(divs, by = c("division.id" = "division")) %>%
    group_by(chamber, time) %>%
    summarise(ALL_CHAMBER = sum(ALL_CHAMBER, na.rm = TRUE),
              FIVE_PERSON = sum(FIVE_PERSON, na.rm = TRUE),
              ONE_PERSON = sum(ONE_PERSON, na.rm = TRUE),
              SEVEN_PERSON = sum(SEVEN_PERSON, na.rm = TRUE),
              THREE_PERSON = sum(THREE_PERSON, na.rm = TRUE),
              unknown = sum(unknown, na.rm = TRUE),
              total = sum(total, na.rm = TRUE))  %>% 
    ungroup() -> count_by_year
  
  dbWriteTable(con, "count_by_year_by_chamber", as.data.frame(count_by_year), overwrite = TRUE)

  # whole Supreme Court
  count_by_month  %>%  
    group_by(time) %>%
    summarise(ALL_CHAMBER = sum(ALL_CHAMBER, na.rm = TRUE),
              FIVE_PERSON = sum(FIVE_PERSON, na.rm = TRUE),
              ONE_PERSON = sum(ONE_PERSON, na.rm = TRUE),
              SEVEN_PERSON = sum(SEVEN_PERSON, na.rm = TRUE),
              THREE_PERSON = sum(THREE_PERSON, na.rm = TRUE),
              unknown = sum(unknown, na.rm = TRUE),
              total = sum(total, na.rm = TRUE))  %>% 
    ungroup() -> count_by_month
  
  dbWriteTable(con, "count_by_month", as.data.frame(count_by_month), overwrite = TRUE)
  
  count_by_year  %>%  
    group_by(time) %>%
    summarise(ALL_CHAMBER = sum(ALL_CHAMBER, na.rm = TRUE),
              FIVE_PERSON = sum(FIVE_PERSON, na.rm = TRUE),
              ONE_PERSON = sum(ONE_PERSON, na.rm = TRUE),
              SEVEN_PERSON = sum(SEVEN_PERSON, na.rm = TRUE),
              THREE_PERSON = sum(THREE_PERSON, na.rm = TRUE),
              unknown = sum(unknown, na.rm = TRUE),
              total = sum(total, na.rm = TRUE))  %>% 
    ungroup() -> count_by_year
  
  dbWriteTable(con, "count_by_year", as.data.frame(count_by_year), overwrite = TRUE)
  
  # clean up
  dbDisconnect(con)
  
  
  
  
  ## CONSTITUTIONAL TRIBUNAL
  
  con <- dbConnect(RSQLite::SQLite(), "data/constitutional_tribunal.db")
  
  judgments <- dbReadTable(con, "judgments")
  judgments %>%
    mutate(month = substr(judgmentDate, 1, 7),
           year = substr(judgmentDate, 1, 4)) -> judgments
  
  # summarise number of judgments by month
  judgments %>%
    group_by(month) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = month) -> count_by_month
  dbWriteTable(con, "count_by_month", as.data.frame(count_by_month), overwrite = TRUE)
  
  # summarise number of judgments by year
  judgments %>%
    group_by(year) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = year) -> count_by_year
  dbWriteTable(con, "count_by_year", as.data.frame(count_by_year), overwrite = TRUE)
  
  # clean up
  dbDisconnect(con)
  
  
  
  
  
  
  ## NATONAL APPEAL CHAMBER
  
  con <- dbConnect(RSQLite::SQLite(), "data/national_appeal_chamber.db")
  
  judgments <- dbReadTable(con, "judgments")
  judgments %>%
    mutate(month = substr(judgmentDate, 1, 7),
           year = substr(judgmentDate, 1, 4)) -> judgments
  
  # summarise number of judgments by month
  judgments %>%
    group_by(month) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = month) -> count_by_month
  dbWriteTable(con, "count_by_month", as.data.frame(count_by_month), overwrite = TRUE)
  
  # summarise number of judgments by year
  judgments %>%
    group_by(year) %>%
    summarise(count = n())  %>% 
    ungroup()  %>% 
    rename(time = year) -> count_by_year
  dbWriteTable(con, "count_by_year", as.data.frame(count_by_year), overwrite = TRUE)
  
  # clean up
  dbDisconnect(con)
  
}