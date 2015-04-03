update_db <- function(judgments) {
  library("RSQLite")
  library("lubridate")
  library("saos")
  
  con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")
  ids <- dbGetQuery(con, "SELECT id FROM judgments;")
  
  courttype <- extract(judgments, "courtType")
  judgments <- judgments[which(courttype$courtType == "COMMON")]
  
  id_new <- extract(judgments, "id")
  
  judgments <- judgments[! id_new$id %in% ids]
  
  
  dates <- extract(judgments, "judgmentDate")
  divs <- extract(judgments, "division")
  names(divs)[2] <- "division_id"
  judges <- extract(judgments, "judges")
  judges$"function." <- NULL
  type <- extract(judgments, "judgmentType")
  keywords <- extract(judgments, "keywords")
  
  # merge these tables where it is possible
  judgments <- merge(merge(dates, divs, by = "id", sort = FALSE),
                     type, by = "id", sort = FALSE)
  
  # add 'month' and 'year' variables
  tmp <- ymd(judgments$judgmentDate)
  judgments$month <- as.character(floor_date(tmp, "month"))
  judgments$year <- as.character(floor_date(tmp, "year"))
  
  # write to database
  dbWriteTable(con, "judgments", judgments, append = TRUE)
  dbWriteTable(con, "judges", judges, append = TRUE)
  dbWriteTable(con, "keywords", keywords, append = TRUE)
  
  dbDisconnect(con)
  
  source("recalculate_db.R")
  recalculate_db()
  return()
}