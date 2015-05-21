library("dplyr")
library("saos")
library("RSQLite")

data(scchambers)
supreme <- readRDS("data//data_supreme.RDS")

date <- extract(supreme, "judgmentDate")
division <- extract(supreme, "division")
personel <- extract(supreme, "personnelType")
form <- extract(supreme, "form")
type <- extract(supreme, "judgmentType")


date %>%
  inner_join(division) %>%
  inner_join(personel) %>%
  inner_join(form) %>%
  inner_join(type) -> judgments

con <- dbConnect(RSQLite::SQLite(), "data/supreme_court.db")

dbWriteTable(con, "judgments", judgments, row.names = FALSE)

judges <- extract(supreme, "judges")

dbWriteTable(con, "judges", judges, row.names = FALSE)
