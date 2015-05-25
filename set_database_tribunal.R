library("dplyr")
library("saos")
library("RSQLite")

tribunal <- readRDS("data//data_constitutional.RDS")

date <- extract(tribunal, "judgmentDate")
type <- extract(tribunal, "judgmentType")


date %>%
  inner_join(type) -> judgments

con <- dbConnect(RSQLite::SQLite(), "data/constitutional_tribunal.db")

dbWriteTable(con, "judgments", judgments, row.names = FALSE)

judges <- extract(tribunal, "judges")

dbWriteTable(con, "judges", judges, row.names = FALSE)

