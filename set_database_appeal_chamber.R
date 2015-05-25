library("saos")
library("dplyr")
library("RSQLite")

judgments <- search_judgments(courtType = "NATIONAL_APPEAL", force = TRUE)
saveRDS(judgments, file = "nac_search.RDS")

tmp <- vector("list", 26)
for (i in 1:25) {
   j <- get_judgments(judgments[((i-1)*500+1) : (i*500)])
   saveRDS(j, paste0(i, ".RDS"))
   tmp[[i]] <- lapply(j, function(x) {
     x$textContent <- NULL
     x
     })
   print(i)
}
j <- get_judgments(judgments[12501:length(judgments)])
saveRDS(j, paste0(26, ".RDS"))
tmp[[26]] <- lapply(j, function(x) {
  x$textContent <- NULL
  x
})
judgments <- do.call(c, tmp)
class(judgments) <- c("saos_judgments", "list")

###


date <- extract(judgments, "judgmentDate")
type <- extract(judgments, "judgmentType")
judges <- extract(judgments, "judges")

date %>%
  inner_join(type) -> judgments

con <- dbConnect(RSQLite::SQLite(), "data/national_appeal_chamber.db")
dbWriteTable(con, "judgments", judgments, row.names = FALSE)

dbWriteTable(con, "judges", judges, row.names = FALSE)

