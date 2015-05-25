library("RSQLite")
library("lubridate")
library("saos")

### Download data, split by month to prevent memory overfilling

dates_start <- seq(as.Date("1994-01-01"), as.Date("2015-05-01"), by="1 month")
dates_end <- (dates_start - 1)[-1]
dates_start <- head(dates_start, -1)

dat <- vector("list", length(dates_start))
for (i in 1:256) {
  dat[[i]] <- get_dump_judgments(judgmentStartDate = dates_start[i], 
                                 judgmentEndDate = dates_end[i],
                                 verbose = TRUE)
  # remove textcontent
  dat[[i]] <- lapply(dat[[i]], function(x) {
    x$textContent <- NULL
    x
  })
  
  # save every month in case of some error
  saveRDS(dat[[i]], file = paste0(dates_start[i], ".RDS"))
  print(dates_start[i])
}

# if download was successfull merge data into one list
dat <- do.call(c, dat)

# save snapshot time
write(as.character(Sys.time() - 3600), file = "snapshot_time.txt")


# split judgments according to type of courts
courttype <- extract(dat2, "courtType")
common <- dat2[which(courttype$courtType == "COMMON")]
supreme <- dat2[which(courttype$courtType == "SUPREME")]
const <- dat2[which(courttype$courtType == "CONSTITUTIONAL_TRIBUNAL")]
appeal <- dat2[which(courttype$courtType == "NATIONAL_APPEAL_CHAMBER")]

# for consitutional tribunal, supreme and national appeal we don't know yet so
# saving complete list temporarily
saveRDS(const, file = "data/data_constitutional.RDS")
saveRDS(appeal, file = "data/data_appeal_chamber.RDS")
saveRDS(supreme, file = "data/data_supreme.RDS")



### Setting database for common courts

con <- dbConnect(RSQLite::SQLite(), "data/common_courts.db")


dates <- extract(common, "judgmentDate")
divs <- extract(common, "division")
names(divs)[2] <- "division_id"
judges <- extract(common, "judges")
judges$"function." <- NULL
type <- extract(common, "judgmentType")
keywords <- extract(common, "keywords")

# merge these tables where it is possible
judgments <- merge(merge(dates, divs, by = "id", sort = FALSE),
                   type, by = "id", sort = FALSE)

# add 'month' and 'year' variables
tmp <- ymd(judgments$judgmentDate)
judgments$month <- as.character(floor_date(tmp, "month"))
judgments$year <- as.character(floor_date(tmp, "year"))

# write to database
dbWriteTable(con, "judgments", judgments)
dbWriteTable(con, "judges", judges)
dbWriteTable(con, "keywords", keywords)


# in addition we write in database all essential tables with data about courts,
# divisions etc.

data(courts)
divisions <- bind_rows(courts$divisions)
divisions$court_id <- rep(courts$id, times = sapply(courts$divisions, nrow))
dbWriteTable(con, "divisions", as.data.frame(divisions))

courts_h <- readRDS("data//courts_hierarchy.RDS")
dbWriteTable(con, "courts", courts_h)

dbDisconnect(con)
