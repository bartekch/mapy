## Script for updating applciation's database

# run in your app's directory with
# Rscript update_script.R

library("saosTools")

# source updating function
source("update_db.R")

# read last time
times <- readLines("snapshot_time.txt")
times <- as.POSIXct(tail(times, 1))

update_all(times, update_db)

# write down time, with 1hour correctio for possible synchro problems
write(as.character(Sys.time() - 3600), 
      file = "snapshot_time.txt", append = TRUE)