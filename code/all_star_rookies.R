################################################################################

# [1] Get a list of all the Topps All-Star Rookie Cards

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rvest)
library(jpeg)
library(googlesheets4)
library(ggplot2)
library(magick)
lu <- function(x) length(unique(x))
su <- function(x) sort(unique(x))

# set prefix:
prefix <- "https://www.tradingcarddb.com"

# read in the list of Topps sets:
topps_full_list <- read_html("https://www.tradingcarddb.com/ViewAll.cfm/sp/Baseball/brand/Topps")

links <- topps_full_list %>%
  html_nodes("li > a") %>%
  html_attr('href')

# Just get the main Topps set from each year:
topps_basic_sets <- grep("[1-2][0-9]{3}-Topps$", links)

# just get the "ViewSet" URL:
topps_view <- links[topps_basic_sets][grep("ViewSet", links[topps_basic_sets])]

# here is the checklist for 1989 Topps:
# checklist_url <- "https://www.tradingcarddb.com/PrintChecklist.cfm/sid/134"

# So, we need the set ID number for each of these 69 Topps sets ('sid')
# strsplit(topps_view[[1]], split = "/")[[1]][4]
sid <- sapply(topps_view, function(x) as.numeric(strsplit(x, split = "/")[[1]][4])) %>%
  as.numeric()

# OK this is a list of all Topps years and set IDs from 1952 onward:
topps <- data.frame(year = 1952:2021, sid = sid[-length(sid)])
n_years <- nrow(topps)

# Now, get a list of all the all-star rookies:
asr <- vector("list", n_years)
for (i in 1:n_years) {
  print(i); print(topps$year[i])
  Sys.sleep(1)
  prefix <- "https://www.tcdb.com/Notes.cfm/sid/"
  asr_url <- paste0(prefix, topps$sid[i], "/", topps$year[i], "-Topps?Note=ASR")
  page <- read_html(asr_url)
  x <- page %>%
    html_table()
  if (length(x) == 6) {
    tmp <- x[[6]] %>% 
      select(X4, X6, X8) %>% 
      rename(number = X4, 
             name = X6, 
             team = X8)
    tmp$name <- gsub(", RC", "", tmp$name)
    tmp$name <- gsub(" RC,", "", tmp$name)
    tmp$name <- gsub(" ASR", "", tmp$name)
    tmp$name <- gsub("Topps 196[0-9] All-Star Rookie", "", tmp$name)
    asr[[i]] <- tmp
    cards <- page %>% 
      html_nodes("td > a") %>%
      html_attr("href")
    cards <- unique(cards[grepl("ViewCard", cards)])
    asr[[i]]$card_url <- cards
  }
}

topps$n_asr <- sapply(asr, function(x) {ifelse(is.null(x), 0, nrow(x))})


# write the cards out to a file, and we'll go there to correct the names
# and filter out the image variations and such...


asr_all <- bind_rows(asr)
asr_all$year <- rep(1952:2021, topps$n_asr)
asr_all <- select(asr_all, year, number:card_url)

fwrite(asr_all, file = "data/asr_all_raw.csv")


asr <- fread("data/asr_all_corrected.csv", data.table = FALSE)

asr %>% 
  group_by(year) %>% 
  summarize(n = n(), 
            n_uniq = n_distinct(number)) %>% 
  as.data.frame()












