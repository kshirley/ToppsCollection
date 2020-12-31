################################################################################

# read a URL from an ebay listing from seller = 'alyssastrunk' and get a 
# data frame with the list of cards available:

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(rvest)
library(tidyr)
library(stringr)
library(googlesheets4)
library(httr)

# get the list of cards that I want to price:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)

# combine all the cards into one data frame:
type <- rep(c("first_ballot", "bbwaa", "vet_or_other"), 
            c(nrow(cards1), nrow(cards2), nrow(cards3)))
my_cards <- bind_rows(cards1, cards2, cards3)
my_cards <- replace_na(my_cards, list(own = 0))
my_cards$type <- type


### function to get cards from 'alyssastrunk' offers:
get_cards <- function(url, years, cards = my_cards) {
  # get the list of cards:
  page <- read_html(url)
  
  x <- page %>%
    html_nodes("iframe") %>%
    html_attr(name = "src")
  
  y <- read_html(x) %>% 
    html_nodes("p") %>%
    html_text()
  
  keys <- paste(years, "Topps", sep = " ")
  nl <- vector("list", length(keys))
  for (i in seq_along(keys)) {
    g <- which(y == keys[i])
    number_list <- strsplit(y[g + 1], split = " ") %>% unlist() %>%
      as.numeric()
    if (sum(is.na(number_list)) > 0) {
      min_char <- min(which(is.na(number_list)))
      number_list <- number_list[1:(min_char - 1)]
    }
    nl[[i]] <- number_list  
  }
  
  # get a data frame for this offer:
  offer <- data.frame(year = as.integer(rep(gsub("[a-zA-Z ]", "", keys), sapply(nl, length))), 
                      number = unlist(nl), 
                      ebay = 1)
  
  # save as a data frame:
  df <- cards %>%
    filter(own == 0, year %in% years) %>%
    mutate(number = as.integer(number)) %>%
    select(name:own, type) %>%
    left_join(offer, by = c("year", "number")) %>%
    replace_na(list(ebay = 0))
  
  return(df)
}


# 2003:2007
# 30 for 1.75
url_03_07 <- "https://www.ebay.com/itm/2003-2004-2005-2006-2007-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-30/112392463237"
df <- get_cards(url = url_03_07, years = 2003:2007)

df %>%
  group_by(type) %>%
  summarize(n = n(), 
            ebay = sum(ebay))

#   type             n  ebay
# 1 bbwaa           36    14
# 2 first_ballot    71    17
# 3 vet_or_other   101    50
# 81 cards

# get each year individually
df %>% filter(ebay == 1, year == 2003) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2004) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2005) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2006)
df %>% filter(ebay == 1, year == 2007) %>% pull(number) %>% sort()


# 08-12
url_08_12 <- "https://www.ebay.com/itm/2008-2009-2010-2011-2012-Topps-UD-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-30/112280511043"
df <- get_cards(url = url_08_12, years = 2008:2012)

df %>%
  group_by(year, type) %>%
  summarize(n = n(), 
            ebay = sum(ebay))

#   type             n  ebay
# 1 bbwaa           11     2
# 2 first_ballot    78    12
# 3 vet_or_other    94    42
# 56 cards

df %>% filter(ebay == 1, year == 2008) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2009) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2010) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2011) %>% pull(number) %>% sort()
df %>% filter(ebay == 1, year == 2012) %>% pull(number) %>% sort()




# stars:
url_stars <- "https://www.ebay.com/itm/1994-1995-1996-1997-Topps-Baseball-Star-Cards-Complete-Your-Set-U-Pick-1/114262492637"
df <- get_cards(url = url_stars, years = 1994:1997)

df %>%
  group_by(year, type) %>%
  summarize(n = n(), 
            ebay = sum(ebay))

df %>% filter(ebay == 1) %>% arrange(year, number)
# 
#   type             n  ebay
# 1 bbwaa           37     2
# 2 first_ballot    14     9
# 3 vet_or_other   101    12


# 1994 Topps: 200 600 700
# 1995 Topps: 199 295 347 360 397 466 472 588
# 1996 Topps: 13 145 197 205 300 435
# 1997 Topps: 1 13 62 370 400










# url <- "https://www.ebay.com/itm/2013-2014-2015-2016-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-30-Ex-Mt/113904528018"
# years <- 2013:2016
# 30 for 1.75

#   type             n  ebay
# 1 first_ballot    39     3
# 2 vet_or_other    65    18
# 21 cards

# url <- "https://www.ebay.com/itm/1974-1975-1976-1977-1978-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-10-Ex/113895835639"
# years <- 1974:1978
# none

# url <- "https://www.ebay.com/itm/1979-1980-1981-1982-Topps-Donruss-Baseball-Cards-U-Pick-20-Complete-Your-Set-Lot/112238981182"
# years <- 1979:1982
# 20 for 2.99

#   type             n  ebay
# 1 bbwaa           25     2
# 2 first_ballot     8     1
# 3 vet_or_other    20     3
# 6 cards

# url <- "https://www.ebay.com/itm/1987-1988-1989-1990-1991-1992-Topps-Baseball-Cards-Complete-Your-Set-U-Pick-50/112208035797"
# years <- 1987:1992
# none



url <- "https://www.ebay.com/itm/1993-1994-1995-1996-1997-Topps-Complete-Your-Set-Lot-Baseball-Cards-U-Pick-30/112392449595"
years <- 1993:1997





################################################################################

# [2] Different URL:
#     manually copied into an excel workbook

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(rvest)
library(tidyr)
library(stringr)
library(googlesheets4)
library(httr)

# read in the offer from here:
# https://www.ebay.com/itm/1981-1982-1983-1984-1985-Topps-Baseball-Complete-Your-Set-U-pick-20-MINT-Cards/133520587908?_trkparms=aid%3D111001%26algo%3DREC.SEED%26ao%3D1%26asc%3D225086%26meid%3Df6e5e70a47364cfdb3642c1d760f645c%26pid%3D100675%26rk%3D1%26rkt%3D8%26mehot%3Dnone%26sd%3D133520587908%26itm%3D133520587908%26pmt%3D0%26noa%3D1%26pg%3D2380057&_trksid=p2380057.c100675.m4236&_trkparms=pageci%3A0656e5ae-3e17-11eb-9314-ca611558d845%7Cparentrq%3A619c89db1760acc6c59a57e3fff87478%7Ciid%3A1

offer <- fread("data/pick_your_set_81-82.csv", data.table = FALSE)
offer$ebay <- 1

# get the list of cards that I want to price:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)

# combine all the cards into one data frame:
type <- rep(c("first_ballot", "bbwaa", "vet_or_other"), 
            c(nrow(cards1), nrow(cards2), nrow(cards3)))
my_cards <- bind_rows(cards1, cards2, cards3)
my_cards <- replace_na(my_cards, list(own = 0))
my_cards$type <- type
my_cards$type <- factor(my_cards$type, 
                        levels = c("first_ballot", "bbwaa", "vet_or_other"))

my_cards %>%
  filter(year %in% 1981:1982, own == 0) %>%
  select(name:own, type) %>%
  arrange(year, type, number) %>%
  as.data.frame()

my_cards %>%
  filter(year %in% 1981:1982, own == 0) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  arrange(year, type, number) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))

  






################################################################################

# read a URL from an ebay listing from seller = 'alyssastrunk' and get a 
# data frame with the list of cards available:

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(rvest)
library(tidyr)
library(stringr)
library(googlesheets4)
library(httr)

# get the list of cards that I want to price:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)

# combine all the cards into one data frame:
type <- rep(c("first_ballot", "bbwaa", "vet_or_other"), 
            c(nrow(cards1), nrow(cards2), nrow(cards3)))
my_cards <- bind_rows(cards1, cards2, cards3)
my_cards <- replace_na(my_cards, list(own = 0))
my_cards$type <- type


# set the URL:
url <- "https://www.ebay.com/itm/2000-2001-2002-2003-Topps-Baseball-Cards-Complete-Your-Set-YOU-PICK-25-List/174522047098"
years <- 2000:2003

# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("font") %>%
  html_text() %>%
  str_trim()


keys <- paste(years, "TOPPS", sep = " ")
nl <- vector("list", length(keys))
for (i in seq_along(keys)) {
  g <- max(which(y == keys[i]))
  tmp <- gsub("\n", " ", y[g + 1])
  number_list <- strsplit(tmp, split = ",") %>% unlist() %>%
    str_trim() %>% strsplit(split = " ") %>% unlist() %>%
    as.numeric()
  if (sum(is.na(number_list)) > 0) {
    min_char <- min(which(is.na(number_list)))
    number_list <- number_list[1:(min_char - 1)]
  }
  nl[[i]] <- number_list  
}

# doesn't for the third one:
y <- read_html(x) %>%
  html_nodes("p") %>%
  html_text() %>%
  str_trim()

tmp <- gsub("\n", " ", y[18])
number_list <- strsplit(tmp, split = ",") %>% unlist() %>%
  str_trim() %>% strsplit(split = " ") %>% unlist() %>%
  as.numeric()
nl[[3]] <- number_list


# get a data frame for this offer:
offer <- data.frame(year = as.integer(rep(gsub("[a-zA-Z ]", "", keys), sapply(nl, length))), 
                    number = unlist(nl), 
                    ebay = 1) %>%
  unique()

# save as a data frame:
df <- my_cards %>%
  filter(own == 0, year %in% years) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))

filter(df, ebay == 1)


df <- df %>%
  filter(ebay == 1) %>%
  arrange(year, number)

# remove:
#  Matt Williams 2002    139  0.10   0 vet_or_other    1
# Carlos Delgado 2002    150  0.23   0 vet_or_other    1
#   Fred McGriff 2002    385  0.23   0 vet_or_other    1

filter(df, ebay == 1, year == 2000) %>% pull(number) %>% paste(collapse = ", ")
filter(df, ebay == 1, year == 2001) %>% pull(number) %>% paste(collapse = ", ")
filter(df, ebay == 1, year == 2002) %>% pull(number) %>% paste(collapse = ", ")
filter(df, ebay == 1, year == 2003) %>% pull(number) %>% paste(collapse = ", ")



# https://www.ebay.com/itm/2000-2001-2002-2003-Topps-Baseball-Cards-Complete-Your-Set-YOU-PICK-25-List/174522047098
# 
# First 25:
# 2000 Topps: 275, 331, 355, 363, 364, 395, 425, 440
# 2001 Topps: 110
# 2002 Topps: 1, 12, 20, 25, 26, 40, 45, 50, 70, 72, 75, 85, 99, 100, 110, 125
# 
# Second 25:
# 2002 Topps: 165, 175, 188, 218, 225, 240, 250, 424, 425, 450, 460, 500, 550, 560, 590
# 2003 Topps: 50, 60, 80, 98, 100, 130, 190, 200, 251, 500






