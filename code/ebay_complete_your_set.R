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
# url <- "https://www.ebay.com/itm/2000-2001-2002-2003-Topps-Baseball-Cards-Complete-Your-Set-YOU-PICK-25-List/174522047098"
# years <- 2000:2003

url <- "https://www.ebay.com/itm/1987-1988-1989-1990-1991-1992-Topps-Baseball-Cards-Complete-Your-Set-U-Pick-50/112208035797"
years <- 1987:1992

# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("font") %>%
  html_text() %>%
  str_trim()


keys <- paste(years, "Topps", sep = " ")
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




### 1987-1992 Alyssastrunk:

# url <- "https://www.ebay.com/itm/1987-1988-1989-1990-1991-1992-Topps-Baseball-Cards-Complete-Your-Set-U-Pick-50/112208035797"
# years <- c(1990, 1992)

url <- "https://www.ebay.com/itm/1974-1975-1976-1977-1978-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-10-Ex/113895835639?hash=item1a84b887f7:g:0GcAAOSwaNRcs8Hk"
years <- 1974:1978

url <- "https://www.ebay.com/itm/1993-1994-1995-1996-1997-Topps-Complete-Your-Set-Lot-Baseball-Cards-U-Pick-30/112392449595?hash=item1a2b1cae3b:g:YNYAAOSwAKxcv7KS"
years <- 1993:1997

url <- "https://www.ebay.com/itm/2013-2014-2015-2016-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-30-Ex-Mt/113904528018?hash=item1a853d2a92:g:LlYAAOSwjaBcxlWD"
years <- 2013:2016

url <- "https://www.ebay.com/itm/1998-1999-2000-2002-Topps-Baseball-Cards-Complete-Your-Set-Lot-U-Pick-30/112425433078?hash=item1a2d13f7f6:g:MtIAAOSwFmtcwQWm"
years <- c(1998:2000, 2002)
  
# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("p") %>%
  html_text() %>%
  str_trim()

keys <- paste(years, "Topps", sep = " ")
# keys[1] <- "1974 topps"
# keys[5] <- "1978 topps"
nl <- vector("list", length(keys))
for (i in seq_along(keys)) {
  g <- max(which(y == keys[i]))
  nl[[i]] <- strsplit(y[g+1], split = "[[:space:]]+")[[1]]
}
  
# get a data frame for this offer:
offer <- data.frame(year = rep(years, sapply(nl, length)), 
                    number = unlist(nl), 
                    ebay = 1) %>%
  unique()

offer$number <- as.integer(offer$number)

# save as a data frame:
df <- my_cards %>%
  filter(own == 0, year %in% years) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))

filter(df, ebay == 1)
# 54 for 93-97 [x - done]
# 21 for 13-17
# 44 for 99-02


fwrite(filter(df, ebay == 1), file = "data/alyssa_93_97.csv")
fwrite(filter(df, ebay == 1), file = "data/alyssa_2013_2017.csv")
fwrite(filter(df, ebay == 1), file = "data/alyssa_99_02.csv")

# read the lists back in:
a1 <- fread("data/alyssa_93_97.csv", data.table = FALSE)
a2 <- fread("data/alyssa_2013_2017.csv", data.table = FALSE)
a3 <- fread("data/alyssa_99_02.csv", data.table = FALSE)


z <- cl %>%
  inner_join(offer, by = c("year", "number"))

filter(z, grepl("Gwynn", name))
# 3  1996      1         Tony Gwynn STP    
# 1  1997    465         Barry Bonds SH    
# 3  1997    463        Paul Molitor SH    
# 3  1996      3        Greg Maddux STP
# 2  1996      2        Mike Piazza STP
# 5  1996      5       Larry Walker STP

add <- filter(z, grepl("Tony Gwynn STP", name)) %>%
  bind_rows(filter(z, grepl("Barry Bonds SH", name))) %>%
  bind_rows(filter(z, grepl("Greg Maddux STP", name))) %>%
  bind_rows(filter(z, grepl("Mike Piazza STP", name))) %>%
  bind_rows(filter(z, grepl("Larry Walker STP", name))) %>%
  bind_rows(filter(z, grepl("Paul Molitor SH", name)))
  

# filter(z, grepl("Bonds", name))
# filter(z, grepl("Anderson", name))
# filter(z, number == 463)
# filter(cl, year == 1996) %>% head(10)


a1 %>%
  bind_rows(add) %>%
  arrange(year, number) %>%
  select(year, number, name) %>%
  fwrite("data/alyssa_order_94_97.txt", sep = "\t")

z <- cl %>%
  inner_join(offer, by = c("year", "number"))

filter(z, grepl("Gwynn", name))
filter(z, grepl("Burrell", name))
filter(z, grepl("Burrell", name))
filter(z, grepl("Zito", name))
filter(z, grepl("Giambi", name))
filter(z, grepl("Wood", name))
filter(z, grepl("Tejada", name))

filter(z, grepl("Giambi", name))

filter(z, grepl("ASR", name))
filter(z, grepl("DP", name))

# need to add 17 cards:

# 1 2002     99 Tony Gwynn    1
# 1 2002    545 Pat Burrell    1
# 1 2002    455 Barry Zito    1
# 1 1998    176  Jason Giambi    1
# 2 1999    324  Jason Giambi    1
# 4 1999    204  Kerry Wood HL    1
# 5 1999    446  Kerry Wood SK    1
# 1 2002    243       Torii Hunter    1
# 1 1999    352 Miguel Tejada    1
# 2 2002    585 Miguel Tejada    1
# 1 1998    497   Tony Saunders ASR    1
# 2 1999     84     Mike Caruso ASR    1
# 3 1999    110      Ben Grieve ASR    1
# 4 1999    155 Magglio Ordonez ASR    1
# 5 1999    391     Bobby Smith ASR    1
# 6 1999    417    Miguel Cairo ASR    1
# 7 1999    419   Jesus Sanchez ASR    1

extras <- fread("data/alyssa_99_02_order_extras.csv", data.table = FALSE)
names(extras) <- c("year", "number", "name")

a3 %>%
  select(year, number, name) %>%
  bind_rows(extras) %>%
  arrange(year, number) %>%
  fwrite("data/alyssa_order_99_02.txt", sep = "\t")


o1 <- fread("data/alyssa_order_94_97.txt", data.table = FALSE)
o2 <- fread("data/alyssa_order_99_02.txt", data.table = FALSE)


o1 %>% select(-name)

o1 %>%
  group_by(year) %>%
  summarize(paste(number, collapse = ", ")) %>%
  as.data.frame() %>%
  fwrite("data/alyssa_o1.txt", sep = "\t")

o2 %>%
  group_by(year) %>%
  summarize(paste(number, collapse = ", ")) %>%
  as.data.frame() %>%
  fwrite("data/alyssa_o2.txt", sep = "\t")


################################################################################

# [4] Read in 1990, 1992 availability from seller:
#     https://www.ebay.com/itm/1990-1991-1992-Topps-Baseball-Cards-Complete-Your-Set-U-Pick-15-Cards-NM-MT/333774117708?_trkparms=aid%3D555021%26algo%3DPL.SIMRVI%26ao%3D1%26asc%3D20190711100440%26meid%3D15fc71a5872f4f5fa75f066bf8391261%26pid%3D100752%26rk%3D5%26rkt%3D17%26mehot%3Dpf%26sd%3D233850924989%26itm%3D333774117708%26pmt%3D1%26noa%3D0%26pg%3D2047675%26algv%3DSimplRVIAMLv5WebWithPLRVIOnTopCombiner&_trksid=p2047675.c100752.m1982
#     'etsfan1'

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

offer <- fread("data/pick_your_card_90_92.csv", data.table = FALSE)
v90 <- offer$num_1990[!is.na(offer$num_1990)]
v92 <- offer$num_1992[!is.na(offer$num_1992)]
offer_raw <- offer
offer <- data.frame(year = rep(c(1990, 1992), c(length(v90), length(v92))), 
                    number = c(v90, v92))
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
  filter(year %in% c(1990, 1992), own == 0) %>%
  select(name:own, type) %>%
  arrange(year, type, number) %>%
  as.data.frame()

o1 <- my_cards %>%
  filter(year %in% c(1990, 1992), own == 0) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  arrange(year, type, number) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))

o1


# check out here:
# https://www.ebay.com/itm/1990-Topps-You-Pick-30-Cards-Complete-Your-Set-Lot/233850924989?hash=item367299efbd:g:V60AAOSwB09YNjp~

other <- data.frame(year = rep(1990, sum(!is.na(offer_raw$other_1990))), 
                    number = offer_raw$other_1990[!is.na(offer_raw$other_1990)])
other$ebay <- 1

o2 <- my_cards %>%
  filter(year == 1990, own == 0) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  arrange(year, type, number) %>%
  left_join(other, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))


both <- o1 %>%
  left_join(select(o2, year, number, ebay_other = ebay)) %>%
  replace_na(list(ebay_other = 0))

sum(both$ebay + both$ebay_other > 0)
# 15 out of 29 are covered by these two offers

sum(both$ebay)
# 12 are covered by this one offer




### Now try scraping the URL for seller 'etlfan1'
url <- "https://www.ebay.com/itm/2004-Topps-Baseball-Series-1-and-2-Lot-Complete-Your-Set-U-You-Pick-2-Cards/333809206109?hash=item4db894435d:g:4zEAAOSwQSdfxYlr"
years <- 2004

# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("font") %>%
  html_text() %>%
  str_trim()

# get the number list:
start <- which(y == "Cards available are")

raw <- paste(unlist(y[(start + 1):length(y)]), collapse = " ")

nl <- gsub("[[:punct:]]", " ", raw) %>% 
  str_trim() %>%
  strsplit(split = "[[:space:]]+") %>%
  .[[1]] %>% 
  as.integer()

# get a data frame for this offer:
offer <- data.frame(year = years, 
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





### now try scraping to get lists from seller 'ledsimpson'
url <- "https://www.ebay.com/itm/2010-Topps-Series-1-2-and-Update-Base-You-Pick-30-Cards-Complete-Your-Set-Lot/333850439466?hash=item4dbb096f2a:g:qpIAAOSwi8xZ99dh"
years <- 2010
n_blocks <- 1

url <- "https://www.ebay.com/itm/2012-Topps-Series-1-2-Update-Base-You-Pick-30-Cards-Complete-Your-Set-Lot/233856396895?hash=item3672ed6e5f:g:zZwAAMXQwKdRbYsJ"
years <- 2012
n_blocks <- 2

url <- "https://www.ebay.com/itm/2014-Topps-Series-1-and-2-You-Pick-25-Cards-Complete-Your-Set-Lot/333850439435?hash=item4dbb096f0b:g:IrwAAOSwo8hTm7p5"
years <- 2014
n_blocks <- 2

url <- "https://www.ebay.com/itm/2016-Topps-Series-1-and-2-You-Pick-15-Cards-Complete-Your-Set-Lot/333850439422?hash=item4dbb096efe:g:x3IAAOSwepJXawFp"
years <- 2016
n_blocks <- 1

url <- "https://www.ebay.com/itm/2015-Topps-Series-1-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233850924976?hash=item367299efb0:g:lE4AAOSwux5YNdx2"
years <- 2015
n_blocks <- 1

url <- "https://www.ebay.com/itm/1992-Topps-Baseball-You-Pick-20-Cards-Complete-Your-Set-Lot/333854755162?hash=item4dbb4b495a:g:NBgAAOSwrEJfuHq1"
years <- 1992
n_blocks <- 1

url <- "https://www.ebay.com/itm/2018-Topps-Series-1-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233850924956?hash=item367299ef9c:g:NecAAOSw61Va16JX"
years <- 2018
n_blocks <- 1

url <- "https://www.ebay.com/itm/2017-Topps-Series-2-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233850924954?hash=item367299ef9a:g:MbsAAOSwbopZSyfY"
years <- 2017
n_blocks <- 1

url <- "https://www.ebay.com/itm/2015-Topps-Series-2-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233850924943?hash=item367299ef8f:g:6B4AAOSw~gRVgfpl"
years <- 2015
n_blocks <- 1

url <- "https://www.ebay.com/itm/2012-Topps-Series-2-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233850924958?hash=item367299ef9e:g:FmEAAOSwKhta34p1"
years <- 2012
n_blocks <- 1

url <- "https://www.ebay.com/itm/2020-Topps-Series-1-and-2-You-Pick-25-Cards-Complete-Your-Set-Lot/233857079180?hash=item3672f7d78c:g:aYEAAOSwdZ5fIgmm"
years <- 2020
n_blocks <- 2

url <- "https://www.ebay.com/itm/2009-Topps-Baseball-You-Pick-10-Cards-Complete-Your-Set-Lot/331544926721?hash=item4d319e1a01:g:cOoAAOxyu0BRfpT~"
years <- 2009
n_blocks <- 1

url <- "https://www.ebay.com/itm/2018-Topps-Series-2-Base-You-Pick-25-Cards-Complete-Your-Set-Lot/233856396894?hash=item3672ed6e5e:g:K3kAAOSwkSxbKCP2"
years <- 2018
n_blocks <- 1



# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("p") %>%
  html_text() %>%
  str_trim()

# count commas to identify card lists:
commas <- as.numeric(sapply(y, str_count, ","))
indices <- order(commas, decreasing = TRUE)[1:n_blocks]

nl <- vector("list", length(indices))
for (i in seq_along(indices)) {
  nl[[i]] <- strsplit(y[indices[i]], split = ",")[[1]] %>%
    str_trim() %>% as.integer()
}

# get a data frame for this offer:
offer <- data.frame(year = rep(rep(years, n_blocks), sapply(nl, length)), 
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
#  2 for 1992 (pick 20 for $1)
#  0 for 2010 series 1-2 
#  7 for 2012 series 1-2 (pick 30 for $1)
#  7 for 2012 series 2 (pick 25 for $1)
#  2 for 2014 series 1-2 (pick 25 for $1)
# 16 for 2015 series 1 (pick 25 for $1)
#  5 for 2015 series 2 (pick 25 from $1)
#  5 for 2016 series 1-2 (pick 15 for $1)
#  2 for 2017 series 2 (pick 25 for $1)
#  3 for 2018 series 1 (pick 25 for $1)
#  6 for 2020 series 1-2 (pick 25 for $2)
#  3 for 2018 series 2 (pick 25 for $1)





filter(my_cards, year == 2011, own == 0) %>%
  select(name:remaining_price) %>%
  mutate(num = as.integer(number)) %>%
  arrange(num)


# Now, do 2015 series 1 against the checklist to fill out my order:
cl <- fread("data/checklists_1952_2020.csv", data.table = FALSE)

x <- filter(cl, year == 2015, number %in% nl[[1]])

grep("Abreu", x$name)
grep("Wong", x$name)
grep("Hamilton", x$name)
grep("Santana", x$name)


filter(cl, year == 2015, grepl("Hamilton", name))
filter(cl, year == 2015, grepl("Wong", name))

x <- filter(cl, year == 2015, grepl("ASR", name))



### Now 'dimeboxcards'

url <- "https://www.ebay.com/itm/2020-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224001891020?hash=item34278d9acc:g:o1IAAOSwNWxes59m"
years <- 2020
n_blocks <- 1

url <- "https://www.ebay.com/itm/2020-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224142529079?hash=item342fef9237:g:pIgAAOSwqDJfT-Ei"
years <- 2020
n_blocks <- 1

url <- "https://www.ebay.com/itm/1990-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223463882085?hash=item34077c3d65:g:eJsAAOSwbURcnUAG"
years <- 1990
n_blocks <- 1

url <- "https://www.ebay.com/itm/2016-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223522189186?hash=item340af5ef82:g:jaoAAOSwrLVc4Nom"
years <- 2016
n_blocks <- 1

url <- "https://www.ebay.com/itm/2017-Topps-Series-One-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223346517550?hash=item34007d662e:g:U8YAAOSw~VBcTR5e"
years <- 2017
n_blocks <- 1

url <- "https://www.ebay.com/itm/2019-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223577887013?hash=item340e47d125:g:pm4AAOSwTU5dIR5G"
years <- 2019
n_blocks <- 1

url <- "https://www.ebay.com/itm/2012-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List-Series-1-2/223377769336?hash=item34025a4378:g:pyQAAOSwmuFcXh6Z"
years <- 2012
n_blocks <- 1

url <- "https://www.ebay.com/itm/1981-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224176185065?hash=item3431f11ee9:g:cF4AAOSwomdfdORi"
years <- 1981
n_blocks <- 1

url <- "https://www.ebay.com/itm/2008-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List-Series-1-2/223362892671?hash=item340177437f:g:~W0AAOSwJiBcVniX"
years <- 2008
n_blocks <- 1

url <- "https://www.ebay.com/itm/2018-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223514832806?hash=item340a85afa6:g:qDMAAOSwu5dc2gwr"
years <- 2018
n_blocks <- 1

url <- "https://www.ebay.com/itm/2002-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223505508935?hash=item3409f76a47:g:kUkAAOSwrJRczxeY"
years <- 2002
n_blocks <- 1

url <- "https://www.ebay.com/itm/1992-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223395126342?hash=item3403631c46:g:g-AAAOSwUDZcZ3jN"
years <- 1992
n_blocks <- 1

url <- "https://www.ebay.com/itm/1999-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223581398362?hash=item340e7d655a:g:7XgAAOSwyNtdJUA5"
years <- 1999
n_blocks <- 1

url <- "https://www.ebay.com/itm/2011-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223372870081?hash=item34020f81c1:g:4Y4AAOSwJ8hcW4OP"
years <- 2011
n_blocks <- 1

# get the list of cards:
page <- read_html(url)

x <- page %>%
  html_nodes("iframe") %>%
  html_attr(name = "src")

y <- read_html(x) %>%
  html_nodes("p") %>%
  html_text() %>%
  str_trim()

# count commas to identify card lists:
commas <- as.numeric(sapply(y, str_count, ","))
indices <- order(commas, decreasing = TRUE)[1:n_blocks]

nl <- vector("list", length(indices))
for (i in seq_along(indices)) {
  nl[[i]] <- strsplit(y[indices[i]], split = ",")[[1]] %>%
    str_trim() %>% as.integer()
}

# get a data frame for this offer:
offer <- data.frame(year = rep(rep(years, n_blocks), sapply(nl, length)), 
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

df

filter(df, ebay == 1)
# 12 for 2020 series 1 (25 for $2.50)
#  5 for 2020 series 2 (25 for $2.50)
# Wow, that's 17 out of 20. Just missing Trout, Bellinger, and Soto

# 22 for 1999 (25 for $2.50)

#  1 for 1990.
#  8 for 2016 series 1
#  6 for 2017 series 1
#  4 for 2019 series 2
#  6 for 1981
#  9 for 2008
#  3 for 2018 series 2
#  3 for 2002
#  9 for 1992 (25 for $2.50)


filter(my_cards, year == 1990, own == 0) %>% 
  select(name:remaining_price) %>%
  mutate(number = as.numeric(number)) %>%
  arrange(number)

filter(cl, year == 1990, grepl("Radin", name))






### Now 'klively331j'

url13 <- "https://www.ebay.com/itm/2013-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393093586517?hash=item5b86341e55:g:oy8AAOSw8GZfwSFG"
url14 <- "https://www.ebay.com/itm/2014-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393099641956?hash=item5b86908464:g:7cYAAOSwVb5fwWbj"
url15 <- "https://www.ebay.com/itm/2015-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393072140547?hash=item5b84ece103:g:fgoAAOSwW8VfxA32"
url16 <- "https://www.ebay.com/itm/2016-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393098264722?hash=item5b867b8092:g:dGUAAOSwBYpeJMzz"
url17 <- "https://www.ebay.com/itm/2017-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393020620067?hash=item5b81dabd23:g:lwsAAOSwCtFeGOTa"
url18 <- "https://www.ebay.com/itm/2018-Topps-Baseball-Series-1-2-You-Pick-25-Cards-Complete-Your-Set-Large-List/393084051643?hash=item5b85a2a0bb:g:PvcAAOSw2B1eD2JZ"


url_vec <- c(url13, url14, url15, url16, url17, url18)
card_list <- vector("list", 6)
for (i in 1:6) {
  url <- url_vec[i]
  years <- 2012 + i
  n_blocks <- 1
  
  # get the list of cards:
  page <- read_html(url)
  
  x <- page %>%
    html_nodes("iframe") %>%
    html_attr(name = "src")
  
  y <- read_html(x) %>%
    html_nodes("span") %>%
    html_text() %>%
    str_trim()
  
  # count commas to identify card lists:a
  commas <- as.numeric(sapply(y, str_count, ","))
  indices <- order(commas, decreasing = TRUE)[1:n_blocks]
  
  nl <- vector("list", length(indices))
  for (j in seq_along(indices)) {
    nl[[j]] <- strsplit(y[indices[j]], split = ",")[[1]] %>%
      str_replace("\n", "") %>%
      str_replace("[[:punct:]]", "") %>%
      str_trim() %>% 
      as.integer()
  }
  
  # get a data frame for this offer:
  offer <- data.frame(year = rep(rep(years, n_blocks), sapply(nl, length)), 
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
  
  df
  
  card_list[[i]] <- filter(df, ebay == 1)
}

arrange(bind_rows(card_list), name)



url19a <- "https://www.ebay.com/itm/2019-Topps-Baseball-Series-1-You-Pick-25-Cards-Complete-Your-Set-Large-Selection/392905957333?hash=item5b7b051fd5:g:lkgAAOSwnd9d58kg"
url19b <- "https://www.ebay.com/itm/2019-Topps-Baseball-Series-2-You-Pick-25-Cards-Complete-Your-Set-Large-Selection/392868710680?hash=item5b78ccc918:g:gNAAAOSwAfheAVF6"
url20a <- "https://www.ebay.com/itm/2020-Topps-Baseball-Series-1-You-Pick-25-Cards-Complete-Your-Set-Large-Selection/393083911053?hash=item5b85a07b8d:g:FFIAAOSwsWpeV94G"
url20b <- "https://www.ebay.com/itm/2020-Topps-Baseball-Series-2-You-Pick-25-Cards-Complete-Your-Set-Large-Selection/393097315987?hash=item5b866d0693:g:wMkAAOSw0OVgAbSb"
year_vec <- c(2019, 2019, 2020, 2020)
url_vec <- c(url19a, url19b, url20a, url20b)

nl <- vector("list", 4)
for (i in 1:4) {
  # get the list of cards:
  page <- read_html(url_vec[i])
  x <- page %>%
    html_nodes("iframe") %>%
    html_attr(name = "src")
  y <- read_html(x) %>%
    html_nodes("span") %>%
    html_text() %>%
    str_trim()

  if (i < 4) {
    ind <- which(nchar(y) > 100)
    nl[[i]] <- lapply(y[ind], strsplit, split = "\n") %>%
      unlist() %>%
      strsplit(split = " ") %>%
      sapply(function(x) x[1]) %>%
      as.integer() %>%
      unique()
  } else {
    y <- y[y != ""]
    nl[[i]] <- y %>%
      strsplit(split = " ") %>%
      sapply(function(x) x[1]) %>%
      as.integer()
  }
}

# get a data frame for this offer:
newer_cards <- vector("list", 4)
for (i in 1:4) {
  offer <- data.frame(year = year_vec[i], 
                      number = nl[[i]], 
                      ebay = 1) %>% unique()
  # save as a data frame:
  df <- my_cards %>%
    filter(own == 0, year %in% year_vec[i]) %>%
    mutate(number = as.integer(number)) %>%
    select(name:own, type) %>%
    left_join(offer, by = c("year", "number")) %>%
    replace_na(list(ebay = 0))
  newer_cards[[i]] <- filter(df, ebay == 1)
}

bind_rows(newer_cards)

all <- bind_rows(card_list, newer_cards)
all <- arrange(all, year, number)
all

select(all, year, number, name)




filter(my_cards, year == 1992, own == 0) %>%
  select(name:remaining_price) %>%
  mutate(number = as.numeric(number)) %>%
  arrange(number)

filter(my_cards, year == 1990, own == 0) %>%
  select(name:remaining_price) %>%
  mutate(number = as.numeric(number)) %>%
  arrange(number)

lot_1990 <- fread("data/1990_lot_klively331j.csv", data.table = FALSE)
lot_1990$year <- 1990
lot_1990$name <- paste(lot_1990$V2, lot_1990$V3, sep = " ")
lot_1990 <- select(lot_1990, year, number = V1, name)


out <- bind_rows(select(all, year, number, name), lot_1990)
out <- filter(out, !(year == 2019 & name == "Ichiro Suzuki"), 
              !(year == 2020 & name == "Ichiro Suzuki"))

out %>%
  group_by(year) %>%
  summarize(paste(number, collapse = ", ")) %>%
  fwrite("data/klively_list.txt", sep = "\t")


filter(cl, year == 2014, number %in% c(100, 103, 113, 390, 557))


v_05 <- c(298, 302, 303, 306, 313, 330, 335, 349, 350, 351, 353, 354, 368, 440, 600, 668, 682, 686, 688, 689, 690, 691, 700, 731, 733)

v_06 <- c(7, 246, 309, 310, 319, 326, 355, 387, 500, 616, 617, 618, 623, 632, 635, 636, 639, 641, 649, 653)

offer <- data.frame(year = rep(2005:2006, c(length(v_05), length(v_06))), 
                    number = c(v_05, v_06), 
                    ebay = 1)

my_cards %>%
  filter(own == 0, year %in% (2005:2006)) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)






################################################################################

# Get the checklist for each year of topps since 1990 from 
# tcdb.com

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(rvest)
library(tidyr)
library(stringr)
library(googlesheets4)
library(httr)

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
topps <- data.frame(year = 1952:2020, sid = sid[1:69])
n_years <- nrow(topps)

checklist <- vector("list", n_years)
for (i in 1:n_years) {
  print(i)
  Sys.sleep(1)
  url <- paste0("https://www.tcdb.com/PrintChecklist.cfm/sid/", topps$sid[i])
  page <- read_html(url)
  
  # extract number and name:
  x <- page %>%
    html_nodes("font") %>%
    html_text()
  y <- lapply(x, strsplit, split = "[[:space:]]+")
  number <- sapply(y, function(x) x[[1]][1])
  desc <- sapply(y, function(x) paste(x[[1]][-1], collapse = " "))
  tmp <- data.frame(number = number, name = desc) %>%
    arrange(number)
  
  # remove alphabetical characters from numbers, and then dupes:
  tmp$number <- gsub("[A-Za-z]", "", tmp$number)
  tmp <- tmp %>% filter(!duplicated(number))

  # remove header row:
  tmp <- tmp %>% filter(!grepl("best results", name))
  
  tmp$number <- as.integer(tmp$number)
  tmp <- arrange(tmp, number)
  checklist[[i]] <- tmp
}

lapply(checklist, tail, 3)
  
lapply(checklist[1:5], tail, 3)

sapply(checklist, function(x) sum(is.na(x[, 1])))

cl <- bind_rows(checklist)
cl$year = rep(1952:2020, sapply(checklist, nrow))
cl <- select(cl, year, number, name)

filter(cl, is.na(number))

cl <- filter(cl, !is.na(number))

fwrite(cl, file = "data/checklists_1952_2020.csv")




# # try one year:
# year <- 2020
# base_url <- "http://baseballcardpedia.com/index.php/"
# 
# url <- paste0(base_url, year, "_Topps")




# # get the list of cards that I want to price:
# google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
# cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
# cards1 <- as.data.frame(cards1)
# cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
# cards2 <- as.data.frame(cards2)
# cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
# cards3 <- as.data.frame(cards3)
# 
# # combine all the cards into one data frame:
# type <- rep(c("first_ballot", "bbwaa", "vet_or_other"), 
#             c(nrow(cards1), nrow(cards2), nrow(cards3)))
# my_cards <- bind_rows(cards1, cards2, cards3)
# my_cards <- replace_na(my_cards, list(own = 0))
# my_cards$type <- type


