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
  

url <- "https://www.ebay.com/itm/2008-2009-2011-Topps-Baseball-Star-Cards-Complete-Your-Set-U-Pick-1/114260790121"
years <- c(2008, 2009, 2011)

url <- "https://www.ebay.com/itm/2000-Topps-Baseball-Star-Cards-Complete-Your-Set-U-Pick-1/114279271050"
years <- 2000

url <- "https://www.ebay.com/itm/2005-2006-Topps-Baseball-Star-Cards-Complete-Your-Set-U-Pick-1/114279298802"
years <- 2005:2006

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
offer <- filter(offer, !is.na(number), number < 1000)


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
# 11 for 08,09,11 stars:

fwrite(filter(df, ebay == 1), file = "data/alyssa_93_97.csv")
fwrite(filter(df, ebay == 1), file = "data/alyssa_2013_2017.csv")
fwrite(filter(df, ebay == 1), file = "data/alyssa_99_02.csv")
fwrite(filter(df, ebay == 1), file = "data/alyssa_08-09-11-stars.csv")

#               name year number price own         type ebay
# 1   Miguel Cabrera 2008     10  1.47   0 first_ballot    1
# 2    Ichiro Suzuki 2008    320  1.28   0 first_ballot    1
# 3      David Ortiz 2009     50  0.40   0 first_ballot    1
# 4   Mariano Rivera 2009     60  1.00   0 first_ballot    1
# 5      Derek Jeter 2009    353  1.50   0 first_ballot    1
# 6      John Smoltz 2009    355  1.00   0 first_ballot    1
# 7    Chipper Jones 2009    475  1.00   0 first_ballot    1
# 8  Clayton Kershaw 2009    575  1.25   0 first_ballot    1
# 9    Roger Clemens 2008    105  0.85   0 vet_or_other    1
# 10  Alex Rodriguez 2009      1  0.95   0 vet_or_other    1
# 11      Joey Votto 2009    390  1.00   0 vet_or_other    1


#            name year number price own         type ebay
# 1 Cal Ripken Jr 2000      4  1.10   0 first_ballot    1
# 2   Derek Jeter 2000     15  1.25   0 first_ballot    1
# 3  Mark McGwire 2000      1  1.00   0 vet_or_other    1

#               name year number price own         type ebay
# 1   Ken Griffey Jr 2005    440  0.49   0 first_ballot    1
# 2      Derek Jeter 2005    600  1.25   0 first_ballot    1
# 3 Justin Verlander 2006    641  4.00   0 first_ballot    1
# 4     David Wright 2005    330  1.00   0 vet_or_other    1
# 5  Felix Hernandez 2005    688  0.70   0 vet_or_other    1


# Look briefly at:
# https://www.ebay.com/itm/2005-Topps-1-250-Baseball-card-PICK-Choose-Player-Complete-your-set/143783091715?_trkparms=aid%3D1110006%26algo%3DHOMESPLICE.SIM%26ao%3D1%26asc%3D20140131123730%26meid%3D6667debac0184892a130e6e1e32d4b1e%26pid%3D100167%26rk%3D3%26rkt%3D15%26sd%3D114279298802%26itm%3D143783091715%26pmt%3D0%26noa%3D1%26pg%3D5411%26algv%3DDefaultOrganic&_trksid=p5411.c100167.m2940
filter(my_cards, year == 2005, own == 0) %>% select(name:remaining_price, type) %>%
  arrange(as.integer(number))

filter(my_cards, year == 1999, own == 0) %>% select(name:remaining_price, type) %>%
  arrange(as.integer(number))


# 2014 stars:
offer <- data.frame(year = 2014, 
                    number = c(100, 103, 113, 390, 557), 
                    ebay = 1)
years <- 2014
df <- my_cards %>%
  filter(own == 0, year %in% years) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0))

filter(df, ebay == 1)

#           name year number price own         type ebay
# 1 Bryce Harper 2014    100     1   0 vet_or_other    1


cl <- fread("data/checklists_1952_2020.csv", data.table = FALSE)
inner_join(cl, offer, by = c("year", "number"))



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
#     'etsfan1'
#     https://www.ebay.com/itm/1990-1991-1992-Topps-Baseball-Cards-Complete-Your-Set-U-Pick-15-Cards-NM-MT/333774117708?_trkparms=aid%3D555021%26algo%3DPL.SIMRVI%26ao%3D1%26asc%3D20190711100440%26meid%3D15fc71a5872f4f5fa75f066bf8391261%26pid%3D100752%26rk%3D5%26rkt%3D17%26mehot%3Dpf%26sd%3D233850924989%26itm%3D333774117708%26pmt%3D1%26noa%3D0%26pg%3D2047675%26algv%3DSimplRVIAMLv5WebWithPLRVIOnTopCombiner&_trksid=p2047675.c100752.m1982

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
url <- "https://www.ebay.com/itm/1981-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224176185065?hash=item3431f11ee9:g:cF4AAOSwomdfdORi"
url <- "https://www.ebay.com/itm/1990-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223463882085?hash=item34077c3d65:g:eJsAAOSwbURcnUAG"
url <- "https://www.ebay.com/itm/1992-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223395126342?hash=item3403631c46:g:g-AAAOSwUDZcZ3jN"
url <- "https://www.ebay.com/itm/1995-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223546834932?hash=item340c6dfff4:g:ZukAAOSw2M5c~fgy"
url <- "https://www.ebay.com/itm/1999-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223581398362?hash=item340e7d655a:g:7XgAAOSwyNtdJUA5"
url <- "https://www.ebay.com/itm/2000-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223536775948?hash=item340bd4830c:g:RoAAAOSwiyBc8c~f"
url <- "https://www.ebay.com/itm/2002-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223505508935?hash=item3409f76a47:g:kUkAAOSwrJRczxeY"
url <- "https://www.ebay.com/itm/2005-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223357527281?hash=item34012564f1:g:Z6QAAOSwEqVcU8mY"
url <- "https://www.ebay.com/itm/2008-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List-Series-1-2/223362892671?hash=item340177437f:g:~W0AAOSwJiBcVniX"
url <- "https://www.ebay.com/itm/2010-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223479074025?hash=item3408640ce9:g:GW4AAOSwmP9cr~RY"
url <- "https://www.ebay.com/itm/2011-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223372870081?hash=item34020f81c1:g:4Y4AAOSwJ8hcW4OP"
url <- "https://www.ebay.com/itm/2012-Topps-Baseball-Complete-Your-Set-Pick-25-Cards-From-List-Series-1-2/223377769336?hash=item34025a4378:g:pyQAAOSwmuFcXh6Z"
url <- "https://www.ebay.com/itm/2016-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223522189186?hash=item340af5ef82:g:jaoAAOSwrLVc4Nom"
url <- "https://www.ebay.com/itm/2016-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223529104433?hash=item340b5f7431:g:JgcAAOSwp1lc6MXG"
url <- "https://www.ebay.com/itm/2017-Topps-Series-One-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223346517550?hash=item34007d662e:g:U8YAAOSw~VBcTR5e"
url <- "https://www.ebay.com/itm/2018-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223513667676?hash=item340a73e85c:g:Bz0AAOSwBQ9c2H6-"
url <- "https://www.ebay.com/itm/2018-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223514832806?hash=item340a85afa6:g:qDMAAOSwu5dc2gwr"
url <- "https://www.ebay.com/itm/2019-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223436697817?hash=item3405dd70d9:g:gCAAAOSweaFcfzil"
url <- "https://www.ebay.com/itm/2019-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/223577887013?hash=item340e47d125:g:pm4AAOSwTU5dIR5G"
url <- "https://www.ebay.com/itm/2020-Topps-Series-1-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224001891020?hash=item34278d9acc:g:o1IAAOSwNWxes59m"
url <- "https://www.ebay.com/itm/2020-Topps-Series-2-Baseball-Complete-Your-Set-Pick-25-Cards-From-List/224142529079?hash=item342fef9237:g:pIgAAOSwqDJfT-Ei"

years <- 1999


# get the list of cards:
n_blocks <- 1
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
#  3 for 2018 series 2
#  3 for 2002
#  9 for 1992 (25 for $2.50)

#  updated 2021-02-13:
#  6 for 1981
#  2 for 1995
#  5 for 2000 (all vet_or_other)
#  0 for 2005
#  6 for 2008 (no first ballot)
#  0 for 2010
#  4 for 2016 series 1
#  0 for 2016 series 2
#  1 for 2017 series 1
#  6 for 2018 series 1
#  1 for 2019 series 1 (verlander)
#  1 for 2019 series 2 (strasburg)




# filter(my_cards, year == 1990, own == 0) %>% 
#   select(name:remaining_price) %>%
#   mutate(number = as.numeric(number)) %>%
#   arrange(number)

# filter(cl, year == 1990, grepl("Radin", name))
# filter(cl, year == 1981, grepl("AS", name))

#            name year number price own         type ebay
# 1  Rich Gossage 1981    460  0.15   0        bbwaa    1
# 2 Bert Blyleven 1981    554  0.40   0        bbwaa    1
# 3    Tony Perez 1981    575  0.60   0        bbwaa    1
# 4  Bruce Sutter 1981    590  0.58   0        bbwaa    1
# 5  Dwight Evans 1981    275  0.90   0 vet_or_other    1
# 6 Alan Trammell 1981    709  0.40   0 vet_or_other    1


extras_81 <- offer %>%
  inner_join(filter(cl, year == 1981, 
                    grepl("(LaCock|Future Stars|Kaat|Buckner|Cooper|Gantner|Mike Scott|Concepcion|Griffey|Willie Wilson|AS|Lansford|Madlock|Bonds)", name)))

# offer %>% inner_join(filter(cl, year == 1981)) %>% tail(100)
# offer %>% inner_join(filter(cl, year == 1981)) %>% head(200)

# get the final list for 1981:
filter(df, ebay == 1) %>%
  select(year, number, name) %>%
  bind_rows(extras_81 %>% select(-ebay)) %>%
  arrange(as.integer(number)) %>%
  filter(!duplicated(number), 
         number %in% c(66, 328) == FALSE) %>%
  summarize(paste(number, collapse = ", "))

# 1981: 9, 109, 275, 280, 356, 360, 375, 381, 430, 450, 460, 465, 482, 502, 520, 554, 555, 563, 575, 590, 625, 635, 639, 709, 715
# 25 cards, no dupes



# 12 cards from 1999
# need 13 more:
# extras_99 <-

filter(cl, year == 1999) %>% inner_join(offer) %>% head(200)

filter(cl, year == 1999) %>% inner_join(offer) %>% tail(100)


extras_99 <- filter(cl, year == 1999, 
       grepl("(Mark McGwire|Sosa|Tejada|ASR|Edmonds|Cameron|Damon|Nomar)", name), 
       !grepl("(HL|LL|AT)",name)) %>%
  inner_join(offer)

# the 1999 purchase:
filter(df, ebay == 1) %>%
  select(year, number, name) %>%
  bind_rows(select(extras_99, -ebay))

filter(df, ebay == 1) %>%
  select(year, number, name) %>%
  bind_rows(select(extras_99, -ebay)) %>%
  arrange(as.integer(number)) %>%
  summarize(paste(number, collapse = ", "))
  
# 1999: 84, 95, 110, 120, 130, 132, 155, 173, 220, 243, 265, 270, 291, 325, 329, 345, 352, 355, 369, 380, 391, 417, 418, 419, 461
# 25, no dupes



# extras from 2018 (need 19 more):
extras_18 <- offer %>%
  inner_join(filter(cl, year == 2018, 
                    grepl("(ASR|Baez|Reynolds|Springer|Markakis|Sabathia|Ichiro|Bautista|Kemp|Ozuna|Fielder|Braun|Guerrero|Freeman|Soto|Torres|Acuna|Judge|Arenado|Lindor|Cole|Altuve|Correa|Tulowitzki|Rendon|Chris Sale|Simmons|Stanton|Puig)", name), 
                    !grepl("(LL|WS)", name)))

offer %>% inner_join(filter(cl, year == 2018)) %>% 
  filter(number >= 201)

order_18 <- filter(df, ebay == 1) %>%
  select(year, number, name) %>%
  bind_rows(select(extras_18, year, number, name)) %>%
  arrange(number) %>%
  filter(!duplicated(number))

order_18 %>%
  summarize(paste(number, collapse = ", "))
# 2018: 10, 15, 20, 30, 36, 42, 89, 118, 132, 140, 157, 170, 180, 183, 206, 233, 235, 236, 238, 250, 265, 275, 285, 300, 302
# 25, no dupes


filter(df, ebay == 1)

#                name year number price own         type ebay
# 1      Mookie Betts 2018    140  1.00   0 first_ballot    1
# 2        Mike Trout 2018    300  2.99   0 first_ballot    1
# 3    Cody Bellinger 2018     42  1.85   0 vet_or_other    1
# 4  Christian Yelich 2018    170  1.00   0 vet_or_other    1
# 5 Stephen Strasburg 2018    233  0.17   0 vet_or_other    1
# 6      Buster Posey 2018    250  0.50   0 vet_or_other    1







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


### read back in the klively order and mark which cards I got:
klive <- readLines("data/klively_order.txt")
year <- as.integer(unlist(lapply(klive, strsplit, split = ": "))[seq(1, by = 2, length = 9)])
nl <- unlist(lapply(klive, strsplit, split = ": "))[seq(2, by = 2, length = 9)]
number <- as.integer(unlist(strsplit(nl, split = ", ")))


klive_order <- data.frame(year = rep(year, str_count(nl, ",") + 1), 
                          number = number)

my_cards %>% filter(year > 1989) %>%
  mutate(number = as.integer(number)) %>%
  inner_join(klive_order, by = c("year", "number")) %>%
  select(name:remaining_price, type) %>%
  filter(own == 0) %>%
  arrange(type, year, number) %>%
  filter(type == "vet_or_other")




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

# Get card list from jvo1968:
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


# # alyssa trunk 120 order:
# o1 <- fread("~/public_git/ToppsCollection/data/alyssa_order_94_97.txt", data.table = FALSE)
# o2 <- fread("~/public_git/ToppsCollection/data/alyssa_order_99_02.txt", data.table = FALSE)
# 
# alyssa_order <- bind_rows(o1, o2) %>%
#   arrange(year, number)
# 
# alyssa_order %>%
#   left_join(my_cards %>%
#               filter(year > 1990) %>%
#               mutate(number = as.integer(number)) %>%
#               select(name:remaining_price, type), 
#             by = c("year", "number")) %>%
#   as.data.frame() %>%
#   filter(!is.na(type))
# OK, only missing 2 out 98 intended cards. w clark and r palmeiro 1998 


### Now 'jvo1968'
jvo <- readLines("data/jvo1968_offer.txt")
yrs <- jvo[seq(1, by = 2, length = 13)]
num <- strsplit(jvo[seq(2, by = 2, length = 13)], split = ",")
num <- lapply(num, gsub, pattern = "\\([0-9]+\\)", replacement = "")

offer <- data.frame(year = as.integer(rep(yrs, sapply(num, length))), 
                    number = as.integer(unlist(num)), 
                    ebay = 1)


z <- my_cards %>%
  filter(own == 0, year > 1999) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)


# o <- bind_rows(o1, o2) %>%
#   mutate(alyssa = 1)

z
#              name year number price own         type ebay alyssa
# 1     Derek Jeter 2000     15  1.25   0 first_ballot    1     NA
# 2       Jim Thome 2000    360  0.25   0 first_ballot    1     NA
# 3     Tom Glavine 2004     13  0.34   0 first_ballot    1     NA
# 4  Mariano Rivera 2006    110  0.81   0 first_ballot    1     NA
# 5   Adrian Beltre 2011    302  0.25   0 first_ballot    1     NA
# 6      Tim Raines 2000     71  0.20   0        bbwaa    1     NA
# 7      Eric Davis 2000    190  0.20   0 vet_or_other    1     NA
# 8    Jose Canseco 2000    200  0.20   0 vet_or_other    1     NA
# 9    Mark Buehrle 2004    127  0.28   0 vet_or_other    1     NA
# 10  Roger Clemens 2006    151  0.45   0 vet_or_other    1     NA
# 11   Billy Wagner 2006    207  0.25   0 vet_or_other    1     NA
# 12  Robinson Cano 2008    136  0.40   0 vet_or_other    1     NA
# 13   David Wright 2011     15  0.35   0 vet_or_other    1     NA
# 14     Tim Hudson 2011     77  0.15   0 vet_or_other    1     NA
# 15    Chase Utley 2011    214  0.43   0 vet_or_other    1     NA
# 16    Scott Rolen 2011    228  0.15   0 vet_or_other    1     NA
# 17   Mark Buehrle 2011    231  0.22   0 vet_or_other    1     NA
# 18   Omar Vizquel 2011    243  0.28   0 vet_or_other    1     NA



### Now for user 'capeauctions'

# url <- "https://www.ebay.com/itm/Finish-Your-2012-TOPPS-Set-1-660-U-PICK-30/202644042034?_trkparms=aid%3D111001%26algo%3DREC.SEED%26ao%3D1%26asc%3D20160908105057%26meid%3D6762594c342940fc993c329eafe856ca%26pid%3D100675%26rk%3D4%26rkt%3D14%26mehot%3Dnone%26sd%3D383908790498%26itm%3D202644042034%26pmt%3D0%26noa%3D1%26pg%3D2380057&_trksid=p2380057.c100675.m4236&_trkparms=pageci%3Abf9f48ef-6268-11eb-9761-6a6f0f4a49ed%7Cparentrq%3A4fa219f11770a4d362453866fffe2e80%7Ciid%3A1"
yrs <- 2011
url <- "https://www.ebay.com/itm/Complete-Finish-Your-2011-TOPPS-Sets-Series-1-2-1-660-U-PICK-30/362591217643"

url <- "https://www.ebay.com/itm/Finish-Your-2008-TOPPS-Sets-Series-1-2-1-660-Updates-1-330-U-PICK-40/362596929821?_trkparms=aid%3D111001%26algo%3DREC.SEED%26ao%3D1%26asc%3D20160908105057%26meid%3D46040ec827a746de8d13ad560519e140%26pid%3D100675%26rk%3D2%26rkt%3D15%26mehot%3Dnone%26sd%3D362624599061%26itm%3D362596929821%26pmt%3D0%26noa%3D1%26pg%3D2380057&_trksid=p2380057.c100675.m4236&_trkparms=pageci%3Aeac13ca1-64db-11eb-adef-f67359843c91%7Cparentrq%3A5fb0501c1770ad3550f91800fff87394%7Ciid%3A1"
yrs <- 2008

url <- "https://www.ebay.com/itm/Finish-Your-2009-2010-TOPPS-Sets-Series-1-2-1-660-Updates-1-330-U-PICK-30/362624599061"
yrs <- 2009:2010

url <- "https://www.ebay.com/itm/Complete-Finish-Your-2013-Topps-Set-1-660-Updates-US-1-330-U-PICK-35-/362591217604?hash=item546c1ee7c4"
yrs <- 2013

url <- "https://www.ebay.com/itm/Complete-Finish-Your-2015-TOPPS-Sets-Series-2-331-701-U-PICK-30-/362609224428?hash=item546d31aaec"
yrs <- 2015

url <- "https://www.ebay.com/itm/Finish-Your-2012-TOPPS-Set-1-660-U-PICK-30-/202644042034?hash=item2f2e86b132"
yrs <- 2012


# read the URL:
page <- read_html(url)

desc_link <- page %>%
  html_nodes("iframe") %>%
  html_attr("src")

tmp <- read_html(desc_link) %>%
  html_nodes("p") %>%
  html_text()

# get the number of numbers in each element of the list:
n_numbers <- sapply(strsplit(tmp, split = "[[:space:]]+"), 
                    function(x) sum(!is.na(as.numeric(x))))


# 
# tmp[n_numbers > 6]
# n_numbers

# nums <- which(n_numbers > 10)[1:14]
# yr_vec <- rep(2009:2010, each = 7)

# nums <- which(n_numbers > 10)
# yr_vec <- rep(2011, 7)

# nums <- which(n_numbers > 6)[1:7]
# yr_vec <- rep(2013, 7)

nums <- which(n_numbers > 10)
yr_vec <- rep(yrs, length(nums))

# now get the numbers themselves:
num_list <- vector("list", length(yrs))
for (i in 1:length(yrs)) {
  l <- lapply(strsplit(tmp[nums][yr_vec == yrs[i]], split = "[[:space:]]+"), as.numeric)
  num_list[[i]] <- unlist(l) %>% unique()
}

offer <- data.frame(year = rep(yrs, sapply(num_list, length)), 
                    number = unlist(num_list), 
                    ebay = 1)

z <- my_cards %>%
  filter(own == 0, year %in% yrs) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)
z

fwrite(z, file = "data/capeauctions_2009-2010.csv")  # 23 (pick 30 $1.75)
fwrite(z, file = "data/capeauctions_2011.csv")  # 31 (pick 30 $1.75)
fwrite(z, file = "data/capeauctions_2008.csv")  # 9 (pick 40 $1.95)



# checklists:
list08 <- fread("data/capeauctions_2008.csv", data.table = FALSE)
list0910 <- fread("data/capeauctions_2009-2010.csv", data.table = FALSE)
list11 <- fread("data/capeauctions_2011.csv", data.table = FALSE)

cl <- fread("data/checklists_1952_2020.csv", data.table = FALSE)

zz <- offer %>%
  left_join(cl) %>%
  as.data.frame()

# Here's 21 more from 2008:
extras_08 <- filter(zz, grepl("(Braun|Fielder|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))
extras_09 <- filter(zz, grepl("(ASR)", name))


bind_rows(select(list08, year, number, name), 
          select(extras_08, year, number, name)) %>%
  arrange(number) %>%
  summarize(paste(number, collapse = ", "))

# 2008: 2, 4, 20, 40, 42, 45, 59, 72, 75, 93, 
#     129, 170, 175, 193, 199, 207, 225, 239, 240, 269, 
#     270, 275, 302, 332, 371, 379, 380, 385, 395, 396, 
#     425, 430, 450, 475, 510, 545, 585, 588, 625, 658

bind_rows(select(list0910, year, number, name), 
          select(extras_09, year, number, name)) %>%
  arrange(year, number) %>%
  summarize(paste(number, collapse = ", "))


# 2009: 24, 30, 46, 85, 100, 130, 155, 160, 185, 210, 212, 220, 224, 260, 287, 300, 320, 515
# 2010: 55, 100, 115, 125, 215, 310, 369, 370, 404, 549, 623, 652


list11 %>%
  select(year, number, name) %>%
  arrange(number) %>%
  summarize(paste(number, collapse = ", "))


# 2011: 5, 13, 15, 33, 42, 50, 67, 100, 108, 128, 
#       130, 150, 169, 183, 198, 200, 214, 220, 228, 231, 
#       243, 253, 275, 277, 302, 315, 360, 450, 515, 530




extras_12 <- filter(zz, grepl("(Braun|Fielder|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))


bind_rows(select(z, year, number, name), 
          select(extras_12, year, number, name)) %>%
  arrange(year, number) %>%
  summarize(paste(number, collapse = ", "))

# 2012: 1, 16, 30, 50, 60, 69, 87, 89, 96, 106, 
#       110, 139, 140, 162, 180, 200, 230, 280, 284, 292, 
#       305, 351, 354, 365, 420, 492, 547, 571, 574, 626


my_cards %>%
  group_by(lot_name) %>%
  summarize(n = n(), 
            price = sum(price)) %>%
  arrange(desc(n)) %>%
  as.data.frame()







################################################################################

# Get card list from stlbrowns:
setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(rvest)
library(tidyr)
library(stringr)
library(googlesheets4)
library(httr)
library(purrr)

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




# list from 'stlbrowns'
url <- "https://www.ebay.com/itm/2003-TOPPS-LOT-INSERTS-GOLD-TRADED-COMPLETE-YOUR-SET-20-PICKS/203241434218"
year <- 2003

# read the URL:
page <- read_html(url)

desc_link <- page %>%
  html_nodes("iframe") %>%
  html_attr("src")

tmp <- read_html(desc_link) %>%
  html_nodes("strong") %>%
  html_text()

# measure number of space-separated words on this page:
n <- sapply(strsplit(tmp, split = "[[:space:]]+"), length)

# it's elements 4-7 for 2003 topps
tmp <- gsub(x = tmp, pattern = "&nbsp;", replacement = " ")
nl <- gsub(x = tmp[4:7], pattern = " picks", replacement = "")
nl <- strsplit(nl, split = "[[:space:]]+")

x <- lapply(nl, strsplit, split = "\\(") %>%
  lapply(function(x) sapply(x, "[", 1)) %>%
  unlist() %>%
  as.numeric() %>%
  as.data.frame() %>%
  rename(number = ".")

picks <- lapply(nl, strsplit, split = "\\(") %>%
  lapply(function(x) sapply(x, "[", 2)) %>%
  unlist() %>%
  gsub(pattern = "\\)", replacement = "")

x$picks <- as.integer(picks)

# fill in 2 picks for 312 and 334:
x$picks[x$number %in% c(312, 334)] <- 2

# get rid of NA numbers and replace NA picks with 1:
offer <- filter(x, !is.na(number)) %>%
  replace_na(list(picks = 1)) %>%
  mutate(ebay = 1, 
         year = 2003)


z <- my_cards %>%
  filter(own == 0, year == 2003) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)
z
# OK, for 2003 topps, I can get every card I need for 17.50 + shipping.


# stlbrowns has tons of years, and every card, basically.
# just have to pay a bit more for them...




### 'savageb12' has individual cards from many years, 70 cents/card with
### > 4 discount, and cheap shipping.





### let's get the 1994 and 1992 topps from 'bash5559' 

url <- "https://www.ebay.com/itm/1993-1994-1993-update-Topps-baseball-pick-40-cards-ex-nm/323743207334?hash=item4b60994fa6:g:Rc4AAMXQBuNQ65RN"
year <- 1994

url <- "https://www.ebay.com/itm/1991-1992-Topps-Baseball-pick-40-comp-your-set-ex-nrmt-plus-1991-topps-traded/323743207322?hash=item4b60994f9a:g:7dwAAMXQya1Q64~f"
year <- 1992

# read the URL:
page <- read_html(url)

desc_link <- page %>%
  html_nodes("iframe") %>%
  html_attr("src")

tmp <- read_html(desc_link) %>%
  html_nodes("p") %>%
  html_text()

# 1994
numbers <- as.integer(unlist(strsplit(tmp[5], split = "-")))
offer <- data.frame(year = 1994, 
                    number = numbers, 
                    ebay = 1)

# 1992
numbers <- as.integer(unlist(strsplit(tmp[7], split = "-")))
offer <- data.frame(year = 1992, 
                    number = numbers, 
                    ebay = 1)


z <- my_cards %>%
  # filter(own == 0, year == 1994) %>%
  filter(own == 0, year == 1992) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)
z

# 1994 get 22 cards (pick 40)

#             name year number price own         type ebay
# 1   Rich Gossage 1992    215  0.10   0        bbwaa    1
# 2  Bert Blyleven 1992    375  0.10   0        bbwaa    1
# 3     Tim Raines 1992    426  0.50   0        bbwaa    1
# 4 Edgar Martinez 1992    553  1.00   0        bbwaa    1
# 5   Carlton Fisk 1992    630  0.30   0        bbwaa    1
# 6   Craig Biggio 1992    715  0.25   0        bbwaa    1
# 7      Lee Smith 1992    565  0.20   0 vet_or_other    1
# 8    Dale Murphy 1992    680  0.15   0 vet_or_other    1
# 9  Dwight Gooden 1992    725  0.09   0 vet_or_other    1

cl <- fread("data/checklists_1952_2020.csv", data.table = FALSE)

zz <- offer %>%
  left_join(cl) %>%
  as.data.frame()

# get extras:
extras_94 <- filter(zz, grepl("(ASR|Listach|Valenzuela|Bernie|Pedro Martinez|Glavine|Molitor|Thome)", name))
extras_92 <- filter(zz, grepl("(ASR|FS|AS|DPK|Listach|Valenzuela|Bernie|Glavine|Molitor|Thome)", name))

bind_rows(select(z, name, year, number), 
          select(extras_94, name, year, number)) %>%
  arrange(number) %>%
  pull(number) %>%
  paste(collapse = ", ")


# 1994:  2, 21, 65, 72, 75, 81, 110, 130, 142, 175, 222, 230, 240, 243, 268, 287, 293, 305, 309, 393, 397, 420, 466, 470, 475, 488, 508, 540, 565, 593, 598, 609, 612, 630, 640, 645, 675, 685, 730, 783


bind_rows(select(z, name, year, number), 
          select(extras_92, name, year, number)) %>%
  mutate(asr = grepl("ASR", name)) %>%
  arrange(desc(asr)) %>%
  head(40) %>%
  arrange(as.integer(number)) %>%
  pull(number) %>%
  paste(collapse = ", ")

# 1992: 9, 36, 66, 84, 96, 215, 246, 292, 305, 306, 336, 369, 375, 386, 388, 389, 391, 392, 393, 394, 395, 397, 398, 402, 403, 404, 406, 407, 414, 426, 474, 504, 537, 553, 565, 614, 630, 680, 715, 725



my_cards %>% 
  filter(year == 1992) %>% 
  filter(own == 0) %>% 
  arrange(as.numeric(number)) %>% 
  select(name:remaining_price, type) %>% 
  as.data.frame()




### Now let's get some lists from lstowell178y:

url <- "https://www.ebay.com/itm/2004-Topps-Baseball-You-pick-15-Finish-your-set/302989627782"
year <- 2004

url <- "https://www.ebay.com/itm/Topps-2006-2007-Base-Cards-You-pick-15-Finish-your-set/301138856342"
year <- 2006

url <- "https://www.ebay.com/itm/Topps-2013-Series-1-2-Updates-Base-Cards-You-pick-15-Finish-your-set/303555754946"
year <- 2013

url <- "https://www.ebay.com/itm/Topps-2014-Base-Cards-Series-1-2-Updates-You-pick-15-Finish-your-set/303743453586?hash=item46b8854992:g:1hkAAOSwTzpfmLSJ"
year <- 2014

url <- "https://www.ebay.com/itm/Topps-2015-Base-Cards-Series-1-2-Updates-You-pick-15-Finish-your-set/254037747843?hash=item3b25d46883:g:6yEAAOSwwjRcHvw4"
year <- 2015

url <- "https://www.ebay.com/itm/2020-Topps-Series-1-2-Updates-Complete-Your-Set-Pick-15/254659248509?hash=item3b4adfc17d:g:zpgAAOSwXmpe9PXp"
year <- 2020



# read the URL:
page <- read_html(url)

desc_link <- page %>%
  html_nodes("iframe") %>%
  html_attr("src")

tmp <- read_html(desc_link) %>%
  #html_nodes("p") %>%
  html_nodes("div") %>%
  html_text()

numbers <- strsplit(tmp[1], split = "([[:space:]]+|,)") %>%
  unlist() %>%
  as.integer() %>%
  discard(is.na)

# for 2004:
numbers <- numbers[-(1:4)]

# for 2006:
numbers <- numbers[numbers < 700]
numbers <- c(numbers, 30, 31)
numbers <- sort(numbers)

# for 2013:
numbers <- numbers[2:629]

# for 2014:
numbers <- numbers[2:483]

numbers <- numbers[c(2:284, 286:615)]

# 2020
numbers <- numbers[5:639]

# set up the offer:
offer <- data.frame(year = 2020, 
                    number = numbers, 
                    ebay = 1)

z <- my_cards %>%
  filter(own == 0, year == 2020) %>%
  mutate(number = as.integer(number)) %>%
  select(name:own, type) %>%
  left_join(offer, by = c("year", "number")) %>%
  replace_na(list(ebay = 0)) %>%
  filter(ebay == 1)
z

fwrite(z, file = "data/lstowell_2004.csv")  # 24 (pick 15 $2.99)
fwrite(z, file = "data/lstowell_2006.csv")  # 27 (pick 15 $2.59)
fwrite(z, file = "data/lstowell_2013.csv")  # 10 (pick 15 $3.19)
fwrite(z, file = "data/lstowell_2020.csv")  #  9 (pick 15 $3.29)


z1 <- fread("data/lstowell_2004.csv", data.table = FALSE)
z2 <- fread("data/lstowell_2006.csv", data.table = FALSE)
z3 <- fread("data/lstowell_2013.csv", data.table = FALSE)
z4 <- fread("data/lstowell_2020.csv", data.table = FALSE)

# get the rest of the checklist:
zz <- offer %>%
  left_join(cl) %>%
  as.data.frame()

extras_04 <- filter(zz, grepl("(Torii|Prior|Rollins|Braun|Fielder|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))

# 2004:
bind_rows(select(z1, name, number, year), 
          select(extras_04, name, number, year)) %>%
  arrange(as.integer(number)) %>%
  pull(number) %>%
  unique() %>%
  paste(collapse = ", ")

# 2004: 13, 25, 29, 49, 50, 62, 71, 76, 84, 105, 125, 127, 143, 150, 173, 352, 368, 372, 375, 378, 386, 388, 390, 400, 402, 431, 438, 456, 468, 500, 502, 508, 512, 516, 518, 527, 531, 565, 575, 582, 590, 592, 604, 620, 623
# 45 cards.

extras_06 <- filter(zz, grepl("(Torii|Prior|Rollins|Braun|Fielder|Sheets|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Hanley Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))
extras_06

bind_rows(select(z2, name, year, number), 
          select(extras_06, name, year, number)) %>%
  arrange(as.integer(number)) %>%
  pull(number) %>%
  unique() %>%
  paste(collapse = ", ")

# 2006: 3, 13, 18, 21, 23, 39, 48, 50, 65, 72, 80, 82, 90, 110, 121, 122, 130, 132, 142, 151, 178, 185, 200, 203, 205, 207, 222, 230, 248, 265, 327, 335, 345, 354, 359, 362, 387, 388, 394, 398, 400, 405, 410, 419, 420, 431, 445, 451, 455, 500, 520, 524, 540, 555, 560, 570, 573, 585, 620, 639
# 60 cards.


# extras_13 <- filter(zz, grepl("(ASR)", name))
# extras_13

extras_13 <- filter(zz, grepl("(Torii|Prior|Rollins|Braun|Fielder|Sheets|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Hanley Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))
extras_13

# need to get rid of 10 of these:
bind_rows(select(z3, name, year, number), 
          select(extras_13, name, year, number)) %>%
  arrange(as.integer(number)) %>%
  filter(number %in% c(23, 246, 272, 283, 359, 485, 496, 533, 575, 453) == FALSE) %>%
  pull(number) %>%
  unique() %>%
  paste(collapse = ", ")

# 2013: 6, 8, 11, 19, 22, 26, 35, 37, 44, 70, 75, 118, 123, 128, 202, 206, 213, 228, 285, 350, 362, 375, 400, 480, 487, 530, 565, 568, 574, 595
# 30 cards.


# extras_20 <- filter(zz, grepl("(ASR)", name))
# extras_20

extras_20 <- filter(zz, grepl("(Altuve|Torres|Ozuna|Acuna|Freeman|Bieber|Bauer|Judge|Stanton|Tatis|Arenado|Rendon|Turner|Seager|Bichette|Guerrero|Torii|Prior|Rollins|Braun|Fielder|Sheets|ASR|Sabathia|Tulow|Giambi|Damon|Zito|Ankiel|Tejada|Matsuzaka|Hart|Weeks|Gwynn|Kerry|Nomar|Kent|Adrian Gonzalez|Howard|Berkman|Hanley Ramirez|Sizemore|Morneau|Willis|Sexson|Hardy)", name))
extras_20

# need to get rid of 7 of these:
bind_rows(select(z4, name, year, number), 
          select(extras_20, name, year, number)) %>%
  arrange(as.integer(number)) %>%
  filter(number %in% c(120, 214, 636, 638, 11, 239, 575) == FALSE) %>%
  pull(number) %>%
  unique() %>%
  paste(collapse = ", ")

# 2020: 1, 7, 14, 16, 49, 50, 140, 192, 200, 224, 230, 250, 304, 316, 324, 345, 347, 350, 367, 455, 491, 497, 500, 547, 549, 550, 571, 602, 617, 620
# 30 cards.




filter(my_cards, own == 0, year == 1981) %>%
  select(name:remaining_price, type) %>%
  arrange(as.integer(number))











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


