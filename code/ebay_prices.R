################################################################################

# [1] scrape an ebay search result page for prices.

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
cards <- bind_rows(cards1, cards2, cards3)

# Now get just the pre-1988 cards:
df <- filter(cards, year <= 1987) %>% select(name:own) %>%
  replace_na(list(own = 0))

# look at the last name:
# table(sapply(strsplit(unique(df$name), split = " "), length))
# unique(df$name)[sapply(strsplit(unique(df$name), split = " "), length) > 2]
last_name <- sapply(strsplit(unique(df$name), split = " "), "[", 2)
last_name[last_name == "Wee"] <- "Reese"

df <- df %>%
  left_join(data.frame(name = unique(df$name), last_name))




### First try, just wrap the call in a tryCatch and see how many I get:
tmp <- vector("list", nrow(df))
for (i in 1:nrow(df)) {
  print(i)
  print(paste("  ", df$year[i], df$last_name[i]))
  url <- paste0("https://www.ebay.com/sch/i.html?_nkw=", 
                df$year[i], 
                "+topps+%23", 
                df$number[i], 
                "+", 
                tolower(df$last_name[i]), 
                "&_in_kw=4&_ex_kw=&_sacat=0&LH_Sold=1&_udlo=&_udhi=&_samilow=&_samihi=&_sadis=15&_stpos=07079&_sargn=-1%26saslc%3D1&_salic=1&_fss=1&_fsradio=%26LH_SpecificSeller%3D1&_saslop=1&_sasl=gregmorriscards&_sop=13&_dmd=1&_ipg=200&LH_Complete=1&_fosrp=1")
  
  
  try(tmp[[i]] <- read_html(x = url))
}

# OK, that took about 20 minutes to loop through all 1316 cards.
# Many errors, but also some successes.

sum(sapply(tmp, length) == 0)  # 256
sum(sapply(tmp, length) == 2)  # 1060

# OK, about an 80% success rate.
tmp2 <- tmp

# Let's try again, just on the 256 errors from the first run:
for (i in 1:nrow(df)) {
  if (length(tmp[[i]]) == 0) {
    print(i)
    print(paste("  ", df$year[i], df$last_name[i]))
    url <- paste0("https://www.ebay.com/sch/i.html?_nkw=", 
                  df$year[i], 
                  "+topps+%23", 
                  df$number[i], 
                  "+", 
                  tolower(df$last_name[i]), 
                  "&_in_kw=4&_ex_kw=&_sacat=0&LH_Sold=1&_udlo=&_udhi=&_samilow=&_samihi=&_sadis=15&_stpos=07079&_sargn=-1%26saslc%3D1&_salic=1&_fss=1&_fsradio=%26LH_SpecificSeller%3D1&_saslop=1&_sasl=gregmorriscards&_sop=13&_dmd=1&_ipg=200&LH_Complete=1&_fosrp=1")
    
    
    try(tmp[[i]] <- read_html(x = url))
  }
}

sum(sapply(tmp, length) == 0)  # 45
sum(sapply(tmp, length) == 2)  # 1271

# OK. Same exact rate of success on second try!

sum(sapply(tmp, length) == 0)  # 13
sum(sapply(tmp, length) == 2)  # 1303

# one more time, # 2 left
# one more time, # 0 left.




### Now loop through the downloaded pages and extract info about the cards:

results <- vector("list", nrow(df))
for (i in 1:nrow(df)) {
  print(i)
  p <- tmp[[i]]
  
  # get item title:
  title <- p %>%
    html_nodes(".vip") %>%
    html_text()
  
  print(paste("  ", length(title)))
  
  # get price
  price <- p %>%
    html_nodes(".bidsold") %>%
    html_text()
  
  price <- as.numeric(gsub("$", "", price, fixed = TRUE))
  
  # get number of bids:
  bids <- p %>% html_nodes(".lvformat") %>% html_text()
  bids <- gsub("bids", "", bids, fixed = TRUE)
  bids <- gsub("\n", "", bids, fixed = TRUE)
  bids <- gsub("\r", "", bids, fixed = TRUE)
  bids <- gsub("\t", "", bids, fixed = TRUE)
  bids <- as.integer(gsub(" ", "", bids, fixed = TRUE))
  
  # get date:
  date <- p %>% html_nodes(".tme") %>% html_text()
  date <- gsub("\n", "", date, fixed = TRUE)
  date <- gsub("\r", "", date, fixed = TRUE)
  date <- gsub("\t", "", date, fixed = TRUE)
  date <- as.Date(date, format = "%b-%d %H:%M")
  
  # get it in a data frame:
  results[[i]] <- data.frame(title, date, bids, price)
}


out <- bind_rows(results)

fwrite(out, file = "data/card_prices.csv")

fwrite(df, file = "data/card_price_list.csv")




################################################################################

# [2] clean the data a bit

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(googlesheets4)

# read in the data:
data <- fread("data/card_prices.csv", data.table = FALSE)
data$year <- as.integer(substr(data$title, 1, 4))

# get the card number:
data$title <- gsub("wrinkle0", "wrinkle)", data$title, fixed = TRUE)
data$title <- gsub("Aaron 715", "Aaron", data$title, fixed = TRUE)
data$number <- gsub("[^0-9]", "", substr(str_trim(data$title), 5, nchar(str_trim(data$title))))

# read in the metadata:
df <- fread("data/card_price_list.csv", data.table = FALSE)
df <- filter(df, !(name == "Alan Trammell" & year == 1978))

# Now join individual card data with metadata:
data <- data %>%
  left_join(select(df, name, year, number, last_name), 
            by = c("year", "number")) %>%
  as.data.frame()  

# get the part of the string with CAPS
x <- lapply(data$title, gregexpr, pattern = "[A-Z]{2}")
index <- sapply(x, function(z) z[[1]][1])

condition <- substr(data$title, index, nchar(data$title))
condition <- gsub(" *GMCARDS*", "", condition, fixed = TRUE)
data <- mutate(data, condition = condition)

# Put it together:
data <- select(data, name, year, number, condition, date, bids, price)

fwrite(data, file = "data/card_prices_clean.csv")


################################################################################

# [3] fit a model for card prices

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(googlesheets4)
library(ggplot2)

# read in the data:
data <- fread("data/card_prices_clean.csv", data.table = FALSE)

# read in the metadata:
df <- fread("data/card_price_list.csv", data.table = FALSE)
df <- filter(df, !(name == "Alan Trammell" & year == 1978))

# Visualize a few cards:
cs <- data %>%
  group_by(name, year, number) %>%
  summarize(n_cards = n(), 
            min_price = min(price), 
            max_price = max(price), 
            median_price = median(price)) %>%
  as.data.frame()

# look at condition:
cond <- data %>%
  group_by(condition) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(p = round(cumsum(n) / sum(n), 4)) %>%
  as.data.frame()

# let's trim some of the rare conditions from the data:
conditions_to_keep <- cond$condition[1:10]

data %>%
  filter(condition %in% conditions_to_keep) %>%
  group_by(year, condition) %>%
  summarize(n = n()) %>%
  spread(key = condition, value = n, fill = 0) %>%
  as.data.frame()


data <- filter(data, condition %in% conditions_to_keep)
data$condition <- factor(data$condition, 
                         levels = cond$condition[c(5, 3, 2, 1, 7, 8, 10, 6, 4, 9)])

# plot the time series for each player:
n_players <- lu(data$name)  # 108
first_yr <- data %>% group_by(name) %>% summarize(yr = min(year)) %>% arrange(yr)
pl <- vector("list", n_players / 4)
for (i in 1:(n_players/4)) {
  pl[[i]] <- data %>%
    filter(name %in% first_yr$name[1:4 + (i - 1)*4]) %>%
    ggplot(aes(x = year, y = price, color = condition)) + 
    geom_point(alpha = 0.5) +
    facet_wrap(~name) + 
    scale_x_continuous(breaks = 1952:1987) + 
    theme(axis.text.x = element_text(angle = 90))
}

pdf(file = "figures/fig_price_time_series.pdf", height = 6, width = 9)
for (i in 1:(n_players/4)) {
  print(pl[[i]])
}
dev.off()















################################################################################

# [4] price the morristown estate sales lots

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(googlesheets4)
library(ggplot2)

# read in the data:
data <- fread("data/card_prices_clean.csv", data.table = FALSE)

# read in the metadata:
df <- fread("data/card_price_list.csv", data.table = FALSE)
df <- filter(df, !(name == "Alan Trammell" & year == 1978))

# get the list of cards that I want to price:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
sale <- read_sheet(google_docs_url, sheet = "scratch")
sale <- as.data.frame(sale)


# summarize data:
x <- data %>%
  group_by(name, year, number, condition) %>%
  summarize(n  = n(), 
            p = median(price)) %>%
  right_join(sale, by = c("name", "year", "number")) %>%
  as.data.frame()


filter(x, condition == "VG-VGEX")


x %>%
  filter(condition %in% c("VG-VGEX", "EX-EXMINT")) %>%
  group_by(lot, year, condition) %>%
  summarize(ebay_price = sum(p), 
            tcdb_price = sum(price)) %>%
  as.data.frame() %>%
  spread(key = condition, value = ebay_price) %>%
  select(lot:tcdb_price, 'VG-VGEX', 'EX-EXMINT') %>%
  mutate(across(c(tcdb_price, 'VG-VGEX', 'EX-EXMINT'), round, 0))

df %>% mutate(across(cols, round, 3)) # 


x %>%
  filter(condition %in% c("VG-VGEX", "EX-EXMINT")) %>%
  group_by(name, number, year, condition, lot) %>%
  summarize(ebay_price = sum(p), 
            tcdb_price = sum(price)) %>%
  as.data.frame() %>%
  spread(key = condition, value = ebay_price) %>%
  arrange(lot) %>%
  select(name:tcdb_price, 'VG-VGEX', 'EX-EXMINT') %>%
  mutate(across(c(tcdb_price, 'VG-VGEX', 'EX-EXMINT'), round, 0))



filter(data, year == 1969, number == 50) %>%
  arrange(condition, desc(price))

filter(data, year == 1969, number == 20) %>%
  arrange(condition, desc(price))



filter(data, year == 1971, number == 513) %>%
  arrange(condition, desc(price))

# 1971 aaron
filter(data, year == 1971, number == 400) %>%
  arrange(condition, desc(price))

# 1971 palmer
filter(data, year == 1971, name == "Jim Palmer") %>%
  arrange(condition, desc(price))

# 1972 yastrzemski
filter(data, year == 1972, name == "Carl Yastrzemski") %>%
  arrange(condition, desc(price))

# 1979 ozzie
filter(data, year == 1979, name == "Ozzie Smith") %>%
  arrange(condition, desc(price))


# 1970 seaver
filter(data, year == 1970, name == "Tom Seaver") %>%
  arrange(condition, desc(price))


# 1983 Gwynn
filter(data, year == 1983, name == "Tony Gwynn") %>%
  arrange(condition, desc(price))

# 1958 harmon killebrew
filter(data, year == 1958, name == "Harmon Killebrew") %>%
  arrange(condition, desc(price))




data %>%
  group_by(condition) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame()
  



data %>% 
  filter(condition %in% c("LOW GRADE", "VG-VGEX", "EX-EXMINT", "NR-MINT", "NM-MT OR BETTER")) %>%
  group_by(year, condition) %>%
  summarize(n = n()) %>%
  spread(key = condition, value = n, fill = 0) %>%
  as.data.frame()


z <- data %>% 
  filter(condition %in% c("LOW GRADE", "VG-VGEX", "EX-EXMINT", "NR-MINT", "NM-MT OR BETTER")) %>%
  group_by(name, year, number) %>%
  summarize(p_lo = median(price[condition == "LOW GRADE"]), 
            p_vg = median(price[condition == "VG-VGEX"]), 
            p_ex = median(price[condition == "EX-EXMINT"]), 
            p_nm = median(price[condition == "NR-MINT"]),
            p_mt = median(price[condition == "NM-MT OR BETTER"]), 
            n_cards = n()) %>%
  as.data.frame()

  
filter(z, name == "Brooks Robinson")
filter(z, name == "Tom Seaver")
filter(z, name == "Roberto Clemente")
filter(z, name == "Frank Robinson")
filter(z, name == "Willie Mays")

filter(z, name == "Hank Aaron")

filter(z, name == "Tony Gwynn")

filter(data, name == "Tony Gwynn") %>% arrange(condition, desc(price))

filter(data, name == "Ryne Sandberg") %>% arrange(condition, desc(price))




################################################################################

# [5] List the 2007 cards by group and price:

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(googlesheets4)

# get the list of cards that I want to price:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)

# combine all the cards into one data frame:
cards <- bind_rows(cards1, cards2, cards3)

x <- cards %>%
  select(name:remaining_price) %>%
  mutate(group = rep(1:3, c(nrow(cards1), nrow(cards2), nrow(cards3)))) %>%
  filter(year == 2007) %>%
  as.data.frame()



filter(cards, year == 2007) %>%
  select()









################################################################################

# [6] List the cards for a given year and match against a seller:

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
cards <- bind_rows(cards1, cards2, cards3)
cards <- replace_na(cards, list(own = 0))
cards$type <- type


filter(cards, year == 1987) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1990, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1996, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1996, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1997, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1993, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1976, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1976, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)


filter(cards, year == 1978, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1978, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)


filter(cards, year == 1979, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1979, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1980, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:price, type)

filter(cards, year == 1980, own == 0) %>%
  arrange(desc(price)) %>%
  select(name:price, type)


yr <- 1986
filter(cards, year == yr, own == 1) %>%
  arrange(desc(price)) %>%
  select(name:own, type) %>%
  bind_rows(
    filter(cards, year == yr, own == 0) %>%
      arrange(desc(price)) %>%
      select(name:own, type))


cards %>% group_by(year) %>%
  summarize(n = n(), 
            own = sum(own)) %>%
  mutate(left = n - own) %>%
  as.data.frame()

  


