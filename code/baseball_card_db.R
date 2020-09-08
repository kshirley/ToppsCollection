################################################################################

# [1] Match my list of hall of famers (from baseball reference + HOF vis)
#     to the Trading Card DataBase list:
setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rvest)

# read in the player data:
player_data <- fread("~/public_git/mlb-hall-of-fame-voting/player_data.csv", 
                     data.table = FALSE)
election_data <- fread("~/public_git/mlb-hall-of-fame-voting/election_data.csv", 
                       data.table = FALSE)

# write out a file of BBWAA hall of famers:
hof <- filter(election_data, pct >= 75) %>%
  arrange(YoB, desc(pct))

vet <- filter(player_data, method == 4)

all <- select(hof, Name, YoB, pct, Year) %>%
  bind_rows(select(vet, Name, Year = induction.year) %>% 
              mutate(YoB = NA, pct = NA))

# 213 players:
nrow(all)

# Get the list of hall of famers from Trading Card DB:
hof_url <- "https://www.tradingcarddb.com/HOF.cfm?Type=Baseball"

# read the URL:
hof_list <- read_html(hof_url)

# extract their names from baseball card DB:
db_hofers <- hof_list %>% 
  html_nodes("li > a") %>%
  html_attr("href")

db_names <- strsplit(db_hofers, split = "/") %>%
  sapply("[", 5)
db_names <- gsub(".", "", db_names, fixed = TRUE)
db_names <- gsub("-", " ", db_names, fixed = TRUE)

db_hofers <- db_hofers[!is.na(db_names)]
db_names <- db_names[!is.na(db_names)]

# # get my first set of players:
# first_ballot <- all %>%
#   filter(YoB == 1)
# 
# first_ballot$Name[first_ballot$Name == "Cal Ripken"] <- "Cal Ripken Jr"
# 
# first_ballot <- first_ballot %>%
#   left_join(data.frame(Name = db_names, 
#                        url = db_hofers), 
#             by = "Name")

# Correct two names:
all$Name[all$Name == "Cal Ripken"] <- "Cal Ripken Jr"
all$Name[all$Name == "Pete Alexander"] <- "Grover Cleveland Alexander"

df <- all %>%
  left_join(data.frame(Name = db_names, 
                       url = db_hofers), 
            by = "Name")


pid <- sapply(strsplit(df$url, split = "/"), function(x) x[[4]])
player_string <- sapply(strsplit(df$url, split = "/"), function(x) x[[5]])
df$pid <- pid
df$player_string <- player_string

# just focus on the first ballot hall of famers
first_ballot <- filter(df, YoB == 1)
n_players <- nrow(first_ballot)

cards <- vector("list", n_players)
player_url <- rep("", n_players)
for (i in 1:n_players) {
  print(i)
  Sys.sleep(2)
  player_prefix <- "https://www.tcdb.com/Person.cfm/pid/"
  filters <- "?sTeam=&sCardNum=&sNote=&sSetName=Topps&sBrand="
  player_url[i] <- paste0(player_prefix, 
                       first_ballot$pid[i], 
                       "/col/Y/yea/0/", 
                       player_string[i], 
                       filters)
  
  # get the raw html of first page:
  raw <- read_html(player_url[i])
  
  # if only one page:
  if (raw %>% html_nodes("nav") %>% length() == 2) {
    cards[[i]] <- read_html(player_url[i]) %>%
      html_nodes("table") %>%
      .[[4]] %>%
      html_nodes("tr") %>%
      html_node("a") %>%
      html_attr("href")
  } else {  # if two pages 
    if (raw %>% html_nodes("nav") %>% length() == 4) {
      part1 <- raw %>%
        html_nodes("table") %>%
        .[[4]] %>%
        html_nodes("tr") %>%
        html_node("a") %>%
        html_attr("href")
      url2 <- paste0(player_prefix, 
                     first_ballot$pid[i], 
                     "/col/Y/yea/0/", 
                     player_string[i], 
                     paste0("?PageIndex=2&", filters))
      part2 <- read_html(url2) %>%
        html_nodes("table") %>%
        .[[4]] %>%
        html_nodes("tr") %>%
        html_node("a") %>%
        html_attr("href")
      cards[[i]] <- c(part1, part2)
    }
  }
}


all_cards <- data.frame(name = rep(first_ballot$Name, sapply(cards, length)), 
                        player_url = rep(player_url, sapply(cards, length)), 
                        card_url = unlist(cards))


fwrite(all_cards, file = "card_collection_v2.csv")  







################################################################################

# [2] Read in annotated card list, with indicators of whether I want to 
#     have it in my collection; this took an hour or two of manual work
#     to mark which card was the 'main' card for a given player and year 
#     (compared to 'special' cards like all-stars, career milestones, etc.)

setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rvest)

# read in the player data:
player_data <- fread("~/public_git/mlb-hall-of-fame-voting/player_data.csv", 
                     data.table = FALSE)
election_data <- fread("~/public_git/mlb-hall-of-fame-voting/election_data.csv", 
                       data.table = FALSE)

# write out a file of BBWAA hall of famers:
hof <- filter(election_data, pct >= 75) %>%
  arrange(YoB, desc(pct))

vet <- filter(player_data, method == 4)

all <- select(hof, Name, YoB, pct, Year) %>%
  bind_rows(select(vet, Name, Year = induction.year) %>% 
              mutate(YoB = NA, pct = NA))


# read in the cards:
cards <- fread("data/card_collection_annotated.csv", data.table = FALSE) %>%
  filter(collect == 1)

# get the URL split up:
tmp <- sapply(strsplit(cards$card_url, split = "/"), function(x) x[[7]])

# Just split the last part of the URL:
card_split <- strsplit(tmp, split = "-")

# pull out the year and the number:
year <- sapply(card_split, function(x) x[[1]])
number <- sapply(card_split, function(x) x[[3]])

# add this to the data frame:
cards <- mutate(cards, year = year, number = number) %>%
  select(name, year, number, url = card_url)

# Now loop through the cards and get prices and image URLs:
n_cards <- nrow(cards)
front_url <- rep("", n_cards)
back_url <- rep("", n_cards)
price <- numeric(n_cards)


prefix <- "https://www.tradingcarddb.com"
for (i in 1:n_cards) {
  if (i %% 10 == 0) print(i)
  Sys.sleep(2)
  card <- read_html(paste0(prefix, cards$url[i]))
  imgs <- card %>% html_nodes("img") %>%
    html_attr("src")
  front_url[i] <- imgs[grep("Fr.jpg", imgs)]
  back_url[i] <- imgs[grep("Bk.jpg", imgs)]
  price_text <- card %>% html_nodes("em") %>%
    html_text()
  ix <- grep("Med. Price", price_text)
  price[i] <- as.numeric(gsub("Med. Price: \\$", "", price_text)[ix])
}


# add the new variables to the data frame:
cards <- cards %>%
  mutate(price = price, front_url = front_url, back_url = back_url)

# look at NA prices:
filter(cards, is.na(price))

# look at the most expensive ones:
arrange(cards, desc(price)) %>%
  select(name, year, number, price) %>%
  head(30)

# write the file to disk:
fwrite(cards, file = "data/list.csv")



# for loop to download images:
# front
setwd("~/public_git/ToppsCollection/images/front")
for (i in 1:n_cards) {
  print(i)
  Sys.sleep(1)
  cmd <- paste0("wget ", prefix, cards$front_url[i])
  system(command = cmd)
}

# back
setwd("~/public_git/ToppsCollection/images/back")
for (i in 1:n_cards) {
  print(i)
  Sys.sleep(2.5)
  cmd <- paste0("wget ", prefix, cards$back_url[i])
  system(command = cmd)
}









################################################################################

# [3] Create the webpage to show the grid of card fronts

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

# read in the player data:
player_data <- fread("~/public_git/mlb-hall-of-fame-voting/player_data.csv", 
                     data.table = FALSE)
election_data <- fread("~/public_git/mlb-hall-of-fame-voting/election_data.csv", 
                       data.table = FALSE)

# write out a file of BBWAA hall of famers:
hof <- filter(election_data, pct >= 75) %>%
  arrange(YoB, desc(pct))

vet <- filter(player_data, method == 4)

all <- select(hof, Name, YoB, pct, Year) %>%
  bind_rows(select(vet, Name, Year = induction.year) %>% 
              mutate(YoB = NA, pct = NA))


# read the google sheet data:
cards <- read_sheet("https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing")
cards <- as.data.frame(cards)

# look at lots:
lots <- read_sheet("https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing", 
                   sheet = "purchase_history")

# join with cards table:
cards <- left_join(cards, lots, by = "lot_name")
cards <- replace_na(cards, list(own = 0))

# summarize by player:
players <- cards %>%
  group_by(name) %>%
  summarize(n = n(), 
            first_year = min(year), 
            last_year = max(year), 
            total_price = sum(price), 
            remaining_price = sum(price[own == 0]), 
            own = sum(own)) %>%
  mutate(remain = n - own) %>%
  arrange(first_year, last_year) %>%
  as.data.frame()

# summarize by year:
years <- cards %>%
  group_by(year) %>%
  summarize(n = n(), 
            total_price = sum(price), 
            remaining_price = sum(price[own == 0]), 
            own = sum(own)) %>%
  mutate(remain = n - own) %>%
  as.data.frame()

# Show the cost per year
years %>%
  ggplot(aes(x = year, y = total_price)) + 
  geom_point() + 
  geom_line()

years %>%
  ggplot(aes(x = year, y = remaining_price)) + 
  geom_point() + 
  geom_line()


lots <- lots %>% 
  left_join(cards %>%
              group_by(lot_name) %>%
              summarize(n = n(), 
                        total_price = sum(price)), 
            by = "lot_name") %>%
  mutate(profit = total_price - lot_cost) %>%
  as.data.frame()

lots


# compute total cost for collecting cards going back a certain number of years:
data.frame(year = rev(years$year), 
           total_cost = cumsum(rev(years$total_price)), 
           remaining_cost = cumsum(rev(years$remaining_price)))


# measure resolution of each card:
filename <- sapply(strsplit(cards$front_url, split = "/"), function(x) x[6])
cards <- mutate(cards, filename = filename)

n_cards <- nrow(cards)
res <- matrix(NA, n_cards, 2)
for (i in 1:n_cards) {
  if (i %% 100 == 0) print(i)
  img <- readJPEG(paste0("images/front/", filename[i]))
  res[i, ] <- dim(img)[1:2]
}

cards <- cards %>%
  mutate(height = res[, 1], width = res[, 2])

# cards %>%
#   ggplot(aes(x = width, y = height)) + 
#   geom_point()


# count number of unique players and years:
n_players <- nrow(players)
n_years <- nrow(years)

# set up 'class' for the opacity toggle:
cards$class <- ifelse(cards$own == 1, "own", "dont-own")


# Now, create the grid layout:

cat("<html>\n", file = "./index.html")
cat("<head>\n", file = "./index.html", append = TRUE)
cat("<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js'></script>\n", 
    file = "./index.html", append = TRUE)
cat("<script src='code/toggle_opacity.js'></script>\n", 
    file = "./index.html", append = TRUE)
cat("</head>\n", file = "./index.html", append = TRUE)
cat("<body>\n", file = "./index.html", append = TRUE)
cat("<div>\n", 
     "<button class='allbutton'>Show all cards</button>\n", 
     "<br>\n", 
     "<button class='ownbutton'>Show cards I own</button>\n",
     "</div>\n", sep = "", file = "./index.html", append = TRUE)
cat("<div style='width: 9000px'>", file = "./index.html", 
    append = TRUE)
cat("<table><tr>", file = "./index.html", append = TRUE)
for (i in 1:n_years) {
  cat("<td style='width:100px; text-align:center'>", 1951 + i, "</td>", file = "./index.html", 
      append = TRUE)
}
cat("</tr></table>", file = "./index.html", append = TRUE)
for (i in 1:n_players) {
  tmp <- filter(cards, name == players$name[i])
  for (j in 1:n_years) {
    if ((1951 + j) %in% tmp$year) {
      cat("<a href='https://www.tcdb.com", tmp$url[tmp$year == 1951 + j], 
          "' style='text-decoration:none;'>", 
          file = "./index.html", append = TRUE, sep = "")
      if (tmp$height[tmp$year == 1951 + j] <= tmp$width[tmp$year == 1951 + j]) {  # get the rotated version
        cat("<img src='images/rotated/rotated_", tmp$filename[tmp$year == 1951 + j], 
            "' height=140px width=100px class='", 
            tmp$class[tmp$year == 1951 + j], "'>\n", 
            sep = "", file = "./index.html", append = TRUE)
      } else {
        cat("<img src='images/front/", tmp$filename[tmp$year == 1951 + j], 
            "' height=140px width=100px class='", 
            tmp$class[tmp$year == 1951 + j], "'>\n", 
            sep = "", file = "./index.html", append = TRUE)
      }
      cat("</a>", file = "./index.html", append = TRUE)
    } else {
      cat("<img src='images/blank.jpg' height=140px width=100px>\n", 
          file = "./index.html", append = TRUE)
    }
  }
  cat("<br>\n", file = "./index.html", append = TRUE)
}
cat("</div>", file = "./index.html", append = TRUE)
cat("</body>\n", file = "./index.html", append = TRUE)
cat("</html>\n", file = "./index.html", append = TRUE)

# this one works.



scp ~/public_git/ToppsCollection/index.html kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards

# python -m http.server 8000



# non-first ballot:
filter(all, YoB > 1)

filter(all, is.na(YoB))




# look at 1974 cards:
# tmp <- filter(cards, year == 1974)

# # identify all horizontal layouts:
# tmp <- filter(cards, width > height)
# 
# # look at them:
# cat("<html>\n", file = "~/personal/bball_cards/index.html")
# cat("<body>\n", file = "~/personal/bball_cards/index.html", append = TRUE)
# for (i in 1:nrow(tmp)) {
#     cat("<img src='front/", tmp$filename[i], "' >\n", 
#         sep = "", file = "~/personal/bball_cards/index.html", append = TRUE)
# }
# cat("</body>\n", file = "~/personal/bball_cards/index.html", append = TRUE)
# cat("</html>\n", file = "~/personal/bball_cards/index.html", append = TRUE)



# # rotate the 80 images that are horizontally laid out and write to disk:
# for (i in 1:nrow(tmp)) {
#   card <- image_read(paste0("data/front/", tmp$filename[i]))
#   image_rotate(card, 270) %>%
#     image_write(paste0("data/rotated/rotated_", tmp$filename[i]))
# }






################################################################################

# [4] For one ebay seller, compute which cards in my collection are available

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

# read in the player data:
player_data <- fread("~/public_git/mlb-hall-of-fame-voting/player_data.csv", 
                     data.table = FALSE)
election_data <- fread("~/public_git/mlb-hall-of-fame-voting/election_data.csv", 
                       data.table = FALSE)

# write out a file of BBWAA hall of famers:
hof <- filter(election_data, pct >= 75) %>%
  arrange(YoB, desc(pct))

vet <- filter(player_data, method == 4)

all <- select(hof, Name, YoB, pct, Year) %>%
  bind_rows(select(vet, Name, Year = induction.year) %>% 
              mutate(YoB = NA, pct = NA))


# read the google sheet data:
cards <- read_sheet("https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing")
cards <- as.data.frame(cards)
cards <- replace_na(cards, list(own = 0))

# summarize by player:
players <- cards %>%
  group_by(name) %>%
  summarize(n = n(), 
            first_year = min(year), 
            last_year = max(year), 
            total_price = sum(price), 
            remaining_price = sum(price[own == 0]), 
            own = sum(own)) %>%
  mutate(remain = n - own) %>%
  arrange(first_year, last_year) %>%
  as.data.frame()

# summarize by year:
years <- cards %>%
  group_by(year) %>%
  summarize(n = n(), 
            total_price = sum(price), 
            remaining_price = sum(price[own == 0]), 
            own = sum(own)) %>%
  mutate(remain = n - own) %>%
  as.data.frame()


# read in set of ebay cards that are available:
ebay <- fread("data/ebay_set_completion.csv", data.table = FALSE) %>%
  mutate(available = 1)


collection <- cards %>% filter(year >= 1993, year <= 1997) %>%
  select(name:lot) %>%
  mutate(number = as.integer(number), 
         year = as.integer(year))


x <- collection %>%
  left_join(ebay, by = c("year", "number")) %>%
  replace_na(list(available = 0))



filter(x, available == 1) %>%
  select(year, number, name) %>%
  print(row.names = FALSE)
  
filter(x, available == 1) %>%
  summarize(sum(price))
  

filter(x, available == 1, own == 0) %>%
  summarize(sum(price))




# petruccos 1973 topps
# 280 Al Kaline; 3 asterisks, 6 picks
# 175 Frank Robinson, 3 asterisks, 6 picks
# 245 Carl Yastremski, 3 asterisks, 6 picks
# 255 Reggie Jackson, 5 asterisks, 12 picks
# 220 Nolan Ryan, 48 picks





################################################################################

# [5] Get the list of Topps cards for HOF players (NumBallots > 1)
#     and my personal list of extra players.

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

# read in the player data:
player_data <- fread("~/public_git/mlb-hall-of-fame-voting/player_data.csv", 
                     data.table = FALSE)
election_data <- fread("~/public_git/mlb-hall-of-fame-voting/election_data.csv", 
                       data.table = FALSE)

# write out a file of BBWAA hall of famers:
hof <- filter(election_data, pct >= 75) %>%
  arrange(YoB, desc(pct))

vet <- filter(player_data, method == 4)

all <- select(hof, Name, YoB, pct, Year) %>%
  bind_rows(select(vet, Name, Year = induction.year) %>% 
              mutate(YoB = NA, pct = NA))

# mark veterans committee electees:
all <- all %>%
  mutate(method = ifelse(is.na(YoB), "vet", "bbwaa"))

# 213 players:
nrow(all)

# Get the list of hall of famers from Trading Card DB:
hof_url <- "https://www.tradingcarddb.com/HOF.cfm?Type=Baseball"

# read the URL:
hof_list <- read_html(hof_url)

# extract their names from baseball card DB:
db_hofers <- hof_list %>% 
  html_nodes("li > a") %>%
  html_attr("href")

db_names <- strsplit(db_hofers, split = "/") %>%
  sapply("[", 5)
db_names <- gsub(".", "", db_names, fixed = TRUE)
db_names <- gsub("-", " ", db_names, fixed = TRUE)

db_hofers <- db_hofers[!is.na(db_names)]
db_names <- db_names[!is.na(db_names)]

# Correct two names:
all$Name[all$Name == "Cal Ripken"] <- "Cal Ripken Jr"
all$Name[all$Name == "Pete Alexander"] <- "Grover Cleveland Alexander"

# add db names to data frame:
df <- all %>%
  left_join(data.frame(Name = db_names, 
                       url = db_hofers), 
            by = "Name")

# extract player ID and string/name:
pid <- sapply(strsplit(df$url, split = "/"), function(x) x[[4]])
player_string <- sapply(strsplit(df$url, split = "/"), function(x) x[[5]])
df$pid <- pid
df$player_string <- player_string


head(df)

#             Name YoB    pct Year method                                  url  pid   player_string
# 1 Mariano Rivera   1 100.00 2019  bbwaa  /Person.cfm/pid/4960/Mariano-Rivera 4960  Mariano-Rivera
# 2    Derek Jeter   1  99.75 2020  bbwaa     /Person.cfm/pid/2885/Derek-Jeter 2885     Derek-Jeter
# 3 Ken Griffey Jr   1  99.32 2016  bbwaa /Person.cfm/pid/2245/Ken-Griffey-Jr. 2245 Ken-Griffey-Jr.
# 4     Tom Seaver   1  98.84 1992  bbwaa      /Person.cfm/pid/5301/Tom-Seaver 5301      Tom-Seaver
# 5     Nolan Ryan   1  98.79 1999  bbwaa      /Person.cfm/pid/5128/Nolan-Ryan 5128      Nolan-Ryan
# 6  Cal Ripken Jr   1  98.53 2007  bbwaa  /Person.cfm/pid/4943/Cal-Ripken-Jr. 4943  Cal-Ripken-Jr.


tail(df)
#               Name YoB pct Year method                                  url   pid  player_string
# 208     Amos Rusie  NA  NA 1977    vet     /Person.cfm/pid/30100/Amos-Rusie 30100     Amos-Rusie
# 209     Joe Kelley  NA  NA 1971    vet      /Person.cfm/pid/3049/Joe-Kelley  3049     Joe-Kelley
# 210 Billy Hamilton  NA  NA 1961    vet /Person.cfm/pid/49416/Billy-Hamilton 49416 Billy-Hamilton
# 211   Jake Beckley  NA  NA 1971    vet     /Person.cfm/pid/374/Jake-Beckley   374   Jake-Beckley
# 212     Buck Ewing  NA  NA 1939    vet     /Person.cfm/pid/36750/Buck-Ewing 36750     Buck-Ewing
# 213    Elmer Flick  NA  NA 1963    vet     /Person.cfm/pid/1854/Elmer-Flick  1854    Elmer-Flick

# OK. We have the URL for the top 213 players.



### read in the others that I want to collect:
others <- read_sheet("https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing", 
                     sheet = "player_list")

others <- filter(others, method %in% c("bbwaa", "vet") == FALSE)

others <- others %>% 
  select(-album) %>%
  mutate(pid = "", player_string = "")


others$url <- gsub("https://www.tcdb.com", "", others$url, fixed = TRUE)

# extract player ID and string/name:
others$pid <- sapply(strsplit(others$url, split = "/"), function(x) x[[4]])
others$player_string <- sapply(strsplit(others$url, split = "/"), function(x) x[[5]])


# Combine the original ones with the personal list
x <- bind_rows(df, others)
x <- filter(x, YoB > 1 | is.na(YoB))
n_players <- nrow(x)
# 222

# set up the list of cards for each player:
cards <- vector("list", n_players)
player_url <- rep("", n_players)
player_prefix <- "https://www.tcdb.com/Person.cfm/pid/"
filters <- "?sTeam=&sCardNum=&sNote=&sSetName=Topps&sBrand="
for (i in 157:n_players) {
  print(i)
  Sys.sleep(2)
  player_url[i] <- paste0(player_prefix, 
                          x$pid[i], 
                          "/col/Y/yea/0/", 
                          x$player_string[i], 
                          filters)
  
  # get the raw html of first page:
  raw <- read_html(player_url[i])
  
  # if only one page:
  if (raw %>% html_nodes("nav") %>% length() == 2) {
    cards[[i]] <- read_html(player_url[i]) %>%
      html_nodes("table") %>%
      .[[4]] %>%
      html_nodes("tr") %>%
      html_node("a") %>%
      html_attr("href")
  } else {  # if two pages 
    if (raw %>% html_nodes("nav") %>% length() == 4) {
      part1 <- raw %>%
        html_nodes("table") %>%
        .[[4]] %>%
        html_nodes("tr") %>%
        html_node("a") %>%
        html_attr("href")
      url2 <- paste0(player_prefix, 
                     x$pid[i], 
                     "/col/Y/yea/0/", 
                     x$player_string[i], 
                     paste0("?PageIndex=2&", filters))
      part2 <- read_html(url2) %>%
        html_nodes("table") %>%
        .[[4]] %>%
        html_nodes("tr") %>%
        html_node("a") %>%
        html_attr("href")
      cards[[i]] <- c(part1, part2)
    }
  }
}


all_cards <- data.frame(name = rep(x$Name, sapply(cards, length)), 
                        player_url = rep(player_url, sapply(cards, length)), 
                        card_url = unlist(cards))


fwrite(all_cards, file = "data/card_collection_part2.csv")  

# Now, using this local file, fill out a 0/1 where 1 = include in the collection.
# Return the file as an "annotated" file.



# cards <- read_sheet("https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing")
# cards <- as.data.frame(cards)



# fwrite(all, file = "data/player_master_list.csv")

# Add Veterans - maybe:
Dave Parker
Steve Garvey
Don Mattingly
Daly Murphy
Mark McGwire
Fred McGriff
Dwight Evans
Kenny Lofton


# Add retired, likely:
Curt Schilling
Barry Bonds
Roger Clemens
Omar Vizquel
Scott Rolen
Todd Helton
Tim Hudson
Alex Rodriguez
David Ortiz
Mark Teixiera
Carlos Beltran
Adrian Beltre
Joe Mauer
Chase Utley
David Wright
Andy Pettitte


# Add retired, possible:
Billy Wagner
Gary Sheffield
Manny Ramirez
Andruw Jones
Sammy Sosa
Mark Buehrle

# Current locks
Mike Trout
Albert Pujols
Miguel Cabrera
Justin Verlander
Max Scherzer
Clayton Kershaw
Mookie Betts
Bryce Harper
Manny Machado
Kris Bryant
Christian Yelich
Cody Bellinger

# Current maybe
Zack Greinke
Robinson Cano
Joey Votto
Buster Posey
Stephen Strasburg




################################################################################

# [6] Simulate opening a box of 36 packs of 1986 Topps
setwd("~/public_git/ToppsCollection")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rvest)

# sample:
n <- 100000
num <- numeric(n)
set.seed(9876)
for (i in 1:n) {
  box <- sample.int(n = 792, size = 540, replace = TRUE)
  num[i] <- sum(1:14 %in% box)
}


hist(num, breaks = 0:15)


data.frame(num = num) %>%
  group_by(num) %>%
  summarize(freq = n()) %>%
  as.data.frame() %>%
  mutate(p = round(freq / 1e5, 3)) %>%
  mutate(cum_prob = cumsum(p)) %>%
  print(row.names = FALSE)


# num  freq     p cum_prob
#   0     4 0.000    0.000
#   1    97 0.001    0.001
#   2   586 0.006    0.007
#   3  2356 0.024    0.031
#   4  6461 0.065    0.096
#   5 12780 0.128    0.224
#   6 18855 0.189    0.413
#   7 20936 0.209    0.622
#   8 17950 0.180    0.802
#   9 11858 0.119    0.921
#  10  5594 0.056    0.977
#  11  1960 0.020    0.995
#  12   480 0.005    0.998
#  13    79 0.001    0.999
#  14     4 0.000    1.000

# P(5 or less) = 22%
# P(6, 7, 8)   = 58%
# P(9 or more) = 20%

# all the below have about a 20% chance:
# 4-5
# 6
# 7
# 8
# 9-10





################################################################################

# [7] Price analysis of bicentennial bats
setwd("~/personal")
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rvest)

# read in the data:
bats <- fread("bicentennial_bats.csv", data.table = FALSE)
bats$date <- as.Date(bats$date)
bats <- arrange(bats, desc(date))


x <- bats %>%
  group_by(player) %>%
  summarize(n = n(), 
            mean_price = mean(price), 
            median_price = median(price), 
            min_price = min(price), 
            max_price = max(price), 
            most_recent_price = price[1]) %>%
  as.data.frame() %>%
  arrange(desc(n))








################################################################################

### old code


# sample URL of a player filter for Topps, major release:
# https://www.tradingcarddb.com/Person.cfm/pid/6114/Larry-Walker?MODE=Filters&ColType=Y&sYear=0&sTeam=&sCardNum=&sNote=&sSetName=&sBrand=Topps

# prefix <- "https://www.tradingcarddb.com"
# paste0(prefix, df$url[1], "?MODE=Filters&ColType=Y&sYear=0&sTeam=&sCardNum=&sNote=&sSetName=&sBrand=Topps")

# filter doesn't work.

# Use the set ID for each Topps set, and get HOF players (using website)
# This gives every URL I want
# Can go through in excel for each year and identify the cards I want


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
topps <- data.frame(year = 1952:2020, sid = sid)
n_years <- nrow(topps)

cards <- vector("list", n_years)
for (i in 1:n_years) {
  print(i)
  Sys.sleep(3)
  url <- paste0(prefix, "/SetHOF.cfm/sid/", topps$sid[i], "/", topps$year[i], "-Topps")
  
  page <- read_html(url)
  
  x <- page %>%
    html_nodes("table") %>%
    html_table() %>% 
    .[[6]]
  
  # get the player info for each card:
  x <- x[, c(4, 6, 8)]
  names(x) <- c("number", "name", "team")

  # get the card ID for each card:
  z <- page %>%
    html_nodes("table") %>%
    .[[6]] %>%
    html_nodes("tr") %>%
    html_node("a") %>%
    html_attr("href")
  
  cards[[i]] <- x %>% mutate(card_url = z)
}


for (i in 1:n_years) {
  cards[[i]] <- mutate(cards[[i]], year = 1951 + i)
}

for (i in 1:n_years) {
  cards[[i]] <- select(cards[[i]], year, number:card_url)
}

for (i in 1:n_years) {
  cards[[i]]$number <- as.character(cards[[i]]$number)
}

# put it all together:
all_cards <- bind_rows(cards)


filter(all_cards, grepl("Tony Gwynn RC", name)) %>%
  bind_rows(filter(all_cards, grepl("^Tony Gwynn$", name)))

i <- 2
filter(all_cards, grepl(paste0(all$Name[i],"*RC"), name)) %>%
  bind_rows(filter(all_cards, grepl(paste0("^", all$Name[i], "$"), name)))

filter(all_cards, grepl(all$Name[i], name))

# Doesn't work perfectly... misses a few cards, hard to identify which
# card in a given set is the 'main' card for the player, vs. some special
# card commemorating a milestone, an all-star game, or something else...


