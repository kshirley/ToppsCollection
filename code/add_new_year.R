################################################################################

# [1] Add 2021 cards
#     Get current list of players and get the new cards

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


# read the google sheet data:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)

# put them all together:
my_cards <- bind_rows(cards1, cards2, cards3)

# get the player ID for every player who might have a card in 2021 Topps
# we can ignore BBWAA players. But have to include others.

player_df <- read_sheet(google_docs_url, sheet = "player_list")
player_df <- as.data.frame(player_df)

# all NA players are not in 2021 topps. only need to check players with a URL
# here in this data frame:
df <- filter(player_df, !is.na(url) & !(method == "current_maybe_add"))
nrow(df)
# 69 players

# get their player ID and player name/string:
pid <- sapply(strsplit(df$url, split = "/"), function(x) x[[6]])
player_string <- sapply(strsplit(df$url, split = "/"), function(x) x[[7]])
df$pid <- pid
df$player_string <- player_string

# just focus on the first ballot hall of famers
n_players <- nrow(df)

cards <- vector("list", n_players)
player_url <- rep("", n_players)
for (i in 1:n_players) {
  print(i)
  Sys.sleep(2)
  player_prefix <- "https://www.tcdb.com/Person.cfm/pid/"
  filters <- "?sTeam=&sCardNum=&sNote=&sSetName=Topps&sBrand="
  player_url[i] <- paste0(player_prefix, 
                          df$pid[i], 
                          "/col/Y/yea/2021/", 
                          df$player_string[i], 
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


new_cards <- data.frame(name = rep(df$Name[1:69], sapply(cards[1:69], length)), 
                        player_url = rep(player_url[1:69], sapply(cards[1:69], length)), 
                        card_url = unlist(cards[1:69]))


fwrite(new_cards, file = "data/card_collection_2021.csv")  




################################################################################

# [2] add attributes of new cards, download images, and rotate if necessary:

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


# read the google sheet data:
google_docs_url <- "https://docs.google.com/spreadsheets/d/1_vuLfUs1QoaBztfUqJRHFz61FE9ep5_cMxBH3EgXu1c/edit?usp=sharing"
cards1 <- read_sheet(google_docs_url, sheet = "first_ballot_cards")
cards1 <- as.data.frame(cards1)
cards2 <- read_sheet(google_docs_url, sheet = "bbwaa_cards")
cards2 <- as.data.frame(cards2)
cards3 <- read_sheet(google_docs_url, sheet = "vet_cards")
cards3 <- as.data.frame(cards3)


# read in the 2021 annotated cards:
new_cards <- fread("data/card_collection_2021_annotated.csv", data.table = FALSE)
new_cards <- filter(new_cards, collect == 1)

head(new_cards)
head(cards1)


### Card Attributes

# get the URL split up:
tmp <- sapply(strsplit(new_cards$card_url, split = "/"), function(x) x[[7]])

# Just split the last part of the URL:
card_split <- strsplit(tmp, split = "-")

# pull out the year and the number:
year <- sapply(card_split, function(x) x[[1]])
number <- sapply(card_split, function(x) x[[3]])

# add this to the data frame:
cards <- mutate(new_cards, year = year, number = number) %>%
  select(name, year, number, url = card_url)

# Now loop through the cards and get prices and image URLs:
n_cards <- nrow(cards)
front_url <- rep("", n_cards)
back_url <- rep("", n_cards)
price <- numeric(n_cards)


prefix <- "https://www.tradingcarddb.com"
for (i in 1:n_cards) {
  if (i %% 2 == 0) print(i)
  # Sys.sleep(1)
  card <- read_html(paste0(prefix, cards$url[i]))
  imgs <- card %>% html_nodes("img") %>%
    html_attr("src")
  front_url[i] <- imgs[grep("Fr.jpg", imgs)]
  back_url[i] <- imgs[grep("Bk.jpg", imgs)]
  price_text <- card %>% html_nodes("em") %>%
    html_text()
  ix <- grep("Med. Price", price_text)
  if (length(ix) > 0) {
    price[i] <- as.numeric(gsub("Med. Price: \\$", "", price_text)[ix])
  } else {
    price[i] <- NA
  }
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


cards <- cards %>% 
  mutate(own = 0, 
         lot_name = "", 
         remaining_price = 0) %>% 
  select(name, year, number, price, own, lot_name, remaining_price, url, 
         front_url, back_url)

# write the file to disk:
fwrite(cards, file = "data/additions_2021_final.csv")




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
  Sys.sleep(1.5)
  cmd <- paste0("wget ", prefix, cards$back_url[i])
  system(command = cmd)
}



### rotate landscape images:
setwd("~/public_git/ToppsCollection")
filename <- sapply(strsplit(cards$front_url, split = "/"), function(x) x[6])
cards <- mutate(cards, filename = filename)

res <- matrix(NA, n_cards, 2)
for (i in 1:n_cards) {
  if (i %% 100 == 0) print(i)
  img <- readJPEG(paste0("images/front/", filename[i]))
  res[i, ] <- dim(img)[1:2]
}

cards <- cards %>%
  mutate(height = res[, 1], width = res[, 2])


# identify all horizontal layouts:
tmp <- filter(cards, width > height)


# rotate the 80 images that are horizontally laid out and write to disk:
for (i in 1:nrow(tmp)) {
  if(!file.exists(paste0("images/rotated/rotated_", tmp$filename[i]))) {
    card <- image_read(paste0("images/front/", tmp$filename[i]))
    image_rotate(card, 270) %>%
      image_write(paste0("images/rotated/rotated_", tmp$filename[i]))
  }
}



# now, scp the files to remote machine.
# also manually paste the rows into googlesheets.

# sync the images from my local machine to the server:
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards/images
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards_part2/images
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards_part3/images











