################################################################################

# [1] Scripts to add topps traded and update cards to the collection

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

type <- rep(c("first_ballot", "bbwaa", "vet_or_other"), 
            c(nrow(cards1), nrow(cards2), nrow(cards3)))
my_cards <- replace_na(my_cards, list(own = 0))
my_cards$type <- factor(type, levels = c("first_ballot", "bbwaa", "vet_or_other"))


# Get the list of players:
p <- my_cards %>% 
  group_by(name, type) %>% 
  summarize(year = min(year)) %>% 
  arrange(year)

fwrite(p, file = "data/traded_and_update.csv")

# read in the annotated file:
new_cards <- fread("data/traded_and_update_annotated.csv", data.table = FALSE)

# read in the 2021 annotated cards:
new_cards <- filter(new_cards, card_url != "")
new_cards <- select(new_cards, -year)

### Card Attributes

# get the URL split up:
tmp <- sapply(strsplit(new_cards$card_url, split = "/"), function(x) x[[9]])

# Just split the last part of the URL:
card_split <- strsplit(tmp, split = "-")

# pull out the year and the number:
year <- sapply(card_split, function(x) x[[1]])
number <- new_cards$number
# number <- sapply(card_split, function(x) x[[3]])

# add this to the data frame:
cards <- mutate(new_cards, year = year, number = number) %>%
  select(name, year, number, url = card_url)

# Now loop through the cards and get prices and image URLs:
n_cards <- nrow(cards)
front_url <- rep("", n_cards)
back_url <- rep("", n_cards)
price <- numeric(n_cards)


prefix <- ""
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
fwrite(cards, file = "data/additions_traded_and_update_final.csv")




# for loop to download images:

# just get one row (for 99 topps traded Sabathia):
cards <- filter(my_cards, year == 1999, name == "C.C. Sabathia")
n_cards <- nrow(cards)

prefix <- "https://www.tradingcarddb.com"

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




# cards$year <- year
cards <- cards %>% left_join(select(p, name, type))
cards$url <- gsub("https://www.tcdb.com", "", cards$url)

c1 <- filter(cards, type == "first_ballot") %>% 
  select(name:back_url)

c2 <- filter(cards, type == "bbwaa") %>% 
  select(name:back_url)

c3 <- filter(cards, type == "vet_or_other") %>% 
  select(name:back_url)

fwrite(c1, file = "data/traded_update_1.csv")
fwrite(c2, file = "data/traded_update_2.csv")
fwrite(c3, file = "data/traded_update_3.csv")

# now, scp the files to remote machine.
# also manually paste the rows into googlesheets.

# sync the images from my local machine to the server:
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards/images
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards_part2/images
# rsync -avzn ~/public_git/ToppsCollection/images/ kes@66.228.42.50:/home/kes/public/kennyshirley.com/public_html/bball_cards_part3/images











