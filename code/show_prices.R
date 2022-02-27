################################################################################

# Make the webpage so that it shows the prices, and the rectangles are
# color-coded by price.

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

# fix URLs:
cards1$url <- gsub(pattern = "https://www.tcdb.com", replacement = "", x = cards1$url, fixed = TRUE)
cards2$url <- gsub(pattern = "https://www.tcdb.com", replacement = "", x = cards2$url, fixed = TRUE)
cards3$url <- gsub(pattern = "https://www.tcdb.com", replacement = "", x = cards3$url, fixed = TRUE)

purch <- read_sheet(google_docs_url, sheet = "purchase_history")







# function to build the webpage locally:
make_page <- function(cards_df, output) {
  
  # set own = 0 instead of NA
  cards <- replace_na(cards_df, list(own = 0))
  
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
  
  
  # count number of unique players and years:
  n_players <- lu(cards$name)
  n_years <- lu(cards$year)
  
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
}


# this one works.


### Create the page for each of the three sets of cards & scp to cloud:
make_page(cards1)

