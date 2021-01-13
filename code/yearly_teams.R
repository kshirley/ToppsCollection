################################################################################

# [1] Write a script to get a list of teams in each year of Topps sets.
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

# get set ID for each year:
set_id <- as.integer(sapply(strsplit(cards1$url, split = "/"), "[", 4))

yr <- unique(data.frame(year = cards1$year, set_id))

team_list <- vector("list", nrow(yr))
u <- rep("", nrow(yr))
for (i in 1:nrow(yr)) {
  print(i)
  print(yr$year[i])
  u[i] <- paste0("https://www.tcdb.com/ViewTeams.cfm/sid/", yr$set_id[i], 
              "/", yr$year[i], "-Topps")

  team_list[[i]] <- read_html(x = u[i]) %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_nodes("li") %>%
    html_text()
  print(length(team_list[[i]]))
}

# gather the teams into a data frame:
team_df <- data.frame(year = rep(1952:2020, sapply(team_list, length)), 
                      team = unlist(team_list), 
                      url = rep(u, sapply(team_list, length)))

# su(team_df$team)
# 
# unique_teams <- team_df %>%
#   group_by(team) %>%
#   summarize(n = n()) %>%
#   as.data.frame() %>%
#   arrange(desc(n))
# 
# fwrite(unique_teams, file = "data/unique_teams.csv")

# read in the edited list of unique teams to keep
ut <- fread("data/unique_teams_edited.csv", data.table = FALSE)

# Filter out teams that were never in MLB:
yearly_teams <- filter(team_df, team %in% ut$team)
# fwrite(yearly_teams, file = "data/yearly_team_list.csv")



### Scrape team wins for each league, each year:
league <- c("AL", "NL")
ids <- c("#expanded_standings_overall", "#standings_E", "#standings_C", "#standings_W", 
         "#standings_E_overall", "#standings_W_overall")

standings <- vector("list", 300)
counter <- 1
for (i in 1:nrow(yr)) {
  print(yr$year[i])
  for (j in 1:2) {
    url <- paste0("https://www.baseball-reference.com/leagues/", league[j], "/", 
                  yr$year[i], ".shtml")
    page <- read_html(url)
    s <- vector("list", 4)
    for (k in 1:6) {
      s[[k]] <- page %>%
        html_nodes(ids[k])
    }
    if (length(s[[1]]) > 0) {
      team_names <- s[[1]] %>% 
        .[[1]] %>%
        html_nodes("a") %>%
        html_attr("title")
      wl <- s[[1]] %>%
        .[[1]] %>%
        html_table() %>%
        head(n = length(team_names))
      st <- data.frame(year = yr$year[i], team = team_names, wins = wl$W, losses = wl$L)
      standings[[counter]] <- st
      counter <- counter + 1
    } else {
      for (k in 2:6) {
        if (length(s[[k]]) > 0) {
          st <- s[[k]] %>% .[[1]] %>% html_table() %>% 
            select(team = Tm, wins = W, losses = L) %>%
            mutate(year = yr$year[i]) %>%
            select(year, team:losses)
          standings[[counter]] <- st
          counter <- counter + 1
        }
      }
    }
  }
}


# bind the rows together:
x <- bind_rows(standings)

fwrite(x, file = "data/yearly_standings.csv")


yearly_teams %>%
  group_by(team) %>%
  summarize(n_sets = n()) %>%
  full_join(x %>% group_by(team) %>%
              summarize(n_yrs = n()), 
            by = "team") %>%
  as.data.frame() %>%
  arrange(desc(n_sets))






