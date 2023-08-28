#--------------------------------------------------#
# Loading libraries and fetching API anime data    #
#--------------------------------------------------#

library(RCurl)
library(XML)
library(dplyr)
library(rvest)

url <- "https://www.animenewsnetwork.com/encyclopedia/reports.php?id=155&nlist=all"
page <- read_html(url)
id_table <- html_table(page)
id_table <- as.data.frame(id_table) %>%
  slice(-1) %>%
  select(-7)

id_table <- rename(id_table,
                   id = Var.1,
                   gid = Var.2, type = Var.3,
                   name = Var.4,
                   precision = Var.5,
                   vintage = Var.6
) %>%
  filter(type %in% c("OAV", "TV", "movie"))  
id_table$name <- sort(id_table$name)


id_anime <- as.list(filter(id_table, type == "TV") %>% select(id))

#----------------------------------------------------#
# Fetching API anime data                            #
#----------------------------------------------------#

anime_df <- list()

baseapi <- "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?"
id <- as.numeric(unlist(id_anime))
anime_url <- paste0(baseapi, "anime=", id)

for (i in anime_url) { # Iterate through each manga id and retrieve info
  get_anime <- getURL(i)
  anime_dat <- xmlParse(get_anime)
  rootnode <- xmlRoot(anime_dat)
  
  title <- xpathSApply(rootnode, "//info[@type='Main title']", xmlValue)
  plot <- xpathSApply(rootnode, "//info[@type='Plot Summary']", xmlValue)
  genres <- as.list(xpathSApply(rootnode, "//info[@type='Genres']", xmlValue))
  themes <- as.list(xpathSApply(rootnode, "//info[@type='Themes']", xmlValue))
  episodes <- xpathSApply(rootnode,"//episode[@num]",xmlAttrs)
  if (length(episodes) > 0) {
    episode_numbers <- as.numeric(episodes)
    max_episode <- max(episode_numbers, na.rm = TRUE)
  } else {
    max_episode <- NA
  }
  
# Anime series are divided by seasons, so max episode # will show upper limit
# for that particular season, the lower limit is the previous season max+1
  
  if (length(genres) < 1) { # dealing with NA and row length
    genres <- NA
  }
  if (length(themes) < 1) {
    themes <- NA
  }
  if (length(genres) == 1) {
    genres <- genres
  }
  if (length(themes) == 1) {
    themes <- themes
  }
  if (length(genres) == 2) {
    genres <- paste(genres[1], ",", genres[2], sep = " ")
  }
  if (length(themes) == 2) {
    themes <- paste(themes[1], ",", themes[2], sep = " ")
  }
  if (length(genres) >= 3) {
    genres <- paste(genres[1], ",", genres[2], ",", genres[3], sep = " ")
  }
  if (length(themes) >= 3) {
    themes <- paste(themes[1], ",", themes[2], ",", themes[3], sep = " ")
  }
  if (length(plot) < 1) {
    plot <- NA
  }
  if(length(episodes)< 1){
    episodes <- NA
  }
  
  
  anime_data <- data.frame(
    animeID = i,
    Title = title,
    Plot = plot,
    Genre = genres,
    Themes = themes,
    stringsAsFactors = F
  )
  
  anime_df <- c(anime_df, list(anime_data))
}
anime_DF <- bind_rows(anime_df)

# Filling in empty Genres and Themes rows
anime_genre <- c("drama", "action", "romance", "science fiction", "tragedy", "comedy", "horror", "psychological")
wrong_cols <- anime_DF[, 6:35]
missing <- anime_DF[which(is.na(anime_DF$Genre) | is.na(anime_DF$Themes)), ]

# Create a logical matrix of missing genres and themes
missing_genres <- is.na(missing$Genre)
missing_themes <- is.na(missing$Themes)

# Define the list of valid manga genres
anime_genre <- c("drama", "action", "romance", "science fiction", "tragedy", 
                 "comedy", "horror","psychological","supernatural","mystery",
                 "adventure","slice of life","fantasy")

# Find the indices of rows with matching genres in the X. columns
matching_indices <- apply(missing[, 6:35], 1, function(row) any(row %in% anime_genre))

# Update the missing Genre and Themes columns with values from the X. columns
missing$Genre[missing_genres] <- apply(missing[missing_genres, 6:35], 1, function(row) {
  matching_genres <- row[row %in% anime_genre]
  if (length(matching_genres) > 0) {
    matching_genres[1] # Take the first matching genre
  } else {
    NA
  }
})

missing$Themes[missing_themes] <- apply(missing[missing_themes, 6:35], 1, function(row) {
  matching_themes <- row[!is.na(row)]
  if (length(matching_themes) > 0) {
    paste(matching_themes, collapse = " and ") # Combine multiple themes
  } else {
    NA
  }
})


anime_DF[which(is.na(anime_DF$Genre) | is.na(anime_DF$Themes)), ] <- missing
anime_DF <- anime_DF[-c(34, 76, 14), -c(1, 6:35)] # Removing API address and dup. cols