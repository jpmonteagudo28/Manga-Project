#------------------------------------------------------------------------------#
# Test on 2 manga titles to check code prior to batch request                  #
#------------------------------------------------------------------------------#

library(RCurl)
library(XML2)
library(dplyr)
library(lubridate)
apimanga <- "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?manga=8991"
apimangaData <- getURL(apimanga)
mangadoc <- xmlParse(apimangaData)
rootnode <- xmlRoot(mangadoc)
info_values <- as.list(xpathSApply(rootnode, "//info", xmlValue))
type_names <- as.list(xpathSApply(rootnode, "//@type"))
plotsummary <- xpathSApply(rootnode, "//info[@type='Plot Summary']", xmlValue)
genres <- as.list(xpathSApply(rootnode, "//info[@type='Genres']", xmlValue))
themes <- as.list(xpathSApply(rootnode, "//info[@type='Themes']", xmlValue))
themes <- paste(themes[2], themes[1], "with", themes[3], sep = " ")
## For two manga titles at once.
## Second manga title does not contain theme information
## According to ANN, can only request 50 titles/request (total 120 titles)
apimanga2 <- "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?manga=1223/20224"

apimangaData2 <- getURL(apimanga2)
mangadoc2 <- xmlParse(apimangaData2)
roonode2 <- xmlRoot(mangadoc2)
kimetsu_node <- getNodeSet(roonode2, "//manga[@name='Demon Slayer: Kimetsu no Yaiba']")
summaryplot <- xpathSApply(kimetsu_node[[1]], "./info[@type='Plot Summary']", xmlValue)
genre <- xpathSApply(kimetsu_node[[1]], "./info[@type='Genres']", xmlValue)

#------------------------------------------------------------------------------#
# Fetching 120 titles (1/request)  from ANN API                                #
#------------------------------------------------------------------------------#

# Create empty data frame to store manga title info from API
manga_df <- list()

baseapi <- "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?"
id <- c( # Saved as api_id.txt doc in Data foldercanv
  8991, 3424, 9577, 3345, 15018, 1343, 12308, 11159, 1214, 2298, 10154, 1341, 2468,
  2411, 3569, 4053, 2898, 22369, 19684, 4242, 11106, 1593, 9857, 29303, 7987, 5146,
  1967, 4354, 20224, 2454, 1335, 970, 297, 3244, 15722, 2869, 6872, 18530, 1547, 2701,
  15848, 2335, 2565, 7912, 3749, 4733, 17141, 1519, 3524, 15682, 3069, 4947, 1898,
  3142, 1559, 76, 3848, 4099, 1900, 21741, 2711, 2799, 11117, 3073, 11816, 1564,
  14394, 354, 4116, 3880, 5666, 1511, 16757, 1598, 2891, 2440, 4441, 1608, 1054,
  15076, 1223, 1570, 7939, 1050, 19476, 1592, 6738, 3438, 7114, 4702, 19596, 1995,
  1578, 4208, 4553, 7191, 15698, 1604, 11050, 10733, 1334, 1596, 11110, 3765, 4152,
  16089, 19957, 1378, 16086, 22854, 4528, 2419, 9144, 5278, 1595, 3420, 9292, 1642,
  6005, 1597
)
manga_url <- paste0(baseapi, "manga=", id)

for (i in manga_url) { # Iterate through each manga id and retrieve info
  get_manga <- getURL(i)
  manga_dat <- xmlParse(get_manga)
  rootnode <- xmlRoot(manga_dat)

  title <- xpathSApply(rootnode, "//info[@type='Main title']", xmlValue)
  plot <- xpathSApply(rootnode, "//info[@type='Plot Summary']", xmlValue)
  genres <- as.list(xpathSApply(rootnode, "//info[@type='Genres']", xmlValue))
  themes <- as.list(xpathSApply(rootnode, "//info[@type='Themes']", xmlValue))

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


  manga_data <- data.frame(
    MangaID = i,
    Title = title,
    Plot = plot,
    Genre = genres,
    Themes = themes,
    stringsAsFactors = F
  )

  manga_df <- c(manga_df, list(manga_data))
}
manga_DF <- bind_rows(manga_df)

# Filling in empty Genres and Themes rows
manga_genre <- c("drama", "action", "romance", "science fiction", "tragedy", "comedy", "horror")
wrong_cols <- manga_DF[, 6:35]
missing <- manga_DF[which(is.na(manga_DF$Genre) | is.na(manga_DF$Themes)), ]

# Create a logical matrix of missing genres and themes
missing_genres <- is.na(missing$Genre)
missing_themes <- is.na(missing$Themes)

# Define the list of valid manga genres
manga_genre <- c("drama", "action", "romance", "science fiction", "tragedy", "comedy", "horror")

# Find the indices of rows with matching genres in the X. columns
matching_indices <- apply(missing[, 6:35], 1, function(row) any(row %in% manga_genre))

# Update the missing Genre and Themes columns with values from the X. columns
missing$Genre[missing_genres] <- apply(missing[missing_genres, 6:35], 1, function(row) {
  matching_genres <- row[row %in% manga_genre]
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


manga_DF[which(is.na(manga_DF$Genre) | is.na(manga_DF$Themes)), ] <- missing
manga_DF <- manga_DF[-c(34, 76, 14), -c(1, 6:35)] # Removing API address and dup. cols

#------------------------------------------------------------------------------#
# Merging manga_DF and merged_manga
#------------------------------------------------------------------------------#

manga_DF$Title <- paste(manga_DF$Title, "(manga)", sep =" ") # to match merged_manga
merged_manga <- read.csv("Best-selling Manga General List.csv", header = T)

# Dealing with mismatch prior to merging
match2 <- manga_DF$Title %in% merged_manga$Title
INDEX <- which(match2 == FALSE)
manga_DF[c(24,84,85,91,97,101),1]
manga_DF[24,1] <- sub("-","",manga_DF[24,1])
merged_manga[c(24,84,85,91,97,101),1]
merged_manga[c(84,85,91,97,101),1] <- paste("The",merged_manga[c(84,85,91,97,101),1], sep = " ")
merged_manga[24,1] <- paste("Shin",merged_manga[24,1], sep = " ")

# Merging 2 datasets to create summary manga dataset
summ_manga_data <- merge(merged_manga, manga_DF, by = "Title")
summ_manga_data$Serielized <- gsub("present","2022",summ_manga_data$Serielized)
write.csv(summ_manga_data,"Manga Summary Data.csv",row.names = FALSE)
