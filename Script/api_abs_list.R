#--------------------------------------------------#
# Loading libraries and fetching API anime data    #
#--------------------------------------------------#

library(rvest)
library(dplyr)
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

#--------------------------------------------------#
# Loading the summ_manga_data to match id_table    #
#--------------------------------------------------#

summ_manga_data <- read.csv("Manga Summary Data.csv", header = T)

# Filtering id_table to match names in summ_manga_data Title column

remove_articles <- function(pattern) { # More effective way to group manga titles
  cleaned_pattern <- gsub("\\b(?:The|An)\\s+", "", pattern, ignore.case = TRUE)
  return(cleaned_pattern)
}

summ_manga_data$Title <- remove_articles(summ_manga_data$Title) # deletes article and creates mismatch for titles 33,38,51,66
summ_manga_data$Title[33] <- "Dragon Quest: The Adventure of Dai"
summ_manga_data$Title[38] <- "Fist of the North Star"
summ_manga_data$Title[51] <- "Hayata the Combat Butler"
summ_manga_data$Title[66] <- "Magi: The Labyrinth of Magic"

id_table <- filter(id_table,name %in% summ_manga_data$Title | name %like% paste0("^(", paste(summ_manga_data$Title, collapse = "|"), ")"))

#------------------------------------------------------------#
# Getting OAV, movie and TV count by manga title (2 ways)    #
#------------------------------------------------------------#

library(stringdist) # Using Jaccard distances - not effective in grouping manga titles
title_dist <- stringdistmatrix(id_table$name,id_table$name, method = "jaccard")
threshold <- .38
groups <- hclust(as.dist(title_dist)) %>% cutree(h = threshold)
anime_groups <- id_table %>% mutate(Group = groups)
max(groups)

# More effective way to group televised work based on original manga title
pattern <- summ_manga_data$Title # Some televised manga not showing if no English translation available:
                                 # Chameleon, Crows, Worst,
title_groups <- sapply(pattern,function(pattern){ 
  id_table$name[which(grepl(pattern,id_table$name, ignore.case = T))]
  })

#---------------------------------------------#
#   #
#---------------------------------------------#




