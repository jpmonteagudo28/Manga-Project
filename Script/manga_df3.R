## Manga Project

library(rvest)
url <- "https://www.animenewsnetwork.com/encyclopedia/reports.php?id=173&nlist=all"
page <-  read_html(url)
table <- html_table(page)
<<<<<<< HEAD
## Saving the file in .csv format
write.csv(table, file = "anime-network_rank_7081.csv", row.names = FALSE)
## Instead of using table variable and turning into df, I use .csv file
## because some titles are misspelled in the original html document. 
## Had to manually check each name prior to merging both datasets
df3 <- as.data.frame(read.csv("anime-network_rank_7081.csv", header = TRUE ))
colnames(df3) <- c("Title","No. Votes","No. Seen","Bayes. Estimate")


=======
df3 <- as.data.frame(table)
colnames(df3) <- c("Title","No. Votes","No. Seen","Bayes. Estimate")
## Saving the file in .csv format
write.csv(table, file = "anime-network_rank_7081.csv", row.names = FALSE)

>>>>>>> 73bc700827821efa01fecd92a5a544f4fab12178
## Remove rows = 1 and cols = 4,5 which contain no important/repeated info
df3 <- df3[-1,-c(4,5)]

## Loading 2nd manga list with detailed publication and sales info to merge
## both datasets
best_manga <-read.csv("List_of_best-selling_manga_1.csv", stringsAsFactors = FALSE)
best_manga$Title <- paste(best_manga$Title, "(manga)", sep =" ") ## to match df3
merged_manga <- merge(best_manga, df3, by = "Title")[, -which(names(merge(best_manga, df3, by = "Title")) == "ID")]
