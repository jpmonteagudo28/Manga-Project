## Manga Project

library(rvest)
url <- "https://www.animenewsnetwork.com/encyclopedia/ratings-manga.php?top50=best_bayesian&n=500"
page <-  read_html(url)
table <- html_table(page)
df3 <- as.data.frame(table[[1]])
colnames(df3) <- c("Rank", "Title","Bayes. Estimate", "No. Votes")
## Saving the file in .csv format
write.csv(table, file = "anime-network_rank_top500.csv", row.names = FALSE)

## Remove rows = c(1,2,503) which contain no important info
row_indices <- c(1,2,503)
df3 <- df3[-row_indices,]

## Loading 2nd manga list with detailed publication and sales info to merge
## both datasets
best_manga <-read.csv("List_of_best-selling_manga_1.csv", stringsAsFactors = FALSE)
best_manga$Title <- paste(best_manga$Title, "(manga)", sep =" ") ## to match df3
merged_manga <- merge(best_manga,df3,by ="Title")
merged_manga$ID <- NULL
