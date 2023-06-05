## Manga Project

library(rvest)
url <- "https://www.animenewsnetwork.com/encyclopedia/reports.php?id=173&nlist=all"
page <-  read_html(url)
table <- html_table(page)
## Saving the file in .csv format
write.csv(table, file = "anime-network_rank_7081.csv", row.names = FALSE) ##STOP!!

## Instead of using table variable and turning into df, I use .csv file
## because some titles are misspelled in the original html document. 
## Had to manually check each name prior to merging both datasets
df3 <- as.data.frame(read.csv("anime-network_rank_7081.csv", header = TRUE ))
colnames(df3) <- c("Title","No. Votes","No. Seen","Bayes. Estimate")
series <- as.data.frame(read.csv("Serielized_best-selling_manga.csv", header = TRUE))
series$Title <- paste(series$Title, "(manga)", sep = " ") ## to match df3
## Remove rows = 1 and cols = 4,5 which contain no important/repeated info
df3 <- df3[-1,-c(4,5)]

## Loading 2nd manga list with detailed publication and sales info to merge
## both datasets
best_manga <-read.csv("List_of_best-selling_manga_1.csv", stringsAsFactors = FALSE)
best_manga$Title <- paste(best_manga$Title, "(manga)", sep =" ") ## to match df3
merged_manga <- merge(best_manga, df3, by = "Title")[, -which(names(merge(best_manga, df3, by = "Title")) == "ID")]
merged_manga <- merge(merged_manga, series, by = "Title")
write.csv(merged_manga, file = "Best-selling Manga General List.csv", row.names = FALSE)

## Converting columns = 3,4,9,10,11 to factors (0,1)
factor_columns <-c("Pub","Demo","Run","Digi","Tele")
merged_manga[factor_columns] <- lapply(merged_manga[factor_columns],factor)
## Mutating columns = 12:14 class from char to numeric. 
merged_manga[,12:14] <-lapply(merged_manga[,12:14],as.numeric)
## Rounding Bayesian Estimate to 2 decimal places.
merged_manga$`Bayes. Estimate` <- round(merged_manga$`Bayes. Estimate`,digits = 2)


