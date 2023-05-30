## Manga Project

library(rvest)
url <- "https://www.animenewsnetwork.com/encyclopedia/ratings-manga.php?top50=
best_bayesian&n=250"
page <-  read_html(url)
table <- html_table(page)
df3 <- as.data.frame(table[[1]])
colnames(df3) <- c("Rank", "Title","Bayes. Estimate", "No. Votes")

## Remove rows = c(1,2,253) which contain no important info
row_indices <- c(1,2,253)
df3 <- df3[-row_indices,]
