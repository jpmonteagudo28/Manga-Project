#------------------------------------------------------------------------------#
# Loading data and conducting EDA                                              #
#------------------------------------------------------------------------------#

summ_manga_data <- read.csv("Manga Summary Data.csv", header = T)

# Getting general feel for the data
str(summ_manga_data)
summary(summ_manga_data)
    # Finding percent missing values in each column
percent_missing <- sapply(summ_manga_data, function(col) 
  {num_missing <- sum(is.na(col))
  total <- length(col)
  percent <- (num_missing/total)*100
  return(round(percent, 2))
})
barplot(percent_missing, ylim = c(0,20),xlab = "Columns", ylab = "Percentage", 
        main = "Percentage of missing values by column", 
        col = c("royalblue","forestgreen","brown2"))
text(x = c(18,20), y = percent_missing[16]+ .5, labels = "16.67", pos = 4, cex = .8)
text(x = (19), y = percent_missing[17]+ .5, labels = "14.17", pos = 4, cex = .8)

  # What manga titles have missing info?
na_manga <- which(is.na(summ_manga_data[16:18]))
na_info <- summ_manga_data[na_manga[1:20],c(1,7,13:15)]
summary(na_info)
max(summ_manga_data$No..Seen); max(na_info$No..Seen)
# [1] 5130
# [1] 177
mean(summ_manga_data$No..Seen[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)]); mean(na_info$No..Seen)
# [1] 729.75
# [1] 54.25
max(summ_manga_data$Bayes..Estimate); max(na_info$Bayes..Estimate)
# [1] 9.240951
# [1] 8.523531
mean(summ_manga_data$Bayes..Estimate[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)],na.rm = T); mean(na_info$Bayes..Estimate)
# [1] 7.598026
# [1] 4.698811
max(summ_manga_data$Sold[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)]); max(na_info$Sold)
# [1] 516.6
# [1] 148
mean(summ_manga_data$Sold[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)]); mean(na_info$Sold)
# [1] 59.85321
# [1] 47.375

library(ggplot2)
y <- summ_manga_data$No..Seen[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)]
plot <- ggplot() + ## Histogram of No. times seen
  geom_histogram(data = summ_manga_data[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119),], aes(x = y), fill = "royalblue", alpha = 0.5, bins = 20) +
  geom_histogram(data = na_info, aes(x = No..Seen), fill = "brown2", alpha = 0.5, bins = 20) +
  labs(x = "No. of tmes Seen", y = "Frequency", title = "Histogram Comparison of No. Seen Manga summary data and missing data") +
  scale_fill_manual(values = c("royalblue", "brown2"), labels = c("Manga Summary Data", "MIssing data")) +
  theme_minimal()
plot
y1 <- summ_manga_data$Bayes..Estimate[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119)]
plot2 <- ggplot() + ## Histogram of Bayes Rating
  geom_histogram(data = summ_manga_data[-c(8,20,21,23,24,26,34,91,94,96,98,99,117,119),], aes(x = y1), fill = "royalblue", alpha = 0.5, bins = 20) +
  geom_histogram(data = na_info, aes(x = Bayes..Estimate), fill = "forestgreen", alpha = 0.5, bins = 20) +
  labs(x = "Bayes Rating", y = "Frequency", title = "Histogram Comparison of Bayes Rating Manga summary data and missing data") +
  scale_fill_manual(values = c("royalblue", "forestgreen"), labels = c("Manga Summary Data", "Missing data")) +
  theme_minimal()
plot2

par(mfrow = c(1,2))
boxplot(y, pch = 2, col = "royalblue", ylim = c(0,2000))
boxplot(na_info$No..Seen, pch = 2, col = "forestgreen", ylim = c(0,2000))

