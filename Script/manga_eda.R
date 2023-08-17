#------------------------------------------------------------------------------#
# Loading data and conducting EDA                                              #
#------------------------------------------------------------------------------#

summ_manga_data <- read.csv("Manga Summary Data.csv", header = T)
summ_manga_data$Pub <- as.factor(summ_manga_data$Pub)
summ_manga_data$Demo <- as.factor(summ_manga_data$Demo)
summ_manga_data$Run <- as.factor(summ_manga_data$Run)
summ_manga_data$Digi <- as.factor(summ_manga_data$Digi)
summ_manga_data$Tele <- as.factor(summ_manga_data$Tele)
#------------------------------------#
# Getting general feel for the data  #
#------------------------------------#

str(summ_manga_data)
summary(summ_manga_data)
# Finding percent missing values in each column
percent_missing <- sapply(summ_manga_data, function(col) {
  num_missing <- sum(is.na(col))
  total <- length(col)
  percent <- (num_missing / total) * 100
  return(round(percent, 2))
})
barplot(percent_missing,
  ylim = c(0, 20), xlab = "Columns", ylab = "Percentage",
  main = "Percentage of missing values by column",
  col = c("royalblue", "forestgreen", "brown2")
)
text(x = c(18, 20), y = percent_missing[16] + .5, labels = "16.67", pos = 4, cex = .8)
text(x = (19), y = percent_missing[17] + .5, labels = "14.17", pos = 4, cex = .8)

#--------------------------------------#
# What manga titles have missing info? #
#--------------------------------------#

na_manga <- which(is.na(summ_manga_data[16:18]))
na_info <- summ_manga_data[na_manga[1:20], c(1, 6, 7, 13:15)]
summary(na_info)
table(na_info$Title)

max(summ_manga_data$No..Seen)
max(na_info$No..Seen)
# [1] 5130
# [1] 177
mean(summ_manga_data$No..Seen[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)])
mean(na_info$No..Seen)
# [1] 729.75
# [1] 54.25
max(summ_manga_data$Bayes..Estimate)
max(na_info$Bayes..Estimate)
# [1] 9.240951
# [1] 8.523531
mean(summ_manga_data$Bayes..Estimate[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)], na.rm = T)
mean(na_info$Bayes..Estimate)
# [1] 7.598026
# [1] 4.698811
max(summ_manga_data$Sold[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)])
max(na_info$Sold)
# [1] 516.6
# [1] 148
mean(summ_manga_data$Sold[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)])
mean(na_info$Sold)
# [1] 59.85321
# [1] 47.375

# Looking at release year of manga and missing info
release_date <- as.numeric(substr(summ_manga_data$Serielized[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)], 1, 4))
na_release_date <- as.numeric(substr(na_info$Serielized, 1, 4))
plot(na_release_date, na_info$Sold, pch = 17, col = "forestgreen", xlab = "Release year", ylab = "No. copies sold")
plot(release_date, summ_manga_data$Sold[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)], pch = 17, col = "brown2", xlab = "Release year", ylab = "No. copies sold")
boxplot(na_release_date, na_info$Sold, pch = 17, col = "forestgreen", xlab = "Release year", ylab = "No. copies sold", ylim = c(0, 1000))
boxplot(release_date, summ_manga_data$Sold[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)], pch = 17, col = "brown2", xlab = "Release year", ylab = "No. copies sold", ylim = c(0, 1000))
abline(h = median(summ_manga_data$Sold[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)]), col = "navy", lty = 8)
quantile(na_release_date)
quantile(release_date)


library(ggplot2)
y <- summ_manga_data$No..Seen[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)]
plot <- ggplot() + ## Histogram of No. times seen
  geom_histogram(data = summ_manga_data[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119), ], aes(x = y), fill = "royalblue", alpha = 0.5, bins = 20) +
  geom_histogram(data = na_info, aes(x = No..Seen), fill = "brown2", alpha = 0.5, bins = 20) +
  labs(x = "No. of tmes Seen", y = "Frequency", title = "Histogram Comparison of No. Seen Manga summary data and missing data") +
  scale_fill_manual(values = c("royalblue", "brown2"), labels = c("Manga Summary Data", "MIssing data")) +
  theme_minimal()
plot
par(mfrow = c(1, 2))
boxplot(y, pch = 2, col = "royalblue", ylim = c(0, 2000), xlab = "No. times Seen-no NA")
boxplot(na_info$No..Seen, pch = 2, col = "forestgreen", ylim = c(0, 2000), xlab = "No. times Seen-NA")
par(mfrow = c(1, 1))

quantile(na_info$No..Seen)
quantile(y)

## Histogram of Bayes Rating
y1 <- summ_manga_data$Bayes..Estimate[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119)]
plot2 <- ggplot() +
  geom_histogram(data = summ_manga_data[-c(8, 20, 21, 23, 24, 26, 34, 91, 94, 96, 98, 99, 117, 119), ], aes(x = y1), fill = "royalblue", alpha = 0.5, bins = 20) +
  geom_histogram(data = na_info, aes(x = Bayes..Estimate), fill = "forestgreen", alpha = 0.5, bins = 20) +
  labs(x = "Bayes Rating", y = "Frequency", title = "Histogram Comparison of Bayes Rating Manga summary data and missing data") +
  scale_fill_manual(values = c("royalblue", "forestgreen"), labels = c("Manga Summary Data", "Missing data")) +
  theme_minimal()
plot2
quantile(na_info$Bayes..Estimate)
quantile(y1)

# Most manga titles missing plot, genre and themes info were released prior to 21st
# century, typically between 1970 - 1990's. On the other hand, most manga titles which
# include plot, genre, and theme were released between late 1980's and early 2000's.
# Most manga with missing plots, genre, and themes were seen approx. 40 times while the
# manga titles with no missing info were watched over 240 times on average. Lastly,
# manga rating (average = 7.71) for incomplete records fell between -1 - 7.94 while
# complete records fluctuated between 7.54 - 8.19 points.

#----------------------------------#
# Publishing Companies
#----------------------------------#

factor_sold <- cut(summ_manga_data$Sold,
                   breaks = c(20,35,60,100,300,550),
                   labels = c("20 - 35(in millions)","36 - 60(in millions)","61 - 100(in millions)","101 - 300(in millions)","301 - 550(in millions)"),
                   include.lowest = T)
table <- as.data.frame.matrix(addmargins(table(factor_sold, summ_manga_data$Pub)))
colnames(table) <- c("Others","Kodansha","Shogakukan","Shueisha", "Total")
round(prop.table(table),2)

fill_col <- c("0" = "royalblue", "1" = "darkseagreen", "2" = "forestgreen", "3" = "brown2")
ggplot(summ_manga_data, aes(x = Demo, y = Sold, fill = Pub)) +
  geom_boxplot() +
  facet_wrap(~ Pub) +
  scale_fill_manual(values = fill_col) +
  labs(x = "Demo", y = "Sold", title = "No. copies sold to selected demographic by Publishing Company") +
  theme_minimal()