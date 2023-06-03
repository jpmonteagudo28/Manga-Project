# Manga-Project
Possible project looking at manga ratings, number of sales, etc. to estimate success of manga. Manga ratings taken from: https://www.animenewsnetwork.com/encyclopedia/ratings-manga.php?top50=best_bayesian&n=500 & https://en.wikipedia.org/wiki/List_of_best-selling_manga

Note on Bayesian estimate: 

This rating only includes titles that have at least 4 votes. The bayesian estimate is a statistical technique used to reduce the noise due to low sample counts. In effect, the less a title has votes, the more it is pulled towards the mean (7.71). In other words, these are the titles that many people agree are great. (formula)
Bayesian rating = (v ÷ (v+m)) × R + (m ÷ (v+m)) × C
where:
R = average for the manga
v = number of votes for the manga
m = minimum votes required to be listed (currently 4)
C = the mean vote across the whole report (currently 7.71)
