# Manga-Project

Possible project looking at manga ratings, number of sales, etc. to estimate success of manga. Manga ratings taken from: https://www.animenewsnetwork.com/encyclopedia/reports.php?id=173&nlist=all & https://en.wikipedia.org/wiki/List_of_best-selling_manga

Note on total number of manga: 


Initially, the list of best-selling manga comprised 181 records, however, when matched with the ANN ratings, only 119 of those records remained as some of the best-selling manga were written and distributed in the 70's, 80's and 90's, and not included in the ANN survey or they were included and those who rated these tankobon most likely did not know about the manga produced in the 20th century.Additionally, many manga tankobon are spin-offs of the original, which means these were not considered to be the same as the manga itself and therefore, not included in the final table. 

Initially, the list of best-selling manga comprised 181 records, however, when matched with the ANN ratings, only 108 of those records remained as some of the best-selling manga were written and distributed in the 70's, 80's and 90's. 


Note on Bayesian estimate: 

This rating only includes titles that have at least 4 votes. The Bayesian estimate is a statistical technique used to reduce the noise due to low sample counts. In effect, the less a title has votes, the more it is pulled towards the mean (7.71). In other words, these are the titles that many people agree are great. (formula)
Bayesian rating = (v ÷ (v+m)) × R + (m ÷ (v+m)) × C
where:
R = average for the manga
v = number of votes for the manga
m = minimum votes required to be listed (currently 4)
C = the mean vote across the whole report (currently 7.71)
