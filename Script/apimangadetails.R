ibrary(RCurl)
library(XML)
apimanga <- "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?manga=1223"
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
##According to ANN, can only request 50 titles/request (total 120 titles)
apimanga2 <-"https://cdn.animenewsnetwork.com/encyclopedia/api.xml?manga=1223/20224"
apimangaData2 <- getURL(apimanga2)
mangadoc2 <- xmlParse(apimangaData2)
roonode2 <-xmlRoot(mangadoc2)
kimetsu_node <- getNodeSet(roonode2,"//manga[@name='Demon Slayer: Kimetsu no Yaiba']")
summaryplot <- xpathSApply(kimetsu_node[[1]],"./info[@type='Plot Summary']",xmlValue)
genre <- xpathSApply(kimetsu_node[[1]],"./info[@type='Genres']",xmlValue)
