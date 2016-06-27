charles.coded <- read.csv("/Users/clarkbernier/Dropbox/Semantic analysis/Topic model 2015/Data/VJ-WJ corpus with CH type additions.csv")
charles.coded <- read.csv("place_docs_here/charles_coded_docs.csv", stringsAsFactors = F)
names(charles.coded)
sum(documents$id == charles.coded$ID)

length(levels(factor(documents$text)))

length(unique(charles.coded$ID))
table(charles.coded$TOPICS)
table(charles.coded$CO)


length(unique(bot.free.node.stats$orgid))
length(unique(bot.free.node.stats$orgid, bot.free.node.stats$window))
