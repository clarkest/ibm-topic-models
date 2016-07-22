charles.coded <- read.csv("/Users/clarkbernier/Dropbox/Semantic analysis/Topic model 2015/Data/VJ-WJ corpus with CH type additions.csv")
charles.coded <- select(charles.coded, -name)
write.csv(charles.coded, file="/Users/clarkbernier/Dropbox/Semantic analysis/Topic model 2015/Data/VJ-WJ corpus with CH type additions.csv")
charles.coded <- read.csv("place_docs_here/VJ-WJ with CH types recovered (edited).csv")

charles.coded <- read.csv("place_docs_here/charles_coded_docs.csv", stringsAsFactors = F)
names(charles.coded)
table(charles.coded$TYPE)

table(charles.coded$continent, charles.coded$jam)
table(threaded.docs$continent, threaded.docs$jam)

charles.coded$TYPE[charles.coded$TYPE == "collab"] <- "Collab"
charles.coded$TYPE[charles.coded$TYPE == "contract"] <- "Contract"
charles.coded$TYPE[charles.coded$TYPE == "trad"] <- "Trad"
charles.coded$TYPE[charles.coded$TYPE == "xCn"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "xTr"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "dup"] <- "0"
charles.coded$TYPE <- factor(charles.coded$TYPE)
table(charles.coded$TYPE)
sum(is.null(charles.coded$parent))
sum(is.null(threaded.docs$parent))

coded.docs <- threaded.docs %>%
  left_join(select(charles.coded, id, TYPE), by="id")  
coded.docs$TYPE[is.na(coded.docs$TYPE)] <- ""
coded.docs %>% select(-ancestors) %>%
  write.csv(file="~/threaded_docs_coded.csv")





#######
# data for Charles
#######

threaded.docs %>% select(-ancestors) %>%
  write.csv(file="~/orig_threaded_docs.csv")


id.set <- c("ffdb040327.efbf4e62.break_down_the_silos", "ffda9923eb.23f00123.break_down_the_silos")
View(filter(threaded.docs, root.id %in% id.set))



########
# train a supervised model on the codes
########
mgr.types <- table(coded.docs$new.mgr, coded.docs$TYPE)[,-1] 
mgr.types
round(mgr.types / rowSums(mgr.types),3)


library(quanteda)
coded.docs$was.coded <- coded.docs$TYPE !=""
corpus <- corpus(coded.docs$text, 
                 docnames=coded.docs$id, 
                 docvars=select(coded.docs, TYPE, was.coded, new.mgr))
stops <- as.character(read.table("200-topic-models/en.txt")$V1)
corp.features <- dfm(corpus, ignoredFeatures=stops, stem=F)

sub.corpus <- subset(corpus, coded.docs$was.coded)
sub.features <- dfm(sub.corpus, ignoredFeatures=stops, stem=F)

library(textir)
type.val <- data.frame(type=as.numeric(sub.corpus$documents$TYPE=="Collab"))
collab.mnir.fit <- mnlm(NULL, # single-threaded for now
                 type.val, # the outcome of interest
                 sub.features, # the document-term counts
                 bins = NULL # the default value, tells it not to use each word 
                 # rather than collapse dimensions
)

# least collaborative words (lower "type")
coef(mnir.fit)[2, order(coefs[2, ])[1:10]]
# most collaborative words (higher "type")
coef(mnir.fit)[2, -order(coefs[2, ])[1:10]]




