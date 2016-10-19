charles.coded <- read.csv("place_docs_here/VJ-WJ with CH types recovered (edited).csv", stringsAsFactors = F)

names(charles.coded)
table(charles.coded$TYPE)

table(charles.coded$continent, charles.coded$jam)
table(th.doc.topics$continent, th.doc.topics$jam)

charles.coded$TYPE[charles.coded$TYPE == "collab"] <- "Collab"
charles.coded$TYPE[charles.coded$TYPE == "contract"] <- "Contract"
charles.coded$TYPE[charles.coded$TYPE == "trad"] <- "Trad"
charles.coded$TYPE[charles.coded$TYPE == "xCn"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "xTr"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "dup"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "Contract"] <- "0"
charles.coded$TYPE[charles.coded$TYPE == "0"] <- "neither"
charles.coded$TYPE <- factor(charles.coded$TYPE)
table(charles.coded$TYPE)
sum(is.null(charles.coded$parent))
sum(is.null(threaded.docs$parent))

coded.docs <- th.doc.topics %>%
  left_join(select(charles.coded, id, TYPE), by="id")  
coded.docs$TYPE[is.na(coded.docs$TYPE)] <- ""
table(coded.docs$TYPE)
coded.docs %>% select(-ancestors) %>%
  write.csv(file="place_docs_here/threaded_docs_coded.csv")
# coded.docs <- read.csv("place_docs_here/threaded_docs_coded.csv", stringsAsFactors=F)


mgr.types <- table(coded.docs$new.mgr, coded.docs$TYPE)[,-1] 
mgr.types
round(mgr.types / rowSums(mgr.types),3)



########
# train a supervised model on the codes
########
# coded.docs <- read.csv("place_docs_here/threaded_docs_coded.csv", stringsAsFactors=F)
library(quanteda)
library(dplyr)
coded.docs[coded.docs$TYPE == "0", "TYPE"] <- "neither"
coded.docs$was.coded <- coded.docs$TYPE != ""
corpus <- corpus(coded.docs$text, 
                 docnames=coded.docs$id, 
                 docvars=select(coded.docs, TYPE, was.coded, new.mgr))
stops <- as.character(read.table("200-topic-models/en.txt")$V1)
corp.features <- dfm(corpus, ignoredFeatures=stops, stem=F)

sub.corpus <- subset(corpus, coded.docs$was.coded & coded.docs$TYPE != "contract")
sub.features <- dfm(sub.corpus, ignoredFeatures=stops, stem=F)
sub.df <- as.data.frame(as.wfm(sub.features))


library(doParallel)
library(parallel)
library(caret)
sub.dtm.outcome <- cbind(sub.df, y = factor(sub.corpus$documents$TYPE))



ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10, 
                     number = 10)

registerDoParallel(24, cores=24)
fit.svm <- train(y ~ ., 
                 data = sub.dtm.outcome, 
                 method = 'svmLinearWeights',
                 trControl = ctrl,
                 preprocess="scale")
fit.rf <- train(y ~ ., 
                 data = sub.dtm.outcome, 
                 method = 'rf',
                 trControl = ctrl,
                verbose=T)
rf.mid.grid <- expand.grid(mtry=c(100,150,250,1000,3000))
fit.rf.mid <- train(y ~ ., 
                data = sub.dtm.outcome, 
                method = 'rf',
                trControl = ctrl,
                tuneGrid = rf.mid.grid,
                verbose=T)
fit.rngr <- train(y ~ ., 
                data = sub.dtm.outcome, 
                method = 'ranger',
                trControl = ctrl)
fit.rngr.mid <- train(y ~ ., 
                  data = sub.dtm.outcome, 
                  method = 'ranger',
                  trControl = ctrl,
                  tuneGrid = rf.mid.grid,
                  verbose=T)
fit.knn <- train(y ~ ., 
                 data = sub.dtm.outcome, 
                 method = 'knn',
                 trControl = ctrl)
fit.dp <- train(y ~ ., 
                 data = sub.dtm.outcome, 
                 method = 'deepboost',
                 trControl = ctrl)
fit.nnet <- train(y ~ ., 
                data = sub.dtm.outcome, 
                 method = 'nnet',
                 trControl = ctrl, MaxNWts=30000)
fit.brnn <- train(y ~ ., 
                  data = sub.dtm.outcome, 
                  method = 'brnn',
                  trControl = ctrl,
                  verbose=T)

stopImplicitCluster()
fit.svm

fit.rf <- train(y ~ ., data = sub.dtm.outcome, method = 'rf')



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


confusionMatrix(fit.nb)
confusionMatrix(fit.svm)
confusionMatrix(fit.knn)
confusionMatrix(fit.nnet)
confusionMatrix(fit.rngr)
confusionMatrix(fit.rngr.mid)
confusionMatrix(fit.rngr.mid)
confusionMatrix(fit.rngr.mid, "average")
confusionMatrix(fit.rngr.mid, "none")


#  how do the topics split by the trad/collab catergories?

doc.topics.unsmooth


#######
# data for Charles
#######

threaded.docs %>% select(-ancestors) %>%
  write.csv(file="~/orig_threaded_docs.csv")


id.set <- c("ffdb040327.efbf4e62.break_down_the_silos", "ffda9923eb.23f00123.break_down_the_silos")
View(filter(threaded.docs, root.id %in% id.set))



