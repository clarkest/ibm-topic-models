source("300-post-model-analyses/mallet_analyses.R")

n.topics <- 30
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
model.name <- "ngram_model"
iters <- 800
maxims <- 50
model.num <- 4

list <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label

################################
# ngram v. no ngram comparisons
################################




###################################
# compare the two ngram models    #
###################################

## Initialize from a previously trained state
model.name <- "ngram_model"
iters <- 800
maxims <- 25
model.num <- 2
topic.model.2 <- load.from.saved.state(model.name, iters, maxims, model.num, n.topics) 

ret.1 <- topic.co.occur(topic.model, topic.model.2=topic.model)

ret.2 <- topic.co.occur(topic.model.2, topic.model.2=topic.model.2)
corr.heatmap(ret.2$corr.matrix, min=-4, max=4)

# we'd like to know what the self-similarity within each model looks like
mean(diag(ret.1$corr.matrix))
mean(diag(ret.2$corr.matrix))

sum(ret.compare$corr.matrix > 2)

# for a given similarity matrix, each topic from each of the two models will have a topic in the
# other model that it is closest to,  In an n X n comparison, there will be 2n such closest. Let's grab those 2n similarity metrics and see how they compare

corr.matrix <- ret.1$corr.matrix
best.match.dist <- cbind(apply(corr.matrix, 1, max), apply(corr.matrix, 2, max))
mean(best.match.dist)
sd(best.match.dist)
min(best.match.dist)

corr.matrix <- ret.2$corr.matrix
best.match.dist <- cbind(apply(corr.matrix, 1, max), apply(corr.matrix, 2, max))
mean(best.match.dist)
sd(best.match.dist)
min(best.match.dist)

ret.compare <- topic.co.occur(topic.model, topic.model.2=topic.model.2)
#corr.heatmap(ret.compare$corr.matrix, min=-3, max=3)
corr.matrix <- ret.compare$corr.matrix
best.match.dist <- cbind(apply(corr.matrix, 1, max), apply(corr.matrix, 2, max))
mean(best.match.dist)
sd(best.match.dist)
min(best.match.dist)

###########################
# Compare how often each pair of docs co-occurs 
###########################


# TODO ASK DAVID
# if we take the correlation of each topic with its highest neighbor, 
# then we can compare the overall doc classification 




####################################
# Load an nongram model and compare to ngram
####################################

model.name <- "nongram_model"
iters <- 800
maxims <- 25
model.num <- 1
topic.model.non <- load.from.saved.state(model.name, iters, maxims, model.num, n.topics) 


ret.compare <- topic.co.occur(topic.model, topic.model.2=topic.model.non)
corr.matrix <- ret.compare$corr.matrix
best.match.dist <- cbind(apply(corr.matrix, 1, max), apply(corr.matrix, 2, max))
mean(best.match.dist)
sd(best.match.dist)
min(best.match.dist)

#############
# can we use topic co-occurence as a way of seeing how different two models are 
# in terms of classifying docs?
#############

# Create a second, no-ngram topic model

ngram.documents <- documents
nongram.model.name <- "nongram_model"

# load the persisted documents -- these are needed before we can load a model from state
nongram.file.name <- paste0(paste("models_dir", nongram.model.name, sep="/"), "-docs.Rdata")
# this shoudl create an object called "documents"
load(nongram.file.name)
nongram.documents <- documents
nongram.mallet.instances <- mallet.import(nongrams.documents$id, 
                                          nongrams.documents$text, 
                                          stop.word.file, 
                                          token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]")

## Initialize from a previously trained state
iters <- 800
maxims <- 20
model_num <- 6
model.label = paste(nongram.model.name, iters, maxims, formatC(model_num, width=2, flag="0"), sep="-")
file.name <- paste(model.dir, paste0(model.label, ".gz"), sep="/")
nongram.topic.model <- MalletLDA(num.topics=n.topics)
nongram.topic.model$loadDocuments(nongram.mallet.instances)
nongram.topic.model$initializeFromState(.jnew("java.io.File", file.name))

# the documents don't line up exactly, so we need to map the two sets together
non.to.n.overlaps <- sapply(nongram.documents$id, function(x) is.element(x, ngram.documents$id))
sum(non.to.n.overlaps)
nongram.documents[!non.to.n.overlaps, "text"]
n.to.non.overlaps <- sapply(ngram.documents$id, function(x) is.element(x, nongram.documents$id))
sum(n.to.non.overlaps)
ngram.documents[!n.to.non.overlaps, "text"]
