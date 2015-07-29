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

#################
#   THREADING   #
#################


# we have multi-level nesting, but would like to group all docs that appeared in the same thread together
# I'm going to call the orginating doc of such a thread the "common.ancestor." the originating doc will 
# have a common.ancestor equal to its own id

# step 1: all parent='null' set to the comment id
documents$common.ancestor <- ifelse(documents$parent=='null', documents$id, 'null')
View(documents[,c("id","parent","common.ancestor")])

# step 2: iterate through each nested layer: set the common.ansestor equal to the common ancestor of the parent 
#         of each comment.  this will populate the next level down in the trees during each pass
sum(documents$common.ancestor=='null')
while (sum(documents$common.ancestor=='null') > 0) {
  documents$parent.ancestor <- left_join(documents[,c("id","parent")], documents[,c("id","common.ancestor")], by=c("parent"="id"))
  documents$common.ancestor <- ifelse(documents$common.ancestor=='null', )
}
