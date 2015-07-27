source("300-post-model-analyses/mallet_analyses.R")

n.topics <- 30
# assumes a working directory with 
#   a folder "model_states" containing .gz of topic models and .Rdata documents object
#   a folder "outputs" where this code sends the graph images

# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)

model.name <- "ngram_model"
iters <- 800
maxims <- 25
model.num <- 1
topic.model <- load.from.saved.state(model.name, iters, maxims, model.num, n.topics)
model.label <- paste(model.name, n.topics, iters, maxims, formatC(model.num, width=2, flag="0"), sep="-")
file.name <- paste0(paste("models_dir", model.name, sep="/"), "-docs.Rdata")
# this shoudl create an object called "documents"
load(file.name)
# make sure the outpus directory for this model exists
output.dir <- file.path(wd, "outputs", model.label) 
dir.create(output.dir, showWarnings = FALSE)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.words.0 <- mallet.topic.words(topic.model, smoothed=F, normalized=T)

## What are the top words in topic 7?
##  Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
#mallet.top.words(topic.model, topic.words[7,])

## Get short labels that can be used as part of filename
topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 3))
topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)

doc.topics.frame <- data.frame(doc.topics)
#names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
names(doc.topics.frame) <- topics.labels

##################################
#   Graph Formatting             #
##################################
text.1.5 <- element_text(size=rel(1.5))
text.1.0 <- element_text(size=rel(1.0))
text.2.0 <- element_text(size=rel(2.0))
thm <- theme(legend.text=text.1.5, 
             axis.title=text.1.5,
             legend.title=text.1.5,
             axis.text=text.1.5,
             plot.title=text.2.0           
)

##################################
#  1. Total Posts Over Time      #
##################################


#by.vars = c("manager", "continent", "jam")
by.vars = c("manager", "jam")
posts.by.window <- get.posts.by.window(documents, by.vars)

#output both the raw and the normalized plots
plt=qplot(as.integer(DateWindow), x.x, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point() 
ggsave(file.path(output.dir, "raw_posts_by_time.png"), plt + thm)

plt=qplot(as.integer(DateWindow), post.rate, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point()
ggsave(file.path(output.dir, "indexed_posts_by_time.png"), plt+thm)

##################################
#  2. Topics Shares Over Time    #
##################################
# for a given document-topic prevalence data frame, generate graphs of prevalence 
# by topic over time

# run for all of the topics
# topic / forum
col.keeps <- c("forum", "continent", "jam", "manager", "DateWindow")
by.vars <- c("jam", "forum")
plot.all.topic.shares(doc.topics.frame, documents, col.keeps, by.vars, 
                      file.path(output.dir, "forum_prev/"), ylim=c(0,0.35))

# managers and jam
col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
plot.all.topic.shares(doc.topics.frame, documents, col.keeps, by.vars, file.path(output.dir, "manager_prev/"))


#diagnostic tool for seeing a particular topic's raw numbers
#View(avg.topic.rate <- aggregate(doc.topics.data[2], by=doc.topics.data[,aggregate.set], mean))


###############################################################
#  3. Topics Doc Counts Over Time, by threshold    #
###############################################################

######
# 3.a. histograms of topic prevalance over documents, with a threshold prevalence
######

threshold <- 0.1
col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
plot.all.topic.shares(doc.topics.frame, documents, col.keeps, by.vars, sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold), ylim=NULL, threshold.for.count=threshold)

############
#  3.b. plot only prevalvence over a threshold
############

threshold <- 0.05

#col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
#doc.topics.data <- cbind(doc.topics.frame, documents[col.keeps])
#doc.topics.data <- doc.topics.data[doc.topics.data$DateWindow %in% unique(posts.by.window$DateWindow),]

this.output.dir <- file.path(output.dir, "topic_prev_threshold/")
for (topic in 1:n.topics) {
  thresh.doc.by.topic <- doc.topics.data[doc.topics.data[topic] > threshold,]
  plot_topic_shares(thresh.doc.by.topic, by.vars, this.output.dir, topic, ylim=c(0,0.3))
}



#################
# working with just the topics of focus
#################

focal.topics <-  c(6, 10, 19, 23, 27, 28, 22)
##  Notice that R indexes from 1, so these indices need incremented by 1
focal.topics.index <- focal.topics + 1

# create a data frame with just the focal topics, with prevalence rebalanced to be proportion of focal topics
focal.doc.topics <- doc.topics.frame[,focal.topics.index] / rowSums(doc.topics.frame[,focal.topics.index])
plot.all.topic.shares(focal.doc.topics, documents, col.keeps, by.vars, 
                      posts.by.window, file.path(output.dir, "focal_prev/"), focal.topics, ylim=c(0,0.3))


mallet.top.words(topic.model, topic.words[20,], 20)


#############
# histo of document lengths
#############

unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
doc.len <- rowSums(unnormal.doc.topics)
documents$words <- doc.len

plt <- qplot(documents$words, binwidth=5, xlab = "number of words")
ggsave(file.path(output.dir, "words_per_doc.png"), plt+thm)
# --and logged to show that it's approx log-normal
qplot(log(documents$words), binwidth=0.1, xlab = "log(number of words)")


# by jam
ggplot(documents, aes(x=words, color=jam)) + geom_density()
ggplot(documents, aes(x=log(words), color=jam)) + geom_density()

# by manager
ggplot(documents, aes(x=words, color=manager)) + geom_density()
ggplot(documents, aes(x=log(words), color=manager)) + geom_density()

# by manager-jam
ggplot(documents, aes(x=words, color=paste(jam,manager,sep="-"))) + geom_density()
plt <- ggplot(documents, aes(x=log(words), color=paste(jam,manager,sep="-"))) + 
          geom_density()
ggsave(file.path(output.dir, "relative_doc_lengths.png"), plt+thm)

################################
# histo of docs per user
################################

docs.per.user <- summarize(group_by(documents, jam, manager, user), docs=n(), words=sum(words))
ggplot(docs.per.user, aes(x=ifelse(docs>20,20,docs), color=paste(jam,manager,sep="-"))) + geom_histogram(postion="dodge", binwidth=1)
ggplot(docs.per.user, aes(x=log(words), color=paste(jam,manager,sep="-"))) + geom_density()

summarize(group_by(docs.per.user, jam, manager), mean(words), sd(words), mean(docs), sd(docs))


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




ret.1 <- topic.co.occur(topic.model)
#topic.counts
ret$co.occur.count
ret$corr.matrix

corr.heatmap(ret.1$corr.matrix, min=-2, max=2)

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

########################
# pull out interesting topics by forum/topic ?
########################





# Compare World and Value Topics 
l1.distances("jam", documents, topic.model)

# people_make_time is the most similar, interesting because it is the one that seems to be the most prevalent
#    across all documents

# Compare across the fora, since we saw that prevalences can be very different
l1.distances("forum", documents, topic.model)

# -- by world jam
## this DOESN'T work yet -- need to pass the full logic vector to the mallet function...
l1.distances("forum", documents, topic.model, documents$jam=="world")
# -- by values jam
l1.distances("forum", documents, topic.model, documents$jam=="values")

# Compare across managers/non-managers
l1.distances("manager", documents, topic.model)
l1.distances("manager", documents, topic.model, lambda=0.7, use.relevance=TRUE)

# Compare across continents
l1.distances("continent", documents, topic.model)


# Compare across time windows
l1.distances("CreationDate", documents, topic.model, documents$jam=="world")
l1.distances("CreationDate", documents, topic.model, documents$jam=="values")

#######################################
# compare vocabs of a topic by factor #
#######################################

topic.vocab.diff("manager", documents, topic.model, 19, number.words=10)
topic.vocab.diff("continent", documents, topic.model, 19, number.words=10)

topic.vocab.diff("manager", documents, topic.model, 19, number.words=10, lambda=0.8, use.relevance=TRUE)
topic.vocab.diff("continent", documents, topic.model, 2, number.words=10)




