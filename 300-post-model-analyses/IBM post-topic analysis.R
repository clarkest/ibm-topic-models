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

# make sure the outpus directory for this model exists
output.dir <- file.path("outputs", model.label) 
dir.create(output.dir, showWarnings = FALSE)

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




########################
# distances between subgroups within each topic
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




