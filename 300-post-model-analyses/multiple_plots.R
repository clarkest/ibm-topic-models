
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")

create_graph_set <- function(model.name, n.topic, model.num) {
  
  #model.object <- load.model.for.analysis(n.topics, model.name, model.num, regex.name="old_punctuation") 
  model.object <- load.model.for.analysis(n.topics, model.name, model.num) 
  topic.model <- model.object$topic.model
  documents <- model.object$documents
  doc.topics <- model.object$doc.topics
  doc.topics.frame <- model.object$doc.topics.frame
  model.label <- model.object$model.label
  
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
  plot.all.topic.shares(model.object, col.keeps, by.vars, 
                        file.path(output.dir, "forum_prev/"), ylim=c(0,0.35))
  
  # managers and jam
  col.keeps <- c("manager", "continent", "jam", "DateWindow")
  by.vars <- c("manager", "jam")
  plot.all.topic.shares(model.object, col.keeps, by.vars, file.path(output.dir, "manager_prev/"))
  
  # gender and jam
  col.keeps <- c("gender", "continent", "jam", "DateWindow", "manager")
  by.vars <- c("gender", "jam")
  plot.all.topic.shares(model.object, col.keeps, by.vars, file.path(output.dir, "gender_prev/"), ylim=c(0,0.15))
  
  #diagnostic tool for seeing a particular topic's raw numbers
  #View(avg.topic.rate <- aggregate(doc.topics.data[2], by=doc.topics.data[,aggregate.set], mean))
  
  
  ###############################################################
  #  3. Topics Doc Counts Over Time, by threshold    #
  ###############################################################
  
  ######
  # 3.a. histograms of topic prevalance over documents, with a threshold prevalence
  ######
  # add the document lengths to the model.object
  unnormal.doc.topics <- mallet.doc.topics(model.object$topic.model, smoothed=F, normalized=F)
  model.object$doc.len <- rowSums(unnormal.doc.topics)
  
  threshold <- 0.1
  # minimum number of words
  col.keeps <- c("manager", "continent", "jam", "DateWindow")
  by.vars <- c("manager", "jam")
  plot.all.topic.shares(model.object,
                        col.keeps, 
                        by.vars, 
                        sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                        ylim=NULL, 
                        threshold.prev=threshold
  )
  threshold<-0.2
  plot.all.topic.shares(model.object,
                        col.keeps, 
                        by.vars, 
                        sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                        ylim=NULL, 
                        threshold.prev=threshold
  )
  threshold<-0.05
  plot.all.topic.shares(model.object,
                        col.keeps, 
                        by.vars, 
                        sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                        ylim=NULL, 
                        threshold.prev=threshold
  )
}
create_graph_set("anchor_ngram",  30, 1)
for (i in 2:8) {
  create_graph_set("anchor_ngram",  30, i)
}
create_graph_set("anchor_ngram",  30, 10)