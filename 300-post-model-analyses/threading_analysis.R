source("300-post-model-analyses/mallet_analyses.R")

n.topics <- 30
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
model.name <- "windows"
iters <- 800
maxims <- 50
model.num <- 1

list <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label

#################
#   THREADING   #
#################

# the ancestry was calculated in SQL, using the non-processed data for cases when culled docs
# were "missing links"

# load the ancestry information from the sql dump
ancestry <- read.delim("place_docs_here/thread_ancestry.csv", 
                       encoding="UTF-8", 
                       sep="\t", quote='', 
                       stringsAsFactors=FALSE)

# map to documents by id 
##  -- note that the document ids have been altered because of the duplicate ids
new.docs <- merge(documents, ancestry, by.x="id", by.y="id")

# transform the ancestry text into a list of ancestors
new.docs$ancestors <- 
  lapply(new.docs$ancestry, function(x) unlist(strsplit(x, split=",")))

names(new.docs$ancestors) <- new.docs$id
new.docs$ancestors[new.docs$ancestors=="null"] <- NA 
melted.ancestors <- melt(new.docs$ancestors[new.docs$ancestors!="null"])
num.children <- summarize(group_by(melted.ancestors, value), n.children=n())
new.docs <- merge(new.docs, num.children, by.x="id", by.y="value", all.x=TRUE, suffixes=c("x",""))
new.docs$n.children[is.na(new.docs$n.children)] <- 0

class <- "manager"
# how many managers v. others get a response when they post?
summarize(group_by_(new.docs, .dots=c("jam",class)), 
          total.posts=n(), 
          with.children=sum(n.children>0),
          with.5=sum(n.children>=5),
          with.10=sum(n.children>=10),
          perc.response=with.children/total.posts,
          perc.5=with.5/total.posts,
          perc.10=with.10/total.posts
          )

# how about for top-line comments?
summarize(group_by_(new.docs[is.na(new.docs$ancestors),], .dots=c("jam", class)), 
          total.posts=n(), 
          with.children=sum(n.children>0),
          with.5=sum(n.children>=5),
          with.10=sum(n.children>=10),
          perc.response=with.children/total.posts,
          perc.5=with.5/total.posts,
          perc.10=with.10/total.posts
)

# are people more likely to respond to people in the same class as them?
class <- "manager"
# same as parent?
match.to.parents <- merge(new.docs[,c("id", "parent_id", class)], 
      new.docs[,c("id", class)], 
      by.x="parent_id",
      by.y="id",
      suffixes=c("child","parent"),
      all.x=TRUE)

match.to.parents$match.parent <- match.to.parents[paste0(class,"child")] == match.to.parents[paste0(class,"parent")]
match.to.parents <- match.to.parents[c("id", "match.parent")]

new.docs <- merge(new.docs, match.to.parents)
  
summarize(group_by_(new.docs, .dots=c("jam", class)), 
         match=sum(match.parent, na.rm=TRUE), 
         different=sum(!match.parent, na.rm=TRUE),
         match_perc = match / (match+different)
         )


  # how many of its ancestors is it like?

#dplyr is already loaded from mallet_analyses


