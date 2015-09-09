
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
# wd <-  "/media/sf_ibm-topic-model"
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

# unfortunately, the Ancestry text was done in windows, so we need the windows version
# of the documents (especially their ids) in order to do the matching
load(sprintf("models_dir/%s-docs.Rdata",model.name))
win.docs <- documents

# now load the unix model data and documents
list <- load.model.for.analysis(n.topics, model.name, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label

# sanity check that the document sets are indexed the same way
#   this should be 0
sum(documents$CreationTime != win.docs$CreationTime)

#################
#   THREADING   #
#################

# the ancestry was calculated in SQL, using the non-processed data for cases when culled docs
# were "missing links"

# load the ancestry information from the sql dump
ancestry <- read.delim("place_docs_here/thread_ancestry_mac.csv", 
                       encoding="UTF-8", 
                       sep="\t", quote='', 
                       stringsAsFactors=FALSE)

# map to documents by id 
##  -- note that the document ids have been altered because of the duplicate ids
merge.win.docs <- merge(win.docs, ancestry, by.x="id", by.y="id")
new.docs <- cbind(documents, merge.win.docs[,c("title","parent_id","ancestry","generation")])

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

class.parent <- paste0(class,".parent")
# same as parent?
match.to.parents <- merge(new.docs[,c("id", "parent_id", class)], 
      new.docs[,c("id", class)], 
      by.x="parent_id",
      by.y="id",
      suffixes=c(".child",".parent"),
      all.x=TRUE)

match.to.parents$match.parent <- 
  match.to.parents[paste0(class,".child")] == match.to.parents[class.parent]
match.to.parents <- match.to.parents[c("id", "match.parent", class.parent)]

new.docs <- merge(new.docs, match.to.parents)

# look at whether comments share the same CLASS as their parent
summarize(group_by_(new.docs, .dots=c("jam", class)), 
         match=sum(match.parent, na.rm=TRUE), 
         different=sum(!match.parent, na.rm=TRUE),
         match_perc = match / (match+different)
         )
subset <- new.docs$jam == "values"
val.class.chi <- chisq.test(table(new.docs[subset, class], new.docs[subset, "match.parent"]))
val.class.chi$residuals

# BETTER: chi-square standard residuals of child class and parent class
subset <- new.docs$jam == "values"
val.class.chi <- chisq.test(table(new.docs[subset, class], new.docs[subset, class.parent]))
val.class.chi$residuals

subset <- new.docs$jam == "world"
world.class.chi <- chisq.test(table(new.docs[subset, class], new.docs[subset, class.parent]))
world.class.chi$residuals


# what we really want is expected values that are based on the total population proportions of comments
# not "the number of postings by manaers that received responses,"
# but, "the number of posts by managers"

t <- table(new.docs[subset, class], new.docs[subset, class.parent])
a <- table(new.docs[subset, class]) / sum(subset)
colSums(t)


# let's focus on just threads above X in length
min.thread <- 5

# get the original comment of those threads
long.threads <- new.docs[new.docs$n.children >= 5 & new.docs$parent_id=="null", c("id","n.children")]
long.threads$original <- long.threads$id
children.of.long <- melted.ancestors[melted.ancestors$value %in% long.threads$id,]
names(children.of.long) <- c("original", "id")
children.of.long <- rbind(long.threads[c("original","id")], children.of.long)
children.of.long <- merge(children.of.long, new.docs[c("id", class)])

class.counts.by.thread <- summarize(group_by_(children.of.long, .dots=c("original")), 
          count=n(),
          num_man = sum(manager=="Manager", na.rm=TRUE),
          num_oth = sum(manager=="Other", na.rm=TRUE),
          perc_oth = num_oth/count,
          perc_man = num_man/count
          )

hist(class.counts.by.thread$perc_man, breaks=10)
hist(class.counts.by.thread$perc_oth, breaks=10)
hist(class.counts.by.thread$count)
summarize(group_by_(class.counts.by.thread, .dots=c("original")),
          tot = sum(count, na.rm=TRUE),
          tot = sum(count, na.rm=TRUE)
)

class.counts.by.thread <- merge(class.counts.by.thread, 
                                new.docs[c("id", class)],
                                by.x="original",
                                by.y="id"
                          )


# get the list of children for each of these



# how many of its ancestors is it like?
class <- "manager"
# same as parent?
match.to.ancestors <- merge(new.docs[,c("id", "parent_id", class)], 
                          new.docs[,c("id", class)], 
                          by.x="parent_id",
                          by.y="id",
                          suffixes=c("child","parent"),
                          all.x=TRUE)

#dplyr is already loaded from mallet_analyses


