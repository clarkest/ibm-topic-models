
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
# wd <-  "/media/sf_ibm-topic-model"
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

# now load the unix model data and documents
list <- load.model.for.analysis(n.topics, model.name, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label

MyParentsParents <- function(id, parents, ancestor.list=c()) {
  parent.id <- toString(parents[id])
  if (parent.id=="NA") {
    return (c("missing", ancestor.list))
  } else if (parent.id == "null") {
    return (ancestor.list)
  } else {
    return (MyParentsParents(parent.id, parents, c(parent.id, ancestor.list)))
  }
}
#TODO -- memoize this!
MyParentsParentsStr <- function(id, parents, ancestor.list="") {
  parent.id <- toString(parents[id])
  if (parent.id=="NA") {
    return (paste("missing", ancestor.list, sep=","))
  } else if (parent.id == "null") {
    return (ancestor.list)
  } else {
    return (MyParentsParentsStr(parent.id, parents, paste(parent.id, ancestor.list, sep=",")))
  }
}

parents <- documents$parent
names(parents) <- documents$id
# i'm not proud of this hackiness, but it works
#   generating the list directly using MyParentsParents wasn't playing well with melt()
#   however, using the old unlist(strsplit()) code worked beautifully
#   so, generate the string first, then use the old code to get back at the lists
ancestors <- sapply(documents$id, MyParentsParentsStr, parents)
names(ancestors) <- documents$id

threaded.docs <- documents
# transform the ancestry text into a list of ancestors
threaded.docs$ancestors <- lapply(ancestors, function(x) unlist(strsplit(x, split=",")))
threaded.docs$generation <- sapply(threaded.docs$ancestors, length) + 1

threaded.docs$ancestors[threaded.docs$ancestors==""] <- NA 
melted.ancestors <- melt(threaded.docs$ancestors)

num.children <- summarize(group_by(melted.ancestors, value), n.children=n())
# drop the count on "missing"
num.children <- num.children[num.children$value!="missing",]
threaded.docs <- merge(threaded.docs, num.children, by.x="id", by.y="value", all.x=TRUE, suffixes=c("x",""))
threaded.docs$n.children[is.na(threaded.docs$n.children)] <- 0

# let's save these, since that recursive ancestry is freaking slow
save(threaded.docs, file=sprintf("place_docs_here/threaded_docs.Rdata", model.name))
# load(sprintf("place_docs_here/threaded_docs.Rdata")

# how many managers v. others get a response when they post?
ResponseRates <- function(class) {
  summarize(group_by_(threaded.docs, .dots=c("jam",class)), 
          total.posts=n(), 
          with.children=sum(n.children>0),
          with.5=sum(n.children>=5),
          with.10=sum(n.children>=10),
          perc.response=with.children/total.posts,
          perc.5=with.5/total.posts,
          perc.10=with.10/total.posts
  )
}
class <- "manager"
ResponseRates("manager")
ResponseRates("gender")
# do post shares change between jams though??
table(threaded.docs$jam, threaded.docs$manager) / as.vector(table(threaded.docs$jam))
table(threaded.docs$jam, threaded.docs$gender) / as.vector(table(threaded.docs$jam))

# how about for top-line comments?
summarize(group_by_(threaded.docs[is.na(threaded.docs$ancestors),], .dots=c("jam", class)), 
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
class <- "gender"

class.parent <- paste0(class,".parent")
# same as parent?
match.to.parents <- merge(threaded.docs[,c("id", "parent", class)], 
      threaded.docs[,c("id", class)], 
      by.x="parent",
      by.y="id",
      suffixes=c(".child",".parent"),
      all.x=TRUE)

match.to.parents$match.parent <- 
  match.to.parents[paste0(class,".child")] == match.to.parents[class.parent]
match.to.parents <- match.to.parents[c("id", "match.parent", class.parent)]
#attr(match.to.parents$match.parent, "dimanmes") <- NULL

threaded.docs <- merge(threaded.docs, match.to.parents)
# oddly, the match.parent column is picking up attributes that kill summarize.  so, remove them
attr(threaded.docs$match.parent, "dimnames") <- NULL
attr(threaded.docs$match.parent, "dimnames") <- NULL




# look at whether comments share the same CLASS as their parent
summarize(group_by_(threaded.docs, .dots=c("jam", class)), 
         match=sum(match.parent, na.rm=TRUE), 
         different=sum(!match.parent, na.rm=TRUE),
         match_perc = match / (match + different)
         )

subset <- threaded.docs$jam == "values"
val.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, "match.parent"]))
val.class.chi$residuals



# BETTER: chi-square standard residuals of child class and parent class
subset <- threaded.docs$jam == "values"
val.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, class.parent]))
val.class.chi$residuals

subset <- threaded.docs$jam == "world"
world.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, class.parent]))
world.class.chi$residuals


# what we really want is expected values that are based on the total population proportions of comments
# not "the number of postings by manaers that received responses,"
# but, "the number of posts by managers"

t <- table(threaded.docs[subset, class], threaded.docs[subset, class.parent])
a <- table(threaded.docs[subset, class]) / sum(subset)
colSums(t)


# let's focus on just threads above X in length
min.thread <- 5

# get the original comment of those threads
long.threads <- threaded.docs[threaded.docs$n.children >= 5 & threaded.docs$parent=="null", c("id","n.children")]
long.threads$original <- long.threads$id
children.of.long <- melted.ancestors[melted.ancestors$value %in% long.threads$id,]
names(children.of.long) <- c("original", "id")
children.of.long <- rbind(long.threads[c("original","id")], children.of.long)
children.of.long <- merge(children.of.long, threaded.docs[c("id", class)])

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
                                threaded.docs[c("id", class)],
                                by.x="original",
                                by.y="id"
                          )


# get the list of children for each of these



# how many of its ancestors is it like?
class <- "manager"
# same as parent?
match.to.ancestors <- merge(threaded.docs[,c("id", "parent", class)], 
                          threaded.docs[,c("id", class)], 
                          by.x="parent",
                          by.y="id",
                          suffixes=c("child","parent"),
                          all.x=TRUE)

#dplyr is already loaded from mallet_analyses


