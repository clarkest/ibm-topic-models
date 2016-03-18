

n.topics <- 30
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "/media/sf_ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
model.name <- "anchor_ngram_model"
iters <- 800
maxims <- 100
model.num <- 8

model.object <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label


co.occur <- topic.co.occur(model.object$topic.model)
corr.heatmap(co.occur$corr.matrix)

correlates <- melt(co.occur$corr.matrix)
#drop everything on or below the diagonal
corr.triangle <- correlates[correlates$Var1 < correlates$Var2,]


corr.triangle[with(corr.triangle, order(-value)),][1:20,]

group.customer <- c(15,4,18)
group.trust <- c(12,9,21,26)
group.modern <- c(22,11,7,17)

get.group.corrs <- function(group, corr.tri) {
  corrs <- c()
  for (topic.1 in group) {
    for (topic.2 in group) {
      if (topic.1 < topic.2) {
        corrs <- rbind(corrs, corr.tri[corr.tri$Var1==topic.1 & corr.tri$Var2==topic.2,])
      }
    }
  }
  return(corrs[with(corrs, order(-value)),])
}

get.cooccur.topics <- function(topic, cutoff, corr.tri) {
  topic.corrs <- corr.tri[corr.tri$value >= cutoff & (corr.tri$Var1==topic | corr.tri$Var2==topic),]
  return(topic.corrs[with(topic.corrs, order(-value)),])
}

get.group.corrs(group.customer, corr.triangle)
get.group.corrs(group.trust, corr.triangle)
get.group.corrs(group.modern, corr.triangle)

get.cooccur.topics(7, 0.5, corr.triangle)
get.cooccur.topics(17, 0.5, corr.triangle)


# what we REALLY want are two topics that co-occur with the same other topic, 
# but rarely or negatively co-occur with one another
cutoff <- 0.5
top.corr <- corr.triangle[corr.triangle$value >= cutoff,]
pair <- list()
for (row in top.corr) { 
  print(row)
  #pair[[Var1]] <- row$Var2
  #pair[[Var2]] <- row$Var1
}



# forum differences by factor
val.docs <- threaded.docs[threaded.docs$jam=="values",]
val.docs$forum <- factor(val.docs$forum)
world.docs <- documents[documents$jam=="world",]

val.chi <- chisq.test(table(val.docs$forum, val.docs$new.mgr))
val.chi$stdres

world.chi <- chisq.test(table(world.docs$forum, world.docs$manager))
world.chi$stdres

# and with the new manager labels
val.chi <- chisq.test(table(val.docs$forum, val.docs$new.mgr))
val.chi$stdres

world.chi <- chisq.test(table(world.docs$forum, world.docs$new.mgr))
world.chi$stdres


