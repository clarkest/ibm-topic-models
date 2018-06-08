library(stm)
library(htmlTable)
library(scales)
library(R2HTML)
source("400-other-analyses/toLDAvis.R")

# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "/Users/clarkbernier/Box Sync/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")

model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

#model.object <- load.model.for.analysis(n.topics, model.name, model.num, regex.name="old_punctuation") 
model.object <- load.model.for.analysis(n.topics, model.name, model.num) 
topic.model <- model.object$topic.model
documents <- model.object$documents
doc.topics <- model.object$doc.topics
doc.topics.frame <- model.object$doc.topics.frame
model.label <- model.object$model.label

world.docs <- filter(documents, jam=='world')

# don't stem the words
stop.eng <- read.csv("200-topic-models/en.txt", header=F)
processed.docs <- textProcessor(world.docs$text, 
                                stem = F,
                                removestopwords = F,
                                customstopwords = stop.eng$V1,
                                metadata = world.docs)
plotRemoved(processed.docs$documents, lower.thresh = seq(1, 130, by = 5))

prepped.docs <- prepDocuments(processed.docs$documents, 
                              processed.docs$vocab, 
                              processed.docs$meta,
                              lower.thresh = 20)

stm.fit.30 <- 
  stm(prepped.docs$documents, 
      prepped.docs$vocab, 
      K = 30, # number of topics
      #prevalence =~ rating + s(day),  
      #content =~ forum,
      max.em.its = 75,
      data = prepped.docs$meta, 
      init.type = "Spectral")

stm.fit.25 <- 
  stm(prepped.docs$documents, 
      prepped.docs$vocab, 
      K = 25, # number of topics
      max.em.its = 75,
      data = prepped.docs$meta, 
      init.type = "Spectral")

stm.fit.30.forum <- 
  stm(prepped.docs$documents, 
      prepped.docs$vocab, 
      K = 30, # number of topics
      prevalence  =~ forum,
      max.em.its = 75,
      data = prepped.docs$meta, 
      init.type = "Spectral")

stm.manyTop <- manyTopics(prepped.docs$documents, 
           prepped.docs$vocab, 
           K = c(15,20,25,30,35), # number of topics
           #prevalence =~ rating + s(day),  
           #content =~ forum,
           max.em.its = 75,
           data = prepped.docs$meta, 
           init.type = "Spectral")

save(stm.fit.30, prepped.docs, file = "place_docs_here/world_stm_30.Rdata")
# load("place_docs_here/world_stm_30.Rdata")
save(stm.fit.25, prepped.docs, file = "place_docs_here/world_stm_25.Rdata")
# load("place_docs_here/world_stm_25.Rdata")
save(stm.fit.30.forum, prepped.docs, file = "place_docs_here/world_stm_30_forum.Rdata")
# load("place_docs_here/world_stm_30_forum.Rdata")

explorePacket <- function(stm.fit, docs, texts, model.name, n.words=50, n.docs=30) {
  dir.path <- sprintf("outputs/%s", model.name)
  dir.create(dir.path, showWarnings = FALSE)
  
  # percent tokens per topic
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2,]))))
  terms.per.topic <- stm.fit$theta * doc.length
  perc.tokens <- colSums(terms.per.topic) / sum(terms.per.topic)
  
  # top 50 words
  topic.words <- t(sageLabels(stm.fit, n=n.words)$marginal$frex)
  
  # top documents and links
  doc.set <- findThoughts(stm.fit, texts=texts, n=n.docs)$docs
  for (topic in 1:length(doc.set)) {
    file.name <- sprintf('top_docs_%s.html', topic)
    doc.df <- data.frame(documents = doc.set[topic])
    HTMLInitFile(outdir = dir.path, filename = sprintf('top_docs_%s', topic))
      HTML(htmlTable(doc.df, align='l', col.rgroup = c("none", "#B1B1B1"),
                     css.cell = "padding-left: .5em; padding-right: .2em; padding-top: .2em; padding-bottom: .2em;"))
    HTMLEndFile(file = HTMLGetFile())
  }  
  links <- sprintf("<a href='top_docs_%s.html'>Top Docs</a>", 1:ncol(topic.set))
  
  
  # put together and output html table
  topic.set <- rbind(percent(perc.tokens), links, topic.words)
  # also dump a csv to the folder
  csv.topic.set <- rbind(perc.tokens, topic.words)
  colnames(csv.topic.set) <- paste0("Topic ", 1:ncol(topic.set))
  write.csv(csv.topic.set, file=sprintf('%s/top_words.csv', dir.path), row.names=F)
  
  httab <- htmlTable(topic.set, 
                     header=paste0("Topic ", 1:ncol(topic.set)),
                     rnames=F,
                     col.rgroup = c("#E1E1E1","none"),
                     rgroup = c("Topic Stats","Words"),
                     n.rgroup = c(2, n.words - 2))
  
  # and the LDAvis
  lda.dir <- sprintf("%s/ldavis", dir.path)
  toLDAvis(stm.fit, docs, out.dir = lda.dir, open.browser=F)
  
  # and all of it into the html file
  HTMLInitFile(outdir = dir.path)
    HTML.title(sprintf("Model: %s", model.name))
    HTML(sprintf("<a href='ldavis/index.html'>Model LDAVis</a><br>", lda.dir))
    HTML("<a href='top_words.csv'>Top Words CSV</a><br>")
    HTML(httab)
  HTMLEndFile(file = HTMLGetFile())
}


explorePacket(stm.fit.30,
              docs=prepped.docs$documents,
              texts=prepped.docs$meta$text,
              model.name="STM_30")
explorePacket(stm.fit.30.forum,
              docs=prepped.docs$documents,
              texts=prepped.docs$meta$text,
              model.name="STM_30_forum")
explorePacket(stm.fit.25,
              docs=prepped.docs$documents,
              texts=prepped.docs$meta$text,
              model.name="STM_25")


labelTopics(stm.fit.30)
sl <- sageLabels(stm.fit.30, n=50)
apply(sl$marginal$frex, 1, paste, collapse=" ")
labels <-

# let's see how the topics differ by forum
fit.effects <- estimateEffect(1:30 ~ forum, 
                              stm.fit.30.forum, 
                              meta = prepped.docs$meta, 
                              uncertainty = "Global")
save(fit.effects, file="place_docs_here/stm_forum_fit.Rdata")
# load("place_docs_here/stm_forum_fit.Rdata")

# get custom labels
sl <- sageLabels(stm.fit.30.forum, n=12)
labels <- apply(sl$marginal$frex[,1:3], 1, paste, collapse=" ")

plot.estimateEffect(fit.effects, 
                    covariate = "forum", 
                    topics = 1:30,
                    model = stm.fit.30.forum, 
                    method = "difference",
                    cov.value1 = "forum 1", cov.value2 = "forum 6",
                    xlab = "More 1 ... More 2",
                    main = "Relative Prevalence by Forum",
                    xlim = c(-.04, .04), 
                    labeltype = "custom",
                    custom.labels = labels,
                    n = 3, # only list the top 3 words
                    width = 35 # allow for longer label strings
)


########################
# LDAvis for content covars
########################
library(devtools)
install_github("cpsievert/LDAvis") 
source("400-other-analyses/toLDAvis.R")
out.dir <- "~/sandbox/STM LDAvis"
toLDAvis(stm.fit.30, prepped.docs$documents)
out.dir <- "~/sandbox/STM LDAvis forum"
toLDAvis(stm.fit.30.forum, prepped.docs$documents, out.dir = out.dir)




###############################
# Analysis with a model
###############################
load("place_docs_here/world_stm_30.Rdata")
model <- stm.fit.30

load("place_docs_here/threaded_docs.Rdata")
load("place_docs_here/by_root.Rdata")





