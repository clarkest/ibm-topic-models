library(stm)

# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
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


# don't stem the words
stop.eng <- read.csv("200-topic-models/en.txt", header=F)
processed.docs <- textProcessor(documents$text, 
                                stem = F,
                                removestopwords = F,
                                customstopwords = stop.eng$V1,
                                metadata = documents)
plotRemoved(processed.docs$documents, lower.thresh = seq(1, 130, by = 5))

prepped.docs <- prepDocuments(processed.docs$documents, 
                              processed.docs$vocab, 
                              processed.docs$meta,
                              lower.thresh = 10)

stm.fit.30 <- 
  stm(prepped.docs$documents, 
      prepped.docs$vocab, 
      K = 30, # number of topics
      #prevalence =~ rating + s(day),  
      content =~ jam,
      max.em.its = 75,
      data = prepped.docs$meta, 
      init.type = "Spectral")
stm.fit.100 <- 
  stm(prepped.docs$documents, 
      prepped.docs$vocab, 
      K = 100, # number of topics
      content =~ jam,
      #prevalence =~ rating + s(day),  
      max.em.its = 75,
      data = prepped.docs$meta, 
      init.type = "Spectral")

save(stm.fit.30, prepped.docs, file = "place_docs_here/ngram_stm_30.Rdata")
# load("place_docs_here/ngram_stm_30.Rdata")

labelTopics(stm.fit.30)
sl <- sageLabels(stm.fit.30)

labels <- apply(sl$marginal$frex[,1:5], 1, paste, collapse=" ")

# let's see how the topics differ by jam
fit.effects <- estimateEffect(1:30 ~ jam, 
                              stm.fit.30, 
                              meta = prepped.docs$meta, 
                              uncertainty = "Global")
save(fit.effects, file="place_docs_here/stm_jam_fit.Rdata")
# load("place_docs_here/stm_jam_fit.Rdata")

# get custom labels
sl <- sageLabels(stm.fit.30)
labels <- apply(sl$marginal$frex[,1:3], 1, paste, collapse=" ")

plot.estimateEffect(fit.effects, 
                    covariate = "jam", 
                    topics = 1:30,
                    model = stm.fit.30, 
                    method = "difference",
                    cov.value1 = "world", cov.value2 = "values",
                    xlab = "More Values ... More World",
                    main = "Relative Prevalence by Jam",
                    xlim = c(-.1, .1), 
                    labeltype = "custom",
                    custom.labels = labels,
                    n = 3, # only list the top 3 words
                    width = 35 # allow for longer label strings
)

# so there is good balance across jams of most topics


##################
# Language Difference By Jam
##################
labels <- apply(sl$marginal$frex, 1, paste, collapse=" ")
index.html <- "<html>
<body>
<h1> STM with Content Covariates By Jam</h1>
"  
for (i in 1:30) {
  loc.file <- paste0("topic-", i, ".png")
  fil.name <- paste0("outputs/", model.label, "/vocab_by_jam/topic-", i, ".png")
  png(fil.name)
  plot.STM(stm.fit.30, type="perspectives", topics=i, text.cex=1, n=30)
  dev.off()
  index.html <- paste0(index.html, "<a href='", loc.file, "'>topic ", i , ": ", labels[i], "</a><br><br>")
}

index.html <- paste0(index.html, "
  </body>
  </html>")
write(index.html, paste0("outputs/", model.label, "/vocab_by_jam/index.html"))

toLDAvis(stm.fit.30, prepped.docs$documents)
