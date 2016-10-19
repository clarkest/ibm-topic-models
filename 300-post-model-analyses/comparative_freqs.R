output.dir <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/library/mimno_browse"

comparative.word.frequencies <- NULL
topic.union.indices <- list()
by.var <- "new.mgr"
by.var.vals <- c("executive", "manager")
# note: this should be under 10 chars or is starts to mess with the html formatting
factor.label <- "exec_mana"

doc.set <- threaded.docs

vocabulary <- mallet.word.freqs(topic.model)$words
factor.top.words <- 
  mallet.subset.topic.words(topic.model, doc.set[,by.var] %in% by.var.vals, normalized=T)
other.top.words <- 
  mallet.subset.topic.words(topic.model, !doc.set[,by.var] %in% by.var.vals, normalized=T)

values.top.words <- 
  mallet.subset.topic.words(topic.model, doc.set$jam == "values", normalized=T)
world.top.words <- 
  mallet.subset.topic.words(topic.model, doc.set$jam == "world", normalized=T)

for (topic in 1:n.topics) {
  ## Top words
  factor.top.indices <- order(factor.top.words[topic,], decreasing=T)[1:10]
  other.top.indices <- order(other.top.words[topic,], decreasing=T)[1:10]
  
  values.top.indices <- order(values.top.words[topic,], decreasing=T)[1:10]
  world.top.indices <- order(world.top.words[topic,], decreasing=T)[1:10]
  
  union.indices <- Reduce(union, 
                          list(factor.top.indices, 
                               other.top.indices, 
                               values.top.indices,
                               world.top.indices))
  
  topic.union.indices[[topic]] <- union.indices
  
  comparative.word.frequencies <- 
    rbind(comparative.word.frequencies, 
          data.frame(topic=topic, words = as.character(vocabulary[union.indices]),
                     factorValues=factor.top.words[topic, union.indices],
                     otherValues=other.top.words[topic, union.indices],
                     valuesWords=values.top.words[topic, union.indices],
                     worldWords=world.top.words[topic, union.indices],
                     type="real"))
}
# create random replicates to set a context for the observed values
for (rep in 1:20) {
  factor.top.words <- 
    mallet.subset.topic.words(topic.model, 
                              sample(doc.set[,by.var] %in% by.var.vals, nrow(doc.set)),  
                              normalized=T)
  other.top.words <- 
    mallet.subset.topic.words(topic.model, 
                              sample(!doc.set[,by.var] %in% by.var.vals, nrow(doc.set)),  
                              normalized=T)
  
  values.top.words <- 
    mallet.subset.topic.words(topic.model, 
                              sample(doc.set$jam == "values", nrow(doc.set)),  
                              normalized=T)
  world.top.words <- 
    mallet.subset.topic.words(topic.model, 
                              sample(doc.set$jam == "world", nrow(doc.set)),  
                              normalized=T)
    
  for (topic in 1:n.topics) {
    union.indices <- topic.union.indices[[topic]]
    
    comparative.word.frequencies <- 
      rbind(comparative.word.frequencies, 
            data.frame(topic=topic, words = as.character(vocabulary[union.indices ]),
                       factorValues=factor.top.words[topic,union.indices ],
                       otherValues=other.top.words[topic,union.indices ],
                       valuesWords=values.top.words[topic,union.indices ],
                       worldWords=world.top.words[topic,union.indices ],
                       type="replicated"))
  }
}

topic.top.docs <- NULL
  
for (topic in 1:n.topics) {
    
  ## Top docs
  doc.indices <- order(doc.topics[,topic], decreasing=T)[1:50]
  topic.top.docs <- rbind(topic.top.docs, 
                          data.frame(topic=topic, 
                                     weight=round(doc.topics[doc.indices, topic], digits=4),
                                     doc=doc.indices - 1))
}


#.defer(d3.csv, "word.frequencies.csv", function(d) { return {topic: +d.topic, word: d.words, role: [d.otherValues, d.managerValues], jam: [d.worldWords, d.valuesWords], type: d.type }; })
write.csv(comparative.word.frequencies, 
          file=sprintf("%s/word.frequencies.csv", output.dir), 
          row.names=F)

# both.tsv
out.doc.set <- select(doc.set, id, text, CreationDate)
out.doc.set$text <- str_trim(out.doc.set$text)
out.doc.set$factor <- ifelse(doc.set[,by.var] %in% by.var.vals,
                             factor.label,
                             "other")

write.table(out.doc.set, 
          file=sprintf("%s/both.tsv", output.dir), 
          row.names=F, sep='\t'
          )

# .defer(d3.csv, "date_manager_topic.csv",
#     topicLabel: d.Topic, date: d.CreationDate, manager: d.manager,
# mean: +d.mean, low: +d.low, high: +d.high }; })

doc.topics.frame <- data.frame(doc.topics)
#names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
names(doc.topics.frame) <- topics.labels

## Create one big data table containing the original documents and also their topic proportions
docs.and.topics <- cbind(threaded.docs, doc.topics.frame)

## Turn the resulting wide table into a long narrow table, with columns
##  for date, manager status, topic, and document-topic weight
date.factor.topic.values <- melt(docs.and.topics, id.vars=c("CreationDate", by.var), 
                                  measure.vars=topics.labels, variable.name="topic")

## It was easier to sort dates if they're strings instead of Date objects
date.factor.topic.values$CreationDate <- 
  as.character(date.manager.topic.values$CreationDate)


date.factor.topic.values$factor <- 
  ifelse(date.factor.topic.values[,by.var] %in% by.var.vals,
         factor.label,
         "other")


## Run a bootstrap test to get high and low confidence intervals for each date/manager-status pair
boot.hi <- function(x) { smean.cl.boot(x)["Upper"] }
boot.low <- function(x) { smean.cl.boot(x)["Lower"] }
mean.hi.low <- group_by(date.factor.topic.values, topic, factor, CreationDate) %>% 
  summarise(mean = round(mean(value), digits=6), 
            low= round(boot.low(value), digits=6), 
            high= round(boot.hi(value), digits=6))
mean.hi.low <- filter(mean.hi.low, CreationDate != "10/29/2004")
write.csv(mean.hi.low, file=sprintf("%s/date_factor_topic.csv", output.dir),
          row.names=F)


# .defer(d3.csv, "topic.labels.csv",
#     topic: +d.topic, words: d.labels, label: d.shortLabels 

doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
## Get short labels that can be used as part of filename
topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 3))
topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)

write.csv(data.frame(labels=topics.long.labels, 
                     shortLabels=topics.labels, 
                     topic=1:n.topics), 
          file=sprintf("%s/topic.labels.csv", output.dir), 
          row.names=F)

# .defer(d3.csv, "topic.top.docs.csv",
#    topic: +d.topic, doc: +d.doc, weight: +d.weight 

topic.top.docs <- NULL

for (topic in 1:n.topics) {
  
  ## Top docs
  doc.indices <- order(doc.topics[,topic], decreasing=T)[1:50]
  topic.top.docs <- rbind(topic.top.docs, 
                          data.frame(topic=topic, 
                                     weight=round(doc.topics[doc.indices, topic], digits=4), 
                                     doc=doc.indices - 1))
}
write.csv(topic.top.docs, 
          file=sprintf("%s/topic.top.docs.csv", output.dir), 
          row.names=F)

