comparative.word.frequencies <- NULL
topic.union.indices <- list()

vocabulary <- mallet.word.freqs(topic.model)$words
manager.top.words <- mallet.subset.topic.words(topic.model, documents$manager == "Manager", normalized=T)
other.top.words <- mallet.subset.topic.words(topic.model, documents$manager == "Other", normalized=T)

values.top.words <- mallet.subset.topic.words(topic.model, documents$CreationDate %in% c("7/29/2003", "7/30/2003", "7/31/2003", "8/1/2003"), normalized=T)
world.top.words <- mallet.subset.topic.words(topic.model, documents$CreationDate %in% c("10/26/2004", "10/27/2004", "10/28/2004"), normalized=T)

for (topic in 1:n.topics) {
  ## Top words
  manager.top.indices <- order(manager.top.words[topic,], decreasing=T)[1:10]
  other.top.indices <- order(other.top.words[topic,], decreasing=T)[1:10]
  
  values.top.indices <- order(values.top.words[topic,], decreasing=T)[1:10]
  world.top.indices <- order(world.top.words[topic,], decreasing=T)[1:10]
  
  union.indices <- union(manager.top.indices, other.top.indices, values.top.indices, world.top.indices)
  topic.union.indices[[topic]] <- union.indices
  
  comparative.word.frequencies <- rbind(comparative.word.frequencies, data.frame(topic=topic, words = as.character(vocabulary[union.indices ]),
                                                                                 managerValues=manager.top.words[topic,union.indices ],
                                                                                 otherValues=other.top.words[topic,union.indices ],
                                                                                 valuesWords=values.top.words[topic,union.indices ],
                                                                                 worldWords=world.top.words[topic,union.indices ],
                                                                                 type="real"))
}

for (rep in 1:20) {
  manager.top.words <- mallet.subset.topic.words(topic.model, sample(documents$manager == "Manager", nrow(documents)), normalized=T)
  other.top.words <- mallet.subset.topic.words(topic.model, sample(documents$manager == "Other", nrow(documents)), normalized=T)
  
  values.top.words <- mallet.subset.topic.words(topic.model, sample(documents$CreationDate %in% c("7/29/2003", "7/30/2003", "7/31/2003", "8/1/2003"), nrow(documents)), normalized=T)
  world.top.words <- mallet.subset.topic.words(topic.model, sample(documents$CreationDate %in% c("10/26/2004", "10/27/2004", "10/28/2004"), nrow(documents)), normalized=T)
  
  for (topic in 1:n.topics) {
  union.indices <- topic.union.indices[[topic]]
  
  comparative.word.frequencies <- rbind(comparative.word.frequencies, data.frame(topic=topic, words = as.character(vocabulary[union.indices ]),
                                                                                 managerValues=manager.top.words[topic,union.indices ],
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
  topic.top.docs <- rbind(topic.top.docs, data.frame(topic=topic, weight=round(doc.topics[doc.indices, topic], digits=4), doc=doc.indices - 1))
}

write.csv(comparative.word.frequencies, file=sprintf("%s/word.frequencies.csv", model.dir), row.names=F)
