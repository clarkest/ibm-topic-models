library(stm)

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


labelTopics(stm.fit.30)


