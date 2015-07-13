#install.packages("LDAvis")
#install.packages("servr")

library(LDAvis)
# craeteJSON input objects from topic model outputs
# "phi"=topic.words, 
# "theta"=doc.topics, 
# "doc.length"=documents.lengths, 
# "vocab"=as.character(vocab$words), 
# "term.frequency"=as.integer(vocab$term.freq)
  
create.ldavis <- function (new.topic.model, model.dir, model.name) {  
  # from the trained topic.model, create the lists needed by LDAvis
  new.topic.words <- mallet.topic.words(new.topic.model, smoothed=T, normalized=T)
  new.doc.topics <- mallet.doc.topics(new.topic.model, smoothed=T, normalized=T)
  unnormal.doc.topics <- mallet.doc.topics(new.topic.model, smoothed=F, normalized=F)
  doc.len <- rowSums(unnormal.doc.topics)
  vocab <- mallet.word.freqs(new.topic.model)
  term.freq <- as.integer(vocab$term.freq)
  term.freq[term.freq == 0] <- 1
  
  out.dir = paste(model.dir, "LDAvis", model.name, sep="/")
  ldavis.json <- createJSON(new.topic.words, new.doc.topics, doc.len, as.character(vocab$words), term.freq, R=50)
  serVis(ldavis.json, out.dir = out.dir, open.browser = FALSE)
  
  # createJSON automatically orders the topics by prevelence,
  # we need to fix the js file so that it labels the topic circles with the original numbers
  # forunately the json includes "topic.order," an array showing the order of the original topic labels
  js_file = paste(out.dir, "ldavis.js", sep="/")
  js <- readChar(js_file, file.info(js_file)$size)
  js.out <- gsub("return d.topics", "return data['topic.order'][d.topics - 1]",js)
  fileConn<-file(js_file)
  writeLines(js.out, fileConn)
  close(fileConn)
  
  # also dump the state to disk so that we can analyze this model later
  new.topic.model$printState(.jnew("java.io.File", paste(model.dir, paste0(model.label, ".gz"), sep="/")))
}




  