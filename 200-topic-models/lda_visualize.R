#install.packages("LDAvis")
#install.packages("servr")
library(LDAvis)
# but use our edited createJSON function
source("library/lda_vis_documents.R")
source("300-post-model-analyses/mallet_analyses.R")

create.ldavis <- function (new.topic.model, 
                           model.dir, 
                           model.name, 
                           cooccurenceThreshold=FALSE, 
                           cooccurenceMinTokens=FALSE) {  
  # from the trained topic.model, create the lists needed by LDAvis
  new.topic.words <- mallet.topic.words(new.topic.model, smoothed=T, normalized=T)
  new.doc.topics <- mallet.doc.topics(new.topic.model, smoothed=T, normalized=T)
  unnormal.doc.topics <- mallet.doc.topics(new.topic.model, smoothed=F, normalized=F)
  doc.len <- rowSums(unnormal.doc.topics)
  vocab <- mallet.word.freqs(new.topic.model)
  term.freq <- as.integer(vocab$term.freq)
  term.freq[term.freq == 0] <- 1

  
  out.dir <- paste(model.dir, "LDAvis", model.name, sep="/")
  ldavis.json <- createJSON(new.topic.words, new.doc.topics, doc.len, as.character(vocab$words), 
                            term.freq, R=50, sort.topics=FALSE)
  serVis(ldavis.json, out.dir = out.dir, open.browser = FALSE)
  # if we've supplied a threshold, output the doc distance matrix as well
  if (cooccurenceThreshold) {
    if (!cooccurenceMinTokens) {cooccurenceMinTokens = 40 * cooccurenceThreshold}
    corr.obj <- topic.co.occur(new.topic.model, cooccurenceMinTokens, cooccurenceThreshold)
    docdist.out.dir = paste(out.dir, "doc_distance", sep="/")
    docdist.ldavis.json <- createJSON(new.topic.words, new.doc.topics, doc.len, as.character(vocab$words), 
                              term.freq, R=50, sort.topics=FALSE,
                              other.dist=corr.obj$dist)
    serVis(docdist.ldavis.json, out.dir = docdist.out.dir, open.browser = FALSE)
    
    # edit the word-dist js to include a link to the doc-dist 
    added.text <- "topicDiv.appendChild(clear);\n
      \t\t// post-hoc addition of link to document distance layout
      \t\ttopicDiv.appendChild(document.createElement('br'));
      \t\tvar a = document.createElement('a');
      \t\tvar linkText = document.createTextNode('%s');
      \t\ta.appendChild(linkText);
      \t\ta.href = '%s';
      \t\ttopicDiv.appendChild(a);"
    
    word.dist.text <- sprintf(added.text, "Word Distance MDS (click to see document-distance)", "doc_distance/index.html")
    docs.dist.text <- sprintf(added.text, "Document Distances MDS (click to see word-distances)", "../index.html")
    
    js_file = paste(out.dir, "ldavis.js", sep="/")
    js <- readChar(js_file, file.info(js_file)$size)
    js.out <- gsub("topicDiv\\.appendChild\\(clear\\);", word.dist.text, js)
    fileConn<-file(js_file)
    writeLines(js.out, fileConn)
    close(fileConn)
    
    js_file = paste(docdist.out.dir, "ldavis.js", sep="/")
    js <- readChar(js_file, file.info(js_file)$size)
    js.out <- gsub("topicDiv\\.appendChild\\(clear\\);", docs.dist.text, js)
    fileConn<-file(js_file)
    writeLines(js.out, fileConn)
    close(fileConn)
  } 
}


# sort order has been dealt with by adding a no-sort flag to the package.  
# this is the deprecated code for editing the js in place to remove topic sorting
# createJSON automatically orders the topics by prevelence,
# we need to fix the js file so that it labels the topic circles with the original numbers
# forunately the json includes "topic.order," an array showing the order of the original topic labels
# js_file = paste(out.dir, "ldavis.js", sep="/")
# js <- readChar(js_file, file.info(js_file)$size)
# js.out <- gsub("return d.topics", "return data['topic.order'][d.topics - 1]",js)
# fileConn<-file(js_file)
# writeLines(js.out, fileConn)
# close(fileConn)


  