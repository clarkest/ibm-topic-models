library(dplyr)
library(ggplot2)

# combine a doc topics with a set of threaded docs
combineConvDT <- function(threaded.docs, doc.topics.unnormal, 
                          grouping=c("jam", "root.id")){
  thread.size <- threaded.docs %>% 
    group_by_(.dots=grouping) %>%
    summarize(n=n())
  
  dtu <- data.frame(doc.topics.unnormal)
  threaded.docs %>% select_(.dots=grouping) %>% 
    cbind(dtu) %>% 
    group_by_(.dots=grouping) %>% 
    summarize_each(funs(sum)) %>%
    left_join(thread.size) 
}


# for a given arrangement of doc.topics, return the list of topics above the threshold
topicLists <- function(doc.top, prev.thresh, prev.format=" %.2f") {
  topic.list <- rep("", nrow(doc.top))
  for (i in 1:nrow(doc.top)) {
    topic.p <- doc.top[i,]
    these.topics <- which(topic.p > prev.thresh)
    df <- data.frame(tp = these.topics, prev=topic.p[these.topics]) %>%
      arrange(desc(prev)) %>%
      # use sprintf to get everything to line up nice in the html
      mutate(val = paste0(sprintf("[%02d]", tp), sprintf(prev.format, prev)))
    topic.list[i] <- paste(df$val, collapse=", ")
  }
  return(topic.list)
}
