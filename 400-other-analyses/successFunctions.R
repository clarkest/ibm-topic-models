cutQuoteModel <- function(prev.thresh=NULL, controls, topic.interaction="", 
                          interaction.terms = c(), filter.set=NULL, do.hurdles=T,
                          remove.topics=NULL) {
  if (is.null(prev.thresh)) {
    thread.dt.cuts <- select_(analysis.set, .dots=c("quoted", controls))
    #thread.dt.cuts <- select_(analysis.set, .dots=c( "responded", controls))
  } else {
    d.tps <- ifelse(model$theta >= prev.thresh, 1, 0)
    colnames(d.tps) <- paste0("t", 1:ncol(d.tps))
    if (!is.null(remove.topics)) {
      d.tps <- d.tps[,-remove.topics]
    }
    thread.dt.cuts <- 
      cbind(select_(analysis.set, .dots=c( "quoted", controls)),
            #cbind(select_(analysis.set, .dots=c( "responded", controls)),
            d.tps)
  }
  if (topic.interaction != "") {
    d.tps.inter <- analysis.set[,topic.interaction] * d.tps
    colnames(d.tps.inter) <- paste0(colnames(d.tps),"-",topic.interaction)
    thread.dt.cuts <- cbind(thread.dt.cuts, d.tps.inter)
  }
  
  if (!is.null(filter.set)) {
    thread.dt.cuts <- thread.dt.cuts[filter.set,]
  }
  interaction.terms <-  c() 
  formula.text <- paste(c("quoted ~ . ", interaction.terms), collapse = " + ")
  #formula.text <- paste(c("responded ~ . ", interaction.terms), collapse = " + ")
  formula <- as.formula(formula.text)
  
  fit.cut <- glm(formula,
                 data=thread.dt.cuts,
                 family="binomial")
  return(fit.cut)
}  

starMe <- function(...) {
  # models, column.labels="", out=""
  stargazer(...,
            #type='text', 
            report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
            star.cutoffs=c(0.05, 0.01, 0.001))
}

outFile <- function(name) {sprintf("outputs/dists/%s.png", name)}


# Similarity/Distance functions
jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
KLD <- function(x,y) sum(x * log(x/y))
symKLD <- function(x,y) (0.5 * KLD(x, y)) + (0.5 * KLD(y, x))
cosSimil <- function(x,y) sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))


forumLabels <- function(forum.vec, forums.with.kickoff=NULL) {
  forum.name <- rep("", length(forum.vec))
  forum.name[forum.vec=="forum 1"] <- "For Clients"
  forum.name[forum.vec=="forum 2"] <- "Delivery"
  forum.name[forum.vec=="forum 3"] <- "For World"
  forum.name[forum.vec=="forum 4"] <- "For IBM"
  forum.name[forum.vec=="forum 5"] <- "Managers"
  forum.name[forum.vec=="forum 6"] <- "Every IBMer"
  forum.kickoff <- rep("", length(forum.vec))
  if (!is.null(forums.with.kickoff)){
    forum.kickoff[forum.vec %in% forums.with.kickoff] <- "[Kicked off]"  
  }
  ret.val <- paste(forum.vec, forum.kickoff, forum.name, sep="\n")
}



topicPrevForum <- function(this.df, var.or.tpc, title="", forum.set.to.add=NULL, 
                           thresh=0, forums.with.kickoff=NULL, always.ci=F) {
  subtitle <- "Point estimate is the mean %s by forum for selected (blue) 
  and non-selected (red) comments.  Error bars represent the 25th and 
  75th percentile value for selected and non-selected comments."
  if (is.numeric(var.or.tpc)) {
    t.name <- sprintf("t%02s",var.or.tpc)
    this.df$focal.var <- pull(this.df, t.name) 
    this.df <- select(this.df, focal.var, forum, quoted)
    title <- sprintf("Topic %d: %s", var.or.tpc, tpc.labels[var.or.tpc])
    ylabel <- "Topic Prevalence"
    if (!is.null(forum.set.to.add)) {
      forum.set.to.add$focal.var <- pull(forum.set.to.add, t.name)  
      forum.set.to.add$quoted <- 2
      this.df <- select(forum.set.to.add, forum, focal.var, quoted) %>%
        rbind(this.df)
      subtitle <- "Point estimate is the mean %s by forum for selected (green), 
      non-selected (red), and Kickoff (blue) comments.  Error bars represent the 25th and 
      75th percentile value for selected and non-selected comments."
      
    }
  } else {
    this.df$focal.var <- pull(this.df, var.or.tpc)
    if (title == "") {
      title <- var.or.tpc
    } 
    ylabel <- title
  }
  
  this.df$forum.lbl <- forumLabels(this.df$forum, forums.with.kickoff)
  
  pd <- position_dodge(0.5)
  
  # repeat all the data with an "overall" forum label
  this.df <- this.df %>% mutate(forum.lbl="Jam Overall") %>%
    rbind(this.df)
  
  # for truly binary variables, we can show the confidence intervals
  if (length(unique(this.df$focal.var))==2) {
    subtitle <- "Point estimate is the mean %s by forum for selected (blue) 
    and non-selected (red) comments.  Error bars are the 95-percent confidence interval of the proportion."
    plot.df <- this.df %>%
      group_by(forum.lbl, quoted) %>%
      summarise(mn.prev = mean(focal.var),
                q25 = mn.prev - 1.96*sqrt(mn.prev*(1-mn.prev)/n()),
                q75 = mn.prev + 1.96*sqrt(mn.prev*(1-mn.prev)/n()))
    if (is.numeric(var.or.tpc)) {ylabel <- sprintf("Posts with Topic over %0.2f", thresh)}
  } else if (always.ci) {
    subtitle <- "Point estimate is the mean %s by forum for selected (blue) 
    and non-selected (red) comments.  Error bars are the 95-percent confidence interval of the proportion."
    plot.df <- this.df %>%
      group_by(forum.lbl, quoted) %>%
      summarise(mn.prev = mean(focal.var),
                q25 = mn.prev - 1.96*sd(focal.var)/sqrt(n()-1),
                q75 = mn.prev + 1.96*sd(focal.var)/sqrt(n()-1))
  } else {
    plot.df <- this.df %>%
      group_by(forum.lbl, quoted) %>%
      summarise(mn.prev = mean(focal.var),
                q25 = quantile(focal.var, probs=0.25),
                q75 = quantile(focal.var, probs=0.75))
  }
  
  plot.df %>%
    ggplot() +
    geom_point(aes(x=forum.lbl, y=mn.prev, color=as.factor(quoted)), position=pd) +
    geom_errorbar(aes(x=forum.lbl, ymin=q25, ymax=q75, color=as.factor(quoted)), position=pd) +
    labs(title=title,
         subtitle=sprintf(subtitle, ylabel),
         colour = "Selected Comments") +
    theme(legend.position="bottom") +
    ylab(ylabel) +
    xlab("Forum") +
    scale_color_hue(labels = c("Not Selected", "Selected", "Kickoff"))
  }





