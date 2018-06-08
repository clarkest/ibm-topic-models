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

