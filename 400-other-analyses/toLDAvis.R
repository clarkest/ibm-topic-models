toLDAvis <- function(mod,docs,R=30,plot.opts=list(xlab ="PC1", ylab = "PC2"),
                   lambda.step=.1,out.dir=tempfile(),open.browser=interactive(),as.gist=FALSE){
  if(!requireNamespace("LDAvis",quietly=TRUE)) 
    stop("Please install LDAvis package to use this function. You will also need servr.")
  
  # elements that are shared by the overall content covariate view and a model with no content covariates
  theta <- mod$theta
  vocab <- mod$vocab
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2,]))))
  term.frequency <- mod$settings$dim$wcounts$x
  
  # if there are no content covariates, vanilla LDAvis will do the job
  if(length(mod$beta$logbeta) == 1) { 
    logbeta <- mod$beta$logbeta[[1]]
    phi <- getSmoothedPhi(logbeta)
    f <- LDAvis::createJSON(phi=phi,theta=theta,doc.length=doc.length,vocab=vocab,term.frequency=term.frequency,lambda.step = lambda.step)
    LDAvis::serVis(f,out.dir=out.dir,open.browser=open.browser,as.gist=as.gist)
  
  } else {
  # if we have a content covariate, then we'll need to 
    # 1. marginalize and create the overall model LDAvis  
    # 2. generate a view for each covariate level
    # 3. embed links to switch between views
    covar.labels <- mod$settings$covariates$yvarlevels
    log.betas <- mod$beta$logbeta
    
    
    marg.log.beta <- marginalLogBeta(mod)
    # marg.phi <- getSmoothedPhi(marg.log.beta)
    marg.phi <- exp(marg.log.beta)
    # note the addition of "sort.topics=F" which will preserve topic numbering in the json
    marginal.vis <- LDAvis::createJSON(phi=marg.phi, 
                                       theta=theta,
                                       doc.length=doc.length,
                                       vocab=vocab,
                                       term.frequency=term.frequency,
                                       lambda.step = lambda.step,
                                       sort.topics=FALSE)
    json.list <- list("OVERALL" = marginal.vis)
    covar.idx <- mod$settings$covariates$betaindex
    for (i in 1:length(log.betas)) {
      this.idx <- covar.idx == i
      theta <- mod$theta[this.idx,]
      # theta[!this.idx, ] <- 0
      doc.subset <- docs[this.idx]
      doc.length <- as.integer(unlist(lapply(doc.subset, function(x) sum(x[2,]))))
      # doc.length[!this.idx] <- 0
      terms <- data.frame(idx = as.character(1:length(vocab)), stringsAsFactors = F)
      
      tf.table <- data.frame(table(unlist(doc.subset)), stringsAsFactors = F)
      tt <- left_join(terms, tf.table, by=c("idx"="Var1")) %>% 
        dplyr::mutate(Freq = ifelse(is.na(Freq), 0 , Freq))
      term.frequency <- tt$Freq
      
      #phi <- getSmoothedPhi(log.betas[[i]])
      phi <- exp(log.betas[[i]])
      this.vis <- LDAvis::createJSON(phi=phi, 
                            theta=theta,
                            doc.length=doc.length,
                            vocab=vocab,
                            term.frequency=term.frequency,
                            lambda.step = lambda.step,
                            sort.topics=FALSE)
      
      json.list[[covar.labels[i]]] <- this.vis
      
    }
    serVis(json.list, out.dir=out.dir,
           open.browser=open.browser,as.gist=as.gist)
    
  } 
  
  
}

# Marginalize the logbetas of 'model' -- shamelessly lifted from sageLabels.R
marginalLogBeta <- function(model) {
  logbeta <- model$beta$logbeta
  margbeta <- exp(logbeta[[1]])
  if(length(logbeta) > 1) {
    weights <- model$settings$covariates$betaindex
    tab <- table(weights)
    weights <- tab/sum(tab)
    #marginalize
    margbeta <- margbeta*weights[1]
    for(i in 2:length(model$beta$logbeta)) {
      margbeta <- margbeta + exp(model$beta$logbeta[[i]])*weights[i]
    }
  }
  margbeta <- log(margbeta)
  return(margbeta)
}

getSmoothedPhi <- function(logbeta) {
  phi <- exp(logbeta)
  if(any(phi==0)){
    phi<-phi + .Machine$double.eps
    phi<-phi/rowSums(phi)
  }
  return(phi)
}
  
mod <- stm.fit.30
str(mod$beta$logbeta)
mod$settings$dim$A
table(mod$settings$covariates$betaindex)

# TODO confirm with Brandon re: location of utility functions 



# extension of LDAvis::servis that can also take a named list of json objects as an input and create a cross-linked set of pages
serVis <- function(json.list, out.dir = tempfile(), open.browser = interactive(), 
                   as.gist = FALSE, ...) {
  dir.create(out.dir)
  src.dir <- system.file("htmljs", package = "LDAvis")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  
  if (length(json.list)==1) {
    json <- json.list[[1]]
    ## Copy html/js/css files to out.dir
    file.copy(to.copy, out.dir, overwrite = TRUE, recursive = TRUE)
    
    ## Write json to out.dir
    cat(json, file = file.path(out.dir, "lda.json"))
  
  } else {
    # only directly copy the .css and the d3.  we need to edit the .js and the .html
    file.copy(to.copy[c(1,3)], out.dir, overwrite = TRUE, recursive = TRUE)
    
    default.page <- sprintf("covar-%s.html", names(json.list)[1])
    index.html <- sprintf('<html>
      <head>
      <meta http-equiv="refresh" content="0; URL=%s">
        <meta name="keywords" content="automatic redirection">
          </head>
          <body>
          If your browser does not automatically go there within a few seconds, you may want to go to <a href="%s">Load LDAvis</a> manually.
          </body>
      </html>', default.page, default.page)
    cat(index.html, file = file.path(out.dir, "index.html"))
    
    # grab the defaul html and js files for editing
    base.js <- readChar(to.copy[4], file.info(to.copy[4])$size)
    base.html <- readChar(to.copy[2], file.info(to.copy[2])$size)
    
    # make a first pass through the names to put together the links header for the js
    added.text <- "topicDiv.appendChild(clear);\n
      \t\t// post-hoc addition of link to document distance layout
        \t\ttopicDiv.appendChild(document.createElement('br'));"
    for (name in names(json.list)) {
      
      added.text <- paste0(added.text, sprintf("
        \t\tvar a = document.createElement('a');
        \t\tvar linkText = document.createTextNode(' |%s| ');
        \t\ta.appendChild(linkText);
        \t\ta.href = 'covar-%s.html';
        \t\ttopicDiv.appendChild(a);", name, name)
      )

    }
    
    # then iterate through the names again to create the js, json, and html for each covar
    for (name in names(json.list)) {
      js.name <- sprintf("covar-%s.js", name)
      html.name <- sprintf("covar-%s.html", name)
      json.name <- sprintf("covar-%s.json", name)
    
      js.out <- gsub("topicDiv\\.appendChild\\(clear\\);", added.text, base.js)
      js.out <- gsub("Intertopic Distance", sprintf("%s Intertopic Distance", name), js.out)
      
      old.title <- "<title>LDAvis</title>"
      new.title <- sprintf("<title>%s</title>", name)
      html.out <- gsub(old.title, new.title, base.html)
      html.out <- gsub("ldavis.js", js.name, html.out)
      html.out <- gsub("lda.json", json.name, html.out)
      
      cat(json.list[[name]], file = file.path(out.dir, json.name))
      cat(js.out, file=file.path(out.dir, js.name))
      cat(html.out, file=file.path(out.dir, html.name))
      
    }
  }
  
  ## Try to upload gist
  if (as.gist) {
    gistd <- requireNamespace('gistr')
    if (!gistd) {
      warning("Please run `devtools::install_github('rOpenSci/gistr')` 
              to upload files to https://gist.github.com")
    } else {
      gist <- gistr::gist_create(file.path(out.dir, list.files(out.dir)), ...)
      if (interactive()) gist
      url_name <- paste("http://bl.ocks.org", gist$id, sep = "/")
      if (open.browser) utils::browseURL(url_name)
    }
    return(invisible())
  }
  
  servd <- requireNamespace('servr')
  if (open.browser) {
    if (!servd) {
      message("If the visualization doesn't render, install the servr package\n",
              "and re-run serVis: \n install.packages('servr') \n",
              "Alternatively, you could configure your default browser to allow\n", 
              "access to local files as some browsers block this by default") 
      utils::browseURL(sprintf("%s/index.html", out.dir))
    } else {
      servr::httd(dir = out.dir)
    }
  }
  return(invisible())
}




