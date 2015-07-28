library(mallet)

greek.text <- read.table("unicode/el.txt", encoding="UTF-8", sep="\t", col.names=c("id", "text"), colClasses=c("character", "character"))

greek.instances <- mallet.import(greek.text$id, 
                                  greek.text$text, 
                                  "200-topic-models/en.txt", 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]"
)

greek.model <- MalletLDA()
greek.model$loadDocuments(greek.instances)

mallet.word.freqs(greek.model)