library("NLP")
library("openNLP")
word_token_annotator <- Maxent_Word_Token_Annotator()
PTA <- Maxent_POS_Tag_Annotator()

getPOSTags <- function(s) {
  annotationSeed <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, annotationSeed)
  a3 <- annotate(s, PTA, a2)
  a3vector <- NULL
  for (i in 2:length(a3)) {
    a3vector <- c(a3vector, a3$features[[i]][[1]])
  }
  
  a3vector
}


splitSentence <- function(s) {
  s.pos <- getPOSTags(s)
  s.vec <- strsplit(s, " ")[[1]]
  if("CC" %in% s.pos) {
    index = which(s.pos=="CC")
    if(s.pos[index+1] == "PRP" || s.pos[index+1] == "PRP$") {
      s.vec <- c(s.vec[1:(index-1)], ".", s.vec[index:length(s.vec)]) 
    }
  }
  paste(s.vec, sep=" ", collapse=" ")
}
