#This prep_text function reads in a dataframe and requires two variable names
# one that describes the document name, and one that describes the text of
# the document. The node_type argument describes whether the user wants to
# create a network where the nodes are words within a document, or the nodes are the
# documents themselves. The function a) removes URLs; b) removes stop words, and c)
# stems words, and d) creates a summarized tidy text format (where each row of the # dataset describes the prevalence of each word within the document)

prep_text <-function(textdata, docname, textvar, node_type=c("docs","words"),
                           remove_url = FALSE, remove_stop_words=FALSE, stem=FALSE) {

  library(dplyr)
  library(tidytext)
  library(stringr)
  library(SnowballC)

  #remove URLS
  if (remove_url) {
    textdata[[textvar]]<-stringr::str_replace_all(textdata[[textvar]], "https?://t\\.co/[A-Za-z\\d]+|https?://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https?", "")
  }


  textdata<-textdata %>%
    select_(docname,textvar) %>%

    # #tidy text
    unnest_tokens_("word", textvar)

  if (remove_stop_words){
    data("stop_words")
    textdata<-textdata %>%
      anti_join(stop_words)
  }

  if (stem){
    textdata<-textdata %>%
      #stem
      mutate_at("word", funs(wordStem((.), language="en")))
  }

  textdata<-textdata %>%
    #now change names to document and term for consistency
    rename_(document=docname)

  if (node_type=="docs"){
    #count terms by document
    textdata<-textdata %>%
      group_by(document) %>%
      count(word, sort = FALSE) %>%
      rename(count=n)
  }

  if (node_type=="words"){
    textdata<-textdata %>%
      group_by(word) %>%
      count(document, sort = FALSE) %>%
      rename(count=n)
  }

  return(textdata)
}
