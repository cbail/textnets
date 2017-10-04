prep_text_noun_phrases <-function(textdata, docname, textvar, node_type=c("docs","words"),
                                        remove_url=TRUE) {

  library(dplyr)
  library(tidytext)
  library(stringr)
  library(SnowballC)
  library(phrasemachine)

  #remove URLS
  if (remove_url) {
    textdata[[textvar]]<-stringr::str_replace_all(textdata[[textvar]], "https?://t\\.co/[A-Za-z\\d]+|https?://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https?", "")
  }

  textdata<-textdata %>%
    select_(docname,textvar)%>%
    #remove all URLS
    mutate_at(textvar, funs(str_replace_all(., "https?://t\\.co/[A-Za-z\\d]+|https?://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https?", ""))) %>%
    #tidy text
    mutate_at(textvar, funs(phrasemachine((.)))) %>%
    group_by_(docname) %>%
    mutate_at(textvar, funs(paste0(unlist(.),collapse=" "))) %>%
    unnest_tokens_("word", textvar)  %>%
    #  #now change names to document and term for consistency
    ungroup


  if (node_type=="docs"){
    #count terms by document
    textdata<-textdata %>%
      rename_(document=docname)  %>%
      # # # #count terms by document
      group_by(document) %>%
      count(word, sort = FALSE) %>%
      rename(count=n)
    return(textdata)

  }
  if (node_type=="words"){
    textdata<-textdata %>%
      rename_(document=docname)  %>%
      # # # #count terms by document
      group_by(document) %>%
      count(word, sort = FALSE) %>%
      rename(count=n)
    return(textdata)
  }

  return(textdata)
}
