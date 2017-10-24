prep_text_noun_phrases <-function(textdata, groupvar, textvar, node_type=c("groups","words"), max_ngram_length = 4,
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
    select_(groupvar,textvar)%>%
    #remove all URLS
    mutate_at(textvar, funs(str_replace_all(., "https?://t\\.co/[A-Za-z\\d]+|https?://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https?", ""))) %>%
    #tidy text
    mutate_at(textvar, funs(phrasemachine(., maximum_ngram_length = max_ngram_length))) %>%
    group_by_(groupvar) %>%
    mutate_at(textvar, funs(paste0(unlist(.),collapse=" "))) %>%
    unnest_tokens_("word", textvar)  %>%
    #  #now change names to document and term for consistency
    ungroup


  if (node_type=="groups"){
    #count terms by document
    textdata<-textdata %>%
      rename_(group=groupvar)  %>%
      # # # #count terms by document
      group_by(group) %>%
      count(word, sort = FALSE) %>%
      rename(count=n)
    return(textdata)

  }
  if (node_type=="words"){
    textdata<-textdata %>%
      rename_(group=groupvar)  %>%
      # # # #count terms by group
      group_by(group) %>%
      count(word, sort = FALSE) %>%
      rename(count=n)
    return(textdata)
  }

  return(textdata)
}
