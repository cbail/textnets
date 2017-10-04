# textnets
R package to perform automated text analysis using network techniques

# Overview 

There is growing interest in automated detection of latent themes in unstructured text data. Thus far, much attention has been given to topic models, but analyzing texts using the tools of network analysis is an interesting alternative. Though we normally think about network analysis as describing relationships between people, it can also be applied to understand relationships between words-- particularly groups of words that co-occur in documents. This lets us do a number of things- from creating new, more intuitive visualizations of textual themes across documents, but also employ cutting-edge community detection algorithms to identify such themes in a manner that has various advantages over topic modeling which are described in Bail (2017)

Though the idea to think about texts as networks is not entirely new, recent advances in natural language processing and community detection analysis have pushed things forward. This package is an attempt to make these things more widely accessible, and encourage others to build off it to make it even better.

The textnets package includes four types of functions. First, it includes two functions that read in text data and produce "tidy text" data, which is particularly amenable to network analysis. Second, it creates networks from tidy-texts. Third, it provides several functions to identify communities of text and interpret their meaning. Finally, the textnets package includes several functions for visualizing text networks. 

# Getting Started

To begin using the textnets package, you'll need to install the current version of the package from github. You'll also need the devtools package installed

```r
library(devtools)
install_github("cbail/textnets")
library(textnets)

```
# Ingesting Text

The textnet package includes two functions to ingest or "read in" unstructured text. The `prep_text` function creates text networks based upon all types of words, and the `prep_text_noun_phrases` prepares text networks based upon nouns and noun phrases that appear in a given document. Users may prefer to create networks based upon nouns or noun phrases because previous studies have shown that such parts of speech are more useful for mapping the topical content of a text than other parts of speech such as verbs or adjectives (e.g. Rule, Bearman, and Cointet 2015).

Let's begin with the `prep_text` function. This function requires the user to specifcy a dataframe (`textdata`), a variable within this dataset that describes the name of each document (such as the name of the person who produced it)  (`docname`), and a variable that describes the column of the dataframe that contains the text that the user wants to analyze (`textvar`). Finally, the `prep_text` function requires the user to specify whether a network should be created where documents will be the nodes in the network (in which case edges describe overlap in terms between documents), or if words will be the nodes in the network (in which case words are assigned edges to each other based upon their co-appearance within documents). An example of the former application is Bail (2016), and an example of the latter application is Rule, Bearman, and Cointet (2015). At present the `prep_text` function also includes three optional arguments. The `remove_url` function eliminates any hyperlinks within the data. The `remove_stop_words` function eliminates very common English-language words such as "and," the" or "at." The `stem` function reduces each term to its stem form. For example, the term "running" would become the term "run" if the user sets `stem=TRUE`.



