# textnets
R package for automated text analysis using network techniques.

## Overview

There is growing interest in automated detection of latent themes in unstructured text data. Thus far, much attention has been given to topic models, but analyzing texts using the tools of network analysis is an interesting alternative. Though in social science we normally think about network analysis as describing relationships between people, it can also be applied to  relationships between words— for example, by linking words that co-occur in the same document, or that co-occur in documents written by the same author, in the same year, etc. Such an approach allows us to generate new, more intuitive visualizations of textual themes across documents, while also allowing us to employ cutting-edge community detection algorithms to identify themes in a manner that has various advantages over topic modeling, described in detail by Bail (2017) ["Combining Network Analysis and Natural Language Processing to Examine how Advocacy Organizations Stimulate Conversation on Social Media." Proceedings of the National Academy of Sciences. 113:42 11823-11828](http://www.pnas.org/content/113/42/11823.full.pdf?with-ds=yes).

Though the idea to think about texts as networks of words is not entirely new, advances in Natural Language Processing and community detection analysis have pushed things forward. The textnets package is an attempt to make these innovations widely accessible, and to encourage others to innovate further. The functions of the textnets package fall into four main categories: those that prepare texts for analysis, those that produce text networks, those that detect communities within these networks, and those that visualize the networks and their communities.

## Getting Started

To begin using the `textnets` package, you'll need to install the current version of the package from Github. To do so, you'll need the R "devtools" package, if you don't have it already.

```r
install.packages("devtools")
library(devtools)
install_github("cbail/textnets")
library(textnets)
```

You'll also need several other packages for network analysis, text analysis, and visualization (install where necessary).

```r
library(dplyr)
library(tidytext)
library(stringr)
library(SnowballC)
library(phrasemachine)
library(igraph)
library(ggraph)
library(networkD3)
```


### Two-mode networks

It is important to note that the textnet package constructs two-mode networks (aka affiliation, or bipartite networks). Such networks include two sets of nodes (vertices), and edges (links) are only created between nodes belonging to different sets. In textnet, one node set is always comprised of the words found across documents. The other node set is specified by the user and can include the documents themselves, or some meta data about those documents, such as the authors, publishers, dates, etc. These two node sets are extracted from the `textvar` and `groupvar` input parameters, described further below.

To clarify this, let's take the example of a network where the first node set is words found in US newspaper headlines on July 20, 1969 (the first moon landing), and the second node set is the newspapers themselves. Here is a two-mode projection of this network:

<div style="width:500px; height=500px">
![](figures/twomode.png)
</div>

As you can see, edges are only drawn between newspapers and words (i.e. nodes belonging to different sets). Newspapers are connected to a word if that word appears in it's headline.

With some reshaping of the data, this two-mode network can be projected in either of its one-mode forms. That is, with either word nodes connected through the newspapers they share in common, or with newspapers connected through the words they share in common.

<div style="width:700px; height=700px">

![](figures/bothmodes.png)

</div>


### Data format

At present, the textnets package requires text to be inside a dataframe. Specifically, a dataframe where each row represents a document, and the text of each document is contained in a column, with other columns including document meta data. To get a better sense of this, let's take a look at some sample data provided by the sotu package in R. We will be using this data throughout the remained for the tutorial.

```r
install.packages("sotu")
library(sotu)
```

This data provides texts from The State of the Union Address, which is a speech given by the president of the United States each year to describe past accomplishments and future challenges facing the nation. It is a popular dataset in the field of Natural Language Processing because it provides a diverse range of language by different individuals over time.
For our purposes, the data are ideal because they contain text for every State of the Union address, and meta data describing the name of the president who delivered the address, the date of delivery, and the president's party affiliation.

The following code binds the text and meta data objects together to make a single dataframe. On the second line of code, we coerce `sotu$sotu_text` into a character vector, as textnets cannot accept factor variables at the moment.

```r
sotu <- data.frame(cbind(sotu_text, sotu_meta), stringsAsFactors=FALSE)
sotu$sotu_text <- as.character(sotu$sotu_text)
```

Be sure to examine the format of this data, as yours will want to look something similar if you plan to analyze them using the textnets package.






# Prepare Text

The textnet package includes two functions to prepare texts for analysis. The `prep_text` function prepares texts for networks using all types of words, while the `prep_text_noun_phrases` prepares text for networks using only nouns and noun phrases. Users may prefer to create networks based on only nouns or noun phrases because previous studies have shown that such parts of speech are more useful in mapping the topical content of a text than other parts of speech, such as verbs or adjectives (e.g. Rule, Cointet, and Bearman 2015).

Let's begin with the `prep_text` function. This function requires the user to provide three inputs: a dataframe (`mydf`), a column within that dataframe containing the texts that the user would like to to analyze (`textvar`), and a column within that dataframe describing the groups through which the words of those texts will be linked (`groupvar`). `groupvar`, for example, could contain unique document ids, if the user would like to link words co-occurring within documents, or it could be unique author ids, if the user would like to link words co-occurring by authors, etc. In network analysis, we would think of this `groupvar` as the  Finally, the `prep_text` function requires the user to specify whether a network should be created where documents will be the nodes in the network (in which case edges describe overlap in terms between documents), or if words will be the nodes in the network (in which case words are assigned edges to each other based upon their co-appearance within documents). An example of the former application is Bail (2016), and an example of the latter application is Rule, Cointet, and Bearman (2015).

At present the `prep_text` function also includes three optional arguments. The `remove_url` function eliminates any hyperlinks within the text. The `remove_stop_words` function eliminates very common English-language words such as "and," the" or "at." The `stem` function reduces each term to its stem form. For example, the term "running" would become the term "run" if the user sets `stem=TRUE`.


The output of the `prep_text` function is a dataframe in tidytext style, where each row of the dataframe describes a word, the document that it appears in, and its overall frequency within that document. The following code reads in the State of the Union Data in order to create a text network where the nodes are presidents, and the edges are overlap in the language they use. In this example we also remove stop words and stem.

```r
sotu_text_data <- prep_text(sotu, "president", "sotu_text", node_type="docs", remove_stop_words=TRUE, stem=TRUE)
```

The syntax for creating a text network using the `prep_text_noun_phrases` function is very similar to the `prep_text` function, but instead of outputing all words in each document, it only outputs nouns and nounphrases. Nouns are identified using the `phrasemachine` package, which requires a version of Java >7, or a Python backend with the Spacy package. Either way, the `prep_text_noun_phrases` package will take much longer than the `prep_text` function because it must perform part-of-speech tagging on each sentence within each document in the dataframe. But users may conclude that the added time is worth it if they belive nouns and noun phrases are more likely to describe the topical content of a document than other parts of speech.  

The syntax for creating a text network using the `prep_text_noun_phrases` function is very similar to the `prep_text` function, but instead of outputing all words in each document, it only outputs nouns and nounphrases. Nouns are identified using the `phrasemachine` package, which requires a version of Java >7, or a Python backend with the Spacy package. Either way, the `prep_text_noun_phrases` package will take much longer than the `prep_text` function because it must perform part-of-speech tagging on each sentence within each document in the dataframe. But users may conclude that the added time is worth it if they belive nouns and noun phrases are more likely to describe the topical content of a document than other parts of speech.

The syntax for creating a textnetwork using the `prep_text_noun_phrases` function is very similar to the `prep_text` function but instead of outputing all words in each document, it only outputs nouns and nounphrases. This function accomplishes this by using the `phrasemachine` package, which requires a version of Java >7, or a Python backend with the Spacy package. Either way, the `prep_text_noun_phrases` package will take much longer than the `prep_text` function because it must perform part-of-speech tagging on each sentence within each document in the dataframe. But users may conclude that the added time is worth it if they belive nouns and noun phrases are more likely to describe the topical content of a document than other parts of speech. The defauly ngram length for noun phrases is set to 4, but the user may specify a different range using the max_ngram_length argument.

```r
sotu_text_data_nouns <- prep_text_noun_phrases(sotu, "president", "sotu_text", node_type="docs")
```

# Creating Text Networks

The workhorse function within the `textnets` package is the `create_textnet` function. This function reads in an object created using the `prep_text` or `prep_text_noun_phrases` functions and outputs a weighted adjacency matrix, or a square matrix where the rows and columns correspond to either the names of the documents (if the user has specificed the `node_type="docs"` argument in the previous stage), or words (if the user has specified the `node_type="words` argument). The cells of the adjacency matrix are the sum of the term-frequency inverse-document frequency (TFIDF) for overlapping terms between two documents. This is the procedure described in Bail (2016).

```r
sotu_text_network <- create_textnet(sotu_text_data, node_type="docs")
```
# Analyzing Text Networks

In order to group documents according to their similarity-- or in order to identify latent themese across texts-- users may wish to cluster documents or words within text networks. The `text_communities` function applies the Louvain community detection algorithm to do this, which automatically determines the number of clusters within a given network. The function outputs a dataframe with the cluster or "modularity" class to which each document or word has been assigned.

```r
sotu_communities <- text_communities(sotu_text_network)
```
In order to further understand which terms are driving the clustering of documents or words, the user can use the `interpret` function, which also reads in an object created by the `create_textnet` function and outputs the words with the 10 highest term-frequency-inverse-document frequencies within each cluster or modularity class. In order to match words, the function requires that the user specify the name of the text data frame object used to create the text network-- in this case `sotu_text_data` (see above).

```r
top_words_modularity_classes <- interpret(sotu_text_network, sotu_text_data)
```

# Centrality Measures

Often in social networks, researchers wish to calculate measures of influence or centrality in order to predict whether or not occupying brokerage positions can create greater social rewards for individuals. As Bail (2016) shows, the same logic can be applied to text networks in order to develop a measure of "cultural betweenness" or the extent to which a given document or word are in between clusters of other words. To calculate cultural betweennes as well as other centrality measures, `textnet` users can use the `centrality` measure.

```r
text_centrality <- centrality(sotu_text_network)
```

# Visualizing Text Networks

Finally, the textnets package includes two functions to visualize text networks created in the previous steps. The `visualize` function creates a network diagram where nodes are colored by their cluster or modularity class (see previous section). In many cases, text networks will be very dense (that is, there will be a very large number of edges because most documents share at least one word). Visualizing text networks therefore creates inherent challenges, because such dense networks are very cluttered. To make text networks more readable, the `visualize` function requires the user to specify a `prune_cut` argument, which specifies which quantile of edges should be kept for the visualization. For example, if the user sets `prune_cut=.9` only edges that have a weight in the 90th percentile or above will be kept. The `visualize` function also includes an argument that determines which nodes will be labeled, since network visualizations with too many node labels can be difficult to interpret. The user specifies an argument called `label_degree_cut` which specifies the degree, or number of each connections, that nodes which are labeled should have. For example, if the user only wants nodes that have at least 3 connections to other nodes to be labeled (and only wants to visualize edges with a weight that is greater than the 50th percentile), she or he would use the following code:

```r
visualize(sotu_text_network, .50, label_degree_cut=3)
```

![Plot](https://raw.github.com/cbail/textnets/master/SOTU_Plot.png)



The final function in the textnets package is the `visualize_d3js` function. This function outputs an interactive javascript visualization of the text network, where the user can mouse over each node in order to reveal its node label. Once again, nodes are coloured by their modularity class, and the user must sepcify a `prune_cut`argument:

```r
visualize_d3js(sotu_text_network, .50)
```
# References

Bail, Christopher A. 2016. "Combining Network Analysis and Natural Language Processing to Examine how Ad-
vocacy Organizations Stimulate Conversation on Social Media." Proceedings of the National Academy of Sciences, 113:42 11823-11828

Rule, Alix and Jean-Philippe Cointet and Peter Bearman. 2015. "Lexical shifts, substantive changes, and continuity in the State of the Union Discourse, 1790-2014.
