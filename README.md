# textnets
R package for automated text analysis using network techniques. 

### Citation/Background Paper
[Bail, Christopher A. 2016. "Combining Network Analysis and Natural Language Processing to Examine how Advocacy Organizations Stimulate Conversation on Social Media." Proceedings of the National Academy of Sciences, 113:42 11823-11828](http://www.pnas.org/content/113/42/11823.full.pdf?with-ds=yes)

### Overview

There is growing interest in automated detection of latent themes in unstructured text data. Though topic models have become a popular choice for such tasks, the textnets package provides an alternative technique that synthesizes recent advances in network analysis/graph theory and natural language processing which has several significant advantages over conventional "bag of words" models.

Though network analysis is often used to describe relationships between people-- particularly within the social sciences-- it can also be applied to relationships between words. For example, network ties can be created by the co-occurence of individual words across documents, or ties can be created between documents themselves using a two-mode network projection. 

The advantage of a network-based approach to automated text analysis are a) like social groups, the meaning of groups of words can be more accurately measured through triadic closure-- or the principle that the meaning of any two words or terms to each other can be more accurately understood if they are placed in the context of a third word; b) text networks can be applied to documents of any length unlike topic models which generally require a significant amount of words to function well. This is a significant advantage in an age where short social media texts are becoming pervasive. Finally, c) this approach benefits from recent advances in the interdisciplinary literature on community detection, which arguably provides more accurate ways of grouping words that benefit from clustering observed within networks as opposed to bag of words models. These advantages are further described in the articles referenced below. 

Though the idea to think about texts as networks of words is not entirely new, advances in Natural Language Processing and community detection analysis have pushed things forward. The textnets package is an attempt to make these innovations widely accessible, and to encourage others to innovate further. The textnets package provides the following functions: 

* preparing texts 
* creating text networks
* detecting themes within text networks
* visualizing text networks

## Getting Started

The most current version of the textnets package is currently available on Github. Therefore to install textnets, the devtools package must also be installed.

```r
install.packages("devtools")
library(devtools)
install_github("cbail/textnets")
library(textnets)
```

You'll also need several other packages for network analysis, text analysis, and visualization (install where necessary).

```r
library(dplyr)
library(Matrix)
library(tidytext)
library(stringr)
library(SnowballC)
library(reshape2)
library(phrasemachine)
library(igraph)
library(ggraph)
library(networkD3)
```


### Two-mode networks

Before we move on to a working example, it's important to note that the textnet package constructs two-mode networks out of texts, also known as affiliation or bipartite networks. Such networks include two sets of nodes, with edges drawn only between nodes of *different* sets. To clarify this concept, let's take the example of a network where the first node set is words found in US newspaper headlines on the day of the first moon landing (July 20, 1969), and the second node set is the newspapers themselves. The data would look something like this:

<div style="width:450px; height=450px">
![](https://raw.github.com/cbail/textnets/master/figures/moonlanding_headlines.png)
</div>

</br>

Here is a two-mode projection of this network. As you can see, edges are only drawn between newspapers and words (i.e. nodes belonging to different sets).



<div style="width:500px; height=500px">
![](https://raw.github.com/cbail/textnets/master/figures/twomode.png)
</div>

With some reshaping of the data, this two-mode network can be projected in either of its one-mode forms. That is, with either words connected through the newspapers they share in common, or with newspapers connected through the words they share in common. Importantly, these two projections represent the `node_type="words"` and `node_type="groups"` parameter settings respectively, which are specified in the `PrepText` function described further below.

<div style="width:700px; height=700px">

![](https://raw.github.com/cbail/textnets/master/figures/bothmodes.png)

When you're using textnets, one node set will always be comprised of the words found in the documents  analyzed; the other node set can be the documents themselves, or some meta data about those documents, such as the authors, publishers, dates, etc.
</div>


### Format of Input Data

At present, the textnets package requires input to be formatted as a dataframe, where each row represents a document. The text of each document is contained in a column, with other columns including meta data. To get a better sense of this, let's take a look at some sample data provided by the sotu package in R. We will be using this data throughout the remainder of the tutorial.

```r
install.packages("sotu")
library(sotu)
```

The sotu data includes texts from The State of the Union Address, which is a speech given by the president of the United States each year to describe past accomplishments and future challenges facing the nation. It is a popular dataset in the field of Natural Language Processing because it provides a diverse range of language by different individuals over time. For our purposes, the data are ideal because they contain the transcript text for every State of the Union address, as well as meta data describing the president who delivered the address, the date of delivery, and the president's party affiliation.

The following code binds the sotu text and meta data objects together to make a single dataframe, as required for textnets analysis. On the second line of code, we make sure that the texts column of this variable `sotu$sotu_text` is a character vector.

```r
sotu <- data.frame(cbind(sotu_text, sotu_meta), stringsAsFactors=FALSE)
sotu$sotu_text <- as.character(sotu$sotu_text)
```

Here is what the data look like. Yours should look similar if you plan to analyze them using the textnets package.

</br></br>


## Prepare Text

The textnet package includes two functions to prepare texts for analysis. You will choose one or the other for your analysis. The `PrepText` function prepares texts for networks using all types of words, while the `PrepTextNounPhrases` prepares text for networks using only nouns and noun phrases. Users may prefer to create networks based on only nouns or noun phrases because previous studies have shown that such parts of speech are more useful in mapping the topical content of a text than other parts of speech, such as verbs or adjectives (e.g. Rule, Cointet, and Bearman 2015).

Let's begin with the `PrepText` function. This function requires the user to provide three inputs: a dataframe (`mydf`), a column within that dataframe containing the texts that the user would like to analyze (`textvar`), and a column within that dataframe describing the groups through which the words of those texts will be linked (`groupvar`). For example, the latter could contain unique document ids, if the user would like to link words co-occurring within documents, or it could be author ids, if the user would like to link words co-occurring by authors, etc. In network analysis terminology, the `textvar` and the `groupvar` are specifying the nodes sets of a two-mode network. 

Finally, the `PrepText` function requires the user to specify which projection of the two-mode network should be created: one in which words will be the nodes (with edges to each other based on co-appearance in the same group), or one in which groups will be the nodes (with edges based on overlap in words between the groups). An example of the former application is Rule, Cointet, and Bearman (2015), and an example of the latter application is Bail (2016). At present the `prep_text` function also includes three optional arguments. The `remove_url` function eliminates any hyperlinks within the provided texts. The `remove_stop_words` function eliminates very common English-language words such as "and", "the", or "at." The `stem` function reduces each term to its stem form. For example, the term "running" would become the term "run" if the user sets `stem=TRUE`. The `remove_numbers` parameter, if set to TRUE, will remove numbers that are unattached to letters (i.e. it will remove "60" and "1960" but not "60s" or "1960s", likewise "2" but not "2nd", etc).

The output of the `PrepText` function is a dataframe in "tidytext" style, where each row of the dataframe describes a word, the document that it appears in, and its overall frequency within that document. 

The following code prepares the State of the Union data, specifying that nodes will be the group of presidents, with edges draw according to the overlap of words used in their speeches. In this example we also remove stop words and stem.

```r
sotu_text_data <- PrepText(sotu, textvar="sotu_text", groupvar="president", node_type="groups", remove_stop_words=TRUE, stem=TRUE)
```

The syntax for using the `prep_text_noun_phrases` function is the same as the `PrepText` function, but instead of outputing all words in each document, it outputs nouns and nounphrases. Nouns are identified using the `phrasemachine` package, which requires a version of Java >7, or a Python backend with the Spacy package. Either way, the `PrepTextNounPhrases` package will take much longer than the `PrepText` function because it must perform part-of-speech tagging on each sentence within each document in the provided dataframe. The amount of time will depend on both the number and the length of texts. But users may conclude that the added time is worth it if they belive nouns and noun phrases are more likely to describe the topical content of a document than other parts of speech. 

The default ngram length for noun phrases is set to 4, but the user may specify a different range using the `max_ngram_length` argument. This may be of particular importance to scholars interested in organizations such as universities or government agencies, as many are likely to have formal titles comprised of than 4 words. Additionally, while all nouns are included in the output, only the top 1,000 noun phrases are included, to prevent an over detection of nested terms (e.g. 'the_President', 'the_President_of_the_United', 'the_President_of_the_United_States', 'the_President_of_the_United_States_of_America'). If the user wishes to extract all noun phrases instead of just the top 1,000, the `top_phrases` parameter should be set to FALSE.

```r
sotu_text_data_nouns <- PrepTextNounPhrases(sotu, "president", "sotu_text", node_type="groups", top_phrases=TRUE)
```

</br></br>

## Creating Text Networks

The workhorse function within the textnets package is the `create_textnet` function. This function reads in an object created using the `PrepText` or `PrepTextNounPhrases` functions and outputs a weighted adjacency matrix, or a square matrix where the rows and columns correspond to either the groups of the group variable (if the user specificed `node_type="group"` in the previous stage), or words (if the user specified `node_type="words"`). The cells of the adjacency matrix are the sum of the term-frequency inverse-document frequency (TFIDF) for overlapping terms between two documents. This is the procedure described in Bail (2016).

```r
sotu_text_network <- CreateTextnet(sotu_text_data, node_type="groups")
```

</br></br>

## Analyzing Text Networks

In order to group documents according to their similarity-- or in order to identify latent themes across texts-- users may wish to cluster documents or words within text networks. The `TextCommunities` function applies the Louvain community detection algorithm to do this, which automatically uses the edge weights and determines the number of clusters within a given network. The function outputs a dataframe with the cluster or "modularity" class to which each document or word has been assigned.

```r
sotu_communities <- TextCommunities(sotu_text_network)
```
In order to further understand which terms are driving the clustering of documents or words, the user can use the `InterpretText` function, which also reads in an object created by the `CreateTextnet` function and outputs the words with the 10 highest TFIDF frequencies within each cluster or modularity class. In order to match words, the function requires that the user specify the name of the text data frame object used to create the text network-- in this case `sotu_text_data` (see above).

```r
top_words_modularity_classes <- InterpretText(sotu_text_network, sotu_text_data)
```

</br></br>

## Centrality Measures

Often in social networks, researchers wish to calculate measures of influence or centrality in order to predict whether or not occupying brokerage positions can create greater social rewards for individuals. As Bail (2016) shows, the same logic can be applied to text networks to develop a measure of "cultural betweenness" or the extent to which a given document or word is between clusters. To calculate cultural betweennes as well as other centrality measures, `textnet` users can use the `TextCentrality` function.

```r
text_centrality <- TextCentrality(sotu_text_network)
```

</br></br>


## Visualizing Text Networks

Finally, the textnets package includes two functions to visualize text networks created in the previous steps. The `VisualizeText` function creates a network diagram where nodes are colored by their cluster or modularity class (see previous section). In many cases, text networks will be very dense (that is, there will be a very large number of edges because most documents share at least one word). Visualizing text networks therefore creates inherent challenges, because such dense networks are very cluttered. To make text networks more readable, the `visualize` function requires the user to specify a `prune_cut` argument, which specifies which quantile of edges should be kept for the visualization. For example, if the user sets `prune_cut=.9` only edges that have a weight in the 90th percentile or above will be kept. 
The `VisualizeText` function also includes an argument that determines which nodes will be labeled, since network visualizations with too many node labels can be difficult to interpret. The user specifies an argument called `label_degree_cut` which specifies the degree, or number of each connections, that nodes which are labeled should have. For example, if the user only wants nodes that have at least 3 connections to other nodes to be labeled (and only wants to visualize edges with a weight that is greater than the 50th percentile), she or he would use the following code:

```r
visualize(sotu_text_network, .50, label_degree_cut=3)
```

![Plot](https://raw.github.com/cbail/textnets/master/figures/SOTU_Plot.png)


The final function in the textnets package is the `VisualizeTextD3js` function. This function outputs an interactive javascript visualization of the text network, where the user can mouse over each node in order to reveal its node label. Once again, nodes are coloured by their modularity class, and the user must sepcify a `prune_cut`argument:

```r
VisualizeTextD3js(sotu_text_network, .50)
```

To save this as an html file for sharing with others or in a presentation, the following can be used. The `height` and `width` parameters are set in pixels, and `bound=TRUE` will prevent the network from dispersing beyond these dimensions. While this may help viewers to see all nodes, it will also cause nodes to cluster at the limits of height and wigth. This can be prevented by increasing the `charge` parameters, which specifies the strength of node repulsion (negative value) or attraction (positive value). The `zoom` parameter indicates whether to allow users to zoom in and out of the network, which can be especially helpful in large networks for exploring clusters.

```r
library(htmlwidgets)
vis <- VisualizeTextD3js(sotu_text_network, 
                      prune_cut=.50,
                      height=1000,
                      width=1400,
                      bound=FALSE,
                      zoom=TRUE,
                      charge=-30)
saveWidget(vis, "sotu_textnet.html")



```
</br></br>

### References

Bail, Christopher A. 2016. "Combining Network Analysis and Natural Language Processing to Examine how Ad-
vocacy Organizations Stimulate Conversation on Social Media." Proceedings of the National Academy of Sciences, 113:42 11823-11828

Rule, Alix and Jean-Philippe Cointet and Peter Bearman. 2015. "Lexical shifts, substantive changes, and continuity in the State of the Union Discourse, 1790-2014.
