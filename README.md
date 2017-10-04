# textnets
R package to perform automated text analysis using network techniques

# Overview 

There is growing interest in automated detection of latent themes in unstructured text data. Thus far, much attention has been given to topic models, but analyzing texts using the tools of network analysis is an interesting alternative. Though we normally think about network analysis as describing relationships between people, it can also be applied to understand relationships between words-- particularly groups of words that co-occur in documents. This lets us do a number of things- from creating new, more intuitive visualizations of textual themes across documents, but also employ cutting-edge community detection algorithms to identify such themes in a manner that has various advantages over topic modeling which are described in Bail (2017)

Though the idea to think about texts as networks is not entirely new, recent advances in natural language processing and community detection analysis have pushed things forward. This package is an attempt to make these things more widely accessible, and encourage others to build off it to make it even better.

The textnets package includes four types of functions. First, it includes two functions that read in text data and produce "tidy text" data, which is particularly amenable to network analysis. Second, it creates networks from tidy-texts. Third, it provides several functions to identify communities of text and interpret their meaning. Finally, the textnets package includes several functions for visualizing text networks. 

# Getting Started
