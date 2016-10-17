# Topic-Modeling-in-R
Visualizing topic models with LDAvis and topicmodels library in R

This project builds a word cloud and visualizes the topics from abstracts of academic publication data. It uses the tm package in R to build a corpus and remove stopwords. A document-term matrix is created from the corpus. A wordcloud is generated with most frequent words. Latent Dirichlet allocation (LDA) is a generative statistical model that allows sets of observations to be explained by unobserved groups that explain why some parts of the data are similar. LDA package is used to create topic models with k=10 topics. LDAVis package reduces the multi-dimensional topics to 2 dimensions using principal component analysis (pca) with pc1 and pc2 axes.  
