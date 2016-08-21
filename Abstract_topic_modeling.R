setwd("/Users/anandgautam/Desktop/topic_modeling")
# Required packages
library(tm)
library(wordcloud)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)
library(servr)

#reading data and splitting the column by commas
dat <- read.csv(file="NAP_abstracts.csv", header=TRUE)

#creating corpus for life sciences cohort
text_obj <- as.data.frame(dat$Abstract)
review_source <-DataframeSource(text_obj)
corpus <- Corpus(review_source)

#cleaning up data 
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),'the','elsevier','also','used','can', 'proposed','based','paper','ieee','information','investigated','show','two','higher','well','due','report','result','achieved','demonstrated','using','high', 'results','first','new','low','associated','may','within','showed','association','however','obtained','one','american', 'society', 'effects','towards','via','studies','design', 'chemistry','general','performance','total','selective','toward','verlag','highly','gmbh','enhanced','study','kgaa', 'weinheim','film', 'available', 'excellence','institute','found','observed','different','prepared','method','exhibit','single','simple','wileyvch','large','approach','applications','technique','compared','temperature','demonstrate','size','work','effect', 'increase','devices','polymers','various','use','including','structure','rights','reserved','three','studied', '2007','2008','2009','2010','2011','2012','2013','2014','2015', '2004','many','ltd','provides','provide','shown','shows','significantly','thus','without','carried', 'copyright','described','directly','easily','efficiently','herein','key','less','previous','readily','see','yields','excellent', 'good','step','amides','catalysts','primary','secondary','realized','reported','corresponding','presented','direct','important','mild','formed','applied','addition','generated','developed','amount','supported','desired','reaction','abstract','product','describes', 'achieves','removed','science','increases','aid','michael','2005','2006','suggest','suggests','useful','place','takes','revealed','indicated','pathways','small','excess','group','allows','promotes','generates','georg','thieme','followed','york','variety','confirmed','characterized','investigations','comprehensively','discussed','upon','modified','possessing','multiple','included','comprehensively','resulting','containing','partner','organisations','leading','onto','employed','targeting','present','presents','significant','indicate','successfully','respectively','research','promising','known','inc','among','identified','findings','required','increased','will','day','days','might','year','per','tool','ask','calling','recently','review','recent','changes','level','factor','growth','several','involved','role','ago','objects','action','activity','performed','time','areas','previously','identify','whether','others','although','therefore','often','unknown','versus','common','represented','considered','specifically','sample','samples', 'test','relatively','part','four','similar', 'proteins','cells','populations','across','regions','data','whereas','analyses', 'even','find','along','range','genes','better','algorithms','achieve','improved','systems', 'techniques','analysis','properties','rate','ask','together','another','way','find','specific','individual', 'either','evidence', 'leads','making','initial','furthermore','example','now','yet','possible','whose','background','methods','analyzed','additional','conclusion','number'))


#converting the corpus into document-term matrix
dtm <- DocumentTermMatrix(corpus, control=list(removeNumbers = TRUE))

rowTotals <- apply(dtm, 1, sum)
#dtm.new <-dtm[rowTotals>0 ,]
empty.rows <- dtm[rowTotals==0,]$dimnames[1][[1]]
corpus <-corpus[-as.numeric(empty.rows)]

dtm2 <- DocumentTermMatrix(corpus, control=list(removeNumbers = TRUE))

dtm.new <- as.matrix(dtm2)
frequency <- colSums(dtm.new)
frequency <- sort(frequency, decreasing=TRUE)

#word cloud based on document title
library(RColorBrewer)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, random.order = FALSE, scale=c(4,0.5),random.color = FALSE, colors=brewer.pal(8,"Dark2"))


#creating a topic model & setting parameters
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
k <- 10

#extracting topics, terms and probabilities from LDA model
ldaOut <-LDA(dtm2,k, method="Gibbs", control=list(nstart=nstart, seed=seed, best=TRUE, burnin=burnin, iter=iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
topicProbabilities <- as.data.frame(ldaOut@gamma)

write.csv(ldaOut.terms, file='lda_terms 2014-15.csv')
write.csv(topicProbabilities, file='topicprobabilities 2014-15.csv')
write.csv(ldaOut.topics, file='lda topics 2014-15.csv')
 

topicmodels_json_ldavis <- function(ldaOut, corpus, dtm){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(ldaOut)$terms %>% as.matrix
  theta <- posterior(ldaOut)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(dtm2)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc_length, term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}
serVis(json_lda, out.dir = 'NAP 10 topics',open.browser = FALSE)