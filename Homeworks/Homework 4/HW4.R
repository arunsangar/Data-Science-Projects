install.packages(tm)
library(tm)

reviewdata <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Homework 4\\movie_reviews.csv")

review_corpus <- Corpus(VectorSource(reviewdata[,1]))

review_corpus2 <- Corpus(VectorSource(reviewdata[,1]))

for(i in seq(review_corpus))   
   {   
     review_corpus2[[i]] <- gsub("[<][b][r][ ][/][>][<][b][r][ ][/][>]", " ", review_corpus[[i]])   
   } 

review_corpus3 <- tm_map(review_corpus2,tolower)

review_corpus4 <- tm_map(review_corpus3,removeNumbers)

review_corpus5 <- tm_map(review_corpus4,removePunctuation)

review_corpus6 <- tm_map(review_corpus5,removeWords, stopwords("english"))

review_corpus7 <- tm_map(review_corpus6,stripWhitespace)

dtm_tfidf <- DocumentTermMatrix(review_corpus7, control = list(weighting = weightTfIdf))

dtm_tfidf2 <- removeSparseTerms(dtm_tfidf,.99) 

cosinesim <- function(doc1,doc2)
{
	sumab <- 0
	sumasq <- 0
	sumbsq <- 0
	retcosim <- 0
	
	for(i in 1:length(doc1))
	{
		sumab = sumab + (doc1[i] * doc2[i])
		sumasq = sumasq + (doc1[i]^2)
		sumbsq = sumbsq + (doc2[i]^2)
	}
	
	if(sumbsq > 0) retcosim = sumab / sqrt(sumasq * sumbsq)
	return (head(unname(retcosim)))
}

mymatrix <- as.matrix(dtm_tfidf2)

cosim <- 0
maxcosim <- 0
maxindex <- 2

for(i in 2:nrow(mymatrix))
{
	cosim = cosinesim(mymatrix[1,],mymatrix[i,])
	if(cosim > maxcosim) 
	{
		maxcosim = cosim
		maxindex = i
	}
}
