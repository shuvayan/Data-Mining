
rm(list = ls())
library('RCurl')
library('XML')
library('rvest')
library('magrittr')
library('tm')
library('wordcloud')
library('RColorBrewer')
library('ggplot2')
library("dplyr")

init <- "http://www.imdb.com/title/tt0418279/reviews?filter=best"

crawlCandicate <- "reviews\\?filter=best"
base = "http://www.imdb.com/title/tt0418279/"

#initialize the number of pages till which it has to move
num = 20
doclist = list()#initializing doclist
anchorlist = vector()#initializing anchorlist

j <- 0

while(j <num){
  #j= 0 on the inital page
  if(j==0){
    doclist[j+1] <- getURL(init) #simple text format of the webpage is downloaded
  }else{
    doclist[j+1] <-  getURL(paste(base,anchorlist[j+1],sep = ""))#complete
  }
  doc <- htmlParse(doclist[[j+1]]) #parsing recreates the <a>,<div> tag which is lost once we downloaded in the texxt format
  anchor <- getNodeSet(doc,"//a") #within the doc only interested in a tags
  #tags are treated as nodes in R a=like div node etc
  #now i am only interested in href tag
  #anchor is list and xmlgetattribute now works on single a tags
  anchor <- sapply(anchor,function(x)xmlGetAttr(x,"href"))#capture all the href
  #not all links are imp hence the filtering is done grep function is text matching which particular link contains the crawl candidate
  #which links which contain start = portion is downloaded
  anchor  <- anchor[grep(crawlCandicate,anchor)]#matching from 1 string to anaother
  #grep the 1st entry is a pattern what you are looking for it is always a string and where it is looking as second parameter
  #grep gives binary output true and false 
  anchorlist = c(anchorlist,anchor)#capturing all the links in the external container
  anchorlist = unique(anchorlist)
  j= j+1
}


reviewDataFrameT=data.frame(Reviews=character(),Reviewer=character(),Rating=character())

library(magrittr)
for (i in 1:10){
  doc = htmlParse(doclist[[i]])
  #to get where the p tags are lying and p has certain class information check the ptag whether it has any attribute or not
  y = getNodeSet(doc , "//div[@id = 'tn15content']/div")
  reviewersT=sapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/div[",2*x-1,"]/a[2]",sep="")))%>%
    sapply(.,xmlValue)
  #y1=lapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/img[",2*x-1,"]/a[2]",sep="")))
  y1=lapply(1:10,function(x)getNodeSet(y[[x]],paste("//div[@id='tn15content']/div[",2*x-1,"]/img",sep = "")))
  ratingsT=sapply(1:10,function(j)tryCatch(xmlGetAttr(y1[[j]][[1]],"alt",default = NA),error=function(e)return(NA)))
  reviewsT=sapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/p[",x,"]",sep="")))%>%
    sapply(.,xmlValue)
  reviewDataFrameT=rbind(reviewDataFrameT,data.frame(Reviews=reviewsT,Reviewers=reviewersT,Ratings=ratingsT))
}

reviewDataFrame=as.data.frame(reviewDataFrameT)


#Use the ratings variable:
#Convert the variable types:
reviewDataFrame$Ratings=as.numeric(reviewDataFrame$Ratings)
reviewDataFrame$Reviews=as.character(reviewDataFrame$Reviews)

library(tm)
reviewCorpus=Corpus(x=VectorSource(reviewDataFrame$Reviews))  #convert into documents
RC=tm_map(reviewCorpus,content_transformer(tolower))  #mapping functions - convert into lower case
RC=tm_map(RC,removeWords,stopwords("en"))  #remove stop words
RC=tm_map(RC,removeNumbers)
RC=tm_map(RC,removePunctuation)
RC=tm_map(RC,stemDocument) #getting the root word (actual word without past tense)

dtm=DocumentTermMatrix(RC) 
tdm = TermDocumentMatrix(RC)
#weightTfIdf = calculate freq of the terms (words) ,standardrize frequency
#Tf * IDF where IDF is inverse document freq(how many word freq/how many total document). and Tf is used to standardrize the frequency.
#Tf is (individualy) particular word freq/ total words used

m=as.matrix(dtm)
v=sort(colSums(m),decreasing=T)
d=data.frame(words=names(v),freq=v)
#wordcloud
library(wordcloud)
wordcloud(d$words,d$freq,max.words=100,colors =brewer.pal(6,"Dark2"),scale=c(5,0,7),random.order=F)


#Polarity of reviews.
library(RTextTools)
library(e1071)
library(qdap)


SimplifyText <- function(x) {
  return(removePunctuation(removeNumbers(stripWhitespace(tolower(x))))) 
}

reve<-data.frame(SimplifyText(reviewDataFrame$Reviews))
View(reve)
poldat <- with(reve, polarity(reviewDataFrame$Reviews))
pol.data <- data.frame(poldat)
polarity_rating <- data.frame(polarity = pol.data$all.polarity,rating = reviewDataFrame$Ratings)
polarity_rating <- na.omit(polarity_rating)
cor(polarity_rating$polarity*10,polarity_rating$rating)

cor(na.omit(pol.data$all.polarity),na.omit(reviewDataFrame$Ratings))
table(poldat$all$polarity>0)

View(poldat)
plot(poldat)
plot(poldat$all$polarity,ylab="All Polarity",xlab="Documents",pch=20,cex=1,col="blue")
mean(poldat$all$polarity)

#ratings prediction:
X=as.matrix(dtm)
X1=as.matrix(dtm1)

X=as.data.frame(X)
X1=as.data.frame(X1)

X=cbind(X,Ratings=reviewDataFrame$Ratings)
X1=cbind(X1,Ratings=reviewDataFrame$Ratings)

X.knownRating=na.omit(X)
X.unknownRating=X[is.na(X$Ratings),]

rating_cat=arules::discretize(X.knownRating$Ratings,method="cluster",
                              categories=3,labels=c("Low","Med","High"))
View(rating_cat)

X.knownRating=cbind(X.knownRating[,-4309],rating_cat)

X.knownRating=X.knownRating[sample(98,98),]
hist(colSums(X.knownRating[,-4309]))
summary(colSums(X.knownRating[,-4309]))
sum(colSums(X.knownRating[,-4309])>=0.05)

X.knownRating=X.knownRating[,colSums(X.knownRating[,-4309])>=0.05]

X.knownRating=cbind(X.knownRating,rating_cat)
trainset=X.knownRating[1:70,]
testset=X.knownRating[71:98,]


nchar(names(X.knownRating))


m1=e1071::naiveBayes(rating_cat~.,data=trainset)

X.knownR



#Clustering using lsa:
termfreq <- colSums(as.matrix(dtm))
df <- data.frame(words = names(termfreq),freq = termfreq)
library(dplyr)
df1 <- arrange(df,freq)

#Read the english dictionary using read.table:
words <- readLines("/home/shuvayan/Downloads/words.txt")
matchedIndex <- df1$words %in% words
#Now take only the words which are in words dict:
tdm = tdm[,matchedIndex == T]
inspect(tdm)
library(lsa)
m=as.matrix(tdm)
lsa.model=lsa(m,dimcalc_share(share=0.60))

# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsa.model$sk) %*% t(lsa.model$dk)

# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)
distMatrix <- 1 - cosine(lsaMatrix)

#Use clustering on Tk matrix to group similar words.
library(cluster)
k50=kmeans(scale(lsa.model$tk),centers=3,nstart=20)
table(k50$cluster)
cluster::silhouette(distMatrix)

centers50=aggregate(cbind(V1,V2,V3)~k50$cluster,
                    data=as.data.frame(lsa.model$dk),FUN=mean)

d=dist(centers50[,-1])
hc=hclust(d,method="ward.D")
plot(hc,hang=-1)
rect.hclust(hc,h=0.3)
rect.hclust(hc,h=0.4,border="blue")
rect.hclust(hc,h=1.0,border="cyan")
rect.hclust(hc,h=1.25,border="green")
rect.hclust(hc,h=1.7,border="red")
#As per the plot, optimum values could be either 2 or 3:

k2=kmeans(scale(lsa.model$dk),centers=2,nstart=20)

centers2=aggregate(cbind(V1,V2,V3)~k2$cluster,
                   data=as.data.frame(lsa.model$dk),FUN=mean)

k3=kmeans(scale(lsa.model$tk),centers=3,nstart=20)
centers3=aggregate(cbind(V1,V2,V3)~k3$cluster,
                   data=as.data.frame(lsa.model$tk),FUN=mean)

m=as.matrix(dtm)
v=sort(colSums(m),decreasing=T)
wordFreq=data.frame(words=names(v),freq=v)
k5_1=wordFreq[k3$cluster==1,]
k5_2=wordFreq[k3$cluster==2,]
k5_3=wordFreq[k3$cluster==3,]
View(k5_1)
View(k5_2)
View(k5_3)

#wordcloud
library(wordcloud)
#WordCloud of words in first cluster:
wordcloud(k5_1$words,k5_1$freq,colors =brewer.pal(6,"Dark2"),scale=c(5,0,7),random.order=F)
#WordCloud of words in second cluster:
wordcloud(k5_2$words,k5_1$freq,
          colors =brewer.pal(6,"Dark2"),
          scale=c(5,0,7),
          random.order=F)



#Use the lsa.model to predict ratings:

lsa_tk = as.data.frame(lsa.model$tk)
lsa_dk = as.data.frame(lsa.model$dk)
#Combine with the ratings variable:

ratings_tk <- data.frame(Ratings = reviewDataFrame$Ratings,lsa_tk)
#Try to predict ratings from this tk matrix:
ratings.lm <- lm(Ratings ~ .,data = ratings_tk)
summary(ratings.lm)

#Revenge of the Fallen:

init <- "http://www.imdb.com/title/tt1055369/reviews?filter=best"
init <- "http://www.imdb.com/title/tt0418279/reviews?filter=best"

crawlCandicate <- "reviews\\?filter=best"
base = "http://www.imdb.com/title/tt0418279/"

#initialize the number of pages till which it has to move
num = 20
doclist = list()#initializing doclist
anchorlist = vector()#initializing anchorlist

j <- 0

while(j <num){
  #j= 0 on the inital page
  if(j==0){
    doclist[j+1] <- getURL(init) #simple text format of the webpage is downloaded
  }else{
    doclist[j+1] <-  getURL(paste(base,anchorlist[j+1],sep = ""))#complete
  }
  doc <- htmlParse(doclist[[j+1]]) #parsing recreates the <a>,<div> tag which is lost once we downloaded in the texxt format
  anchor <- getNodeSet(doc,"//a") #within the doc only interested in a tags
  #tags are treated as nodes in R a=like div node etc
  #now i am only interested in href tag
  #anchor is list and xmlgetattribute now works on single a tags
  anchor <- sapply(anchor,function(x)xmlGetAttr(x,"href"))#capture all the href
  #not all links are imp hence the filtering is done grep function is text matching which particular link contains the crawl candidate
  #which links which contain start = portion is downloaded
  anchor  <- anchor[grep(crawlCandicate,anchor)]#matching from 1 string to anaother
  #grep the 1st entry is a pattern what you are looking for it is always a string and where it is looking as second parameter
  #grep gives binary output true and false 
  anchorlist = c(anchorlist,anchor)#capturing all the links in the external container
  anchorlist = unique(anchorlist)
  j= j+1
}


reviewDataFrameT=data.frame(Reviews=character(),Reviewer=character(),Rating=character())

library(magrittr)
for (i in 1:10){
  doc = htmlParse(doclist[[i]])
  #to get where the p tags are lying and p has certain class information check the ptag whether it has any attribute or not
  y = getNodeSet(doc , "//div[@id = 'tn15content']/div")
  reviewersT=sapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/div[",2*x-1,"]/a[2]",sep="")))%>%
    sapply(.,xmlValue)
  #y1=lapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/img[",2*x-1,"]/a[2]",sep="")))
  y1=lapply(1:10,function(x)getNodeSet(y[[x]],paste("//div[@id='tn15content']/div[",2*x-1,"]/img",sep = "")))
  ratingsT=sapply(1:10,function(j)tryCatch(xmlGetAttr(y1[[j]][[1]],"alt",default = NA),error=function(e)return(NA)))
  reviewsT=sapply(1:10, function(x)getNodeSet(y[[x]],paste("//div[@id = 'tn15content']/p[",x,"]",sep="")))%>%
    sapply(.,xmlValue)
  reviewDataFrameT=rbind(reviewDataFrameT,data.frame(Reviews=reviewsT,Reviewers=reviewersT,Ratings=ratingsT))
}

reviewDataFrame=as.data.frame(reviewDataFrameT)


#Use the ratings variable:
#Convert the variable types:
reviewDataFrame$Ratings=as.numeric(reviewDataFrame$Ratings)
reviewDataFrame$Reviews=as.character(reviewDataFrame$Reviews)

library(tm)
reviewCorpus=Corpus(x=VectorSource(reviewDataFrame$Reviews))  #convert into documents
RC=tm_map(reviewCorpus,content_transformer(tolower))  #mapping functions - convert into lower case
RC=tm_map(RC,removeWords,stopwords("en"))  #remove stop words
RC=tm_map(RC,removeNumbers)
RC=tm_map(RC,removePunctuation)
RC=tm_map(RC,stemDocument) #getting the root word (actual word without past tense)

dtm=DocumentTermMatrix(RC) 
tdm = TermDocumentMatrix(RC)
#weightTfIdf = calculate freq of the terms (words) ,standardrize frequency
#Tf * IDF where IDF is inverse document freq(how many word freq/how many total document). and Tf is used to standardrize the frequency.
#Tf is (individualy) particular word freq/ total words used

m=as.matrix(dtm)
v=sort(colSums(m),decreasing=T)
d=data.frame(words=names(v),freq=v)
#wordcloud
library(wordcloud)
wordcloud(d$words,d$freq,max.words=100,colors =brewer.pal(6,"Dark2"),scale=c(5,0,7),random.order=F)


#Polarity of reviews.
library(RTextTools)
library(e1071)
library(qdap)


SimplifyText <- function(x) {
  return(removePunctuation(removeNumbers(stripWhitespace(tolower(x))))) 
}

reve<-data.frame(SimplifyText(reviewDataFrame$Reviews))
View(reve)
poldat <- with(reve, polarity(reviewDataFrame$Reviews))
pol.data <- data.frame(poldat)
polarity_rating <- data.frame(polarity = pol.data$all.polarity,rating = reviewDataFrame$Ratings)
polarity_rating <- na.omit(polarity_rating)
cor(polarity_rating$polarity*10,polarity_rating$rating)

cor(na.omit(pol.data$all.polarity),na.omit(reviewDataFrame$Ratings))
table(poldat$all$polarity>0)

View(poldat)
plot(poldat)
plot(poldat$all$polarity,ylab="All Polarity",xlab="Documents",pch=20,cex=1,col="blue")
mean(poldat$all$polarity)

#ratings prediction:
X=as.matrix(dtm)
X1=as.matrix(dtm1)

X=as.data.frame(X)
X1=as.data.frame(X1)

X=cbind(X,Ratings=reviewDataFrame$Ratings)
X1=cbind(X1,Ratings=reviewDataFrame$Ratings)

X.knownRating=na.omit(X)
X.unknownRating=X[is.na(X$Ratings),]

rating_cat=arules::discretize(X.knownRating$Ratings,method="cluster",
                              categories=3,labels=c("Low","Med","High"))
View(rating_cat)

X.knownRating=cbind(X.knownRating[,-4309],rating_cat)

X.knownRating=X.knownRating[sample(98,98),]
hist(colSums(X.knownRating[,-4309]))
summary(colSums(X.knownRating[,-4309]))
sum(colSums(X.knownRating[,-4309])>=0.05)

X.knownRating=X.knownRating[,colSums(X.knownRating[,-4309])>=0.05]

X.knownRating=cbind(X.knownRating,rating_cat)
trainset=X.knownRating[1:70,]
testset=X.knownRating[71:98,]


nchar(names(X.knownRating))


m1=e1071::naiveBayes(rating_cat~.,data=trainset)

X.knownR



#Clustering using lsa:
termfreq <- colSums(as.matrix(dtm))
df <- data.frame(words = names(termfreq),freq = termfreq)
library(dplyr)
df1 <- arrange(df,freq)

#Read the english dictionary using read.table:
words <- readLines("/home/shuvayan/Downloads/words.txt")
matchedIndex <- df1$words %in% words
#Now take only the words which are in words dict:
tdm = tdm[,matchedIndex == T]
inspect(tdm)
library(lsa)
m=as.matrix(tdm)
lsa.model=lsa(m,dimcalc_share(share=0.60))

# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsa.model$sk) %*% t(lsa.model$dk)

# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)
distMatrix <- 1 - cosine(lsaMatrix)

#Use clustering on Tk matrix to group similar words.
library(cluster)
k50=kmeans(scale(lsa.model$tk),centers=3,nstart=20)
table(k50$cluster)
cluster::silhouette(distMatrix)

centers50=aggregate(cbind(V1,V2,V3)~k50$cluster,
                    data=as.data.frame(lsa.model$dk),FUN=mean)

d=dist(centers50[,-1])
hc=hclust(d,method="ward.D")
plot(hc,hang=-1)
rect.hclust(hc,h=0.3)
rect.hclust(hc,h=0.4,border="blue")
rect.hclust(hc,h=1.0,border="cyan")
rect.hclust(hc,h=1.25,border="green")
rect.hclust(hc,h=1.7,border="red")
#As per the plot, optimum values could be either 2 or 3:

k2=kmeans(scale(lsa.model$dk),centers=2,nstart=20)

centers2=aggregate(cbind(V1,V2,V3)~k2$cluster,
                   data=as.data.frame(lsa.model$dk),FUN=mean)

k3=kmeans(scale(lsa.model$tk),centers=3,nstart=20)
centers3=aggregate(cbind(V1,V2,V3)~k3$cluster,
                   data=as.data.frame(lsa.model$tk),FUN=mean)

m=as.matrix(dtm)
v=sort(colSums(m),decreasing=T)
wordFreq=data.frame(words=names(v),freq=v)
k5_1=wordFreq[k3$cluster==1,]
k5_2=wordFreq[k3$cluster==2,]
k5_3=wordFreq[k3$cluster==3,]
View(k5_1)
View(k5_2)
View(k5_3)

#wordcloud
library(wordcloud)
#WordCloud of words in first cluster:
wordcloud(k5_1$words,k5_1$freq,colors =brewer.pal(6,"Dark2"),scale=c(5,0,7),random.order=F)
#WordCloud of words in second cluster:
wordcloud(k5_2$words,k5_1$freq,
          colors =brewer.pal(6,"Dark2"),
          scale=c(5,0,7),
          random.order=F)



#Use the lsa.model to predict ratings:

lsa_tk = as.data.frame(lsa.model$tk)
lsa_dk = as.data.frame(lsa.model$dk)
#Combine with the ratings variable:

ratings_tk <- data.frame(Ratings = reviewDataFrame$Ratings,lsa_tk)
#Try to predict ratings from this tk matrix:
ratings.lm <- lm(Ratings ~ .,data = ratings_tk)
summary(ratings.lm)
















#MDS:
library(ggplot2)
library(scatterplot3d)
library(MASS)
#Do multi dimensional scaling,k signifies the number of dimensions in which the data is to be represented:
dist.mat.lsa <- dist(t(as.textmatrix(lsa.model))) # compute distance matrix

#Find out the appropriate number of dimensions:
mds.k_2 = isoMDS(dist.mat.lsa,k = 2)
mds.k_3 = isoMDS(dist.mat.lsa,k = 99)
#Use the minimum stress k:
data_dim <- data.frame()
mds_optimal_k <- function(dist){
  #A for loop for the number of runs:
  for (i in 2:length(dist.mat.lsa)-1){
    #model a mds space using isoMDS:
    mds.model = isoMDS(dist.mat.lsa,k = i)
    #Find out the stress of the model:
    stress <- mds.model$stress
    #If stress is less than 5% stop:
    if (stress < 20) i = length(dist.mat.lsa)-1
    #Else store the value of k and stress in a dataframe:
    stress_k <- cbind(i,stress);i = i+1
    #Increase the value of k by 1:
    data_dim <- rbind(data_dim,stress_k)
  }
  #Return the data frame
  return (data_dim)
}

mds_optimal_k(dist.mat.lsa)
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 3)
colors <- rep(c("blue", "green", "red"), each = 3,length.out = length(fit$points[,1]))
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color = colors,
              pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y",
              zlab = "z", type = "h")

