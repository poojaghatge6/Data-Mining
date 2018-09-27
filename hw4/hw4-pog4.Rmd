---
title: "hw4-pog4"
author: "Pooja Ghatge"
date: "13 March 2018"
output: html_document
---
### Task1
#### 1)
```{r}
data = read.csv('stock_price.csv')
dataset <- scale(t(data))
my.cor = cor(dataset)
PCAstock_price = prcomp(dataset, scale=TRUE)
loadings <- PCAstock_price$rotation
plot(loadings[,1],xlab='Weeks',ylab='PC1 value')
mtext(side=3, "Loadings of 1st Principal Component",  line=1, font=2)
stock_pricePC = predict(PCAstock_price)
screeplot(PCAstock_price,npcs = 30,type = "lines",main = "Principal Components")
```


##### It can be observed from the screeplot that the first three Principal Components capture the most variability in the dataset.



#### 2)

```{r}
plot(stock_pricePC[,1:2],main='Stocks in terms of PC1 & PC2')
text(x=stock_pricePC[,1], y=stock_pricePC[,2],labels = colnames(data))
abline(0,0,col="red")
abline(0,90,col="green")
```

#### 3)
```{r}
stock_price_Dist <- dist(dataset)
stock_price_MDS <- cmdscale(stock_price_Dist)
plot(stock_price_MDS, type = 'n',main = 'MDS map for Stocks')
text(stock_price_MDS, labels=colnames(data))
```

### 4)
```{r}
kmeans_clustering <- function(k){
clust_k3 = kmeans(dataset, centers=k, nstart=10)
plot(stock_price_MDS,type = "n",main=paste0("MDS map for ",k,"-(k)means clustering"))
text(stock_price_MDS, labels=colnames(data),col = clust_k3$cluster+1)
}
hclustering <- function(link,k)
{
  hc = hclust(stock_price_Dist,method=link)
  plot(hc,main=paste0("Dendogram for ",k,"-hierarchical clustering with "
                      ,link,"-linkage"))
  hcclasses = cutree(hc,k)
  plot(stock_price_MDS, type="n",
       main=paste0("MDS map for ",k,"-hierarchical clustering with ",link,"-linkage"))
  text(stock_price_MDS, labels = colnames(data), col=hcclasses+1)
}

kmeans_clustering(k=3)
kmeans_clustering(k=6)
hclustering(link='complete',k=3)
hclustering(link='complete',k=6)
hclustering(link='single',k=3)
hclustering(link='single',k=6)
hclustering(link='average',k=3)
hclustering(link='average',k=6)
```


### task 2 
#### 1)
```{r}
library(car)
library('foreign') ## for loading dta files using read.dta
library('ggplot2')

data.url = 'http://www.yurulin.com/class/spring2018_datamining/data/roll_call/'
#data.dir = file.path("data", "roll_call")
#data.files = list.files(data.dir)
data.files = c("sen113kh.dta")
## Add all roll call vote data frames to a single list
rollcall.data = lapply(data.files,
                       function(f) {
                         read.dta(file.path(data.url, f), convert.factors = FALSE)
                       })
dim(rollcall.data[[1]])
head(rollcall.data[[1]][,1:20])

rollcall.simplified <- function(df) {
  no.pres <- subset(df, state < 99)
  ## to group all Yea and Nay types together
  for(i in 10:ncol(no.pres)) {
    no.pres[,i] = ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 1, -1, no.pres[,i])
  }

  return(as.matrix(no.pres[,10:ncol(no.pres)]))
}

rollcall.simple = lapply(rollcall.data, rollcall.simplified)

rollcall.dist = lapply(rollcall.simple, function(m) dist(m %*% t(m)))
## Do the MDS
rollcall.mds = lapply(rollcall.dist,
                      function(d) as.data.frame((cmdscale(d, k = 2)) * -1))

## Add identification information about Senators back into MDS data frames
congresses = 113

for(i in 1:length(rollcall.mds)) {
  names(rollcall.mds[[i]]) = c("x", "y")

  congress = subset(rollcall.data[[i]], state < 99)

  congress.names = sapply(as.character(congress$name),
                          function(n) strsplit(n, "[, ]")[[1]][1])

  rollcall.mds[[i]] = transform(rollcall.mds[[i]],
                                name = congress.names,
                                party = as.factor(congress$party),
                                congress = congresses[i])
}

head(rollcall.mds[[1]])
cong.113 <- rollcall.mds[[1]]

base.113 <- ggplot(cong.113, aes(x = x, y = y)) +
  scale_alpha(guide="none") + theme_bw() +
  theme(axis.ticks = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_blank()) +
  xlab("") +
  ylab("") +
  scale_shape(name = "Party", breaks = c("100", "200", "328"),
              labels = c("Dem.", "Rep.", "Ind."), solid = FALSE) +
  scale_color_manual(name = "Party", values = c("100" = "blue",
                                                "200" = "red",
                                                "328"="grey"),
                     breaks = c("100", "200", "328"),
                     labels = c("Dem.", "Rep.", "Ind."))

print(base.113 + geom_point(aes(shape = party,
                                alpha = 0.75),size=4))


```






#### 2)

```{r}
library(car)
rollcall.data[[1]][,10:666]=scale(rollcall.data[[1]][,10:666])

set.seed(1) ## fix the random seed to produce the same results 
x=data.frame(summary(rollcall.data[[1]]))
rollcall.data[[1]]$eh1=ifelse(is.na(rollcall.data[[1]]$eh1), ave(rollcall.data[[1]]$eh1, FUN = function(x) median(x,na.rm=TRUE)), rollcall.data[[1]]$eh1) 
rollcall.data[[1]]$eh2=ifelse(is.na(rollcall.data[[1]]$eh2), ave(rollcall.data[[1]]$eh2, FUN = function(x) median(x,na.rm=TRUE)), rollcall.data[[1]]$eh2)
any(is.na(rollcall.data[[1]]))


## KMeans
grpSenators = kmeans(rollcall.data[[1]][,10:666], centers=2, nstart=10)
summary(rollcall.mds[[1]]$y)

plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")

text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$name, col=rainbow(2)[grpSenators$cluster])


## HClustering
library(cluster)


Rcall = dist(rollcall.data[[1]][,10:666]) ## use dist to obtain distance matrix


hc = hclust(Rcall,method='complete')
plot(hc)
hc1_complete = cutree(hc,k=2)
plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$name, col=rainbow(2)[hc1_complete])


hc = hclust(Rcall,method='single')
plot(hc)
hc1_single = cutree(hc,k=3)
plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$name, col=rainbow(3)[hc1_single])

hc = hclust(Rcall,method='average')
plot(hc)
hc1_average = cutree(hc,k=3)
plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$name, col=rainbow(3)[hc1_average])


```








#### 3)

```{r}

rollcall.data[[1]]$party=recode(rollcall.data[[1]]$party,"'100'=1;'200'=2;else=0")
rollcall.data[[1]]$party

Rcall = dist(rollcall.data[[1]][,10:666]) ## use dist to obtain distance matrix


plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$party, col=rainbow(2)[hc1_complete])
## Hierarchical Clustering with Complete link 
table(rollcall.data[[1]]$party,hc1_complete)
y=hc1_complete
x=cbind(rollcall.data[[1]],y)
#party=1 democrat; party=2 Republic;party=0 individual;y=1 republic;y=2 democrat
library("dplyr")

##Individuals wrongly grouped as Democrats
filter(x,party==0,y==2)$name

#Democrats wrongly grouped with Republicans
filter(x,party==1,y==1)$name

#Rebublicans wrongly grouped with Democrats
filter(x,party==2,y==2)$name



plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$party, col=rainbow(2)[hc1_single])

## Hierarchical Clustering with Single link 
# Highly imbalanced cluster
table(rollcall.data[[1]]$party,hc1_single)
y=hc1_single
x=cbind(rollcall.data[[1]],y)
library("dplyr")

##Except the democrat whose name is mentioned below, rest all democrats and republicans are wrongly grouped in a single cluster
filter(x,party==1,y==1)$name


plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")
text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$party, col=rainbow(3)[hc1_average])

## Hierarchical Clustering with Average link 
## highly imbalanced when k=2 hence we take k=3
table(rollcall.data[[1]]$party,hc1_average)
#party=1 democrat; party=2 Republic;party=0 individual;y=1 individual;y=2 republic;y=3 democrat
y=hc1_average
x=cbind(rollcall.data[[1]],y)
library("dplyr")

##Individuals wrongly grouped as Democrats
filter(x,party==0,y==3)$name

#Democrats wrongly grouped as Individuals
filter(x,party==1,y==1)$name

#Rebublicans wrongly grouped with Democrats
filter(x,party==2,y==3)$name


# Kmeans Result Analysis
plot(rollcall.mds[[1]]$x, rollcall.mds[[1]]$y, type="n", xlim=c(-3395,4715), ylim=c(-1371,671), xlab="x", ylab="y")

text(x=rollcall.mds[[1]]$x, y=rollcall.mds[[1]]$y, labels=rollcall.data[[1]]$party, col=rainbow(2)[grpSenators$cluster])

##confusion matrix
table(rollcall.data[[1]]$party,grpSenators$cluster)
y=grpSenators$cluster
x=cbind(rollcall.data[[1]],y)
#party=1 democrat; party=2 Republic;party=0 individual;y=1 republic;y=2 democrat
library("dplyr")

##Individuals wrongly grouped as Democrats
filter(x,party==0,y==2)$name

#Democrats wrongly grouped with Republicans
filter(x,party==1,y==1)$name

#Rebublicans wrongly grouped with Democrats
filter(x,party==2,y==2)$name
```




#### 4)

```{r}


cluster.purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

cluster.entropy <- function(clusters,classes) {
  en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {if (p) -p*log2(p) else 0} ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}


kmeans=cluster.purity(grpSenators$cluster,rollcall.data[[1]]$party)
hclust_single=cluster.purity(hc1_single,rollcall.data[[1]]$party)
hclust_average=cluster.purity(hc1_average,rollcall.data[[1]]$party)
hclust_complete=cluster.purity(hc1_complete,rollcall.data[[1]]$party)
purity=cbind(c('purity'),kmeans,hclust_single,hclust_average,hclust_complete)

kmeans=cluster.entropy(grpSenators$cluster,rollcall.data[[1]]$party)
hclust_single=cluster.entropy(hc1_single,rollcall.data[[1]]$party)
hclust_average=cluster.entropy(hc1_average,rollcall.data[[1]]$party)
hclust_complete=cluster.entropy(hc1_complete,rollcall.data[[1]]$party)
entropy=cbind(c('entropy'),kmeans,hclust_single,hclust_average,hclust_complete)

eval=rbind(purity,entropy)
eval

```


#### 5)

Kmeans and Hierarchical Clustering with average link generate the most meaningful results.
Among the others these have more purity and less entropy, which determines that they are least disordered and more uniform(pure). 
Moreover, k-means gives the least number of misclassified senators. It generates only 5 misclassified records.Also, misclassified records due to Hierarchical Clustering with average link is very less i.e 6.

