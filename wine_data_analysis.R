rm(list = ls())
winedata <- read.csv('Wine_data.csv',header=TRUE,sep = ',')
any(is.na(winedata))
sum(which(any(is.nan(winedata))))
length(which(is.na(winedata)))
sum(is.na(winedata$TYPE))
sum(is.na(winedata$ALCOHOL))
sum(is.na(winedata$MALIC))
sum(is.na(winedata$ASH))
sum(is.na(winedata$ALCALINITY))
sum(is.na(winedata$MAGNESIUM))
sum(is.na(winedata$PHENOLS))
sum(is.na(winedata$FLAVANOIDS))
sum(is.na(winedata$NONFLAVANOIDS))
sum(is.na(winedata$PROANTHOCYANINS))
sum(is.na(winedata$COLOR))
sum(is.na(winedata$HUE))
sum(is.na(winedata$DILUTION))
sum(is.na(winedata$PROLINE))

# 7 errors in total
newwine <- winedata
# Malic 1
winedata[138,]
summary(winedata)
# malic mean is 2.318
which(is.na(winedata$MALIC))

newwine$MALIC[is.na(newwine$MALIC)] = 2.318

# MAgensium 1

which(is.na(winedata$MAGNESIUM))
summary(winedata)
newwine$MAGNESIUM[is.na(newwine$MAGNESIUM)] =  99.39 

# color 1
which(is.na(winedata$COLOR))
newwine[160,]
summary(newwine)
summary(winedata)
# Hue 1
which(is.na(winedata$HUE))

which(is.na(newwine$PROLINE))
summary(newwine)

# removing all missing values won't change data much and 7/178 is less than 5% of the whole data so it shouldn't be a problem
newwine <- winedata[complete.cases(winedata),]
any(is.na(newwine))

any(is.na(newwine))
which(is.na(newwine))
library(corrplot)
cor(newwine)
corrplot(cor(newwine),method='number')
str(newwine)
length(newwine$PROLINE)
library(purrr)
library(NbClust)
# newwine structure

str(newwine)
summary(newwine)


# EDA

# type
ggplot(newwine,aes(newwine$TYPE,fill=factor(TYPE)))+
  geom_histogram(bins=3)

summary(newwine$TYPE)

# alcohol%
ggplot(newwine,aes(newwine$ALCOHOL,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$ALCOHOL,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$ALCOHOL,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$ALCOHOL)

# malic
ggplot(newwine,aes(newwine$MALIC,fill=factor(TYPE)))+
  geom_histogram(bins = 20)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$MALIC,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$MALIC,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$MALIC)

# Ash
ggplot(newwine,aes(newwine$ASH,fill=factor(TYPE)))+
  geom_histogram(bins = 10)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$ASH,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$ASH,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$ASH)

# Alcalinity
ggplot(newwine,aes(newwine$ALCALINITY,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$ALCALINITY,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$ALCALINITY,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$ALCALINITY)

# MAGNESIUM
ggplot(newwine,aes(newwine$MAGNESIUM,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$MAGNESIUM,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$MAGNESIUM,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$MAGNESIUM)

# PHENOLS
ggplot(newwine,aes(newwine$PHENOLS,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$PHENOLS,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$PHENOLS,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$PHENOLS)

# FLAVANOIDS
ggplot(newwine,aes(newwine$FLAVANOIDS,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$FLAVANOIDS,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$FLAVANOIDS,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$FLAVANOIDS)

# NONFLAVANOIDS
ggplot(newwine,aes(newwine$NONFLAVANOIDS,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$NONFLAVANOIDS,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$NONFLAVANOIDS,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$NONFLAVANOIDS)

# PROANTHOCYANINS
ggplot(newwine,aes(newwine$PROANTHOCYANINS,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$PROANTHOCYANINS,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$PROANTHOCYANINS,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$PROANTHOCYANINS)

# COLOR
ggplot(newwine,aes(newwine$COLOR,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$COLOR,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$COLOR,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$COLOR)

# HUE
ggplot(newwine,aes(newwine$HUE,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$HUE,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$HUE,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$HUE)

# DILUTION
ggplot(newwine,aes(newwine$DILUTION,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$DILUTION,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$DILUTION,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$DILUTION)

# PROLINE
ggplot(newwine,aes(newwine$PROLINE,fill=factor(TYPE)))+
  geom_histogram(bins = 30)+
  facet_wrap(~TYPE)

ggplot(newwine,aes(1:171,newwine$PROLINE,col=factor(TYPE)))+
  geom_point()

ggplot(newwine,aes(1:171,newwine$PROLINE,col=factor(TYPE)))+
  geom_boxplot()

summary(newwine$PROLINE)


# elbow method
tot_withinss=map_dbl(1:10,function(k){
  model = kmeans(newwine,centers = k)
  model$tot.withinss
})
plot(1:10,tot_withinss,type = 'o')

SilClust<-NbClust(newwine,distance = 'euclidean',min.nc = 2, max.nc = 10,method = 'kmeans',index = 'silhouette')
gapClust<-NbClust(newwine,distance = 'euclidean',min.nc = 2, max.nc = 10,method = 'kmeans',index = 'gap')
CHClust<-NbClust(newwine,distance = 'euclidean',min.nc = 2, max.nc = 10,method = 'kmeans',index = 'ch')


par(mfrow=c(1,1))
plot(2:10,SilClust$All.index,type='o')
plot(2:10,gapClust$All.index,type='o')
plot(2:10,CHClust$All.index,type='o')

# optimal number ofclusters
corrplot(cor(newwine))
par(mfrow=c(1,1))
kmeansmdl=kmeans(norm_winedata,centers=3,nstart=25)

plot(norm_winedata,pch = 1,col=kmeansmdl$cluster)
points(kmeansmdl$center,col='blue', pch=16)
kmeansmdl$centers
# 0.877993
kmeansmdl$betweenss/kmeansmdl$totss
newwine2 <- select(newwine[,2-14])
ggplot(newwine,aes(newwine$HUE,newwine$ALCOHOL,col=as.factor(kmeansmdl$cluster)))+
  geom_point()

install.packages('GGally')
library(GGally)
ggpairs(cbind(norm_winedata, Cluster=as.factor(kmeansmdl$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()


clusplot(newwine,kmeansmdl$cluster)

kmeansmdl=kmeans(newwine[2:3],centers=3,nstart=25)

# 3:4
kmeansmdl=kmeans(newwine,centers=3,nstart=25)

plot(newwine[3],newwine[12],pch = 1,col=kmeansmdl$cluster)
points(kmeansmdl$center,col='blue', pch=16)
kmeansmdl$centers
