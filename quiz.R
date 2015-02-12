#Q2-2
library(nlme)
library(lattice)
plot(xyplot(weight ~ Time | Diet, BodyWeight))

#Q2-4
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
plot(p)

#Q2-4
?splom
  # Draw Conditional Scatter Plot Matrices and Parallel Coordinate Plots
?trellis.par.set
  # Functions used to query, display and modify graphical parameters for 
  # fine control of Trellis displays. Modifications are made to the settings 
  # for the currently active device only.
?print.trellis
  # The print and plot methods produce a graph from a "trellis" object. 
?par
  # par can be used to set or query graphical parameters. 

#Q2-7
require(ggplot2) || install.packages("ggplot2")
library(ggplot2)
library(datasets)
data(airquality)
airquality <- transform(airquality, Month = factor(Month))
plot(
  qplot(Wind,Ozone,data = airquality, facets = .~ Month)
  )

#Q2-10
require(ggplot2) || install.packages("ggplot2")
library(ggplot2)
print(
  #qplot(votes, rating, data = movies)
  #qplot(votes, rating, data = movies, smooth = loess)
  #qplot(votes, rating, data = movies, panel = panel.loess)
  qplot(votes, rating, data = movies) + geom_smooth()
  #qplot(votes, rating, data = movies) + stats_smooth("loess")
  )


#############################
#Video Lessons Week 3
set.seed(17)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
#dataMatrix[1,1] <- 2
#dataMatrix[40,10] <- 2
# to get the matrix index [1,1] to end up in bottom left corner
# and the index [40,10] in the topright the matric at to be
# transposed and the coulm of the transpose (being the original rows)
# has to be in inverted order
image(1:10,1:40, t(dataMatrix)[,nrow(dataMatrix):1]) #transpose and columns in inverted order
heatmap(dataMatrix)

set.seed(17)
for ( i in 1:40) {
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  if(coinFlip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
  }
}
par(mar = rep(0.2, 4))
image(1:10,1:40, t(dataMatrix)[,nrow(dataMatrix):1]) #transpose and columns in inverted order
heatmap(dataMatrix)


hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, pch = 19)
plot(colMeans(dataMatrixOrdered), pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, pch = 19)
plot(1:10, svd1$v[,1], pch = 19)

par(mfrow = c(1,3))
plot(1:10, svd1$d, pch = 19)
plot(1:10, svd1$d^2/sum(svd1$d^2), pch = 19)
plot(1:10, scale(svd1$d), pch = 19)

par(mfrow = c(1,1))
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch = 19)
abline(c(0,1),1)

constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]) {
  constantMatrix[i,] <- rep(c(0,1), each = 5)
}
svd2 <- svd(constantMatrix)
par(mfrow = c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(1:10, svd2$d, pch = 19)
plot(1:10, svd2$d^2/sum(svd2$d^2), pch = 19)

set.seed(171)
for ( i in 1:40) {
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  if(coinFlip1) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each = 5)
  }
  if(coinFlip2) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), 5)
  }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
svd3 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(1:10, svd3$v[,1], pch = 19)
plot(1:10, svd3$v[,2], pch = 19)
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(1:10, svd3$d, pch = 19)
plot(1:10, svd3$d^2/sum(svd3$d^2), pch = 19)

source("http://bioconductor.org/biocLite.R")
biocLite("impute")
dataMatrix2 <- dataMatrixOrdered
dataMatrix2 [sample(1:100,size=40,replace = FALSE)] <- NA
dataMatrix2 <- impute::impute.knn(dataMatrix2)$data
svd3 <- svd(scale(dataMatrixOrdered))
svd4 <- svd(scale(dataMatrix2))
par(mfrow = c(1,3))
plot(1:10, svd3$v[,1], pch = 19)
plot(1:10, svd4$v[,1], pch = 19)

#############################
#Video Lessons Week 4
pm99 <- read.table("./data/RD_501_88101_1999-0.txt",header = FALSE, sep = "|",
                   na.strings ="",comment.char = "#")
pm12 <- read.table("./data/RD_501_88101_2012-0.txt",header = FALSE, sep = "|",
                   na.strings ="",comment.char = "#")


headline <- readLines("./data/RD_501_88101_1999-0.txt", n=1)
headline <- strsplit(headline, "|",fixed = TRUE)

names(pm99) <- make.names(headline[[1]])
names(pm12) <- make.names(headline[[1]])

x99 <- pm99$Sample.Value
summary(x99)
mean(is.na(x99))

x12 <- pm12$Sample.Value
summary(x12)
mean(is.na(x12))

boxplot(log10(x99),log10(x12))

negativeX99 <- x99 < 0
sum(negativeX99,na.rm= TRUE)
mean(negativeX99,na.rm= TRUE)
negativeX12 <- x12 < 0
sum(negativeX12,na.rm= TRUE)
mean(negativeX12,na.rm= TRUE)

dates99 <- as.Date(as.character(pm99$Date), "%Y%m%d")
dates12 <- as.Date(as.character(pm12$Date), "%Y%m%d")

par(mfrow = c(1,3))
hist(dates99,"month")
hist(dates12,"month")
hist(dates12[negativeX12],"month")

pm99$County.Site <- with(pm99,paste(County.Code,Site.ID, sep="."))
pm12$County.Site <- with(pm12,paste(County.Code,Site.ID, sep="."))

site99 <- unique(subset(pm99,subset = State.Code == 36, select = c(County.Site, Sample.Value)))
site12 <- unique(subset(pm12,subset = State.Code == 36, select = c(County.Site, Sample.Value)))                                                  
both <- intersect(site99$County.Site, site12$County.Site)

county99 <- subset(pm99, subset = State.Code == 36 & County.Site %in% both, select = c(County.Site, Sample.Value))
county12 <- subset(pm12, subset = State.Code == 36 & County.Site %in% both, select = c(County.Site, Sample.Value))

sapply(split(county99,county99$County.Site),nrow)
sapply(split(county12,county12$County.Site),nrow)

pm99sub <- subset(pm99, subset = State.Code == 36 & County.Site == "63.2008")
pm12sub <- subset(pm12, subset = State.Code == 36 & County.Site == "63.2008")

dates99sub <- as.Date(as.character(pm99sub$Date), "%Y%m%d")
dates12sub <- as.Date(as.character(pm12sub$Date), "%Y%m%d")

x99sub <- pm99sub$Sample.Value
x12sub <- pm12sub$Sample.Value

ylim <- range(c(x99sub,x12sub), na.rm = TRUE)
par(mfrow = c(1,2))
plot(dates99sub,x99sub, ylim = ylim, pch = 20)
#abline(h = mean(x99sub, na.rm=TRUE))
abline(h = median(x99sub, na.rm=TRUE))
plot(dates12sub,x12sub, ylim = ylim, pch = 20)
#abline(h = mean(x12sub, na.rm=TRUE))
abline(h = median(x12sub, na.rm=TRUE))

#state99 <- tapply(pm99$Sample.Value, pm99$State.Code, mean, na.rm = TRUE)
#state99 <- pm99 %>% group_by(State.Code) %>% summarize(mean(Sample.Value, na.rm = TRUE))
state99 <- summarize(group_by(pm99,State.Code), mean(Sample.Value,na.rm = TRUE))

#state12 <- tapply(pm12$Sample.Value, pm12$State.Code, mean, na.rm = TRUE)
state12 <- summarize(group_by(pm12,State.Code), mean(Sample.Value,na.rm = TRUE))

mrg <- merge(state99, state12, by = "State.Code")
names(mrg) <- c("State.Code", "x99state", "x12state")

par(mfrow = c(1,1))
with(mrg, plot(rep(1999,53),x99state,xlim = c(1999,2012)))
with(mrg, points(rep(2012,53),x12state))
segments(rep(1999,53),mrg$x99state,rep(2012,53),mrg$x12state)

#############################
