library(naivebayes)
library(ggplot2)
library(dplyr)
library(psych)
movieData<-read.csv(file = "/Users/punith/Downloads/aa data analytics/sem 2/ACRM/tmdb_5000_movies.csv", 
                    header=T, na.strings = c(""), stringsAsFactors = T)
i <- 0
movieData$success<-c()
for (x in movieData$profit) { 
  i = i+1
  x
  if(x > 0) 
    movieData$success[i] <- c("yes")
  else if(x <= 0)
    movieData$success[i] <- c("no")
}
movieData$success <- as.factor(movieData$success)
write.csv(movieData, file = "/Users/punith/Downloads/aa data analytics/sem 2/ACRM/movies.csv")

set.seed(1234)
index <- sample(1:dim(movieData)[1], dim(movieData)[1] * .75, replace=FALSE)
movieTrain <- movieData[index == 1, ]
movieTest <- movieData[index == 2, ]

index <- sample(1:dim(movieData)[1], dim(movieData)[1] * .75, replace=FALSE)
movieTrain <- movieData[index, ]
movieTest <- movieData[-index, ]


index <- sample(1:dim(movieData)[1], dim(movieData)[1] * .75, replace=FALSE)
movieTrain <- movieData[index, ]
movieTest <- movieData[-index, ]

model <- naive_bayes(success ~ popularity + vote_average + vote_count 
                     + revenue, data=movieTrain)

NB_Predictions=predict(model,movieTest)
confusionMatrix(NB_Predictions, movieTest$success, positive = "yes")

library(C50)
library(caret)
library(e1071)
cFifty <- C5.0(movieData$success ~ movieData$popularity + movieData$vote_average + movieData$vote_count 
               + movieData$revenue, data=movieTrain, trials=10)
cFifty <- C5.0(success ~ popularity + vote_average + vote_count 
               + revenue + status, data=movieTrain, trials=20)
cFiftyWinnow <- C5.0(movieData$success ~ movieData$popularity + movieData$vote_average + movieData$vote_count 
                     + movieData$revenue, data = movieTrain, control = C5.0Control(winnow = TRUE))

rpartPrediction <- predict(cFifty, movieTest, type = "class")
table(movieTest$success, rpartPrediction)
confusionMatrix(rpartPrediction, movieTest$success, positive = "yes")


library(randomForest)
library(corrplot)
data.imputed <- rfImpute(success ~ runtime, data = movieData, iter=6)
forest <- randomForest(success ~ popularity + vote_average + vote_count 
                       + revenue + status, data=movieTrain, importance=TRUE, ntree=2000)
movie <- randomForest(success ~ vote_average + vote_count + 
                        budget + runtime, data=movieTrain, importance=TRUE, ntree=2000)
rfmovie <- predict(movie, movieTest, type = "class")
confusionMatrix(rfmovie, movieTest$success, positive = "yes")
forest
varImpPlotData <- varImpPlot(forest)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(forest$err.rate), times=3),
  Type=rep(c("OOB", "no", "yes"), each=nrow(forest$err.rate)),
  Error=c(forest$err.rate[,"OOB"],
          forest$err.rate[,"no"],
          forest$err.rate[,"yes"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

table(movieData$success)

sapply(movieData, function(x) sum(is.na(x)))
movieruntime<-mean(movieData$runtime, na.rm = TRUE)
if(is.na(movie$runtime)){movie$runtime<-movieruntime}
which(is.na(movie$runtime), arr.ind=TRUE)
movieData$runtime[2657]<-movieruntime
movieData$runtime[4141]<-movieruntime
movie[is.na(movie$runtime)] <- movieruntime
movie<-movieData
movieData$homepage<-NULL
movieData$overview<-NULL
movieData$release_date<-NULL
movieData$tagline<-NULL
movie.numeric<-as.data.frame(lapply(movie,as.numeric))
corrplot(cor(movie.numeric, method="spearman"), method = "number")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(movie.numeric, method="spearman"), method = "color", col = col(200),
          order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank"
         # hide correlation coefficient on the principal diagonal
         )
cor(movie.numeric, use="everything")
cor(Carseats[sapply(Carseats, function(x) !is.factor(x))])

tail(forest$err.rate, 10)

fit <- aov(profit ~ mstatus, data=movieTest)
mstatus<-as.factor(movieTest$status)
fit1 <- kruskal.test(success ~ mstatus, data=movieTest)


