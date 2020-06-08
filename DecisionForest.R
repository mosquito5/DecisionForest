library("randomForest")



data<-read.csv('C:\\Users\\bests\\Desktop\\Sztuczna In\\lab\\tic-tac-toe.data',header=FALSE)

str(data)







train<-sample(1:nrow(data),800)
data.train<-data[train,]
data.test<-data[-train,]




rf <- randomForest(V10 ~ . , data = data.train)


cm <- table(data.test$V10, predict(rf, data.test))



cm2 <- cm
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
round(cbind(cm2, err),2)
#recognition rate
rr <- 0
for (a in 1:nrow(cm))
{
  rr <- rr + cm[a,a]
}
round(rr / sum(cm),4)