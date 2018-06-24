## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
source("nicefigs.R")
options(digits=4, width=55)

## ----rorigin, include=FALSE----------------------------------------------
for(i in 2:6)
{
  fname <- paste("rollingorigin",i,sep="")
  savepdf(fname,height=9.5,width=15)
  plot(0,0,xlim=c(0,28),ylim=c(0,1),
       xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
  for(j in 1:20)
  {
    test <- (6+j):26
    train <- 1:(5+j)
    arrows(0,1-j/20,27,1-j/20,0.05)
    points(train,rep(1-j/20,length(train)),pch=19,col="blue")
    if(length(test) >= i)
      points(test[i], 1-j/20, pch=19, col="red")
    if(length(test) >= i)
      points(test[-i], rep(1-j/20,length(test)-1), pch=19, col="gray")
    else
      points(test, rep(1-j/20,length(test)), pch=19, col="gray")
  }
  text(28,.95,"time")
  endpdf()
}
for(k in 1:20) {
  fname <- paste("rorigin",k,sep="")
  savepdf(fname,height=9.5,width=15)
  plot(0,0,xlim=c(0,28),ylim=c(0,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
  for(j in 1:k) {
    test <- (6+j):26
    train <- 1:(5+j)
    arrows(0,1-j/20,27,1-j/20,0.05)
    points(train,rep(1-j/20,length(train)),pch=19,col="blue")
    if(length(test) >= 1) {
      points(test[1], 1-j/20, pch=19, col="red")
      points(test[-1], rep(1-j/20,length(test)-1), pch=19, col="gray")
    }
    else {
      points(test, rep(1-j/20,length(test)), pch=19, col="gray")
    }
  }
  text(28,.95,"time")
  endpdf()
}

