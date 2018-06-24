## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ---- fig.height=4.2, echo=FALSE-----------------------------------------
beertrain <- window(ausbeer, start=1992)
autoplot(beertrain) +
  xlab("Year") + ylab("megalitres") +
    ggtitle("Australian quarterly beer production")

## ---- fig.height=4.2, echo=FALSE-----------------------------------------
autoplot(window(pigs/1e3, start=1990)) +
  xlab("Year") + ylab("thousands") +
  ggtitle("Number of pigs slaughtered in Victoria")

## ---- fig.height=4.2, echo=FALSE-----------------------------------------
autoplot(dj) + xlab("Day") +
  ggtitle("Dow-Jones index") + ylab("")

## ----beerf, warning=FALSE, message=FALSE, echo=FALSE, fig.height=4.6-----
beertrain <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beertrain) +
  forecast::autolayer(meanf(beertrain, h=11), PI=FALSE, series="Mean") +
  forecast::autolayer(naive(beertrain, h=11), PI=FALSE, series="Naïve") +
  forecast::autolayer(snaive(beertrain, h=11), PI=FALSE, series="Seasonal naïve") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

## ----djf,  message=FALSE, warning=FALSE, echo=FALSE, fig.height=4.6------
# Set training data to first 250 days
dj2 <- window(dj,end=250)
# Plot some forecasts
autoplot(dj2) +
  forecast::autolayer(meanf(dj2, h=42), PI=FALSE, series="Mean") +
  forecast::autolayer(rwf(dj2, h=42), PI=FALSE, series="Naïve") +
  forecast::autolayer(rwf(dj2, drift=TRUE, h=42), PI=FALSE, series="Drift") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") +
  xlab("Day") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

## ----dj3, echo=TRUE------------------------------------------------------
autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

## ----dj4, echo=TRUE, warning=FALSE, fig.height=3.6-----------------------
fits <- fitted(naive(goog200))
autoplot(goog200, series="Data") +
  autolayer(fits, series="Fitted") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

## ----dj5, echo=TRUE, fig.height=3.6--------------------------------------
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

## ----dj6, warning=FALSE--------------------------------------------------
gghistogram(res, add.normal=TRUE) +
  ggtitle("Histogram of residuals")

## ----dj7-----------------------------------------------------------------
ggAcf(res) + ggtitle("ACF of residuals")

## ---- echo=TRUE, fig.height=4--------------------------------------------
checkresiduals(naive(goog200))

## ---- echo=FALSE---------------------------------------------------------
object <- naive(dj2)
main <- paste("Residuals from", object$method)
res <- residuals(object)
# Do Ljung-Box test
      LBtest <- Box.test(zoo::na.approx(res), fitdf=0, lag=10, type="Ljung")
      LBtest$method <- "Ljung-Box test"
      LBtest$data.name <- main
      names(LBtest$statistic) <- "Q*"
      print(LBtest)
      cat(paste("Model df: ",0,".   Total lags used: ",10,"\n\n",sep=""))

## ----traintest, fig.height=1, echo=FALSE, cache=TRUE---------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col="blue")
points(test,  test*0+0.5,  pch=19, col="red")
text(26,0.5,"time")
text(10,1,"Training data",col="blue")
text(21,1,"Test data",col="red")

## ----googaccuracy, echo=FALSE, fig.height=5------------------------------
googtrain <- window(goog200,end=180)
googfc1 <- meanf(googtrain,h=20)
googfc2 <- rwf(googtrain,h=20)
googfc3 <- rwf(googtrain,h=20,drift=TRUE)
tmp <- cbind(Data=goog200,
             Mean=googfc1[["mean"]],
             Naive=googfc2[["mean"]],
             Drift=googfc3[["mean"]])
autoplot(tmp) + xlab("Day") + ylab("Price") +
  ggtitle("Forecasts for GOOG stock price") +
  scale_color_manual(values=c('#000000','#1b9e77','#d95f02','#7570b3'),
                     breaks=c("Mean","Naive","Drift"),
                     name="Forecast Method")

## ----googaccuracyagain, echo=FALSE, fig.height=5, dependson="googaccuracy"----
autoplot(tmp) + xlab("Day") + ylab("Price") +
  ggtitle("Forecasts for GOOG stock price") +
  scale_color_manual(values=c('#000000','#1b9e77','#d95f02','#7570b3'),
                     breaks=c("Mean","Naive","Drift"),
                     name="Forecast Method")

## ----beertable, echo=FALSE, dependson='googaccuracy'---------------------
tab <- matrix(NA,ncol=4,nrow=3)
tab[1,] <- accuracy(googfc1, goog200)[2,c(2,3,5,6)]
tab[2,] <- accuracy(googfc2, goog200)[2,c(2,3,5,6)]
tab[3,] <- accuracy(googfc3, goog200)[2,c(2,3,5,6)]
colnames(tab) <- c("RMSE","MAE","MAPE","MASE")
rownames(tab) <- c("Mean method", "Naïve method", "Drift method")
knitr::kable(tab, digits=2)

## ----traintest2, fig.height=1, echo=FALSE, cache=TRUE--------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col="blue")
points(test,  test*0+0.5,  pch=19, col="red")
text(26,0.5,"time")
text(10,1,"Training data",col="blue")
text(21,1,"Test data",col="red")

## ----traintest3, fig.height=1, echo=FALSE, cache=TRUE--------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col="blue")
points(test,  test*0+0.5,  pch=19, col="red")
text(26,0.5,"time")
text(10,1,"Training data",col="blue")
text(21,1,"Test data",col="red")

## ----cv1, cache=TRUE, echo=FALSE, fig.height=4---------------------------
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,28),ylim=c(0,1),
       xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
i <- 1
for(j in 1:10)
{
  test <- (16+j):26
  train <- 1:(15+j)
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

## ----tscv, cache=TRUE----------------------------------------------------
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2,
                                     na.rm=TRUE))

## ----djforecasts, echo=TRUE, cache=TRUE----------------------------------
rwf(goog200, level=95, drift=TRUE)

