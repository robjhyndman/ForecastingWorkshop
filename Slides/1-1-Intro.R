## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ---- echo=FALSE---------------------------------------------------------
autoplot(melsyd[,"Economy.Class"],
  main="Economy class passengers: Melbourne-Sydney",
  xlab="Year",ylab="Thousands")

## ----austa1, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.width=9, fig.height=6----
fit <- ets(austa)
df <- cbind(austa, simulate(fit,10))
for(i in seq(9))
  df <- cbind(df, simulate(fit,10))
colnames(df) <- c("Data", paste("Future",1:10))
autoplot(df) +
  ylim(min(austa),10) +
  ylab("Millions of visitors") + xlab("Year") +
  ggtitle("Total international visitors to Australia") +
 scale_colour_manual(values=c('#000000',rainbow(10)),
                     breaks=c("Data",paste("Future",1:10)),
                     name=" ") +
  ylim(.85,10.0)

## ----austa2, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.width=8.6, fig.height=6----
autoplot(forecast(fit)) +
  ylab("Millions of visitors") + xlab("Year") +
  ggtitle("Forecasts of total international visitors to Australia") +
  ylim(0.85,10.0)

