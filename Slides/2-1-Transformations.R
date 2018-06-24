## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
source("nicefigs.R")
options(digits=4, width=55)

## ---- echo=FALSE, fig.height=4.6-----------------------------------------
autoplot(elec) +
  xlab("Year") + ylab("") +
  ggtitle("Electricity production")

## ---- echo=FALSE, fig.height=4.6-----------------------------------------
autoplot(elec^0.5) +
  xlab("Year") + ylab("") +
  ggtitle("Square root electricity production")

## ---- echo=FALSE, fig.height=4.6-----------------------------------------
autoplot(elec^0.33333) +
  xlab("Year") + ylab("") +
  ggtitle("Cube root electricity production")

## ---- echo=FALSE, fig.height=4.6-----------------------------------------
autoplot(log(elec)) +
  xlab("Year") + ylab("") +
  ggtitle("Log electricity production")

## ---- echo=FALSE, fig.height=4.6-----------------------------------------
autoplot(-1/elec) +
  xlab("Year") + ylab("") +
  ggtitle("Inverse electricity production")

## ----elec5, cache=TRUE, echo=FALSE---------------------------------------
library(latex2exp)
lambda <- seq(1, -1, by=-0.01)
for(i in seq_along(lambda))
{
  savepdf(paste("elecBC",i,sep=""))
  print(autoplot(BoxCox(elec,lambda[i])) + xlab("Year") +
    ylab("") +
    ggtitle(
      TeX(paste("Transformed Australian electricity demand:  $\\lambda =",format(lambda[i],digits=2,nsmall=2),"$"))
    ) +
    scale_y_continuous(breaks=NULL,minor_breaks=NULL) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()))
  endpdf()
}

## ----elec6,echo=TRUE,fig.height=4----------------------------------------
autoplot(BoxCox(elec,lambda=1/3))

## ---- echo=TRUE----------------------------------------------------------
(BoxCox.lambda(elec))

## ----echo=TRUE,fig.height=3.6--------------------------------------------
fit <- snaive(elec, lambda=1/3)
autoplot(fit)

## ----echo=TRUE,fig.height=4----------------------------------------------
autoplot(fit, include=120)

## ----echo=TRUE,fig.height=3.7--------------------------------------------
elec %>% snaive(lambda=1/3, biasadj=FALSE) %>%
  autoplot(include=120)

## ----echo=TRUE,fig.height=3.7--------------------------------------------
elec %>% snaive(lambda=1/3, biasadj=TRUE) %>%
  autoplot(include=120)

## ----echo=TRUE,fig.height=3.7--------------------------------------------
eggs %>% ses(lambda=1/3, biasadj=FALSE) %>%
  autoplot

## ----echo=TRUE,fig.height=3.7--------------------------------------------
eggs %>% ses(lambda=1/3, biasadj=TRUE) %>%
  autoplot

