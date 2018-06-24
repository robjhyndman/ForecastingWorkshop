## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ---- echo=FALSE---------------------------------------------------------
autoplot(dj) + ylab("Dow Jones Index") + xlab("Day")

## ---- echo=FALSE---------------------------------------------------------
autoplot(diff(dj)) + ylab("Change in Dow Jones Index") + xlab("Day")

## ---- echo=FALSE---------------------------------------------------------
autoplot(strikes) + ylab("Number of strikes") + xlab("Year")

## ---- echo=FALSE---------------------------------------------------------
autoplot(hsales) + xlab("Year") + ylab("Total sales") +
  ggtitle("Sales of new one-family houses, USA")

## ---- echo=FALSE---------------------------------------------------------
autoplot(eggs) + xlab("Year") + ylab("$") +
  ggtitle("Price of a dozen eggs in 1993 dollars")

## ---- echo=FALSE---------------------------------------------------------
autoplot(window(pigs/1e3, start=1990)) + xlab("Year") + ylab("thousands") +
  ggtitle("Number of pigs slaughtered in Victoria")

## ---- echo=FALSE---------------------------------------------------------
autoplot(lynx) + xlab("Year") + ylab("Number trapped") +
  ggtitle("Annual Canadian Lynx Trappings")

## ---- echo=FALSE---------------------------------------------------------
autoplot(window(ausbeer, start=1992)) + xlab("Year") + ylab("megalitres") +
  ggtitle("Australian quarterly beer production")

## ------------------------------------------------------------------------
autoplot(dj) + ylab("Dow Jones Index") + xlab("Day")

## ------------------------------------------------------------------------
ggAcf(dj)

## ------------------------------------------------------------------------
autoplot(diff(dj)) +
  ylab("Change in Dow Jones Index") + xlab("Day")

## ------------------------------------------------------------------------
ggAcf(diff(dj))

## ---- echo=TRUE, fig.height=4--------------------------------------------
usmelec %>% autoplot()

## ---- echo=TRUE, fig.height=4--------------------------------------------
usmelec %>% log() %>% autoplot()

## ---- echo=TRUE, fig.height=3.5------------------------------------------
usmelec %>% log() %>% diff(lag=12) %>%
  autoplot()

## ---- echo=TRUE, fig.height=3.5------------------------------------------
usmelec %>% log() %>% diff(lag=12) %>%
  diff(lag=1) %>% autoplot()

## ---- echo=TRUE----------------------------------------------------------
library(urca)
summary(ur.kpss(goog))

## ---- echo=TRUE----------------------------------------------------------
ndiffs(goog)

## ---- echo=TRUE----------------------------------------------------------
usmelec %>% log() %>% nsdiffs()
usmelec %>% log() %>% diff(lag=12) %>% ndiffs()

