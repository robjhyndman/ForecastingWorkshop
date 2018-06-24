## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
source("nicefigs.R")
options(digits=4, width=55)

## ----tstable, echo=FALSE, cache=TRUE-------------------------------------
x <- c(123,39,78,52,110)
yr <- 2012:2016
knitr::kable(data.frame(Year=yr,Observation=x), booktabs=TRUE)

## ---- echo=TRUE----------------------------------------------------------
ausgdp

## ---- echo=TRUE, fig.height=4--------------------------------------------
autoplot(ausgdp)

## ------------------------------------------------------------------------
elecsales

## ---- echo=TRUE, fig.height=4--------------------------------------------
autoplot(melsyd[,"Economy.Class"])

## ----a10, echo=TRUE------------------------------------------------------
autoplot(a10) + ylab("$ million") + xlab("Year") +
         ggtitle("Antidiabetic drug sales")

## ---- echo=TRUE, fig.height=3.5------------------------------------------
ggseasonplot(a10, ylab="$ million",
  year.labels=TRUE, year.labels.left=TRUE) +
  ggtitle("Seasonal plot: antidiabetic drug sales")

## ---- fig.height=5, out.width="7.5cm"------------------------------------
ggseasonplot(a10, polar=TRUE) + ylab("$ million")

## ---- echo=TRUE, fig.height=3.5------------------------------------------
ggsubseriesplot(a10) + ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

## ------------------------------------------------------------------------
beer <- window(ausbeer,start=1992)
autoplot(beer)

## ------------------------------------------------------------------------
ggseasonplot(beer,year.labels=TRUE)

## ------------------------------------------------------------------------
ggsubseriesplot(beer)

## ---- fig.height=3.9-----------------------------------------------------
autoplot(window(elec, start=1980)) +
  ggtitle("Australian electricity production") +
  xlab("Year") + ylab("GWh")

## ---- fig.height=3.9-----------------------------------------------------
autoplot(bricksq) +
  ggtitle("Australian clay brick production") +
  xlab("Year") + ylab("million units")

## ---- fig.height=3.9-----------------------------------------------------
autoplot(hsales) +
  ggtitle("Sales of new one-family houses, USA") +
  xlab("Year") + ylab("Total sales")

## ---- fig.height=3.9-----------------------------------------------------
autoplot(ustreas) +
  ggtitle("US Treasury Bill Contracts") +
  xlab("Day") + ylab("price")

## ---- fig.height=3.9-----------------------------------------------------
autoplot(lynx) +
  ggtitle("Annual Canadian Lynx Trappings") +
  xlab("Year") + ylab("Number trapped")

## ---- echo=FALSE, fig.height=6, fig.width=6, out.width="8cm"-------------
beer <- window(ausbeer, start=1992)
gglagplot(beer, lags=9, do.lines=FALSE, continuous=FALSE)

## ---- echo=FALSE---------------------------------------------------------
beeracf <- matrix(acf(c(beer), lag.max=9,
                      plot=FALSE)$acf[-1,,1], nrow=1)
colnames(beeracf) <- paste("$r_",1:9,"$",sep="")
knitr::kable(beeracf, booktabs=TRUE,
             align="c", digits=3,
             format.args=list(nsmall=3))

## ----beeracf, fig.height=2.5---------------------------------------------
ggAcf(beer)

## ---- fig.height=4-------------------------------------------------------
elec2 <- window(elec, start=1980)
autoplot(elec2)

## ------------------------------------------------------------------------
ggAcf(elec2, lag.max=48)

## ------------------------------------------------------------------------
autoplot(goog)

## ------------------------------------------------------------------------
ggAcf(goog, lag.max=100)

## ---- fig.height=6, fig.width=12, echo=FALSE, out.width="11.7cm"---------
tp1 <- autoplot(cowtemp) + xlab("") + ylab("chirps per minute") +
  ggtitle("1. Daily temperature of cow")
tp2 <- autoplot(USAccDeaths/1e3) + xlab("") + ylab("thousands") +
  ggtitle("2. Monthly accidental deaths")
tp3 <- autoplot(AirPassengers) + xlab("") + ylab("thousands") +
  ggtitle("3. Monthly air passengers")
tp4 <- autoplot(mink/1e3) + xlab("") + ylab("thousands") +
  ggtitle("4. Annual mink trappings")
acfb <- ggAcf(cowtemp, ci=0) + xlab("") + ggtitle("B") + ylim(-0.4,1)
acfa <- ggAcf(USAccDeaths, ci=0) + xlab("") + ggtitle("A") + ylim(-0.4,1)
acfd <- ggAcf(AirPassengers, ci=0) + xlab("") + ggtitle("D") + ylim(-0.4,1)
acfc <- ggAcf(mink, ci=0) + xlab("") + ggtitle("C") + ylim(-0.4,1)
gridExtra::grid.arrange(tp1,tp2,tp3,tp4,
                        acfa,acfb,acfc,acfd,nrow=2)

## ---- fig.height=3.5-----------------------------------------------------
wn <- ts(rnorm(36))
autoplot(wn)

## ---- results='asis', echo=FALSE-----------------------------------------
wnacf <- matrix(acf(c(wn), lag.max=10,
                      plot=FALSE)$acf[-1,,1], nrow=1)
colnames(wnacf) <- paste("$r_{",1:10,"}$",sep="")
print(xtable::xtable(t(wnacf)),
    sanitize.rownames.function=identity,
    booktabs=TRUE,
    include.colnames = FALSE,
    hline.after = FALSE,
    size='footnotesize',
    comment=FALSE,
    floating=FALSE)

## ---- echo=FALSE---------------------------------------------------------
savepdf("wnacf")
ggAcf(wn, max.lag=15)
endpdf()

## ---- fig.height=3.5-----------------------------------------------------
pigs2 <- window(pigs, start=1990)
autoplot(pigs2) +
  xlab("Year") + ylab("thousands") +
  ggtitle("Number of pigs slaughtered in Victoria")

## ------------------------------------------------------------------------
ggAcf(pigs2)

