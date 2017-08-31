# Create GIF
# Circa 2016
# By https://chrisdienes.github.io/

# ---------------- #
# Load Stuff
# ---------------- #

load(file="C:/Users/Chris/Desktop/runBayes/myResults.RDA")
myPath = "C:/Users/Chris/Desktop/runBayes/"
myData = read.csv(file=paste0(myPath,"myData.csv"), header=TRUE, stringsAsFactors = FALSE)
myData$Total = cumsum(myData$Value)
myData$Goal  = (1000/52)*(1:52)
myData$GoalDiff = myData$Total - myData$Goal
myData$DateOfInterest = ifelse(((1:52) %% 5 == 0) ,1,0)
library(fanplot)
library(animation)

sum(myData$Value,na.rm=TRUE)
sum(myData$TM,na.rm=TRUE)
100 - 100*sum(myData$TM,na.rm=TRUE)/sum(myData$Value,na.rm=TRUE)

# ---------------- #
# Make Plot
# ---------------- #
N = 52
makeplot <- function(){
  for(K in -9:(N+9)){
    if(K <=0){
      plot(0, 0, type="l", xlim=c(0,52), xaxt = "n",
           xlab="Week", ylab = "Cumulative Total", 
           ylim = c(0,1500),
           main=paste0(format(round(100*mean(hold[[1]][,52] >= 1000),digits=0),digits=1, nsmall=1), "% Chance of Reaching Goal") )
      axis(side = 1, at = c(0,52))
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue")
      lines(c(0,1),c(0,20), type="l",lwd=2)
      fan(hold[[1]][,1:52], start=1, alpha=1)
      abline(h = 1000, lty="dotted")
    }
    if(K < N & K > 0){
      plot(0:K, c(0,myData$Total[1:K]), type="l", xlim=c(0,52), xaxt = "n",
           xlab="Week", ylab = "Cumulative Total", 
           ylim = c(0,1500),
           main=paste0(format(100*mean(hold[[K+1]][,52] >= 1000),digits=1, nsmall=1), "% Chance of Reaching Goal") )
      axis(side = 1, at = c(K,52))
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue")
      lines(0:K, c(0,myData$Total[1:K]), type="l", lwd=2)
      fan(hold[[K+1]][,K:52], start=K, alpha=1)
      abline(h = 1000, lty="dotted")
    }
    if(K >= N){
      plot(0:N, c(0,myData$Total[1:N]), type="l", xlim=c(0,52), xaxt = "n",
           xlab="Week", ylab = "Cumulative Total", 
           ylim = c(0,1500),
           main="100.0% Chance of Reaching Goal")
      axis(side = 1, at = c(N,52))
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue")
      lines(0:N, c(0,myData$Total[1:N]), type="l", lwd=2)
      #fan(hold[[N+1]][,N:52], start=N, alpha=1)
      abline(h = 1000, lty="dotted")
    }
  }
}
# ---------------- #
# Save Stuff
# ---------------- #
setwd("C:/Users/Chris/Desktop/")
oopt = ani.options(interval = 0.33, nmax = N+11)
saveGIF(makeplot(),movie.name = "runBayes.gif")
ani.options(oopt)
