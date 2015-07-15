##A function that calculates concentration with solute decay and replacement
##with non-dosed water then re-dosing with a concentrate

#y0 = starting concentration (micro g/L)
#yt = concentration at time t (micro g/L)
#tHalf = known half-life (days)
#t = total time of test (days)
#rInt = replacement interval (days)
#v0 = Initical volume (L) (default = 1.5L)
#rVol = volume to be replaced (L) (default = 50% replacement)
#dConc = Concentration of dose
#dVol = volume of dose (default = 1ml)
#lag = days before addition of animals

HLrepBeforeDose <- function (y0, tHalf,t, rInt, v0=1.5, rVol=0.5*v0, dConc, dVol, lag=0) {
  #Generate x-data
  xPoints <- seq(0,t,length.out=(t*10)+1)
  
  #fit y-data and calculate average y
  decayData <- data.frame(xPoints)
  decayData$yPoints <- exp((-(log(2)/tHalf)*xPoints)+log(y0))
  average.y <<- mean(decayData$yPoints)
  
  #plot as a line
  plot(decayData$xPoints,decayData$yPoints,type="l",col="black", ylim=c(0,y0), 
       xlab="Time (days)",ylab="Concentration (micro g/L)")
  lines(x=c(0,max(xPoints)),y=c(average.y,average.y),col="red",lty=2)
  
  #Generate sequence of replacement days
  rDays <- seq(0,t,by = rInt)
  rDays <- rDays[-1]
  
  #Calculate vector with conc at each xPoint where if xPoint is in rDays replacement added.
  
  ny0 <- y0
  ryPoints <- c()
  t0 <- 0
  
  for (i in decayData$xPoints) {
    if (!(i %in% rDays)) {
      iyPoint <- exp((-(log(2)/tHalf)*(i-t0))+log(ny0))
    }
    else {
      yBeforeRep <- exp((-(log(2)/tHalf)*(i-t0))+log(ny0))
      yAfterRep <- (yBeforeRep * (v0 - rVol)) / v0
      iyPoint <- (yAfterRep * v0) + (dConc * dVol)
      ny0 <- iyPoint
      t0=i
    }
    ryPoints <- append(ryPoints,iyPoint)   
  }
  
  decayData$yRepPoints <- ryPoints
  decayData.out <<- decayData
  
  #calculate average concentration during exposure
  
  average.ry <<- mean(decayData$yRepPoints)
  lagged.decayData <- subset(decayData.out,xPoints >= lag)
  average.ryLag <<-mean(lagged.decayData$yRepPoints)
  
  plot(decayData$xPoints,ryPoints,type="l",xlab="Time (days)",ylab="Concentration (micro g/L)",
       ylim=c(0,max(ryPoints)))
  lines(x=c(0,max(xPoints)),y=c(average.ry,average.ry),col="red",lty=2)
  lines(x=c(lag,max(xPoints)),y=c(average.ryLag,average.ryLag),col="blue",lty=2)
  legend("bottomleft",xpd=TRUE,c("Mean with no lag","Mean with lag"),col=c("red","blue"),
         lty=c(2,2),bty="n")
  
  
}