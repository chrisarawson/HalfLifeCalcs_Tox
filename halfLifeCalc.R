##the above as a single Function
#y0 = starting concentration (micro g/L)
#yt = concentration at time t (micro g/L)
#tHalf = known half-life (days)
#t = total time of test (days)
#rInt = replacement interval (days)
#v0 = Initical volume (L) (default = 1.5L)
#rVol = volume to be replaced (L) (default = 50% replacement)
#rConc = Concentration of replacement volume (default = initial concentration)

halfLifeCalc <- function (y0, tHalf,t, rInt, v0=1.5, rVol=0.5*v0, rConc=y0, lag=0) {
  #Generate x-data
  xPoints <- seq(0,t,length.out=(t*10)+1)
  
  #fit y-data
  decayData <- data.frame(xPoints)
  decayData$yPoints <- exp((-(log(2)/tHalf)*xPoints)+log(y0))
  
  #plot as a line
  plot(decayData$xPoints,decayData$yPoints,type="l",col="black", ylim=c(0,y0), 
       xlab="Time (days)",ylab="Concentration (micro g/L)")
  lines(x=c(0,max(xPoints)),y=c(average.y,average.y),col="red")
  
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
      iyPoint <- (((v0-rVol) * tail(ryPoints,n=1)) + (rVol * rConc))/v0
      ny0 <- iyPoint
      t0=i
    }
    ryPoints <- append(ryPoints,iyPoint)   
  }
  
  decayData$yRepPoints <- ryPoints
  decayData.out <<- decayData
  
  #calculate average concentration during exposure
  average.y <<- mean(decayData$yPoints)
  average.ry <<- mean(decayData$yRepPoints)
  
  lagged.decayData <- subset(decayData.out,xPoints >= lag)
  
  average.ryLag <<-mean(lagged.decayData$yRepPoints)
  
  plot(decayData$xPoints,ryPoints,type="l",xlab="Time (days)",ylab="Concentration (micro g/L)",
       ylim=c(0,max(ryPoints)))
  lines(x=c(0,max(xPoints)),y=c(average.ry,average.ry),col="red")
  lines(x=c(0,max(xPoints)),y=c(average.ryLag,average.ryLag),col="blue")
  legend(-5,-8,xpd=TRUE,c("Mean with no lag","Mean with lag"),col=c("red","blue"),
         lty=c(1,1),bty="n")
  
 
}