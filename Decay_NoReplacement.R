##plot general relationship with no replacement.

#Inputs
#y0 = starting concentration (micro g/L)
#yt = concentration at time t (micro g/L)
#tHalf = known half-life (days)
#t = total time of test (days)

y0 <- 50
tHalf <- 14
t <- 30

#Generate x-data
xPoints <- seq(0,t,length.out=(t*10)+1)

#fit y-data
decayData <- data.frame(xPoints)
decayData$yPoints <- exp((-(log(2)/tHalf)*xPoints)+log(y0))

#plot as points and/or lines
plot(decayData$xPoints,decayData$yPoints,type="l",col="black", ylim=c(0,y0), 
     xlab="Time (days)",ylab="Concentration (micro g/L)")