mobilityAnalyzeNCurve = function(fn,select){  
  # Script to analyze SCLC mobility
  rm(list=ls())
  # A25_V1
  # Import data
  rawInput <- read.csv(file=fn)
  colnames(rawInput) <- c("Volts","Amps")
  
  # break curve into 4 parts: pos/up, pos/down, neg/down, neg/up
  high <- which(rawInput$Volts==max(rawInput$Volts))
  low <- which(rawInput$Volts==min(rawInput$Volts))
  midZero <- which(rawInput$Volts==min(abs(rawInput$Volts)))
  zeroToMax <- rawInput[1:high,]
  maxToZero <- rawInput[high:midZero[2],]
  zeroToMin <- -rawInput[midZero[2]:low,]
  minToZero <- -rawInput[low:length(rawInput[,1]),]
  
  # select which data to use for analysis
  if (select == 1){
    data <- zeroToMax
  } else if (select == 3){
    data <- zeroToMin
  }
    # plot data
  plot(data$Volts,data$Amps)
  x <- data$Volts
  y <- data$Amps
  z <- nls(y ~ a*x^b, start = list(a=1,b=4), algorithm = "port", trace=F,
           upper = list(100,100), lower = list(0,0), control=nls.control(maxiter=150))
  lines(x,coefficients(z)[1]*x^(coefficients(z)[2]),col="red")
  # print(max(data$Amps))
  return(coefficients(z)[2])
}

mobilityAnalyzeTotal = function(fn,select,exclVLowx,exclVHix){  
  # Script to analyze SCLC mobility
  rm(list=ls())
  # A25_V1
  # Import data
  rawInput <- read.csv(file=fn)
  colnames(rawInput) <- c("Volts","Amps")
  
  # break curve into 4 parts: pos/up, pos/down, neg/down, neg/up
  high <- which(rawInput$Volts==max(rawInput$Volts))
  low <- which(rawInput$Volts==min(rawInput$Volts))
  midZero <- which(rawInput$Volts==min(abs(rawInput$Volts)))
  zeroToMax <- rawInput[1:high,]
  maxToZero <- rawInput[high:midZero[2],]
  zeroToMin <- -rawInput[midZero[2]:low,]
  minToZero <- -rawInput[low:length(rawInput[,1]),]
  
  # select which data to use for analysis
  if (select == 1){
    data <- zeroToMax
  } else if (select == 3){
    data <- zeroToMin
  }
  # plot data
  # plot(data$Volts,data$Amps)
  x <- data$Volts
  y <- data$Amps
  z <- nls(y ~ a*x^b, start = list(a=1,b=4), algorithm = "port", trace=F,
           upper = list(100,100), lower = list(0,0), control=nls.control(maxiter=150))
  # lines(x,coefficients(z)[1]*x^(coefficients(z)[2]),col="red")
  # print(max(data$Amps))
  
  # plot log-log data
  # plot(log(abs(data$Volts[2:length(data$Volts)])),log(abs(data$Amps[2:length(data$Amps)])))
  # linearMod1 <- lm(log(abs(data$Amps[2:length(data$Amps)])) ~ log(abs(data$Volts[2:length(data$Volts)])))
  # print(linearMod1)
  # abline(a=linearMod1$coefficients[1],b=linearMod1$coefficients[2])
  # 
  # reject data with series resistance
  highCutoff <- 20
  cleanData <- data[which(data$Volts<highCutoff),]
  
  # replot data
  # plot(data$Volts,data$Amps)
  # points(cleanData$Volts,cleanData$Amps,col="blue")
  # 
  # # replot log-log data
  # plot(log10(abs(data$Volts)),log10(abs(data$Amps)))
  # points(log10(abs(cleanData$Volts)),log10(abs(cleanData$Amps)),col="blue")
  # 
  # correct for series resistance:
  Rseries <- 0
  Vint <- cleanData$Volts - Rseries*cleanData$Amps
  data2 <- cbind(cleanData,Vint)
  
  # replot data
  # plot(data$Volts,data$Amps)
  # points(cleanData$Volts,cleanData$Amps,col="brown")
  # points(data2$Vint,data2$Amps,col="blue")
  # 
  # # replot log-log data
  # plot(log(abs(data$Volts)),log(abs(data$Amps)),ylim=c(-20,-2))
  # points(log(abs(cleanData$Volts)),log(abs(cleanData$Amps)),col="brown")
  # points(log(abs(data2$Vint)),log(abs(data2$Amps)),col="blue")
  A=.04
  Vbi <- 0
  Vint2 <- cleanData$Volts - Rseries*cleanData$Amps - Vbi
  # replot log-log data
  plot(log10(abs(data$Volts)),log10(abs(data$Amps/A)),axes=F)
  xaxlist = log10(c(seq(0.01,0.1,by=.01),seq(0.2,1,by=.1),seq(2,10,by=1)))
  xaxl2 = c(rep("",9),"01",rep("",8),"1",rep("",8),"10")
  axis(1,pos=-7,at=xaxlist,labels=xaxl2)
  yaxlist = log10(c(seq(0.001,0.01,by=.001),seq(0.02,0.1,by=0.01)))
  yaxl2 = c("1mA",rep("",8),rep("",9),"10")
  axis(2,pos=log10(.01),at=yaxlist,labels=yaxl2)
  abline(v=log10(1))
  abline(v=log10(10))
  abline(h=log10(.001),col="blue")
  abline(h=log10(.01))
  abline(h=log10(.1))
  points(log10(abs(cleanData$Volts)),log10(abs(cleanData$Amps/A)),col="brown")
  points(log10(abs(Vint2)),log10(abs(data2$Amps/A)),col="blue")
  
  xxs <- Vint2[which(Vint2>exclVLowx)]
  xxxs <- xxs[which(xxs<exclVHix)]
  yys <- data2[which(Vint2>exclVLowx),]$Amps
  yyys <- yys[which(xxs<exclVHix)]
  data3 <- cbind(xxxs,yyys)
  
  colnames(data3) <- c("Vint2","Amps")
  
  xs <- log10(abs(data3[,1]))
  xs <- xs[-1]
  ys <- log10(abs(data3[,2]))
  ys <- ys[-1]
  points(log10(abs(data3[,1])),log10(abs(data3[,2]/A)),col="red")
  
  Murg <- function(mu, Vinternal, gamma){
    A <- 0.04
    perm <- 3.5
    d <- 1200e-8
    perm0 <- 8.854187e-14
    m <- log10((A*mu*(9/8)*perm*perm0)/(d**3)) + 2*((Vinternal))
    n <- ((0.387*gamma)/(sqrt(d)))*(10**(0.5*((Vinternal))))
    #n <- ((0.387*gamma)/(sqrt(d)))*(sqrt(10**Vinternal))
    return(m+n)}
  
  m <- nls(ys ~ Murg(mu1,xs,gm), algorithm = "port", start = list(mu1=10e-6,gm=2e-3), trace=T,
           upper = list(10e-1,5e-1), lower = list(10e-20,10e-6), control=nls.control(maxiter=150,warnOnly = T))
  m <- nls(ys ~ Murg(mu1,xs,gm), algorithm = "port", start = list(mu1=coefficients(m)[1],gm=coefficients(m)[2]), trace=T,
           upper = list(10e-1,5e-1), lower = list(10e-20,10e-6), control=nls.control(maxiter=150,warnOnly = T))
  
  A <- .04 # cm^2
  currDensity <- 1000*data3[,2]/A # A/cm^2
  voltData <- data3[,1] # Volts
  d <- 1200e-8 # cm (1200 Angstroms)
  yData <- log((currDensity)/((voltData/d)**2))
  xData <- sqrt(voltData/d)
  #plot(xData,yData)
  linearMod <- lm(yData ~ xData)
  abline(a=linearMod$coefficients[1],b=linearMod$coefficients[2])
  print(linearMod)
  perm0 <- 8.854187e-14 # F/cm
  perm <- 3.5
  muFit <- (8/9)*(d/(perm*perm0))*(exp(linearMod$coefficients[1])) # cm^2/V*s
  gammaFit <- linearMod$coefficients[2]
  print(muFit)
  print(gammaFit)
  return(c(strsplit(fn[1],"/")[[1]][length(strsplit(fn[1],"/")[[1]])],select,exclVLowx,exclVHix,muFit,gammaFit))
}
  # lines(xs,Murg(coefficients(m)[1],xs,coefficients(m)[2]),col="blue",lwd=2)
  # 
  # plot(xs,ys)
  # 
  # lines(log10(abs(xs)),log10(abs(Murg(coefficients(m)[1],xs,coefficients(m)[2]))),col="blue",lwd=2)