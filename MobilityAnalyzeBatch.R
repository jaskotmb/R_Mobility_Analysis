source("Mobility_Analyze_function.R")
dir = "C:/Users/jasko/IdeaProjects/OLEDPowerMeter/Mobility_MeasMUX/1806_DepRateStudy_mCBP"
dirNew = readline(paste("Current Dir is:",dir,"\n","    Press q to change, enter to continue"))
if(dirNew =='q'){
  newDir = readline("Directory Name: ")
  dir = newDir
}
print("Selected Directory: ",end='')
print(dir)
readline()
files = list.files(dir)
print(files)
print("Files:",end='')
print(length(files))
v2filesyn = readline("Select only files containing certain string? (y/n): ")
if(v2filesyn == 'y'){
  vstring = readline("String: ")
  files = files[grep(vstring,files)]
}
print(files)
print(paste("Files:",length(files)))
totalData=matrix(0,nrow=2*length(files),ncol=6)
for(i in seq(1,length(files))){
  for(select in c(1,3)){
    if(select==1){
      side='+'
    }
    if(select==3){
      side='-'
    }
    fn = paste(dir,files[i],sep='/')
    print(paste("Current File:",files[i],side,i,"of",length(files)))
    # Plot data, show "n"
    nval = mobilityAnalyzeNCurve(files[i],side)
    print(nval)
    
    volts=c(3,8)
    readline("Press Enter to continue to mobility fitting")
    # Plot fit, show parameters Vlow, Vhi, mobility, gamma
    exclVLowx = as.numeric(volts[1])
    exclVHix = as.numeric(volts[2])
    dataOut = mobilityAnalyzeTotal(files[i],1,as.numeric(volts[1]),as.numeric(volts[2]))
    input1 = readline("Press q to modify parameters, enter to continue ")
    # replot if changed
    while(input1=='q'){
      low = readline("New V_low = ")
      high = readline("New V_hi = ")
      volts = c(low,high)
      exclVLowx = as.numeric(volts[1])
      exclVHix = as.numeric(volts[2])
      dataOut = mobilityAnalyzeTotal(files[i],1,as.numeric(volts[1]),as.numeric(volts[2]))
      input1 = readline("Press q to modify parameters, enter to continue ")
    }
    dataOut = unname(dataOut)
    print(dataOut)
    
    # Accept data?
    accept = readline("Accept Data? (y/n): ")
    if(accept != 'n'){
      totalData[i+((select-1)/2),] = dataOut
    }
  }
}

write.csv(totalData,paste(dir,"/Mobility_Fits.csv",sep=''))
