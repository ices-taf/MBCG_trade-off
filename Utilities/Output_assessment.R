
###### Figures and tables

  # define directory to load figure and table data products
  pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/")

  # set directory for output
  setwd(paste(pathdir,"5 - Output",sep="/"))  
  dir.create(paste(Assunit), showWarnings = FALSE)
  setwd(paste(pathdir,"5 - Output",Assunit,sep="/"))  
  dir.create(paste(Assregion), showWarnings = FALSE)
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))

##### Figure A.1
  load(paste(pathdir_prodFT,"FigureA1.RData",sep="/"))

  sar    <- (map_plot(figA1,"surface_sar",AssPeriod,purples,Assregion))
  if (p %in% regions_with_impact){
      longevi <- (map_plot(figA1,"medlong",AssPeriod,sealand,Assregion))
  } else { longevi <- ggplot() + theme_void() + geom_text(aes(0,0,label='Not available')) +  xlab(NULL)}
  #e_gva     <- (map_plot(figA1,"total_gva",GVAPeriod,yellowred,Assregion))
  value     <- (map_plot(figA1,"total_value",AssPeriod,yellowred,Assregion))
  weight  <- (map_plot(figA1,"total_weight",AssPeriod,yellowred,Assregion))

  png(paste(Assregion,"figureA1.png",sep="_"),width=12,height=9, units = "in", res = 150 ) 
  print(grid.arrange(sar,longevi,value,weight, nrow = 2))
  dev.off()

##### Figure A.2
  png(paste(Assregion,"figureA2.png",sep="_"),width=6,height=4.5, units = "in", res = 150) 
  print(sar)
  dev.off()

##### Figure A.3
  load(paste(pathdir_prodFT,"FigureA3.RData",sep="/"))

  png(paste(Assregion,"figureA3.png",sep="_"),width=8,height=4, units = "in", res = 150) 
  par(mar=c(5,5,1,1))
  par(mfrow=c(1,3))
  
  #left panel
  ma <- round(max(A3fig[[1]][,1]), digits=1)
  left<-as.data.frame(A3fig[[1]])
  plot(left[,1]~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,ma+0.5),
       ylab=expression(plain("I-1: Average fishing intensity (year")^plain("-1")*")"))
  axis(2,las=1)

  # middle panel
  middle <- as.data.frame(A3fig[[2]])
  middle[,1] <- middle[,1]* 100
  plot(middle[,1]~middle$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,100),
       ylab="I-3: Proportion of area fished",xlab="Year")
  axis(2,seq(0,100,20),las=1)

  # right panel
  right<-as.data.frame(A3fig[[3]])
  right[,1] <- right[,1]* 100
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,100),
       ylab="I-4: Smallest  prop. of c-squares with 90% of effort",xlab="Year")
  axis(2,seq(0,100,20),las=1)
  dev.off()

# Figure A.4
  load(paste(pathdir_prodFT,"FigureA4.RData",sep="/"))
  
  if(is.na(sum(A4dat$sweptcumu))) {A4dat$sweptcumu <- 1}
  if(is.na(sum(A4dat$landcumu))) {A4dat$landcumu <- 1}
  if(is.na(sum(A4dat$valuecumu))) {A4dat$valuecumu <- 1}
  #if(is.na(sum(A4dat$GVAcumu))) {A4dat$GVAcumu <- 1}
  png(paste(Assregion,"figureA4.png",sep="_"),width=5.5,height=4.5, units = "in", res = 150) 
  plot(A4dat$sweptcumu~A4dat$indixcumu, xlab="Surface area \n(grid cells sorted from high to low trawling intensity)",
       ylab="Cumulative proportion",las=1,yaxt="n", lty=1, col="white", type="l")
  lines(x=c(-1,2),y=c(0.2,0.2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.4,0.4),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.6,0.6),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.8,0.8),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.2,0.2),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.4,0.4),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.6,0.6),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.8,0.8),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  
  lines(A4dat$sweptcumu~A4dat$indixcumu, lty=1, col="black",type="l")
  lines(A4dat$landcumu~A4dat$indixcumu, lty=2, col="blue",type="l")
  lines(A4dat$valuecumu~A4dat$indixcumu, lty=3, col="red",type="l")
  #lines(A4dat$GVAcumu~A4dat$indixcumu, lty=4, col="purple",type="l")
  
  legend(0.5,0.8,legend=c("Swept area", "Landings","Value"),
         col=c("black","blue", "red"),lty=1:4, cex=0.8, 
         x.intersp=2,box.lty=0, bg=NULL)
  # legend(0.5,0.8,legend=c("Swept area", "Landings","Value","GVA"),
  #        col=c("black","blue", "red","purple"),lty=1:4, cex=0.8, 
  #        x.intersp=2,box.lty=0, bg=NULL)
  
  axis(2,c(0,0.2,0.4,0.6,0.8,1),las=1)
  dev.off()
  
  if (p %in% regions_with_impact){
##### Figure A.5
  load(paste(pathdir_prodFT,"FigureA5.RData",sep="/"))
  impact_PD <- (map_plot(figA5,"impact",AssPeriod,sealand,Assregion))
  impact_IL <- (map_plot(figA5,"impact_IL",AssPeriod,sealand,Assregion))
  
  png(paste(Assregion,"figureA5.png",sep="_"),width=12,height=4.5, units = "in", res = 150) 
  print(grid.arrange(impact_PD,impact_IL, nrow = 1))
  dev.off()
  
#Figure A.6
  load(paste(pathdir_prodFT,"FigureA6.RData",sep="/"))
  
  png(paste(Assregion,"figureA6.png",sep="_"),width=7,height=7, units = "in", res = 150) 
  par(mfrow=c(2,2),mar=c(4,5,2,2)+0.1)
  
  #left panel
  left<-as.data.frame(A6fig[[1]])
  plot((1-left[,1])~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Average impact (PD)",xlab="Year")
  lines((1-left[,2])~left$Year, col="red", type="o", lty=2)
  lines((1-left[,3])~left$Year, col="blue", type="o", lty=3)
  lines((1-left[,4])~left$Year, col="orange", type="o", lty=4)
  lines((1-left[,5])~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(left[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=0.2,y.intersp = 0.8)
  
  # right panel
  right<-as.data.frame(A6fig[[2]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Prop. of area \n PD impact < 0.2",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  #left panel
  left<-as.data.frame(A6fig[[3]])
  plot((1-left[,1])~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Average impact (PD-sens)",xlab="Year")
  lines((1-left[,2])~left$Year, col="red", type="o", lty=2)
  lines((1-left[,3])~left$Year, col="blue", type="o", lty=3)
  lines((1-left[,4])~left$Year, col="orange", type="o", lty=4)
  lines((1-left[,5])~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  # right panel
  right<-as.data.frame(A6fig[[4]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Prop. of area \n with PD-sens impact < 0.2",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  dev.off()

# Figure A.8
  load(paste(pathdir_prodFT,"FigureA8_A9.RData",sep="/"))
  
  gears2 <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  gears <- gears2[which(gears2 %in% subset(ActiveMetiers, Activity == "Active")$gears)]
  if(length(gears)>0){
    nam <- paste("state",rep(gears[1],length(AssPeriod)),AssPeriod,sep="_")
    Avgear <- data.frame(rowMeans(A8_A9fig[, nam]))
    
    for (q in 2:length(gears)){
      nam <- paste("state",rep(gears[q],length(AssPeriod)),AssPeriod,sep="_")
      Avgear <- cbind(Avgear,data.frame(rowMeans(A8_A9fig[, nam])))
    }
    colnames(Avgear) <- gears
    
    Avgear <- t(Avgear)
    colnames(Avgear) <- A8_A9fig[,1]
    Avgear <- 1-Avgear # get impact
    Avgear <- t(Avgear)
    Avgear_PD <- Avgear
    
    # Figure A.8 - PD-sens
    load(paste(pathdir_prodFT,"FigureA8_A9_IL.RData",sep="/"))
    
    gears2 <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
    gears <- gears2[which(gears2 %in% subset(ActiveMetiers, Activity == "Active")$gears)]
    
    nam <- paste("state",rep(gears[1],length(AssPeriod)),AssPeriod,sep="_")
    Avgear <- data.frame(rowMeans(A8_A9fig[, nam]))
    
    for (q in 2:length(gears)){
      nam <- paste("state",rep(gears[q],length(AssPeriod)),AssPeriod,sep="_")
      Avgear <- cbind(Avgear,data.frame(rowMeans(A8_A9fig[, nam])))
    }
    colnames(Avgear) <- gears
    
    Avgear <- t(Avgear)
    colnames(Avgear) <- A8_A9fig[,1]
    Avgear <- 1-Avgear # get impact
    Avgear <- t(Avgear)
    y_max <- round(max(Avgear)+0.02,digits = 3) # get max for y-axis
    
    png(paste(Assregion,"figureA8.png",sep="_"),width=12,height=9, units = "in", res = 150) 
    par(mfrow=c(2,1),mar=c(4,5,2,2)+0.1)
    
    b<-barplot(Avgear_PD,beside=T,yaxt="n",xaxt="n",ylab="Impact (PD)",ylim=c(0,y_max), xlab="Metier")
    legend(x=max(b)+1,y_max*0.9, as.character(A8_A9fig$MSFD),
           fill = gray.colors(4),bty = "n", xjust=1)
    axis(1,at=b[3,]-0.5,labels=gears, tick=F) 
    axis(2,c(0,y_max/2,y_max),las=1)
    box()
    
    barplot(Avgear,beside=T,yaxt="n",xaxt="n",ylab="Impact (PD-sens)",ylim=c(0,y_max),xlab="Metier")
    #legend(30,0.16, as.character(A8_A9fig$MSFD),
    #       fill = gray.colors(4),bty = "n")
    axis(1,at=b[3,]-0.5,labels=gears, tick=F)
    axis(2,c(0,y_max/2,y_max),las=1)
    box()
    
    dev.off()
    
    
  } # end if gears>0 loop
  }
  
##########
# Table A1
  load(paste(pathdir_prodFT,"TableA1.RData",sep="/"))
  col1 <- c("Average fishing intensity (I-1)", 
            "Proportion of fished c-squares (I-2)", 
            "Proportion of area fished (I-3)", 
            "Smallest  prop. of c-squares with 90% of total fishing intensity (I-4)",
            "Proportion of persistently unfished c-squares (I-5)", 
            "Average PD impact (I-6a)",
            "Average PD-sens impact (I-6b)", 
            "Proportion of c-squares with PD impact < 0.2 (I-7a)", 
            "Proportion of c-squares with PD-sens impact < 0.2 (I-7b)")
  A1table <- round(A1table, digits = 2) 
  A1table <- A1table[,1:3] # remove the below 800m column. (silenced for now)
  A1table <- data.frame(Indicators = col1, values = A1table)
  colnames(A1table) <- c("Indicators","0 to 200 m","200 to 400 m", "400 m to 800 m") 
  #A1table[A1table=="Inf"] <- NA
  write.csv(A1table, file= paste(Assregion,"Table_1.csv",sep="_"), row.names=FALSE)
  
# Table A1SSF
  load(paste0(pathdir, "/1 - Input env/QCtab.Rdata"))
  if(Assunit == "Division"){
    QCT <- QCtab[division == Assregion]}
  if(Assunit == "(sub-)Region"){
    QCT <- QCtab[EcoRegion == EcoReg]}
  QCT <- QCT[,c("FAOregion", "FAOname", "Tot_KWFD", "SSFc", "rangeSSF")]
  QCT <- QCT[!duplicated(QCT),]
  colnames(QCT) <- c("FAO region code", "FAO region name", "Average annual total fishing effort \n(kW * Fishing days)", "Average contribution of SSF (%)", "Observed contribution range of SSF [min - max%]")
  write.csv(QCT, file= paste(Assregion,"Table_2.csv",sep="_"), row.names=FALSE)
  
# Table A2
  load(paste(pathdir_prodFT,"TableA2.RData",sep="/"))
  A2table <- A2table[,c(1:2,11,3:5,7,6,10,8)]
  
  colnames(A2table) <- c("MSFD broad habitat type","Extent of habitat (x1000 km^2^)",
                         "Relative habitat abundance (%)",
                         "Landings (x1000 tonnes)","Value (x10^6^ euro)",
                         "Swept area (x1000 km^2^)","Average fishing intensity (I-1)",
                         "Average annual extent fished (%)", 
                         "Smallest proportion of extent with 90% of fishing effort", 
                         "Percentage extent unfished (%)")
  # colnames(A2table) <- c("MSFD broad habitat type","Extent of habitat (1000 km2)",
  #                        "Landings 1000 tonnes","Value 10^6^ euro","GVA 10^6^ euro",
  #                        "Swept area 1000 km2","Average fishing intensity (I-1)",
  #                        "Prop. of area in fished grid cells (I-2)", "Prop. of area fished per year (I-3)",
  #                        "Smallest  prop. of area with 90% of fishing effort (I-4)")
  
  A2table[,c(2,4:7)] <- round(A2table[,c(2,4:7)], digits = 2)
  
  write.csv(A2table, file= paste(Assregion,"Table_3.csv",sep="_"), row.names=FALSE)
  
# Table A3
  load(paste(pathdir_prodFT,"TableA3.RData",sep="/"))
  A3table[A3table < 0.005 & A3table >0] <- -100
  A3table <- round(A3table, digits = 2)
  new <- A3table %>%
     mutate_all(as.character)
  rownames(new) <- rownames(A3table)
  new[new == "-100"] <- "<0.005"
  A3table <- new
  write.csv(A3table, file= paste(Assregion,"Table_4.csv",sep="_"), row.names=T)
  
  if (p %in% regions_with_impact){

# Table A4
  load(paste(pathdir_prodFT,"TableA4.RData",sep="/"))
  A4table <- round(A4table, digits = 3)
  write.csv(A4table, file= paste(Assregion,"Table_5.csv",sep="_"), row.names=T)
  
  }
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                              'EcoReg_index','GVA','GVAMet','Period','AssPeriod',"GVAPeriod",
                              "EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion",
                              "msfd_csq","regions_with_corefishing", "State_GMP10", "State_GMP20",
                              "State_GMP5", "State_sens_GMP10", "State_sens_GMP20", "State_sens_GMP5",
                              "ActiveMetiers"))])  