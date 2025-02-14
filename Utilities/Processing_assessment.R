
###### DATA PROCESSING

###### figures and tables for ICES advice
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")
  #GVA_year   <- paste("total_gva",GVAPeriod,sep="_")
  
  setwd(pathdir_nogit)  
  dir.create("Producing figures and tables", showWarnings = FALSE)
  setwd(paste(pathdir_nogit,"Producing figures and tables",sep="/"))  
  dir.create(paste(Assunit), showWarnings = FALSE)
  setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,sep="/"))  
  dir.create(paste(Assregion), showWarnings = FALSE)
  setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/"))  
  
  ## Select for depths < 800m and >0m
  Region <- subset(Region, Region@data$depthclass %in% c("3. 400 - 800m", "2. 200 - 400m", "1. 0 - 200m"))

  # Define active metiers in this region (> unique csquares)
  ActiveMetiers <- data.frame(gears= c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL"),
                      nrCsquares = NA)
  
  for(iGear in unique(ActiveMetiers$gears)){
    AcMet <- Region@data
    nam <- paste(rep(paste(iGear,"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    AcMet <- cbind(AcMet, FisheriesMet[match(AcMet$csquares,FisheriesMet$csquares), c(nam)])
    AcMet[,c(nam)][is.na(AcMet[,c(nam)])] <- 0
    AcMet$avgsar <- rowMeans(AcMet[,nam])
    AcMet <- subset(AcMet, avgsar > 0)
    ActiveMetiers[ActiveMetiers$gears == iGear,]$nrCsquares <- length(unique(AcMet$csquares))
  }
  ActiveMetiers$Activity <- ifelse(ActiveMetiers$nrCsquares > 50, "Active", "not-active")

  
  
#####
# Figure A.1
################
  figA1 <- Region@data
  nam <- c(SSAR_year,weight_year,value_year)
  figA1 <- cbind(figA1, Fisheries[match(figA1$csquares,Fisheries$csquares), c(nam)])
  #figA1 <- cbind(figA1, GVA[match(figA1$csquares,GVA$csquares), c(GVA_year)])
  #figA1[,GVA_year] <- figA1[,GVA_year]+0.001 # add small value to distinguish from NAs, only for map
  save(figA1, file="FigureA1.RData")

#####
# Table A.1
################
  TA1dat_all <- Region@data
  
  # estimate four depth boundaries
  mindepth <- c(0, -200, -400, -800)
  maxdepth <- c(-200, -400, -800, -8000)
  
  A1table <- (matrix(data="NP", ncol=3,nrow=9))
  
  for(iDepth in 1:3){
    TA1dat <-  subset(TA1dat_all,TA1dat_all$Depth < mindepth[iDepth] & TA1dat_all$Depth >= maxdepth[iDepth])
    if(nrow(TA1dat) >0){
      nam <- c(SSAR_year)
      TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(nam)])
      TA1dat[,c(nam)][is.na(TA1dat[,c(nam)])] <- 0
      TA1dat$avgsar <- rowMeans(TA1dat[,SSAR_year]) 
  
    # indicator 1 intensity
      TA1dat$sweptarea <- TA1dat[,"avgsar"]*TA1dat$area_sqkm
      ind1 <- as.character(round(sum(TA1dat$sweptarea,na.rm=T)/sum(TA1dat$area_sqkm), digits=2))
    
    # indicator 2 proportion of area within fished grid cells (fished irrespective of swept area)
      ind2 <-  ifelse(TA1dat[,"avgsar"] > 0,1,0)
      ind2 <- as.character(round(sum(ind2 * TA1dat$area_sqkm)/sum(TA1dat$area_sqkm) * 100, digits=1))
      
    # indicator 3 proportion of area swept each year
      TA1dat$sweptarea2 <- TA1dat$sweptarea
      TA1dat$sweptarea2 <- ifelse(TA1dat$sweptarea > TA1dat$area_sqkm,TA1dat$area_sqkm,TA1dat$sweptarea)
      ind3 <- as.character(round(sum(TA1dat$sweptarea2,na.rm=T)/sum(TA1dat$area_sqkm) * 100, digits=1))
    
    # indicator 4 aggregation of fishing pressure (extent based!)
      if(sum(TA1dat$sweptarea >0)){
      TA1dat <- TA1dat[order(TA1dat[,"sweptarea"],decreasing = T),]
      TA1dat$cumSSAR <- cumsum(TA1dat[,"sweptarea"])
      TA1dat$cumSSAR <- TA1dat$cumSSAR / sum(TA1dat[,"sweptarea"])
      Corefishing <- TA1dat[which(TA1dat$cumSSAR <= 0.9),]
      ind4 <- as.character(round(sum(Corefishing$area_sqkm)/sum(TA1dat$area_sqkm)*100, digits=1))
      } else {ind4 <- "NA"}
      
    # indicator 5 proportion of area within persistently unfished grid cells
      ind5 <- as.character(round(100-as.numeric(ind2), digits=1))
      
    # all areas without impact prediction
      ind6_PD <- NA; ind6_IL<- NA; ind7_PD <-NA; ind7_IL <- NA
      
      if (p %in% regions_with_impact){
      # indicator 6 average impact - PD model
        nam <- c(state_year)
        TA1dat_PD <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(nam)])
        TA1dat_PD[,c(nam)][is.na(TA1dat_PD[,c(nam)])] <- 1
        TA1dat_PD$avgstate <- rowMeans(TA1dat_PD[,state_year]) 
        TA1dat_PD$avgstate_weight <- TA1dat_PD$avgstate*TA1dat_PD$area_sqkm
        ind6_PD <- as.character(round(100- (sum(TA1dat_PD$avgstate_weight,na.rm=T)/sum(TA1dat_PD$area_sqkm)*100), digits=1))
      
      # indicator 7 proportion of area with impact < 0.2 - PD model
        ind7_PD <-  ifelse(TA1dat_PD[,"avgstate"] >= 0.8,1,0)
        ind7_PD <- as.character(round(sum(ind7_PD * TA1dat_PD$area_sqkm)/sum(TA1dat_PD$area_sqkm) * 100, digits=1))
        
      # indicator 6 average impact - IL model
        nam <- c(state_year)
        TA1dat_IL <- cbind(TA1dat, State_reg_IL[match(TA1dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
        TA1dat_IL[,c(nam)][is.na(TA1dat_IL[,c(nam)])] <- 1
        TA1dat_IL$avgstate <- rowMeans(TA1dat_IL[,state_year]) 
        TA1dat_IL$avgstate_weight <- TA1dat_IL$avgstate*TA1dat_IL$area_sqkm
        ind6_IL <- as.character(round(100- (sum(TA1dat_IL$avgstate_weight,na.rm=T)/sum(TA1dat_IL$area_sqkm)*100), digits=1))
        
        # indicator 7 proportion of area with impact < 0.2 - IL model
        ind7_IL <-  ifelse(TA1dat_IL[,"avgstate"] >= 0.8,1,0)
        ind7_IL <- as.character(round(sum(ind7_IL * TA1dat_IL$area_sqkm)/sum(TA1dat_IL$area_sqkm)*100, digits=1))
        }
    A1table[,iDepth] <- c(ind1,ind2,ind3,ind4,ind5,ind6_PD,ind6_IL,ind7_PD,ind7_IL)
    }}
  
  save(A1table, file="TableA1.RData")

#----------------------

#####
# Table A.2
################
  TA2dat <-  Region@data
  
  # account for area of MSFD habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  TA2dat <- merge(TA2dat,msfd_csq_new,by = "csquares", all.x =T)
  TA2dat$MSFD <- as.character(TA2dat$MSFD)
  TA2dat$MSFD[TA2dat$MSFD=="Na"]= "Unknown"
  TA2dat$grid <- 1
  
  nam <- c(SSAR_year,weight_year,value_year)
  TA2dat <- cbind(TA2dat, Fisheries[match(TA2dat$csquares,Fisheries$csquares), c(nam)])
  TA2dat[,c(nam)][is.na(TA2dat[,c(nam)])] <- 0
  #TA2dat <- cbind(TA2dat, GVA[match(TA2dat$csquares,GVA$csquares), c(GVA_year)])
  #TA2dat[,c(GVA_year)][is.na(TA2dat[,c(GVA_year)])] <- 0
  TA2dat$avgsar    <- rowMeans(TA2dat[,SSAR_year]) # get the c-sq average within c-sq per year
  TA2dat$avgweight <- rowMeans(TA2dat[,weight_year],na.rm=T) * (TA2dat$area_km2 / TA2dat$tot_area) # distribute weight equally across MSFD habitats in each c-sq
  TA2dat$avgvalue  <- rowMeans(TA2dat[,value_year],na.rm=T) * (TA2dat$area_km2 / TA2dat$tot_area) # distribute value equally across MSFD habitats in each c-sq
  #TA2dat$avgGVA    <- rowMeans(TA2dat[,GVA_year],na.rm=T) * (TA2dat$area_km2 / TA2dat$tot_area) # distribute value equally across MSFD habitats in each c-sq
  
  TA2dat$sweptarea <- TA2dat[,"avgsar"]*TA2dat$area_km2
  
  # estimate proportion of area swept each year (average annual footprint I-3)
  TA2dat$propswept <- TA2dat$sweptarea
  TA2dat$propswept <- ifelse(TA2dat$sweptarea > TA2dat$area_km2,TA2dat$area_km2,TA2dat$sweptarea)
  
  # estimate proportion of area fished (all area within c-squares with SAR > 0)
  TA2dat$propfished <- ifelse(TA2dat[,"avgsar"] > 0,1,0)
  TA2dat$propfished <- TA2dat$propfished * TA2dat$area_km2
  
  nam <- c("area_km2", "grid", "avgweight", "avgvalue", "sweptarea","propfished","propswept")
  #nam <- c("area_km2", "grid", "avgweight", "avgvalue","avgGVA","sweptarea","propfished","propswept")
  indexcol <- which(names(TA2dat) %in% nam)
  A2table = aggregate( TA2dat[, indexcol], by= list(TA2dat$MSFD), FUN=function(x){sum(x)})
  names(A2table)[1] = 'MSFD'
  
  A2table <- as.data.frame(A2table)
  A2table <- A2table[order(A2table$area_km2,decreasing = T),]
  A2table$MSFD <- as.character(A2table$MSFD)
  
  A2table$avgsar <- round(A2table$sweptarea/A2table$area_km2, digits=2)
  A2table$propfished <- (A2table$propfished / A2table$area_km2) * 100
  A2table$propUnfished <- round(100 - A2table$propfished, digits=1) 
  A2table$propswept <- round(A2table$propswept / A2table$area_km2 *100, digits=1)
  A2table$area_km2 <- round(A2table$area_km2/1000, digits=2) 
  A2table$sweptarea <- round(A2table$sweptarea/1000, digits=2)
  A2table$avgweight <- round(A2table$avgweight/1000000, digits=2)
  A2table$avgvalue <- round(A2table$avgvalue/1000000, digits=2)

  ## New code swept area
  A2table$eff_fish <- "NA"
  llenght <- max(which(A2table$grid>20))
  for (i in 1:llenght){
    hab <- subset(TA2dat,TA2dat$MSFD == A2table[i,1])
    hab <- hab[order(hab[,"sweptarea"],decreasing = T),] # rank by swept area (fishing effort)
    hab$cumSSAR <- cumsum(hab[,"sweptarea"])
    hab$cumSSAR <- hab$cumSSAR / sum(hab[,"sweptarea"])
    Corefishing <- hab[which (hab$cumSSAR <=.9),]
    PctAreaCF <- round(sum(Corefishing$area_km2) / sum(hab$area_km2) * 100, digits=1)
    A2table$eff_fish[i] <- ifelse(is.na(hab$cumSSAR[1]),NA, PctAreaCF)
  }
  
  A2table$PctHabExt <- round(A2table$area_km2/sum(A2table$area_km2)*100, digits=1)
  A2table$grid <- NULL
  A2table$propfished <- NULL
  save(A2table, file="TableA2.RData")

#####
# Figure A.3
################ 
  A3dat <-  Region@data
  
  # get SAR for the whole period
  SSARNames <- paste("surface_sar",Period,sep="_") 
  A3dat <- cbind(A3dat, Fisheries[match(A3dat$csquares,Fisheries$csquares), c(SSARNames)])
  A3dat[,c(SSARNames)][is.na(A3dat[,c(SSARNames)])] <- 0
  
  # get data for left, middle and right panel
  A3left <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A3middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A3right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  # calculate indicator 1 average SAR all years all data
  indexcol <- which(names(A3dat) %in% SSARNames)
  A3dat[,indexcol] <- A3dat[,indexcol] * A3dat$area_sqkm
  All = apply(A3dat[, indexcol], 2,  FUN=function(x){sum(x)})
  A3left[,1] <- All/sum(A3dat$area_sqkm) 
  A3dat[,indexcol] <- A3dat[,indexcol] / A3dat$area_sqkm
  
  # calculate indicator 3 area swept per year
  A3dat_swept <- A3dat
  A3dat_swept[,indexcol] <- A3dat_swept[,indexcol] * A3dat_swept$area_sqkm
  for (j in 1: length(Period)){
    idx <- indexcol[j]
    A3dat_swept[,idx] <- ifelse(A3dat_swept[,idx] > A3dat_swept$area_sqkm,A3dat_swept$area_sqkm,A3dat_swept[,idx])
    A3middle[j,1]     <- sum(A3dat_swept[,idx],na.rm=T)/sum(A3dat_swept$area_sqkm)
  }
 
  # indicator 4 aggregation of fishing pressure per year
  A3dat_swept <- A3dat
  A3dat_swept[,indexcol] <- A3dat_swept[,indexcol] * A3dat_swept$area_sqkm
  for (j in 1: length(Period)){
    idx <- indexcol[j]
    A3dat_swept <- A3dat_swept[order(A3dat_swept[,idx],decreasing = T),]
    if(sum(A3dat_swept[,idx] > 0)){
    A3dat_swept$cumSSAR <- cumsum(A3dat_swept[,idx])
    A3dat_swept$cumSSAR <- A3dat_swept$cumSSAR / sum(A3dat_swept[,idx])
    Corefishing <- A3dat_swept[A3dat_swept$cumSSAR <= 0.9,,]
    A3right[j,1] <- sum(Corefishing$area_sqkm)/sum(A3dat_swept$area_sqkm)
    } else {A3right[j,1] <- NA} 
  }
  
   # now estimate again for most extensive MSFD habitats that are fished
  A3msfd <- A3dat
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  A3msfd <- merge(A3msfd,msfd_csq_new,by = "csquares", all.x =T)
  A3msfd$MSFD <- as.character(A3msfd$MSFD)
  A3msfd$MSFD[A3msfd$MSFD=="Na"]= "Unknown"
  
  mostcommonMSFD <- A2table[1:4,1]
  
  for (imsfd in 1:4){
    hab <- subset(A3msfd,A3msfd$MSFD == mostcommonMSFD[imsfd])
    
    # calculate indicator 1 average SAR all years all data
    indexcol <- which(names(hab) %in% SSARNames)
    hab[,indexcol] <- hab[,indexcol] * hab$area_km2
    All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
    A3left[,(imsfd+1)] <- All/sum(hab$area_km2) 
    hab[,indexcol] <- hab[,indexcol] / hab$area_km2
    
    # calculate indicator 3 area swept per year
    hab_swept <- hab
    hab_swept[,indexcol] <- hab_swept[,indexcol] * hab_swept$area_km2
    for (j in 1: length(Period)){
      idx <- indexcol[j]
      hab_swept[,idx] <- ifelse(hab_swept[,idx] > hab_swept$area_km2,hab_swept$area_km2,hab_swept[,idx])
      A3middle[j,(imsfd+1)]     <- sum(hab_swept[,idx],na.rm=T)/sum(hab_swept$area_km2)
    }
    
    # indicator 4 aggregation of fishing pressure per year
    hab_swept <- hab
    hab_swept[,indexcol] <- hab_swept[,indexcol] * hab_swept$area_km2
    for (j in 1: length(Period)){
      idx <- indexcol[j]
      hab_swept <- hab_swept[order(hab_swept[,idx],decreasing = T),]
      if(sum(hab_swept[,idx] > 0)){
      hab_swept$cumSSAR <- cumsum(hab_swept[,idx])
      hab_swept$cumSSAR <- hab_swept$cumSSAR / sum(hab_swept[,idx])
      Corefishing <- hab_swept[which(hab_swept$cumSSAR <= 0.9),]
      PctAreaCF <- sum(Corefishing$area_km2) / sum(hab_swept$area_km2)
      A3right[j,(imsfd+1)] <- PctAreaCF
      } else {A3right[j,(imsfd+1)] <- NA} 
    }
  }
  
  A3left <- cbind(A3left,Period)  
  colnames(A3left) <-c("All",mostcommonMSFD,"Year")
  A3middle <- cbind(A3middle,Period); colnames(A3middle) <- colnames(A3left)  
  A3right <- cbind(A3right,Period); colnames(A3right) <- colnames(A3left)  
  A3fig <- list(A3left,A3middle,A3right)
  save(A3fig, file="FigureA3.RData")
  
#####
# Figure A.4
################
  A4dat <-  Region@data
  
  nam <- c(SSAR_year,weight_year,value_year)
  A4dat <- cbind(A4dat, Fisheries[match(A4dat$csquares,Fisheries$csquares), c(nam)])
  A4dat[ ,c(nam)][is.na(A4dat[ ,c(nam)]) ] = 0 
  #A4dat <- cbind(A4dat, GVA[match(A4dat$csquares,GVA$csquares), c(GVA_year)])
  #A4dat[,c(GVA_year)][is.na(A4dat[,c(GVA_year)])] <- 0
  
  A4dat$avgsar    <- rowMeans(A4dat[,SSAR_year]) 
  A4dat$avgweight <- rowMeans(A4dat[,weight_year],na.rm=T)
  A4dat$avgvalue  <- rowMeans(A4dat[,value_year],na.rm=T)
  #A4dat$avgGVA    <- rowMeans(A4dat[,GVA_year],na.rm=T) 
  
  A4dat<-A4dat[order(-A4dat[,"avgsar"]),]
  A4dat$sweptcumu <-cumsum(A4dat[,"avgsar"])/sum(A4dat[,"avgsar"])
  A4dat$landcumu  <-cumsum(A4dat[,"avgweight"])/sum(A4dat[,"avgweight"])
  A4dat$valuecumu <-cumsum(A4dat[,"avgvalue"])/sum(A4dat[,"avgvalue"])
  #A4dat$GVAcumu   <-cumsum(A4dat[,"avgGVA"])/sum(A4dat[,"avgGVA"])
  A4dat$indixcumu <-(1:nrow(A4dat))/nrow(A4dat)

  save(A4dat, file="FigureA4.RData")

#####
# Table A.3
################
  datT3 <- Region@data
  
  gears2 <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  gears <- gears2[which(gears2 %in% subset(ActiveMetiers, Activity == "Active")$gears)]
  if(length(gears)>0){
    A3table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 5))
    
    for (pp in 1:length(gears)){
      datgear <- datT3
      
      nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
      datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
      datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
      datgear$avgsar <- rowMeans(datgear[,nam])
      datgear$sweptarea <- datgear[,"avgsar"]*datgear$area_sqkm
      A3table[1,pp] <- sum(datgear$sweptarea,na.rm=T)/1000
      
      nam <- paste(rep(paste(gears[pp],"total_weight",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
      datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
      datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
      datgear$avgweight <- rowMeans(datgear[,nam])
      A3table[2,pp] <- sum(datgear$avgweight)/1000000
      
      nam <- paste(rep(paste(gears[pp],"total_value",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
      datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
      datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
      datgear$avgvalue <- rowMeans(datgear[,nam])
      A3table[3,pp] <- sum(datgear$avgvalue)/1000000
      
      A3table[4,pp] <- (sum(datgear$avgweight)) / (sum(datgear$sweptarea,na.rm=T))
      A3table[5,pp] <- (sum(datgear$avgvalue)) / (sum(datgear$sweptarea,na.rm=T))
    }
    
    colnames(A3table) <- gears
    rownames(A3table) <- c("Area swept (\u00D71000 km^2^)","Landings (\u00D71000 tonnes)","Value (\u00D710^6^ euro)",
                           "Landings / Area swept (kg/km^2^)",
                           "Value / Area swept (euro/km^2^)")

    ## order gears by largest swept area
    SA <- data.frame(SwAr = as.numeric(t(A3table[1,1:ncol(A3table)])),
                     Met = colnames(A3table))
    SA <- SA[order(SA$SwAr, decreasing = T),]
    A3table <- A3table[,c(SA$Met)]
    save(A3table, file="TableA3.RData")
  } # end if gears>0 loop
 
  
### impact estimations
  if (p %in% regions_with_impact){
  
  #####
  # Figure A.5
  ################
    figA5 <- Region@data
    nam <- c(state_year)
    figA5 <- cbind(figA5, State_reg_IL[match(figA5$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
    indexcol <- which(names(figA5) %in% state_year)
    colnames(figA5)[indexcol] <-  paste("state_IL",AssPeriod,sep="_")
    
    figA5 <- cbind(figA5, State_reg[match(figA5$csquares,State_reg$Fisheries.csquares), c(nam)])
    save(figA5, file="FigureA5.RData")
    
  #####
  # Figure A.6
  ################
    A6dat <-  Region@data
    
    # remove areas deeper than 200 meter for overview of Greater North Sea
    if (Assregion == "Greater North Sea"){
      A6dat <-  subset(A6dat,A6dat$Depth >= -200)
    }
    
    stateNames <- paste("state",Period,sep="_") # get state for the whole period
    A6dat <- cbind(A6dat, State_reg_IL[match(A6dat$csquares,State_reg_IL$Fisheries.csquares), c(stateNames)])
    A6dat[,c(stateNames)][is.na(A6dat[,c(stateNames)])] <- 1
    
    # get data for left, right panel
    A6left_IL <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
    A6right_IL <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
    
    # calculate average L1 all years all data
    indexcol <- which(names(A6dat) %in% stateNames)
    A6dat[,indexcol] <- A6dat[,indexcol] * A6dat$area_sqkm
    All = apply(A6dat[, indexcol], 2,  FUN=function(x){sum(x)})
    A6left_IL[,1] <- All/sum(A6dat$area_sqkm) 
    A6dat[,indexcol] <- A6dat[,indexcol] / A6dat$area_sqkm
    
    # calculate prop area >0.8 state
    for (j in 1: length(Period)){
      idx <- stateNames[j]
      looparea <-  ifelse(A6dat[,idx] >= 0.8,1,0)
      A6right_IL[j,1] <- sum(looparea * A6dat$area_sqkm)/sum(A6dat$area_sqkm)
    }
    
    # now estimate again for most extensive MSFD habitats
    A6msfd <- A6dat
    colnames(A6msfd)[which(colnames(A6msfd)=="MSFD")] <- "MSFD_midpoint" 
    
    # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
    tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
    colnames(tnew) <- c("csquares","areanew")
    msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
    colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
    A6msfd <- merge(A6msfd,msfd_csq_new,by = "csquares", all.x =T)
    A6msfd$MSFD <- as.character(A6msfd$MSFD)
    A6msfd$MSFD[A6msfd$MSFD=="Na"]= "Unknown"
    
    mostcommonMSFD <- A2table[1:4,1]
    
    for (imsfd in 1:4){
      hab <- subset(A6msfd,A6msfd$MSFD == mostcommonMSFD[imsfd])
      
      # calculate average state
      indexcol <- which(names(hab) %in% stateNames)
      hab[,indexcol] <- hab[,indexcol] * hab$area_km2
      All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
      A6left_IL[,(imsfd+1)] <- All/sum(hab$area_km2) 
      hab[,indexcol] <- hab[,indexcol] / hab$area_km2
      
      # calculate prop area >0.8 state
      for (j in 1: length(Period)){
        idx <- stateNames[j]
        looparea <-  ifelse(hab[,idx] >= 0.8,1,0)
        A6right_IL[j,(imsfd+1)] <- sum(looparea * hab$area_km2)/sum(hab$area_km2)
    }}
    
  # do the same for PD state
    A6dat <-  Region@data
    
    # remove areas deeper than 200 meter for overview of Greater North Sea
    if (Assregion == "Greater North Sea"){
      A6dat <-  subset(A6dat,A6dat$Depth >= -200)
    }
    
    stateNames <- paste("state",Period,sep="_") # get state for the whole period
    A6dat <- cbind(A6dat, State_reg[match(A6dat$csquares,State_reg$Fisheries.csquares), c(stateNames)])
    A6dat[,c(stateNames)][is.na(A6dat[,c(stateNames)])] <- 1
    
    # get data for left, right panel
    A6left_PD  <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
    A6right_PD <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
    
    # calculate average L1 all years all data
    indexcol <- which(names(A6dat) %in% stateNames)
    A6dat[,indexcol] <- A6dat[,indexcol] * A6dat$area_sqkm
    All = apply(A6dat[, indexcol], 2,  FUN=function(x){sum(x)})
    A6left_PD[,1] <- All/sum(A6dat$area_sqkm) 
    A6dat[,indexcol] <- A6dat[,indexcol] / A6dat$area_sqkm
    
    # calculate prop area >0.8 state
    for (j in 1: length(Period)){
      idx <- stateNames[j]
      looparea <-  ifelse(A6dat[,idx] >= 0.8,1,0)
      A6right_PD[j,1] <- sum(looparea * A6dat$area_sqkm)/sum(A6dat$area_sqkm)
    }
    
    # now estimate again for most extensive MSFD habitats
    A6msfd <- A6dat
    colnames(A6msfd)[which(colnames(A6msfd)=="MSFD")] <- "MSFD_midpoint" 
    
    # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
    tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
    colnames(tnew) <- c("csquares","areanew")
    msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
    colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
    A6msfd <- merge(A6msfd,msfd_csq_new,by = "csquares", all.x =T)
    A6msfd$MSFD <- as.character(A6msfd$MSFD)
    A6msfd$MSFD[A6msfd$MSFD=="Na"]= "Unknown"
    mostcommonMSFD <- A2table[1:4,1]
    
    for (imsfd in 1:4){
      hab <- subset(A6msfd,A6msfd$MSFD == mostcommonMSFD[imsfd])
      
      # calculate average state
      indexcol <- which(names(hab) %in% stateNames)
      hab[,indexcol] <- hab[,indexcol] * hab$area_km2
      All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
      A6left_PD[,(imsfd+1)] <- All/sum(hab$area_km2) 
      hab[,indexcol] <- hab[,indexcol] / hab$area_km2
      
      # calculate prop area >0.8 state
      for (j in 1: length(Period)){
        idx <- stateNames[j]
        looparea <-  ifelse(hab[,idx] >= 0.8,1,0)
        A6right_PD[j,(imsfd+1)] <- sum(looparea * hab$area_km2)/sum(hab$area_km2)
    }}
    
    A6left_IL <- cbind(A6left_IL,Period)  
    colnames(A6left_IL) <-c("All",mostcommonMSFD,"Year")
    A6left_PD <- cbind(A6left_PD,Period); colnames(A6left_PD) <- colnames(A6left_IL)  
    A6right_PD <- cbind(A6right_PD,Period); colnames(A6right_PD) <- colnames(A6left_IL)  
    A6right_IL <- cbind(A6right_IL,Period); colnames(A6right_IL) <- colnames(A6left_IL)  
    
    A6fig <- list(A6left_PD,A6right_PD,A6left_IL,A6right_IL)
    save(A6fig, file="FigureA6.RData")
  
  ######
  # Figure A.8
  ################
    A8dat <- Region@data
    
    colnames(A8dat)[which(colnames(A8dat)=="MSFD")] <- "MSFD_midpoint" 
    
    # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
    tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
    colnames(tnew) <- c("csquares","areanew")
    msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
    colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
    A8dat <- merge(A8dat,msfd_csq_new,by = "csquares", all.x =T)
    A8dat$MSFD <- as.character(A8dat$MSFD)
    A8dat$MSFD[A8dat$MSFD=="Na"]= "Unknown"
    
    # remove areas deeper than 200 meter for overview of Greater North Sea
    if (Assregion == "Greater North Sea"){
      A8dat <-  subset(A8dat,A8dat$Depth >= -200)
    }
    if(length(unique(subset(ActiveMetiers, Activity == "Active")$gears))>0) {
      gears <- colnames(A3table)
      
      nam <- paste("state",rep(gears[1],length(Period)),Period,sep="_")
      A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
      A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
      A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
      Avgear <- aggregate(A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
      Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
      colnames(Avgear)[1] <- 'MSFD'
      Avgear <- Avgear[,-(ncol(Avgear))]
      AvMSFD_metier <- as.data.frame(Avgear)
      
      for (pp in 2:length(gears)){
        nam <- paste("state",rep(gears[pp],length(Period)),Period,sep="_")
        A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
        A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
        A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
        Avgear <- aggregate(A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
        Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
        colnames(Avgear)[1] <- 'MSFD'
        Avgear <- Avgear[,-(ncol(Avgear))]
        AvMSFD_metier <- cbind(AvMSFD_metier, Avgear[match(AvMSFD_metier$MSFD,Avgear$MSFD), c(2:(length(Period)+1))])
      }
      
      A8_A9fig <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
      save(A8_A9fig, file="FigureA8_A9.RData")
      
      # redo analysis for inverse longevity impact
      A8dat <- Region@data
      
      colnames(A8dat)[which(colnames(A8dat)=="MSFD")] <- "MSFD_midpoint" 
      
      # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
      tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
      colnames(tnew) <- c("csquares","areanew")
      msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
      colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
      A8dat <- merge(A8dat,msfd_csq_new,by = "csquares", all.x =T)
      A8dat$MSFD <- as.character(A8dat$MSFD)
      A8dat$MSFD[A8dat$MSFD=="Na"]= "Unknown"
      
      # remove areas deeper than 200 meter for overview of Greater North Sea
      if (Assregion == "Greater North Sea"){
        A8dat <-  subset(A8dat,A8dat$Depth >= -200)
      }
      
      nam <- paste("state",rep(gears[1],length(Period)),Period,sep="_")
      A8dat <- cbind(A8dat, State_reg_IL[match(A8dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
      A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
      A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
      Avgear <- aggregate( A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
      Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
      colnames(Avgear)[1] <- 'MSFD'
      Avgear <- Avgear[,-(ncol(Avgear))]
      AvMSFD_metier <- as.data.frame(Avgear)
      
      for (pp in 2:length(gears)){
        nam <- paste("state",rep(gears[pp],length(Period)),Period,sep="_")
        A8dat <- cbind(A8dat, State_reg_IL[match(A8dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
        A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
        A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
        Avgear <- aggregate( A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
        Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
        colnames(Avgear)[1] <- 'MSFD'
        Avgear <- Avgear[,-(ncol(Avgear))]
        AvMSFD_metier <- cbind(AvMSFD_metier, Avgear[match(AvMSFD_metier$MSFD,Avgear$MSFD), c(2:(length(Period)+1))])
      }
      
      A8_A9fig <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
      save(A8_A9fig, file="FigureA8_A9_IL.RData")
    } # end if active gears > 0
    

  #####
  # Table A.4
  ################
    datT4 <- Region@data
    
    # remove areas deeper than 200 meter for overview of Greater North Sea
    if (Assregion == "Greater North Sea"){
      datT4 <-  subset(datT4,datT4$Depth >= -200)
    }
    
    if(length(gears)>0){
      A4table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 4))
      
      for (pp in 1:length(gears)){
        datgear <- datT4
        
        nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
        datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
        datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
        datgear$avgsar <- rowMeans(datgear[,nam])
        
        nam <- paste(rep(paste(gears[pp],"total_weight",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
        datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
        datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
        datgear$avgweight <- rowMeans(datgear[,nam])
        
        nam <- paste(rep(paste(gears[pp],"total_value",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
        datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
        datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
        datgear$avgvalue <- rowMeans(datgear[,nam])
        
        nam <- paste(rep(paste("state",gears[pp],sep="_"),length(AssPeriod)),AssPeriod,sep="_")
        datgear_PD <- cbind(datgear, State_reg[match(datgear$csquares,State_reg$Fisheries.csquares), c(nam)])
        datgear_PD[,c(nam)][is.na(datgear_PD[,c(nam)])] <- 0
        datgear_PD$avgimpact <- 1- rowMeans(datgear_PD[,nam])
        datgear_PD <- subset(datgear_PD,datgear_PD$avgsar >0)
        A4table[1,pp] <- round((sum(datgear_PD$avgweight)/1000) / sum(datgear_PD$avgimpact), digits=1)
        A4table[2,pp] <- round((sum(datgear_PD$avgvalue)/1000) / sum(datgear_PD$avgimpact), digits=1)

        datgear_IL <- cbind(datgear, State_reg_IL[match(datgear$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
        datgear_IL[,c(nam)][is.na(datgear_IL[,c(nam)])] <- 0
        datgear_IL$avgimpact <- 1- rowMeans(datgear_IL[,nam])
        datgear_IL <- subset(datgear_IL,datgear_IL$avgsar >0)
        A4table[3,pp] <- round((sum(datgear_IL$avgweight)/1000) / sum(datgear_IL$avgimpact), digits=1)
        A4table[4,pp] <- round((sum(datgear_IL$avgvalue)/1000) / sum(datgear_IL$avgimpact), digits=1)
      }
      
      colnames(A4table) <- gears
      rownames(A4table) <- c("Landings (tonnes)/PD impact",
                             "Value (\u00D71000 euro)/PD impact",
                             "Landings (tonnes)/PD-sens impact",
                             "Value (\u00D71000 euro)/PD-sens impact")
       save(A4table, file="TableA4.RData")
      
    } # end if gears>0 loop
    
    
  }
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                              'EcoReg_index','GVA','GVAMet','Period','AssPeriod',"GVAPeriod",
                              "EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion",
                              "msfd_csq","regions_with_corefishing", "State_GMP10", "State_GMP20",
                              "State_GMP5", "State_sens_GMP10", "State_sens_GMP20", "State_sens_GMP5",
                              "ActiveMetiers"))])
