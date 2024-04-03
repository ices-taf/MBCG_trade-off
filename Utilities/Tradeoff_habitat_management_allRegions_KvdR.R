### Script adjusted by Karin 5Feb to produce table with absolute values for all hab types scenario only.
###### Tradeoff - effort reduction with spatial closure per MSFD habitat type
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
state_year <- paste("state",AssPeriod,sep="_")
weight_year <- paste("total_weight",AssPeriod,sep="_")
value_year <- paste("total_value",AssPeriod,sep="_")

# set folder directory
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))

# define directory to load figure and table data products
pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/")
load(paste(pathdir_prodFT,"TableA2.RData",sep="/"))
habitat <- as.character(A2table[,1]) # Select all habitat types.

# get regional data and connect to MSFD habitats with information within the c-square
datmsfd <- Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  datmsfd <-  subset(datmsfd,datmsfd$Depth >= -200)
}

# account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
colnames(tnew) <- c("csquares","areanew")

msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
datmsfd <- merge(datmsfd,msfd_csq_new,by = "csquares", all.x =T)
datmsfd$MSFD <- as.character(datmsfd$MSFD)
datmsfd$MSFD[datmsfd$MSFD=="Na"]= "Unknown"
datmsfd$grid <- 1
habitat <- habitat[habitat %in% datmsfd$MSFD] # double check that only existing habitats are included

# create three tables with all habitats (with at least 20 grid cells)
mateffort <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
matweight <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
matvalue  <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))

for (iHabitat in 1:length(habitat)){
  Trade_RE1 <- subset(datmsfd,datmsfd$MSFD == habitat[iHabitat])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,nam], na.rm=T)
  
  nam <- c(value_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgvalue <- rowMeans(Trade_RE1[,nam]) * (Trade_RE1$area_km2 / Trade_RE1$tot_area)
  
  nam <- c(weight_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgweight <- rowMeans(Trade_RE1[,nam]) * (Trade_RE1$area_km2 / Trade_RE1$tot_area)
  
  Trade_RE1$sweptarea <- Trade_RE1$avgsar * Trade_RE1$area_km2
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"sweptarea"], -Trade_RE1[,"area_km2"] ,decreasing = F),] 
  Trade_RE1$cumarea <- cumsum(Trade_RE1[,"area_km2"])
  Trade_RE1$cumarea <- Trade_RE1$cumarea / sum(Trade_RE1[,"area_km2"])
  
  quat<- c(seq(0.1,1,0.1))
  for (q in 1:length(quat)){
    idx <- min(which(Trade_RE1$cumarea >= quat[q]))
    mateffort[iHabitat,q] <-  sum(Trade_RE1$sweptarea[1:idx])
    matweight[iHabitat,q] <- sum(Trade_RE1$avgweight[1:idx])
    matvalue[iHabitat,q]  <- sum(Trade_RE1$avgvalue[1:idx])
  }
}

## Add total, and provide both absolute (mateffort) and relative (mateffpct) numbers 
EffortSums <- colSums(mateffort)
mateffort <- rbind(EffortSums, mateffort)
mateffpct <- mateffort[,1:9]/mateffort[,10] *100
mateffpct[mateffpct < 0.1 & mateffpct > 0] <- -100
mateffpct <- format(round(mateffpct, digits = 1), nsmall = 1) 
mateffpct <- mateffpct %>%
  mutate_all(as.character)
mateffpct[mateffpct == "-100.0"] <- "<0.1"
rownames(mateffpct) <- c("Total", habitat)
colnames(mateffpct) <- paste0("pct_",quat[1:9]*100)

#mateffort <- mateffort
mateffort[mateffort < 0.1 & mateffort > 0] <- -100000
mateffort <- format(round(mateffort, digits = 1), nsmall = 1) 
mateffort <- mateffort %>%
  mutate_all(as.character)
mateffort[mateffort == "-100000.0"] <- "<0.1"
rownames(mateffort) <- c("Total", habitat)
colnames(mateffort) <- paste0("pct_",quat*100)

## Add total, and provide both absolute and relative numbers 
WeightSums <- colSums(matweight)
matweight <- rbind(WeightSums, matweight)
matwghtpct <- matweight[,1:9]/matweight[,10]*100
matwghtpct[matwghtpct < 0.1 & matwghtpct >0] <- -100
matwghtpct <- format(round(matwghtpct, digits = 1), nsmall = 1) 
matwghtpct <- matwghtpct %>%
  mutate_all(as.character)
matwghtpct[matwghtpct == "-100.0"] <- "<0.1"
rownames(matwghtpct) <- c("Total", habitat)
colnames(matwghtpct) <- paste0("pct_",quat[1:9]*100)
matwghtpct$Totwght <- round(matweight[,10]/1E3, digits=1) # so unit is kg x1000

matweight <- matweight / 1E3 # so unit is kg x1000
matweight[matweight < 0.1 & matweight >0] <- -100
matweight <- format(round(matweight, digits = 1), nsmall = 1) 
matweight <- matweight %>%
  mutate_all(as.character)
matweight[matweight == "-100.0"] <- "<0.1"
rownames(matweight) <- c("Total", habitat)
colnames(matweight) <- paste0("pct_",quat*100)

## Add total, and provide both absolute and relative numbers 
ValueSums <- colSums(matvalue)
matvalue <- rbind(ValueSums, matvalue)
matvalpct <- matvalue[,1:9]/matvalue[,10] * 100
matvalpct[matvalpct < 0.1 & matvalpct >0] <- -100
matvalpct <- format(round(matvalpct, digits = 1), nsmall = 1) 
matvalpct <- matvalpct %>%
  mutate_all(as.character)
matvalpct[matvalpct == "-100.0"] <- "<0.1"
rownames(matvalpct) <- c("Total", habitat)
colnames(matvalpct) <- paste0("pct_",quat[1:9]*100)
matvalpct$TotVal <- round(matvalue[,10]/1E3, digits=1) # so unit is euro x 1000

matvalue <- matvalue / 1E3 # so unit is euro x1000
matvalue[matvalue < 0.1 & matvalue >0] <- -10000
matvalue <- format(round(matvalue, digits = 1), nsmall = 1) 
matvalue <- matvalue %>%
  mutate_all(as.character)
matvalue[matvalue == "-10000.0"] <- "<0.1"
rownames(matvalue) <- c("Total", habitat)
colnames(matvalue) <- paste0("pct_",quat*100)

# Combine with info from table 2
totArea <- round(sum(subset(A2table, MSFD %in% habitat)$area_km2), digits=2)
mateffort <- data.frame(MSFD = c("total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), 
                        Sweptarea = c(100, round(subset(A2table, MSFD %in% habitat)$sweptarea, digits =2)), mateffort[,1:9])
colnames(mateffort)[1:3] <- c("MSFD","HabExt", "SweptArea")
mateffpct <- data.frame(MSFD = c("total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), 
                        Sweptarea = c(100, round(subset(A2table, MSFD %in% habitat)$sweptarea, digits =2)), mateffpct)
colnames(mateffpct)[1:3] <- c("MSFD","HabExt", "SweptArea")

matweight <- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matweight[,1:9])
colnames(matweight)[1:2] <- c("MSFD","HabExt")
matwghtpct<- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matwghtpct)
colnames(matwghtpct)[1:2] <- c("MSFD","HabExt")
matwghtpct <- matwghtpct[,c(1,2,12, 3:11)]
matvalue <- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matvalue[,1:9])
colnames(matvalue)[1:2] <- c("MSFD","HabExt")
matvalpct <- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matvalpct)
colnames(matvalpct)[1:2] <- c("MSFD","HabExt")
matvalpct <- matvalpct[,c(1:2, 12, 3:11)]

## Save as txt files.
write.csv(mateffort, file= paste(Assregion,"habitat_effort2.txt",sep="_"), row.names=FALSE)
write.csv(matweight, file= paste(Assregion,"habitat_weight2.txt",sep="_"), row.names=FALSE) 
write.csv(matvalue, file= paste(Assregion,"habitat_value2.txt",sep="_"), row.names=FALSE)
write.csv(mateffpct, file= paste(Assregion,"habitat_effortPCT2.txt",sep="_"), row.names=FALSE)
write.csv(matwghtpct, file= paste(Assregion,"habitat_weightPCT2.txt",sep="_"), row.names=FALSE) 
write.csv(matvalpct, file= paste(Assregion,"habitat_valuePCT2.txt",sep="_"), row.names=FALSE)

rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                            'EcoReg_index','GVA','GVAMet','Period','AssPeriod',"GVAPeriod",
                            "EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                            'Region','State_reg','State_reg_IL',"Assunit","Assregion",
                            "msfd_csq","regions_with_corefishing", "State_GMP10", "State_GMP20",
                            "State_GMP5", "State_sens_GMP10", "State_sens_GMP20", "State_sens_GMP5"))])
