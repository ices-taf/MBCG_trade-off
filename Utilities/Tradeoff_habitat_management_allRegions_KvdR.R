### Script adjusted by Karin 5Feb to produce table with absolute values for all hab types scenario only.
###### Tradeoff - effort reduction with spatial closure per MSFD habitat type
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
state_year <- paste("state",AssPeriod,sep="_")
weight_year <- paste("total_weight",AssPeriod,sep="_")
value_year <- paste("total_value",AssPeriod,sep="_")
#GVA_year   <- paste("total_gva",GVAPeriod,sep="_")

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
#matGVA    <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
matweight <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
matvalue  <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))

for (iHabitat in 1:length(habitat)){
  Trade_RE1 <- subset(datmsfd,datmsfd$MSFD == habitat[iHabitat])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,nam], na.rm=T)
  
  # nam <- c(GVA_year)
  # Trade_RE1 <- cbind(Trade_RE1, GVA[match(Trade_RE1$csquares,GVA$csquares), c(nam)])
  # Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  # Trade_RE1$avgGVA <- rowMeans(Trade_RE1[,nam],na.rm=T) * (Trade_RE1$area_km2 / Trade_RE1$tot_area)
   
  nam <- c(value_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgvalue <- rowMeans(Trade_RE1[,nam]) * (Trade_RE1$area_km2 / Trade_RE1$tot_area)
  
  nam <- c(weight_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgweight <- rowMeans(Trade_RE1[,nam]) * (Trade_RE1$area_km2 / Trade_RE1$tot_area)
  
  Trade_RE1$sweptarea <- Trade_RE1$avgsar * Trade_RE1$area_km2
  # Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"sweptarea"],decreasing = F),] # old code
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"sweptarea"], -Trade_RE1[,"area_km2"] ,decreasing = F),] ## new code
  Trade_RE1$cumarea <- cumsum(Trade_RE1[,"area_km2"])
  Trade_RE1$cumarea <- Trade_RE1$cumarea / sum(Trade_RE1[,"area_km2"])
  
  quat<- c(seq(0.1,1,0.1))
  for (q in 1:length(quat)){
    idx <- min(which(Trade_RE1$cumarea >= quat[q]))
    mateffort[iHabitat,q] <-  sum(Trade_RE1$sweptarea[1:idx])/sum(Trade_RE1$sweptarea)
    matweight[iHabitat,q] <- sum(Trade_RE1$avgweight[1:idx])
    #matGVA[iHabitat,q]  <- sum(Trade_RE1$avgGVA[1:idx])
    matvalue[iHabitat,q]  <- sum(Trade_RE1$avgvalue[1:idx])
  }
}

mateffort <- mateffort * 100 # so unit is % of total effort.
mateffort[mateffort < 0.1 & mateffort > 0] <- -100
mateffort <- format(round(mateffort, digits = 1), nsmall = 1) 
mateffort <- mateffort %>%
  mutate_all(as.character)
mateffort[mateffort == "-100.0"] <- "<0.1"
rownames(mateffort) <- habitat
colnames(mateffort) <- quat

WeightSums <- colSums(matweight)
matweight <- rbind(WeightSums, matweight)
matweight <- matweight / 1E6 # so unit is kg x1000000
matweight[matweight < 0.1 & matweight >0] <- -100
matweight <- format(round(matweight, digits = 1), nsmall = 1) 
matweight <- matweight %>%
  mutate_all(as.character)
matweight[matweight == "-100.0"] <- "<0.1"
rownames(matweight) <- c("Total", habitat)
colnames(matweight) <- quat

ValueSums <- colSums(matvalue)
matvalue <- rbind(ValueSums, matvalue)
matvalue <- matvalue / 1E6 # so unit is euro x1000000
matvalue[matvalue < 0.1 & matvalue >0] <- -100
matvalue <- format(round(matvalue, digits = 1), nsmall = 1) 
matvalue <- matvalue %>%
  mutate_all(as.character)
matvalue[matvalue == "-100.0"] <- "<0.1"
rownames(matvalue) <- c("Total", habitat)
colnames(matvalue) <- quat

# GVASUM <- colSums(matGVA)
# matGVA <- rbind(GVASUM, matGVA)
# matGVA <- matGVA  / 1000 # so unit = GVA x1000
# matGVA[matGVA < 0.1 & matGVA >0] <- -100
# matGVA <-  format(round(matGVA, digits = 1), nsmall = 1) 
# matGVA <- matGVA %>%
#   mutate_all(as.character)
# matGVA[matGVA == "-100.0"] <- "<0.1"
# rownames(matGVA) <- c("Total", habitat)
# colnames(matGVA) <- quat

totArea <- round(sum(subset(A2table, MSFD %in% habitat)$area_km2), digits=2)
mateffort <- data.frame(MSFD = habitat, area_km2 = round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2), 
                        Sweptarea = round(subset(A2table, MSFD %in% habitat)$sweptarea, digits =2), mateffort)
colnames(mateffort)[1:3] <- c("MSFD broad habitat type","Extent of habitat 1000 km2", "Swept area (x1000 km^2^)")
# matGVA <- data.frame(MSFD = c("Total", A2table[,1]), area_km2 = c(totArea, round(A2table[,2], digits =2)), matGVA)
# colnames(matGVA)[1:2] <- c("MSFD broad habitat type","Extent of habitat 1000 km2")
matweight <- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matweight)
colnames(matweight)[1:2] <- c("MSFD broad habitat type","Extent of habitat 1000 km2")
matvalue <- data.frame(MSFD = c("Total", habitat), area_km2 = c(totArea, round(subset(A2table, MSFD %in% habitat)$area_km2, digits =2)), matvalue)
colnames(matvalue)[1:2] <- c("MSFD broad habitat type","Extent of habitat 1000 km2")

write.csv(mateffort, file= paste(Assregion,"habitat_effort2.txt",sep="_"), row.names=FALSE)
#write.csv(matGVA, file= paste(Assregion,"habitat_GVA2.txt",sep="_"), row.names=FALSE)  
write.csv(matweight, file= paste(Assregion,"habitat_weight2.txt",sep="_"), row.names=FALSE) 
write.csv(matvalue, file= paste(Assregion,"habitat_value2.txt",sep="_"), row.names=FALSE)

rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                            'EcoReg_index','GVA','GVAMet','Period','AssPeriod',"GVAPeriod",
                            "EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                            'Region','State_reg','State_reg_IL',"Assunit","Assregion",
                            "msfd_csq","regions_with_corefishing", "State_GMP10", "State_GMP20",
                            "State_GMP5", "State_sens_GMP10", "State_sens_GMP20", "State_sens_GMP5"))])
