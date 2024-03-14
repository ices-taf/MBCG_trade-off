## Script to produce summary tables from impact assessment

rm(list = ls())

### github folder
pathdir <- "D:/MBCG_trade-off"

### get all libraries
source(paste(pathdir,"Utilities/Libraries_WKTRADE3.R",sep="/"))

### create dt of marine reporting areas
Sregions <- c("Greater North Sea", "Baltic Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
NS_div <- c("Kattegat", "Norwegian Trench", "Central North Sea", "Southern North Sea", "Channel")
BS_div <- c("Bothnian_BS", "GulfF_BS", "GulfR_BS", "Proper_BS", "ArkBor_BS","Western_BS")
CS_div <- c("Northern Celtic Sea" ,"Southern Celtic Sea")
BoBIC_div <- c("Gulf of Biscay", "North-Iberian Atlantic", "South-Iberian Atlantic", "Gulf of Cadiz")
RegDivs <- data.table(EcoReg=c(rep(Sregions[1],5),rep(Sregions[2],6),
                                    rep(Sregions[3],2),rep(Sregions[4],4)),
                      Division = c(NS_div, BS_div, CS_div, BoBIC_div))



#------------------------------------------------------------------------------------------------------------------
## Table 1: Spatial extent of areas
#------------------------------------------------------------------------------------------------------------------
Tab1 <- data.table(depthclass=c("Shape", "Overall", "1. 0 - 200m", "2. 200 - 400m", "3. 400 - 800m", "4. >= 800m",
                                "pct_Overall", "pct_DC1", "pct_DC2", "pct_DC3", "pct_DC4"))
load(paste0(pathdir, "/1 - Input env/RegDivShapes2.Rdata"))
ShapeExt <- data.table(st_drop_geometry(RegDivShapes))
ShapeExt$Extkm2 <- round(as.numeric(st_area(st_make_valid(RegDivShapes)))/1000000, digits=0)
ShapeExt2 <- ShapeExt[,.(Extkm2 = sum(Extkm2)), by="EcoReg"]

for(iReg in unique(RegDivs$EcoReg)){
  setwd(paste0(pathdir, "/5 - Output/(sub-)Region/", iReg))
  # read in division-based extents
  T1 <- data.table(read.table("AssAreaExt.txt", header=T))
  T1 <- T1[division=="All"]
  # Determine overall extent and % per depth class
  T1a <- data.table(depthclass="Overall",
                    area_sqkm = sum(T1$area_sqkm))
  T1$pct <- round(T1$area_sqkm/T1a$area_sqkm*100, digits=1)
  T1b <- T1[,c("depthclass", "pct")]
  T1b$depthclass <- ifelse(T1b$depthclass == "1. 0 - 200m", "pct_DC1",
                           ifelse(T1b$depthclass == "2. 200 - 400m", "pct_DC2",
                                  ifelse(T1b$depthclass == "3. 400 - 800m", "pct_DC3", "pct_DC4")))
  names(T1b) <- c("depthclass", "area_sqkm")
  
  # Determine Shape extent 
  T1c <- data.table(depthclass = "Shape",
                    area_sqkm  = ShapeExt2[EcoReg == iReg,]$Extkm2)
  
  # Determine pct_overall
  T1d <- data.table(depthclass = "pct_Overall",
                    area_sqkm  = round(T1a$area_sqkm/T1c$area_sqkm*100, digits=1))

  ## combine all, and merge with Tab1
  T1 <- rbind(T1c, T1a, T1[,c("depthclass", "area_sqkm")], T1d, T1b)
  Tab1[[iReg]] <- T1$area_sqkm [match(Tab1$depthclass, T1$depthclass)]
  
  
  for(iDiv in unique(subset(RegDivs, EcoReg == iReg)$Division)){
    # read in division-based extents
    T1 <- data.table(read.table("AssAreaExt.txt", header=T))
    T1 <- T1[division== iDiv]
    
    # Determine overall extent and % per depth class
    T1a <- data.table(depthclass="Overall",
                      area_sqkm = sum(T1$area_sqkm))
    T1$pct <- round(T1$area_sqkm/T1a$area_sqkm*100, digits=1)
    T1b <- T1[,c("depthclass", "pct")]
    T1b$depthclass <- ifelse(T1b$depthclass == "1. 0 - 200m", "pct_DC1",
                             ifelse(T1b$depthclass == "2. 200 - 400m", "pct_DC2",
                                    ifelse(T1b$depthclass == "3. 400 - 800m", "pct_DC3", "pct_DC4")))
    names(T1b) <- c("depthclass", "area_sqkm")
    
    # Determine Shape extent 
    T1c <- data.table(depthclass = "Shape",
                      area_sqkm  = ShapeExt[Division == iDiv,]$Extkm2)
    
    # Determine pct_overall
    T1d <- data.table(depthclass = "pct_Overall",
                      area_sqkm  = round(T1a$area_sqkm/T1c$area_sqkm*100, digits=1))
    
    ## combine all, and merge with Tab1
    T1 <- rbind(T1c, T1a, T1[,c("depthclass", "area_sqkm")], T1d, T1b)
    Tab1[[iDiv]] <- T1$area_sqkm [match(Tab1$depthclass, T1$depthclass)]
  }}

## Write full table to csv (regions + divisions)
write.csv(Tab1, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab1D_AreaExt.csv"), 
          row.names = FALSE)


## Create region only table and save to csv
cols <- c("depthclass", unique(RegDivs$EcoReg))
Tab1R <- Tab1[,..cols]
write.csv(Tab1R, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab1R_AreaExt.csv"), 
          row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------
## Table 2: Extent and aggregation of fished areas per reg+div
#------------------------------------------------------------------------------------------------------------------
Tab2 <- data.table(Ind= c(paste("I-3", c("0-200m", "200-400m", "400-800m"), sep="_"), paste("I-4", c("0-200m", "200-400m", "400-800m"), sep="_")))
for(iReg in unique(RegDivs$EcoReg)){
  setwd(paste0(pathdir, "/5 - Output/(sub-)Region/", iReg))
  T2 <- read.table(paste0(iReg, "_Table_1.csv"), header=T, sep=",")
  T2_I3 <- T2[3,]
  T2_I3 <- data.table(t(T2_I3))
  T2_I3 <- T2_I3[-1,]
  names(T2_I3) <- c(iReg)
  
  T2_I4 <- T2[4,]
  T2_I4 <- data.table(t(T2_I4))
  T2_I4 <- T2_I4[-1,]
  names(T2_I4) <- c(iReg)
  
  T2 <- rbind(T2_I3, T2_I4)
  Tab2 <- cbind(Tab2, T2)
  
  ## Same for divisions
  for(iDiv in unique(subset(RegDivs, EcoReg == iReg)$Division)){
    setwd(paste0(pathdir, "/5 - Output/Division/", iDiv))
    T2 <- read.table(paste0(iDiv, "_Table_1.csv"), header=T, sep=",")
    T2_I3 <- T2[3,]
    T2_I3 <- data.table(t(T2_I3))
    T2_I3 <- T2_I3[-1,]
    names(T2_I3) <- c(iDiv)
    
    T2_I4 <- T2[4,]
    T2_I4 <- data.table(t(T2_I4))
    T2_I4 <- T2_I4[-1,]
    names(T2_I4) <- c(iDiv)
    
    T2 <- rbind(T2_I3, T2_I4)
    Tab2 <- cbind(Tab2, T2)
  }}

## Write full table to csv (regions + divisions)
write.csv(Tab2, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab2D_Ext&AggFishExt.csv"), 
          row.names = FALSE)

## Create region only table and save to csv
cols <- c("Ind", unique(RegDivs$EcoReg))
Tab2R <- Tab2[,..cols]
write.csv(Tab2R, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab2R_Ext&AggFishExt.csv"), 
          row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------
## Table 3: Unfished extent (%) per habitat type per region/divison
#------------------------------------------------------------------------------------------------------------------
Tab3 <- data.table(MSFD = c("Littoral rock and biogenic reef", "Littoral sediment", 
                            "Infralittoral rock and biogenic reef", "Infralittoral coarse sediment",
                            "Infralittoral mixed sediment", "Infralittoral sand", 
                            "Infralittoral mud", "Infralittoral mud or Infralittoral sand", 
                            "Circalittoral rock and biogenic reef", "Circalittoral coarse sediment",
                            "Circalittoral mixed sediment", "Circalittoral sand", 
                            "Circalittoral mud", "Circalittoral mud or Circalittoral sand",
                            "Offshore circalittoral rock and biogenic reef", "Offshore circalittoral coarse sediment",
                            "Offshore circalittoral mixed sediment", "Offshore circalittoral sand", 
                            "Offshore circalittoral mud", "Offshore circalittoral mud or Offshore circalittoral sand",
                            "Upper bathyal rock and biogenic reef", "Upper bathyal sediment", 
                            "Upper bathyal sediment or Upper bathyal rock and biogenic reef",
                            "Lower bathyal rock and biogenic reef", "Lower bathyal sediment", 
                            "Lower bathyal sediment or Lower bathyal rock and biogenic reef",
                            "Abyssal", "Unknown"))
T3a <- data.table()
for(iReg in unique(RegDivs$EcoReg)){
  setwd(paste0(pathdir, "/5 - Output/(sub-)Region/", iReg))
  T3 <- read.table(paste0(iReg, "_Table_3.csv"), header=T, sep=",")
  T3 <- T3[,c(1,10)]
  names(T3) <- c("MSFD", "ExtUnfished")
  T3$EcoReg <- iReg
  T3$Division <- "All"
  T3a <- rbind(T3a, T3)
  
  for(iDiv in unique(subset(RegDivs, EcoReg == iReg)$Division)){
    setwd(paste0(pathdir, "/5 - Output/Division/", iDiv))
    T3 <- read.table(paste0(iDiv, "_Table_3.csv"), header=T, sep=",")
    T3 <- T3[,c(1,10)]
    names(T3) <- c("MSFD", "ExtUnfished")
    T3$EcoReg <- iReg
    T3$Division <- iDiv
    T3a <- rbind(T3a, T3)
  }}

for(iReg in unique(RegDivs$EcoReg)){
  T3 <- subset(T3a, EcoReg == iReg & Division == "All")
  Tab3[[iReg]] <- T3$ExtUnfished [match(Tab3$MSFD, T3$MSFD)]
  
  for(iDiv in unique(subset(RegDivs, EcoReg == iReg)$Division)){
    T3 <- subset(T3a, EcoReg == iReg & Division == iDiv)
    Tab3[[iDiv]] <- T3$ExtUnfished [match(Tab3$MSFD, T3$MSFD)]
  }}

## Write full table to csv (regions + divisions)
write.csv(Tab3, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab3D_UnfishedExtHab.csv"), 
          row.names = FALSE)

## Create region only table and save to csv
cols <- c("MSFD", unique(RegDivs$EcoReg))
Tab3R <- Tab3[,..cols]
write.csv(Tab3R, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab3R_UnfishedExtHab.csv"), 
          row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------
## Table 4: Potential costs to achieve differing proportions of each MSFD BHT without MSFD fishing (based on value)
#------------------------------------------------------------------------------------------------------------------
Tab4 <- data.table()
## Read in ecoregion level info on costs
for(iReg in unique(RegDivs$EcoReg)){
  setwd(paste0(pathdir, "/5 - Output/(sub-)Region/", iReg))
  T4 <- read.table(paste0(iReg, "_habitat_value2.txt"), header=T, sep=",")
  T4$EcoReg <- iReg
  T4$Div <- "All"
  Tab4 <- rbind(Tab4, T4[1,])
  
  for(iDiv in unique(subset(RegDivs, EcoReg == iReg)$Division)){
    setwd(paste0(pathdir, "/5 - Output/Division/", iDiv))
    T4 <- read.table(paste0(iDiv, "_habitat_value2.txt"), header=T, sep=",")
    T4$EcoReg <- iReg
    T4$Div <- iDiv
    Tab4 <- rbind(Tab4, T4[1,])
  }
}

Tab4$MSFD.broad.habitat.type <- NULL
names(Tab4) <- c("HabExt_1000km2", "Pct10", "Pct20", "Pct30", "Pct40", "Pct50", "Pct60", "Pct70", 
                      "Pct80", "Pct90", "Pct100", "Ecoregion", "Division")
Tab4 <- Tab4[,c("Ecoregion", "Division", "HabExt_1000km2", "Pct10", "Pct20", "Pct30", "Pct40",
                          "Pct50", "Pct60", "Pct70", "Pct80", "Pct90", "Pct100")]

## Write full table to csv (regions + divisions)
write.csv(Tab4, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab4D_Val_FP_Red.csv"), 
          row.names = FALSE)

## Create region only table and save to csv
Tab4R <- Tab4[Division=="All",]
Tab4R$Division <- NULL
write.csv(Tab4R, file=paste0(pathdir, "/5 - Output/Summary_tables/Tab4R_Val_FP_Red.csv"), 
          row.names = FALSE)





