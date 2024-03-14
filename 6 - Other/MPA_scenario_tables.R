###########################################################################################################################
#
## Script to evaluate MBCG fisheries trade-offs under MPA-scenarios.
## Presented in the WKD6ASSESS report, ICES 2024.
## Code by Karin van der Reijden and Daniel van Denderen.
#
###########################################################################################################################
rm(list=ls())

#--------------------------------------------------
## Load libraries, set datapaths
#--------------------------------------------------
library(sf)
library(data.table)
library(terra)
sf_use_s2(FALSE)

### github folder
pathdir <- "D:/MBCG_trade-off"

#--------------------------------------------------
## Load and merge VME and MPA shapefiles
# - VME: EU. 2022. Regulation 2022/1614. http://data.europa.eu/eli/reg_impl/2022/1614/oj
# - MPA: Seawise 2023. 
#--------------------------------------------------
# ## load VME and MPA shapefiles
# VME <- st_read(dsn = "C:/Users/Karin.Reijden/OneDrive - International Council for the Exploration of the Sea (ICES)/Documents/_GIS/VME_WW_Closures_MP_Official",
#                layer = "VME_WW_Closures_MP_Official")
# MPA <- st_read(dsn = "C:/Users/Karin.Reijden/OneDrive - International Council for the Exploration of the Sea (ICES)/Documents/_GIS/Seawise",
#                layer = "EU__plus_UK_future_restrictions")
# 
# ## bind MPA and VMEs
# MPA2 <- MPA[,c("SiteCode", "rmv_bt__S0", "geometry")]
# names(MPA2) <- c("SiteCode", "vulnerable_to_BT", "geometry")
# VME$SiteCode <- paste0("VME_", 1:87)
# VME$vulnerable_to_BT <- 1
# VME2 <- VME[, c("SiteCode", "vulnerable_to_BT", "geometry")]
# MPAVME <- rbind(MPA2, VME2)
# rm(MPA, MPA2, VME, VME2)
# #save(MPAVME, file=paste0(pathdir, "/6 - Other/MPAVME.Rdata"))
# save(MPAVME, file="C:/Users/Karin.Reijden/OneDrive - International Council for the Exploration of the Sea (ICES)/Documents/_GIS/Seawise/MPAVME.Rdata")
load(file="C:/Users/Karin.Reijden/OneDrive - International Council for the Exploration of the Sea (ICES)/Documents/_GIS/Seawise/MPAVME.Rdata")


#--------------------------------------------------
## Merge all regional grids, overlap with MPAVME, save as grid_over. 
#--------------------------------------------------
setwd(paste(pathdir,"1 - Input env",sep="/"))

## Get first region as template
load("Greater North Sea_EU2region_grid_sensitivity.RData")
Region <- Region[,c("csquares","Ecoregion", "area_sqkm","longitude","latitude","Depth",
                    "medlong","intercept","slope","division")]
Regiontot <- Region

## Merge other regional data to template
for(iReg in c("Baltic Sea", "Celtic Seas", "Bay of Biscay and the Iberian Coast")){
  load(paste0(iReg, "_EU2region_grid_sensitivity.RData"))
  Region <- Region[,c("csquares","Ecoregion", "area_sqkm","longitude","latitude","Depth",
                      "medlong","intercept","slope","division")]
  Regiontot <- rbind(Regiontot, Region)
} # end iReg-loop
rm(Region)

Regiontot <- st_as_sf(Regiontot)
Regiontot <- st_transform(Regiontot, 4326)

# ## Select all csquares that intersect
# grid_over   <- st_intersects(st_make_valid(Regiontot),st_make_valid(MPAVME)) ## takes long time!
# grid_over   <- as.data.frame(grid_over)
# Regionsub   <- Regiontot[grid_over$row.id,] # only keep csquares with VME-MPA match
# Regionsub   <- Regionsub[!duplicated(Regionsub$csquares), ] # only keep unique csquare-MPAVME matches.
# 
# ## Obtain actual intersection for all MPAVME-data
# grid_over   <- st_intersection(Regionsub,st_make_valid(MPAVME))
# grid_over$areakm2  <- as.numeric(st_area(st_make_valid(grid_over))/10^6)
# save(grid_over, file=paste0(pathdir, "/6 - Other/grid_over.Rdata"))
load(file=paste0(pathdir, "/6 - Other/grid_over.Rdata"))

#--------------------------------------------------
## Determine MPAVME-coverage per region
#--------------------------------------------------
# Obtain MPA-coverage for all MPAs
grid_inMPA    <- aggregate(grid_over$areakm2,by=list(grid_over$csquares),FUN=sum)
Regiontot$areasqkm_MPA <- grid_inMPA$x [match(Regiontot$csquares,grid_inMPA$Group.1)]
Regiontot$areasqkm_MPA[is.na(Regiontot$areasqkm_MPA)] <- 0
Regiontot$fraction_MPA <- ifelse(Regiontot$areasqkm_MPA > Regiontot$area_sqkm, 1,
                               Regiontot$areasqkm_MPA/Regiontot$area_sqkm)

# obtain MPA-coverage for all vulnerable MPAs
grid_overVULN <- subset(grid_over, vulnerable_to_BT == 1)
grid_inMPAVuln <- aggregate(grid_overVULN$areakm2,by=list(grid_overVULN$csquares),FUN=sum)
Regiontot$areasqkm_MPAV<- grid_inMPAVuln$x [match(Regiontot$csquares,grid_inMPAVuln$Group.1)]
Regiontot$areasqkm_MPAV[is.na(Regiontot$areasqkm_MPAV)] <- 0
Regiontot$fraction_MPAV <- ifelse(Regiontot$areasqkm_MPAV > Regiontot$area_sqkm, 1,
                                 Regiontot$areasqkm_MPAV/Regiontot$area_sqkm)


#--------------------------------------------------
## create dt of marine reporting areas, incl right order
#--------------------------------------------------
Sregions <- c("Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas",  "Greater North Sea")
NS_div <- c("Kattegat", "Norwegian Trench", "Central North Sea", "Southern North Sea", "Channel")
BS_div <- c("Bothnian_BS", "GulfF_BS", "GulfR_BS", "Proper_BS", "ArkBor_BS","Western_BS")
CS_div <- c("Northern Celtic Sea" ,"Southern Celtic Sea")
BoBIC_div <- c("Gulf of Biscay", "North-Iberian Atlantic", "South-Iberian Atlantic", "Gulf of Cadiz")
RegDivs <- data.table(EcoReg=c(rep(Sregions[1],6),rep(Sregions[2],4),
                               rep(Sregions[3],2),rep(Sregions[4],5)),
                      Division = c(BS_div, BoBIC_div, CS_div, NS_div))
RegDivs$DivName <- c("Bothnian area", "Gulf of Finland", "Gulf of Riga", "Baltic Proper", "Arkona & Bornholm Basin", "Western Baltic Sea",
                     "Gulf of Biscay", "North-Iberian Atlantic", "South-Iberian Atlantic", "Gulf of Cadiz", "Northern Celtic Sea", "Southern Celtic Sea", 
                     "Kattegat", "Norwegian Trench", "Central North Sea", "Southern North Sea", "Channel")
RegDivs$DivOrder <- c(1:nrow(RegDivs))
RegDivs$RegABB  <- c(rep("BS", 6), rep("BBIC", 4), rep("CS", 2), rep("GNS", 5))
RegDivs$DivABB  <- c("BA", "GoF", "GoR", "BP", "ABB", "WBS", "GoB", "NIA", "SIA", "GoC", "NCS", "SCS", "KG", "NT", "CNS", "SNS", "Ch")


#--------------------------------------------------
# output  tab 1 and 2; mpa area per region/division (both all and vulnerable MPAs)
#--------------------------------------------------
# MPA area per ecoregion
tab1 <- aggregate(list(Regiontot$areasqkm_MPA, Regiontot$areasqkm_MPAV, Regiontot$area_sqkm),by=list(Regiontot$Ecoregion),FUN=sum)
colnames(tab1) <- c("Ecoregion","Ext_MPA","Ext_MPAV", "TotExt")
tab1$Pct_MPA <- round(tab1$Ext_MPA/ tab1$TotExt * 100, digits=1)
tab1$Pct_MPAV <- round(tab1$Ext_MPAV/ tab1$TotExt * 100, digits=1)
tab1[,2:4] <- round(tab1[,2:4], digits=0)
tab1 <- tab1[,c("Ecoregion", "TotExt", "Ext_MPA", "Ext_MPAV", "Pct_MPA", "Pct_MPAV")]

#                           Ecoregion  TotExt  Ext_MPA Ext_MPAV  Pct_MPA Pct_MPAV
#                          Baltic Sea  367275    87225    45462     23.7     12.4
# Bay of Biscay and the Iberian Coast  753778   203115    62470     26.9      8.3
#                         Celtic Seas  492691    90536    35349     18.4      7.2
#                   Greater North Sea  220977   102376    46476     46.3     21.0

# MPA area per subdivision
tab2 <- aggregate(list(Regiontot$areasqkm_MPA, Regiontot$areasqkm_MPAV, Regiontot$area_sqkm),by=list(Regiontot$division, Regiontot$Ecoregion),FUN=sum)
colnames(tab2) <- c("Division","Ecoregion", "Ext_MPA", "Ext_MPAV","TotExt")
tab2$Pct_MPA <- round(tab2$Ext_MPA/ tab2$TotExt * 100, digits=1)
tab2$Pct_MPAV <- round(tab2$Ext_MPAV/ tab2$TotExt * 100, digits=1)
tab2[,3:5] <- round(tab2[,3:5], digits=0)
tab2 <- tab2[,c("Ecoregion","Division", "TotExt", "Ext_MPA", "Ext_MPAV", "Pct_MPA", "Pct_MPAV")]
tab2$Division <- RegDivs$DivName [match(tab2$Division, RegDivs$Division)]
tab2$DivOrder <- RegDivs$DivOrder [match(tab2$Division, RegDivs$DivName)]
tab2 <- tab2[order(tab2$DivOrder),]
tab2$DivOrder<- NULL

#                           Ecoregion                Division TotExt Ext_MPA Ext_MPAV Pct_MPA Pct_MPAV
#                          Baltic Sea           Bothnian area 114889    6074     2882     5.3      2.5
#                          Baltic Sea         Gulf of Finland  17831    4002     1732    22.4      9.7
#                          Baltic Sea            Gulf of Riga  19106   17987     7007    94.1     36.7
#                          Baltic Sea           Baltic Proper 136149   20280    11781    14.9      8.7
#                          Baltic Sea Arkona & Bornholm Basin  59953   28464    16913    47.5     28.2
#                          Baltic Sea      Western Baltic Sea  19264   10417     5145    54.1     26.7
# Bay of Biscay and the Iberian Coast          Gulf of Biscay  84068   69808     9719    83.0     11.6
# Bay of Biscay and the Iberian Coast  North-Iberian Atlantic 388827   90455    19857    23.3      5.1
# Bay of Biscay and the Iberian Coast  South-Iberian Atlantic 268679   37512    27689    14.0     10.3
# Bay of Biscay and the Iberian Coast           Gulf of Cadiz  12204    5339     5205    43.7     42.7
#                         Celtic Seas     Northern Celtic Sea 245705   25789    20736    10.5      8.4
#                         Celtic Seas     Southern Celtic Sea 246986   64746    14613    26.2      5.9
#                   Greater North Sea                Kattegat  23197   10266     3923    44.3     16.9
#                   Greater North Sea        Norwegian Trench   9484    3849     1916    40.6     20.2
#                   Greater North Sea       Central North Sea  18991    2737     1671    14.4      8.8
#                   Greater North Sea      Southern North Sea 143002   73482    34282    51.4     24.0
#                   Greater North Sea                 Channel  26089   11747     4596    45.0     17.6

write.csv2(tab1, file=paste0(pathdir, "/6 - Other/table1.csv"), row.names = FALSE)
write.csv2(tab2, file=paste0(pathdir, "/6 - Other/table2.csv"), row.names = FALSE)


#--------------------------------------------------
# Determine MSFD habitat per MPAs (Table 3)
#--------------------------------------------------
# MPA area per MSFD habitat
load("Celtic Seas_MSFD2_per_csquare.RData")
msfd <- msfd_csq
load("Greater North Sea_MSFD2_per_csquare.RData")
msfd <- rbind(msfd,msfd_csq)
load("Bay of Biscay and the Iberian Coast_MSFD2_per_csquare.RData")
msfd <- rbind(msfd,msfd_csq)
load("Baltic Sea_MSFD2_per_csquare.RData")
msfd <- rbind(msfd,msfd_csq)

msfd <- cbind(msfd,Regiontot[match(msfd$csquares,Regiontot$csquares),c("fraction_MPA", "fraction_MPAV", "Ecoregion", "division")])
msfd$MPA_Ext_msfd <- msfd$fraction_MPA * msfd$area_km2
msfd$MPAV_Ext_msfd <- msfd$fraction_MPAV * msfd$area_km2

# Determine % MPA and %MPAV per habitat per ecoregion
tab3R <- aggregate(list(msfd$MPA_Ext_msfd,msfd$MPAV_Ext_msfd, msfd$area_km2),by=list(msfd$MSFD, msfd$Ecoregion),FUN=sum)
colnames(tab3R) <- c("MSFD", "Ecoregion", "Ext_MPA", "Ext_MPAV", "TotExt")
tab3R$Pct_MPA <- round(tab3R$Ext_MPA/ tab3R$TotExt * 100, digits = 1)
tab3R$Pct_MPAV <- round(tab3R$Ext_MPAV/ tab3R$TotExt * 100, digits = 1)

# Determine % MPA and %MPAV per habitat per division
tab3D <- aggregate(list(msfd$MPA_Ext_msfd,msfd$MPAV_Ext_msfd, msfd$area_km2),by=list(msfd$MSFD, msfd$Ecoregion, msfd$division),FUN=sum)
colnames(tab3D) <- c("MSFD", "Ecoregion", "Division", "Ext_MPA", "Ext_MPAV", "TotExt")
tab3D$Pct_MPA <- round(tab3D$Ext_MPA/ tab3D$TotExt * 100, digits = 1)
tab3D$Pct_MPAV <- round(tab3D$Ext_MPAV/ tab3D$TotExt * 100, digits = 1)

## create Table 3 - per ecoregion
for(iReg in unique(RegDivs$EcoReg)){
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
                              "Abyssal", "Unknown"), 
                     habOrd = 1:28)
  
  tab3RSel <- subset(tab3R, Ecoregion == iReg)
  tab3RSel <- tab3RSel[,c("MSFD", "TotExt", "Pct_MPA", "Pct_MPAV")]
  tab3RSel$TotExt <- round(tab3RSel$TotExt, digits=0)
  RegABB <- unique(subset(RegDivs, EcoReg == iReg)$RegABB)
  names(tab3RSel) <- c("MSFD", paste(RegABB, c("TotExt", "Pct_MPA", "Pct_MPAV"), sep="_"))
  Tab3 <- merge(Tab3, tab3RSel, by="MSFD", all.x=T)
  
  for(iDiv in unique(subset(RegDivs, EcoReg==iReg)$Division)){
    tab3DSel <- subset(tab3D, Division == iDiv)
    tab3DSel <- tab3DSel[,c("MSFD", "TotExt", "Pct_MPA", "Pct_MPAV")]
    tab3DSel$TotExt <- round(tab3DSel$TotExt, digits=0)
    DivABB <- unique(subset(RegDivs, Division == iDiv)$DivABB)
    names(tab3DSel) <- c("MSFD", paste(DivABB, c("TotExt", "Pct_MPA", "Pct_MPAV"), sep="_"))
    Tab3 <- merge(Tab3, tab3DSel, by="MSFD", all.x=T)
  } # end iDiv
  Tab3 <- Tab3[order(Tab3$habOrd),]
  Tab3$habOrd <- NULL
  write.csv2(Tab3, file=paste0(pathdir, "/6 - Other/Table3_", RegABB, ".csv"), row.names=F)
}

#--------------------------------------------------
# Determine fishing activity in MPA and MPAV per ecoregion/division
#--------------------------------------------------
#Load fisheries data
pathdir_nogit <- "D:/TRADE4_Karin/WKTRADE4 - Fisheries restricted"
load(paste(pathdir_nogit,paste("Greater North Sea_fisheries.RData",sep="_"),sep="/")) 
findat <- Fisheries
load(paste(pathdir_nogit,paste("Baltic Sea_fisheries.RData",sep="_"),sep="/")) 
findat <- rbind(findat,Fisheries)
load(paste(pathdir_nogit,paste("Bay of Biscay and the Iberian Coast_fisheries.RData",sep="_"),sep="/")) 
findat <- rbind(findat,Fisheries)
load(paste(pathdir_nogit,paste("Celtic Seas_fisheries.RData",sep="_"),sep="/")) 
findat <- rbind(findat,Fisheries)
findat[is.na(findat)] <- 0
  
AssPeriod <- 2017:2022
SARnam <- paste("surface_sar",AssPeriod,sep="_")
VALnam <- paste("total_value", AssPeriod, sep="_")
KGnam <- paste("total_weight", AssPeriod, sep="_")
Regiontot <- cbind(Regiontot, findat[match(Regiontot$csquares,findat$csquares), c(SARnam, VALnam, KGnam)])
# determine average sar, landings value and weight over the 6 years
Regiontot$avgsar <- rowMeans(st_drop_geometry(Regiontot[,SARnam]))
Regiontot$avgval <- rowMeans(st_drop_geometry(Regiontot[,VALnam]))
Regiontot$avglkg <- rowMeans(st_drop_geometry(Regiontot[,KGnam]))

# calculate average swept area per c-square
Regiontot$avSweptArea <- Regiontot$avgsar * Regiontot$area_sqkm

Regiontot$MPAsa <- Regiontot$avSweptArea * Regiontot$fraction_MPA
Regiontot$MPAVsa <- Regiontot$avSweptArea * Regiontot$fraction_MPAV
Regiontot$MPAval <- Regiontot$avgval * Regiontot$fraction_MPA
Regiontot$MPAVval <- Regiontot$avgval * Regiontot$fraction_MPAV
Regiontot$MPAlkg <- Regiontot$avglkg * Regiontot$fraction_MPA
Regiontot$MPAVlkg <- Regiontot$avglkg * Regiontot$fraction_MPAV

tab4R <- aggregate(list(Regiontot$area_sqkm, Regiontot$MPAsa, Regiontot$MPAVsa, Regiontot$avSweptArea, 
                       Regiontot$MPAval, Regiontot$MPAVval, Regiontot$avgval,
                       Regiontot$MPAlkg, Regiontot$MPAVlkg, Regiontot$avglkg),by=list(Regiontot$Ecoregion),FUN=sum)
colnames(tab4R) <- c("Ecoregion", "TotExt", "MPAsa", "MPAVsa", "TOTsa","MPAval","MPAVval", "TOTval","MPAlkg","MPAVlkg", "TOTlkg")
tab4R$MPAsaP <- round(tab4R$MPAsa/ tab4R$TOTsa * 100, digits=1)
tab4R$MPAVsaP <- round(tab4R$MPAVsa/ tab4R$TOTsa * 100, digits=1)
tab4R$MPAvalP <- round(tab4R$MPAval/ tab4R$TOTval * 100, digits=1)
tab4R$MPAVvalP <- round(tab4R$MPAVval/ tab4R$TOTval * 100, digits=1)
tab4R$MPAlkgP <- round(tab4R$MPAlkg/ tab4R$TOTlkg * 100, digits=1)
tab4R$MPAVlkgP <- round(tab4R$MPAVlkg/ tab4R$TOTlkg * 100, digits=1)
tab4R <- tab4R[,c("Ecoregion","TotExt", "TOTsa", "MPAsaP", "MPAVsaP", "TOTval", "MPAvalP", "MPAVvalP", "TOTlkg", "MPAlkgP", "MPAVlkgP")]
tab4R$Division = "All combined"
tab4R$DivOrd <- 0

tab4D <- aggregate(list(Regiontot$area_sqkm, Regiontot$MPAsa, Regiontot$MPAVsa, Regiontot$avSweptArea, 
                        Regiontot$MPAval, Regiontot$MPAVval, Regiontot$avgval,
                        Regiontot$MPAlkg, Regiontot$MPAVlkg, Regiontot$avglkg),by=list(Regiontot$Ecoregion, Regiontot$division), FUN=sum)
colnames(tab4D) <- c("Ecoregion", "Division", "TotExt", "MPAsa","MPAVsa", "TOTsa","MPAval","MPAVval", "TOTval","MPAlkg","MPAVlkg", "TOTlkg")
tab4D$MPAsaP <- round(tab4D$MPAsa/ tab4D$TOTsa * 100, digits=1)
tab4D$MPAVsaP <- round(tab4D$MPAVsa/ tab4D$TOTsa * 100, digits=1)
tab4D$MPAvalP <- round(tab4D$MPAval/ tab4D$TOTval * 100, digits=1)
tab4D$MPAVvalP <- round(tab4D$MPAVval/ tab4D$TOTval * 100, digits=1)
tab4D$MPAlkgP <- round(tab4D$MPAlkg/ tab4D$TOTlkg * 100, digits=1)
tab4D$MPAVlkgP <- round(tab4D$MPAVlkg/ tab4D$TOTlkg * 100, digits=1)
tab4D <- tab4D[,c("Ecoregion", "Division", "TotExt","TOTsa", "MPAsaP", "MPAVsaP", "TOTval", "MPAvalP", "MPAVvalP", "TOTlkg", "MPAlkgP", "MPAVlkgP")]
tab4D$Division <- RegDivs$DivName [match(tab4D$Division, RegDivs$Division)]
tab4D$DivOrd <- RegDivs$DivOrder [match(tab4D$Division, RegDivs$DivName)]

Tab4 <- rbind(tab4R, tab4D)
Tab4 <- Tab4[order(Tab4$Ecoregion, Tab4$DivOrd),]
Tab4$DivOrd <- NULL
Tab4$TOTval <- round(Tab4$TOTval / 1E6, digits=0)
Tab4$TOTlkg <- round(Tab4$TOTlkg / 1E6, digits=0)
Tab4$TOTsa <- round(Tab4$TOTsa, digits=0)
Tab4$TotExt <- round(Tab4$TotExt, digits=0)
Tab4 <- Tab4[,c(1, 12, 2:11)] # change order of columns

write.csv2(Tab4, file=paste0(pathdir, "/6 - Other/Table4.csv"), row.names = FALSE)
