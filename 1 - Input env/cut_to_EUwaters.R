#### Script to crop the sensitivity layers and all other data to the EU-waters for the ICES 2024 advice on bottom trawl impacts

library(sf)
library(data.table)
library(terra)
setwd("D:/MBCG_trade-off/1 - Input env")


EUwaters <- st_read(dsn="D:/MBCG_trade-off/1 - Input env/MSFD",
                   layer = "MSFD_Marine_Waters_LAEA_20220419_without_UK_and_CS")
EUwaters <- st_transform(EUwaters, 4326)
EUwaters$EUwaters <- "yes"
EUwaters$Shape_leng <- NULL

csquaresEU <- data.table()
for(iReg in c("Baltic Sea", "Greater North Sea", "Celtic Seas", "Bay of Biscay and the Iberian Coast")){
  load(paste0(iReg, "_region_grid_sensitivity.Rdata"))
  Reg <- st_as_sf(Region)
  Reg <- st_transform(Reg, 4326)
  Reg <- st_drop_geometry(Reg)
  Reg2 <- st_as_sf(Reg, coords = c("longitude","latitude"))
  
  RegEU <- terra:: intersect(vect(Reg2), vect(EUwaters))
  RegEU <- st_as_sf(RegEU)
  RegEU <- data.table(st_drop_geometry(RegEU))
  RegEU <- RegEU[,c("csquares", "Ecoregion", "division", "EUwaters")]
  csquaresEU <- rbind(csquaresEU, RegEU)

  RegionEU <- subset(Region, csquares %in% RegEU$csquares)
  Region <- RegionEU
  save(Region, file=paste0(iReg, "_EUregion_grid_sensitivity.Rdata"))
} # end iReg-loop
# save(csquaresEU, file="csquaresEU.Rdata")



### IN ADDITION: 'correct' for mismatching region - MSFD division csquares
## Region "Bay of Biscay and the Iberian Coast includes 104 c-squares that belong to the "Southern Celtic Sea" division (Celtic Seas)
## Region "Celtic Seas" includes 44 c-squares that belong the the "English Channel" division (Greater North Sea)

## Correct BBIC: Southern Celtic Sea division
setwd("D:/MBCG_trade-off/1 - Input env")
load("Bay of Biscay and the Iberian Coast_EUregion_grid_sensitivity.Rdata")
SCS <- subset(Region, division == "Southern Celtic Sea")
SCS$Ecoregion <- "Celtic Seas"
Region <- subset(Region, !division == "Southern Celtic Sea")
save(Region, file = "Bay of Biscay and the Iberian Coast_EU2region_grid_sensitivity.Rdata")

## Correct Celtic Seas: remove Channel division, add Southern Celtic Sea.
setwd("D:/MBCG_trade-off/1 - Input env")
load("Celtic Seas_EUregion_grid_sensitivity.Rdata")
channel <- subset(Region, division =="Channel")
channel$Ecoregion <- "Greater North Sea"
Region<- subset(Region, !division == "Channel")
Region <- rbind(Region, SCS)
save(Region, file="Celtic Seas_EU2region_grid_sensitivity.Rdata")

## Add Channel csquares to Greater North Sea Region dataset
load("Greater North Sea_EUregion_grid_sensitivity.Rdata")
Region <- rbind(Region, channel)
save(Region, file="Greater North Sea_EU2region_grid_sensitivity.Rdata")

## rename "Baltic Sea" region datafile for consistency in scripting.
load("Baltic Sea_EUregion_grid_sensitivity.Rdata")
save(Region, file="Baltic Sea_EU2region_grid_sensitivity.Rdata")

## Create updated regional MSFD_per_csquare-files. (with the correct c-squares)
MSFDCSQ <- data.table()
for (iReg in c("Baltic Sea", "Greater North Sea", "Celtic Seas", "Bay of Biscay and the Iberian Coast")){
  load(paste0(iReg, "_MSFD_per_csquare.Rdata"))
  MSFDCSQ <- rbind(MSFDCSQ, data.table(msfd_csq))
}

for(iReg in c("Baltic Sea", "Greater North Sea", "Celtic Seas", "Bay of Biscay and the Iberian Coast")){
  load(paste0(iReg, "_EU2region_grid_sensitivity.Rdata"))
  msfd_csq <- MSFDCSQ[csquares %in% unique(Region@data$csquares)]
  save(msfd_csq, file=paste0(iReg, "_MSFD2_per_csquare.Rdata"))
}

