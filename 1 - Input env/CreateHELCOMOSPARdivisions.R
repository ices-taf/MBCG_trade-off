## Script to create HELCOM and OSPAR divisions/ecoregions

setwd("D:/MBCG_trade-off/1 - Input env")
library(sf)
library(ggplot2)

OSPAR <- st_read("OSPAR/ospar_phys_dist_habs_au_2022_06_001.shp")
OSPAR <- st_zm(OSPAR) # remove Z-dimension
OSPAR$EcoReg <- ifelse(OSPAR$Region == "IV", "Bay of Biscay and the Iberian Coast",
                       ifelse(OSPAR$Region == "III", "Celtic Seas",
                              ifelse(OSPAR$Region == "II", "Greater North Sea", "Other")))
OSPAR$Division <- OSPAR$Label
OSPAR <- OSPAR[,c("EcoReg", "Division", "geometry")]
OSPAR <- st_transform(OSPAR, 4326)

HELCOM <- st_read("HELCOM/HELCOM_subbasins_2022_level2.shp") # https://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/d4b6296c-fd19-462c-94d2-4c81b9313d77
HELCOM <- st_zm(HELCOM) # remove Z and M dimensions
HELCOM$Division <- ifelse(HELCOM$level_2 %in% c("Great Belt", "Kiel Bay", "Bay of Mecklenburg"), "Western_BS",
                          ifelse(HELCOM$level_2 %in% c("Arkona Basin", "Bornholm Basin"), "ArkBor_BS",
                                 ifelse(HELCOM$level_2 %in% c("Western Gotland Basin", "Gdansk Basin", 
                                                              "Eastern Gotland Basin", "Northern Baltic Proper"), "Proper_BS",
                                        ifelse(HELCOM$level_2 %in% c("Åland Sea", "Bothnian Bay", "Bothnian Sea", "The Quark"), "Bothnian_BS",
                                               ifelse(HELCOM$level_2 == "Gulf of Finland", "GulfF_BS",
                                                      ifelse(HELCOM$level_2 == "Gulf of Riga", "GulfR_BS", "Other"))))))
HELCOM$EcoReg <- ifelse(HELCOM$Division %in% c("Western_BS", "ArkBor_BS", "Proper_BS", "Bothnian_BS", "GulfF_BS", "GulfR_BS"), "Baltic Sea", "Other")
HELCOM <- HELCOM[,c("EcoReg", "Division", "geometry")]
HELCOM <- st_transform(HELCOM, 4326)
RegDivShapes <- rbind(OSPAR, HELCOM)
RegDivShapes <- subset(RegDivShapes, EcoReg != "Other")
save(RegDivShapes, file="RegDivShapes.Rdata")

## Manual merging of HELCOM basins in ARCGIS done
setwd("D:/MBCG_trade-off/1 - Input env")

RegDivShapes <- st_read("RegDivShapes/RegDivShapes2.shp")
RegDivShapes$DivName <- c(unique(RegDivShapes$Division)[1:11], "Bothnian area", "Arkona & Bornholm Basin", "Western Baltic Sea", "Baltic Proper", "Gulf of Finland", "Gulf of Riga")
save(RegDivShapes, file="RegDivShapes2.Rdata")


#ggplot() + geom_sf(data=RegDivShapes, aes(fill=Division))


