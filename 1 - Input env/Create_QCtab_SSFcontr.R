## Script to process Quality Control tables from WKTRADE4, to show SSF contribution in HTMLs.
library(data.table)
library(readxl)

setwd("D:/MBCG_trade-off/QC_tables")
QCtab <- data.table(read.csv("fdiMBCG_VL0012_year.csv", header=T))

## Determine mean, min, and max over 2017-2022 per FDIregion
QCtab2 <- QCtab[,.(kwfd = mean(kWFD_total),
                   SSF = mean(pctVL0012_kWFD),
                   SSFmin = min(pctVL0012_kWFD),
                   SSFmax = max(pctVL0012_kWFD)), by=c("sub_region")]
QCtab2 <- QCtab2[kwfd>2000,] ## Make cut of KWFD_total > 2000 kWFD
QCtab2$rangeSSF <- paste0("[", QCtab2$SSFmin, " - ", QCtab2$SSFmax, "]") 
names(QCtab2) <- c("FAOregion", "Tot_KWFD", "SSFc", "SSFmin", "SSFmax", "rangeSSF")
QCtab2 <- QCtab2[,c("FAOregion", "Tot_KWFD", "SSFc", "rangeSSF")]
QCtab2[,c(2,3)] <- round(QCtab2[,c(2,3)], digits=2)


# ## Determine mean, min, and max over 2017-2022 per FDIregion
# QCtab$SSFc <- round(rowMeans(QCtab[,2:7]), digits=1)
# QCtab$minSSF <- apply(QCtab[,2:7], MARGIN=1, FUN=function(x) min(x))
# QCtab$maxSSF <- apply(QCtab[,2:7], MARGIN=1, FUN=function(x) max(x))
# QCtab$rangeSSF <- paste0("[", QCtab$minSSF, " - ", QCtab$maxSSF, "]") 
# QCtab <- QCtab[,c("FAOregion", "SSFc", "rangeSSF")]

## Look-up data table based on manual match between FDI-subregions and our divisions.
divtoFAOreg <- data.table(division = c("Norwegian Trench", # 4a
                                       "Norwegian Trench", # 3a20
                                       "Central North Sea", #4a 
                                       "Central North Sea", # 4b
                                       "Central North Sea", # 3a20
                                       "Southern North Sea", # 4b
                                       "Southern North Sea", # 4c
                                       "Southern North Sea", # 3a20
                                       "Southern North Sea", #7d
                                       "Channel", # 7d
                                       "Channel", # 7e
                                       "Channel", # 4c
                                       "Kattegat", # 3a20
                                       "Kattegat", # 3b23
                                       "Kattegat", # 3a21
                                       "GulfF_BS", # d32 ## Start Baltic
                                       "GulfR_BS", # d28.1
                                       "GulfR_BS", # d29
                                       "GulfR_BS", # d32
                                       "ArkBor_BS", #d25
                                       "ArkBor_BS", #d24
                                       "ArkBor_BS", #b23
                                       "Western_BS", #c22
                                       "Western_BS", #d24
                                       "Proper_BS", #d25
                                       "Proper_BS", #d26
                                       "Proper_BS", #d27
                                       "Proper_BS", #d28.2
                                       "Proper_BS", #d29
                                       "Proper_BS", #d32
                                       "Bothnian_BS", #d29
                                       "Bothnian_BS", #d30
                                       "Bothnian_BS", #d31
                                       "Northern Celtic Seas", # 6a, 6b, 7b, 7c, 7k, 7j, 7a
                                       "Northern Celtic Seas",
                                       "Northern Celtic Seas",
                                       "Northern Celtic Seas",
                                       "Northern Celtic Seas",
                                       "Northern Celtic Seas",
                                       "Northern Celtic Seas",
                                       "Southern Celtic Seas", #7k, 7j, 7g, 7h, 7e
                                       "Southern Celtic Seas",
                                       "Southern Celtic Seas",
                                       "Southern Celtic Seas",
                                       "Southern Celtic Seas",
                                       "North-Iberian Atlantic", # 8a, 8b, 8c, 8d, 8e, 9a, 9b
                                       "North-Iberian Atlantic",
                                       "North-Iberian Atlantic",
                                       "North-Iberian Atlantic",
                                       "North-Iberian Atlantic",
                                       "North-Iberian Atlantic",
                                       "North-Iberian Atlantic",
                                       "Gulf of Biscay", # 8a, 8b, 8c, 8d
                                       "Gulf of Biscay",
                                       "Gulf of Biscay",
                                       "Gulf of Biscay",
                                       "South-Iberian Atlantic", # 9a, 9b
                                       "South-Iberian Atlantic",
                                       "Gulf of Cadiz"), # 9a
                          FAOregion = c("27.4.A", # NT
                                        "27.3.A.20", # NT
                                        "27.4.A", # CNS
                                        "27.4.B", # CNS
                                        "27.3.A.20", # CNS
                                        "27.4.C", # SNS
                                        "27.4.B", # SNS
                                        "27.3.A.20", # SNS
                                        "27.7.E", # SNS
                                        "27.7.D", # C
                                        "27.7.E", # C
                                        "27.4.C", # C
                                        "27.3.A.20", # KG
                                        "27.3.A.21", # KG
                                        "27.3.B.23", # KG; end NS
                                        "27.3.D.32", #Gulf Finland
                                        "27.3.D.28.1", # Gulf Riga
                                        "27.3.D.29", # gulf Riga
                                        "27.3.D.32", # Gulf riga
                                        "27.3.B.23", # arkbor
                                        "27.3.D.24", # arkbor
                                        "27.3.D.25", # arkbor
                                        "27.3.D.24", # Western
                                        "27.3.C.22", # western
                                        "27.3.D.26", # proper
                                        "27.3.D.27", #proper
                                        "27.3.D.28.2", #proper
                                        "27.3.D.29", #Proper
                                        "27.3.D.32", # proper
                                        "27.3.D.25", # proper
                                        "27.3.D.29", # bothnian x3
                                        "27.3.D.30",
                                        "27.3.D.31", # end baltic
                                        "27.6.A", # Northern CS
                                        "27.6.B",
                                        "27.7.A",
                                        "27.7.B",
                                        "27.7.C",
                                        "27.7.K",
                                        "27.7.J", # End Northern CS
                                        "27.7.E", # Southern CS
                                        "27.7.G",
                                        "27.7.H",
                                        "27.7.J",
                                        "27.7.K", # end Celtic
                                        "27.8.A", # North IB
                                        "27.8.B",
                                        "27.8.C",
                                        "27.8.D",
                                        "27.8.E",
                                        "27.9.A",
                                        "27.9.B", # end North IB
                                        "27.8.A", # Bay of Biscay
                                        "27.8.B",
                                        "27.8.C",
                                        "27.8.D", # end Bay of Biscay
                                        "27.9.A", # South-IB coast
                                        "27.9.B", # 
                                        "27.9.A"), # Gulf Cadiz
                          EcoRegion = c(rep("Greater North Sea", 15),
                                        rep("Baltic Sea", 18),
                                        rep("Celtic Seas", 12),
                                        rep("Bay of Biscay and the Iberian Coast", 14)))
divtoFAOreg$FAOregion <- tolower(divtoFAOreg$FAOregion)

FAOregs <- data.table(FAOregion = unique(divtoFAOreg$FAOregion), # https://www.fao.org/fishery/en/area/27/en
                      FAOname = c("Northern North Sea",
                                  "Skagerrak",
                                  "Central North Sea",
                                  "Southern North Sea",
                                  "Eastern English Channel",
                                  "Western English Channel",
                                  "Kattegat",
                                  "The Sound",
                                  "Gulf of Finland",
                                  "Gulf of Riga",
                                  "Archipelago Sea",
                                  "Baltic West of Bornholm",
                                  "Southern Central Baltic - West",
                                  "Belt Sea",
                                  "Southern Central Baltic - East",
                                  "West of Gotland",
                                  "East of Gotland (Open sea)",
                                  "Bothnian Sea",
                                  "Bothnian Bay",
                                  "Northwest Coast of Scotland and North Ireland or as the west of Scotland",
                                  "Rockall",
                                  "Irish Sea",
                                  "West of Ireland",
                                  "Porcupine Bank",
                                  "Southwest of Ireland - West",
                                  "Southwest of Ireland - East",
                                  "Celtic Sea North",
                                  "Celtic Sea South",
                                  "Bay of Biscay - North",
                                  "Bay of Biscay - Central",
                                  "Bay of Biscay - South",
                                  "Bay of Biscay - Offshore",
                                  "West of Bay of Biscay",
                                  "Portuguese Waters - East",
                                  "Portuguese Waters - West"))

QCtab <- merge(divtoFAOreg, QCtab2, on="FAOregion")
QCtab <- merge(QCtab, FAOregs, on = "FAOregion")
QCtab <- QCtab[,c(1,7,2:6)]
save(QCtab, file="D:/MBCG_trade-off/1 - Input env/QCtab.Rdata")
