# quality-extent threshold trade-offs

## Create table to store outputs per Assregion
GM_QETtable <- data.table()

### Determine most abundant habitat types
## Add habitat info
Reg <- st_as_sf(Region)
Reg <- data.table(st_drop_geometry(Reg))
RegHab <- merge(Reg, msfd_csq, all=TRUE, by="csquares")
if(Assunit == "Division"){
  RegHab <- RegHab[division == Assregion,]
}

## Remove gridcells 
RegHab <- RegHab[!is.na(MSFD),] # with unknown MSFD
RegHab <- RegHab[!is.na(medlong)] # without medlong

## Select 5 most abundant BHTs (copy from table 3)
tab3 <- read.csv(file=paste(pathdir, "5 - Output", Assunit, Assregion, paste0(Assregion, "_Table_3.csv"), sep="/"))
tab3 <- subset(tab3, tab3$MSFD.broad.habitat.type %in% RegHab$MSFD)
IDs <- tab3$MSFD.broad.habitat.type[1:5]

## Calculate average RBS-value over 2017-2022
clmnids <- c(paste0("state_", 2017:2022))
State_reg2 <- data.table(State_reg)
State_reg2 <- State_reg2[, ..clmnids]
State_reg2[is.na(State_reg2)] <- 1 # I think Daniel does the same?
State_reg2[State_reg2>1] <- 1 # you cannot have a RBS > 1
State_reg2$State <- rowMeans(State_reg2)
State_reg2$csquares <- State_reg$Fisheries.csquares
RegHab$State <- State_reg2$State [match(RegHab$csquares, State_reg2$csquares)]    

## Select grid cells with RBS info in selected habitat types
RegHab <- filter(RegHab, MSFD %in% IDs) 
RegHab2 <- RegHab[!is.na(RegHab$State)]

## create table for proportion of grid cells with RBS above quality thresholds
columns <- c('ID')
table <- data.frame(matrix(nrow = 5, ncol = length(columns)))
colnames(table) <- columns

for(i in 1:length(IDs)){
  Unit.data = subset(RegHab2, MSFD == IDs[i])
  Unit.data = Unit.data[order(Unit.data$State),]
  n = nrow(Unit.data)
  table$q.95[i] = nrow(Unit.data[Unit.data$State >= .95,])/n
  table$q.90[i] = nrow(Unit.data[Unit.data$State >= .90,])/n
  table$q.85[i] = nrow(Unit.data[Unit.data$State >= .85,])/n
  table$q.80[i] = nrow(Unit.data[Unit.data$State >= .80,])/n
  table$q.75[i] = nrow(Unit.data[Unit.data$State >= .75,])/n
  table$q.70[i] = nrow(Unit.data[Unit.data$State >= .70,])/n
  table$q.65[i] = nrow(Unit.data[Unit.data$State >= .65,])/n
  table$q.60[i] = nrow(Unit.data[Unit.data$State >= .60,])/n
  table$q.55[i] = nrow(Unit.data[Unit.data$State >= .55,])/n
  table$ID[i] = unique(Unit.data$MSFD)
} # end i

## Formatting table to nice output  
table2 <- gather(table, key = "quality", value = "extent",
                   q.95, q.90, q.85, q.80, q.75, q.70, q.65, q.60, q.55)
table2 <- table2 %>%
  mutate(quality = recode(quality, "q.95" = "0.95", "q.90" = "0.90", "q.85" = "0.85",
                          "q.80" = "0.80","q.75" = "0.75", "q.70" = "0.70", 
                          "q.65" = "0.65", "q.60" = "0.60","q.55" = "0.55"))
  
table2[3] <- round(table2[3], digits = 2)
table2 <- table2[order(table2$ID),]
table2$ModificationLevel = 0
table2$Indicator = "PD"

## Save results in overall table.
GM_QETtable <- rbind(GM_QETtable, table2)
rm(State_reg2, table, table2, RegHab2) 

## Now for the 3 gear modification levels (PD)
for(iGMP in c(0.05, 0.1, 0.2)){

  ## Determine average State over 2017-2022
  clmnids <- c(paste0("GMPstate_", iGMP, "_", 2017:2022))
  if(iGMP == 0.05) {
    State_reg2 <- data.table(State_GMP5)}
  if(iGMP == 0.1) {
    State_reg2 <- data.table(State_GMP10)}
  if(iGMP == 0.2) {
    State_reg2 <- data.table(State_GMP20)}
  State_reg2 <- State_reg2[, ..clmnids]
  State_reg2[is.na(State_reg2)] <- 1 # I think Daniel does the same?
  State_reg2[State_reg2>1] <- 1 # you cannot have a RBS > 1
  State_reg2$State <- rowMeans(State_reg2)
  State_reg2$csquares <- State_reg$Fisheries.csquares
  
  ## Assign this new state to RegHab, remove any (new) NAs
  RegHab$State <- State_reg2$State [match(RegHab$csquares, State_reg2$csquares)]
  RegHab2 <- RegHab[!is.na(RegHab$State)]
  
  ## create table for proportion of grid cells with RBS above quality thresholds
  columns <- c('ID')
  table <- data.frame(matrix(nrow = 5, ncol = length(columns)))
  colnames(table) <- columns
  
  for(i in 1:length(IDs)){
    Unit.data = subset(RegHab2, MSFD == IDs[i])
    Unit.data = Unit.data[order(Unit.data$State),]
    n = nrow(Unit.data)
    table$q.95[i] = nrow(Unit.data[Unit.data$State >= .95,])/n
    table$q.90[i] = nrow(Unit.data[Unit.data$State >= .90,])/n
    table$q.85[i] = nrow(Unit.data[Unit.data$State >= .85,])/n
    table$q.80[i] = nrow(Unit.data[Unit.data$State >= .80,])/n
    table$q.75[i] = nrow(Unit.data[Unit.data$State >= .75,])/n
    table$q.70[i] = nrow(Unit.data[Unit.data$State >= .70,])/n
    table$q.65[i] = nrow(Unit.data[Unit.data$State >= .65,])/n
    table$q.60[i] = nrow(Unit.data[Unit.data$State >= .60,])/n
    table$q.55[i] = nrow(Unit.data[Unit.data$State >= .55,])/n
    table$ID[i] = unique(Unit.data$MSFD)
  } # end i
  
  table2 <- gather(table, key = "quality", value = "extent",
                   q.95, q.90, q.85, q.80, q.75, q.70, q.65, q.60, q.55)
  
  table2 <- table2 %>%
    mutate(quality = recode(quality, "q.95" = "0.95", "q.90" = "0.90", "q.85" = "0.85",
                            "q.80" = "0.80","q.75" = "0.75", "q.70" = "0.70", 
                            "q.65" = "0.65", "q.60" = "0.60","q.55" = "0.55"))
  
  table2[3] <- round(table2[3], digits = 2)
  table2 <- table2[order(table2$ID),]
  table2$ModificationLevel = iGMP
  table2$Indicator = "PD"
  
  GM_QETtable <- rbind(GM_QETtable, table2)
  rm(State_reg2, table, table2, Unit.data, RegHab2)
  } # end iGMP
  
## Now for PDsens, status quo
## Determine average State over 2017-2022
clmnids <- c(paste0("state_", 2017:2022))
State_reg2 <- data.table(State_reg_IL)
State_reg2 <- State_reg2[, ..clmnids]
State_reg2[is.na(State_reg2)] <- 1 # I think Daniel does the same?
State_reg2[State_reg2>1] <- 1 # you cannot have a RBS > 1
State_reg2$State <- rowMeans(State_reg2)
State_reg2$csquares <- State_reg$Fisheries.csquares

## Assign this new state to RegHab, remove any (new) NAs
RegHab$State <- State_reg2$State [match(RegHab$csquares, State_reg2$csquares)]
RegHab2 <- RegHab[!is.na(RegHab$State)]

## create table for proportion of grid cells with RBS above quality thresholds
columns <- c('ID')
table <- data.frame(matrix(nrow = 5, ncol = length(columns)))
colnames(table) <- columns

for(i in 1:length(IDs)){
  Unit.data = subset(RegHab2, MSFD == IDs[i])
  Unit.data = Unit.data[order(Unit.data$State),]
  n = nrow(Unit.data)
  table$q.95[i] = nrow(Unit.data[Unit.data$State >= .95,])/n
  table$q.90[i] = nrow(Unit.data[Unit.data$State >= .90,])/n
  table$q.85[i] = nrow(Unit.data[Unit.data$State >= .85,])/n
  table$q.80[i] = nrow(Unit.data[Unit.data$State >= .80,])/n
  table$q.75[i] = nrow(Unit.data[Unit.data$State >= .75,])/n
  table$q.70[i] = nrow(Unit.data[Unit.data$State >= .70,])/n
  table$q.65[i] = nrow(Unit.data[Unit.data$State >= .65,])/n
  table$q.60[i] = nrow(Unit.data[Unit.data$State >= .60,])/n
  table$q.55[i] = nrow(Unit.data[Unit.data$State >= .55,])/n
  table$ID[i] = unique(Unit.data$MSFD)
} # end i

table2 <- gather(table, key = "quality", value = "extent",
                 q.95, q.90, q.85, q.80, q.75, q.70, q.65, q.60, q.55)

table2 <- table2 %>%
  mutate(quality = recode(quality, "q.95" = "0.95", "q.90" = "0.90", "q.85" = "0.85",
                          "q.80" = "0.80","q.75" = "0.75", "q.70" = "0.70", 
                          "q.65" = "0.65", "q.60" = "0.60","q.55" = "0.55"))

table2[3] <- round(table2[3], digits = 2)
table2 <- table2[order(table2$ID),]
table2$ModificationLevel = 0
table2$Indicator = "PD-sens"

GM_QETtable <- rbind(GM_QETtable, table2)
rm(State_reg2, Reg, RegHab2, table, table2, Unit.data)

## Now for the 3 gear modification levels (PD-sens)
for(iGMP in c(0.05, 0.1, 0.2)){

  ## Determine average State over 2017-2022
  clmnids <- c(paste0("GMP_state_", iGMP, "_", 2017:2022))
  if(iGMP == 0.05) {
    State_reg2 <- data.table(State_sens_GMP5)}
  if(iGMP == 0.1) {
    State_reg2 <- data.table(State_sens_GMP10)}
  if(iGMP == 0.2) {
    State_reg2 <- data.table(State_sens_GMP20)}
  
  State_reg2 <- State_reg2[, ..clmnids]
  State_reg2[is.na(State_reg2)] <- 1 # I think Daniel does the same?
  State_reg2[State_reg2>1] <- 1 # you cannot have a RBS > 1
  State_reg2$State <- rowMeans(State_reg2)
  State_reg2$csquares <- State_reg$Fisheries.csquares

  ## Assign this new state to RegHab, remove any (new) NAs
  RegHab$State <- State_reg2$State [match(RegHab$csquares, State_reg2$csquares)]
  RegHab2 <- RegHab[!is.na(RegHab$State)]
  
  ## create table for proportion of grid cells with RBS above quality thresholds
  columns <- c('ID')
  table <- data.frame(matrix(nrow = 5, ncol = length(columns)))
  colnames(table) <- columns
  
  for(i in 1:length(IDs)){
    Unit.data = subset(RegHab2, MSFD == IDs[i])
    Unit.data = Unit.data[order(Unit.data$State),]
    n = nrow(Unit.data)
    table$q.95[i] = nrow(Unit.data[Unit.data$State >= .95,])/n
    table$q.90[i] = nrow(Unit.data[Unit.data$State >= .90,])/n
    table$q.85[i] = nrow(Unit.data[Unit.data$State >= .85,])/n
    table$q.80[i] = nrow(Unit.data[Unit.data$State >= .80,])/n
    table$q.75[i] = nrow(Unit.data[Unit.data$State >= .75,])/n
    table$q.70[i] = nrow(Unit.data[Unit.data$State >= .70,])/n
    table$q.65[i] = nrow(Unit.data[Unit.data$State >= .65,])/n
    table$q.60[i] = nrow(Unit.data[Unit.data$State >= .60,])/n
    table$q.55[i] = nrow(Unit.data[Unit.data$State >= .55,])/n
    table$ID[i] = unique(Unit.data$MSFD)
  } # end i
  
  table2 <- gather(table, key = "quality", value = "extent",
                   q.95, q.90, q.85, q.80, q.75, q.70, q.65, q.60, q.55)
  
  table2 <- table2 %>%
    mutate(quality = recode(quality, "q.95" = "0.95", "q.90" = "0.90", "q.85" = "0.85",
                            "q.80" = "0.80","q.75" = "0.75", "q.70" = "0.70", 
                            "q.65" = "0.65", "q.60" = "0.60","q.55" = "0.55"))
  
  table2[3] <- round(table2[3], digits = 2)
  table2 <- table2[order(table2$ID),]
  table2$ModificationLevel = iGMP
  table2$Indicator = "PD-sens"
  
  GM_QETtable <- rbind(GM_QETtable, table2)
  rm(State_reg2, RegHab2, table, table2, Unit.data)
} # end iGMP

## Write final results to the right subfolder 
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
save(GM_QETtable, file="GM_QETtable.Rdata")



