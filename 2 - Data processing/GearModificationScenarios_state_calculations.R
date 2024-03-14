
##################################################################
# Gear modification scenarios
# calculate state for each year


for(iGMP in c(0.05, 0.1, 0.2)){
  
  ### load sensitivity layer and fishing to calculate impact for an ecoregion
  # load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"EU2region_grid_sensitivity.RData",sep="_"),sep="/")) 
  
  # load the Fisheries for the region
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  
  # load RBS and RBS-sens scripts
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Impact_PD_sensitive.R")
  
  
  ## 'normal PD' indicator
  # loop each year and calculate state 
  state_year <- c()
  ccname <- c()
  
  FisheriesMet <-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  # depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
  for (i in 1: length(Period)){
    loopdata <- FisheriesMet
    Depl_DRB_MOL          <- 0.200 * (1 - iGMP) * loopdata[,paste("DRB_MOL_surface_sar",Period[i],sep="_")]
    Depl_OT_CRU           <- 0.100 * (1 - iGMP) * loopdata[,paste("OT_CRU_surface_sar",Period[i],sep="_")]
    Depl_OT_DMF           <- 0.026 * (1 - iGMP) * loopdata[,paste("OT_DMF_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX           <- 0.074 * (1 - iGMP) * loopdata[,paste("OT_MIX_surface_sar",Period[i],sep="_")]
    Depl_OT_SPF           <- 0.009 * (1 - iGMP) * loopdata[,paste("OT_SPF_surface_sar",Period[i],sep="_")]
    Depl_SDN_DMF          <- 0.009 * (1 - iGMP) * loopdata[,paste("SDN_DMF_surface_sar",Period[i],sep="_")]
    Depl_SSC_DMF          <- 0.016 * (1 - iGMP) * loopdata[,paste("SSC_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_CRU          <- 0.060 * (1 - iGMP) * loopdata[,paste("TBB_CRU_surface_sar",Period[i],sep="_")]
    Depl_TBB_DMF          <- 0.140 * (1 - iGMP) * loopdata[,paste("TBB_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_MOL          <- 0.060 * (1 - iGMP) * loopdata[,paste("TBB_MOL_surface_sar",Period[i],sep="_")]
    
     ### calculate state for all gears
    Depl <- cbind(Depl_DRB_MOL,Depl_OT_CRU,Depl_OT_DMF,Depl_OT_MIX,
                  Depl_OT_SPF,Depl_SDN_DMF,Depl_SSC_DMF,Depl_TBB_CRU,Depl_TBB_DMF,Depl_TBB_MOL)
    Depl_tot<-rowSums(Depl,na.rm=T)
    loopdata$Depl_tot <- Depl_tot
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_tot[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("GMPstate", iGMP, Period[i],sep="_"))

  } # end i
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
  setwd(paste0(pathdir_nogit, "/GM_Scenarios"))
  save(State_reg,file=paste0(EcoReg,"_state_PD_GMP", iGMP, ".RData"))
  

  rm(State_reg, Region, Fisheries, FisheriesMet, state_year, ccname, loopdata)
  ## new PD-sens indicator
  
  ### load sensitivity layer and fishing to calculate impact for an ecoregion
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"EU2region_grid_sensitivity.RData",sep="_"),sep="/")) 
  
  # load the Fisheries for the region
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  
  # loop each year and calculate state 
  state_year <- c()
  ccname <- c()
  
  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  # depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
  for (i in 1: length(Period)){
    loopdata <- FisheriesMet
    Depl_DRB_MOL          <- 0.200 * (1 - iGMP) * loopdata[,paste("DRB_MOL_surface_sar",Period[i],sep="_")]
    Depl_OT_CRU           <- 0.100 * (1 - iGMP) * loopdata[,paste("OT_CRU_surface_sar",Period[i],sep="_")]
    Depl_OT_DMF           <- 0.026 * (1 - iGMP) * loopdata[,paste("OT_DMF_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX           <- 0.074 * (1 - iGMP) * loopdata[,paste("OT_MIX_surface_sar",Period[i],sep="_")]
    Depl_OT_SPF           <- 0.009 * (1 - iGMP) * loopdata[,paste("OT_SPF_surface_sar",Period[i],sep="_")]
    Depl_SDN_DMF          <- 0.009 * (1 - iGMP) * loopdata[,paste("SDN_DMF_surface_sar",Period[i],sep="_")]
    Depl_SSC_DMF          <- 0.016 * (1 - iGMP) * loopdata[,paste("SSC_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_CRU          <- 0.060 * (1 - iGMP) * loopdata[,paste("TBB_CRU_surface_sar",Period[i],sep="_")]
    Depl_TBB_DMF          <- 0.140 * (1 - iGMP) * loopdata[,paste("TBB_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_MOL          <- 0.060 * (1 - iGMP) * loopdata[,paste("TBB_MOL_surface_sar",Period[i],sep="_")]
    
    ### calculate state for all gears
    Depl <- cbind(Depl_DRB_MOL,Depl_OT_CRU,Depl_OT_DMF,Depl_OT_MIX,
                      Depl_OT_SPF,Depl_SDN_DMF,Depl_SSC_DMF,Depl_TBB_CRU,Depl_TBB_DMF,Depl_TBB_MOL)
    Depl_tot<-rowSums(Depl,na.rm=T)
    loopdata$Depl_tot <- Depl_tot
    
    # calculate slope and intercept and use RBS function for PD-sens
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_tot[j] > 0 ) {
        state[j] <- RBS_sens(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("GMP_state",iGMP, Period[i],sep="_"))
  } # end i
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
  setwd(paste0(pathdir_nogit, "/GM_Scenarios"))
  save(State_reg,file=paste0(EcoReg,"_state_PDsens_GMP", iGMP, ".RData"))
  
} # end iGMP
