rm(list = ls())

### github folder
  pathdir <- "D:/MBCG_trade-off"

### folder for restricted VMS data
  pathdir_nogit <- "D:/MBCG_trade-off_NOGIT"

### get all libraries
  source(paste(pathdir,"Utilities/Libraries_WKTRADE3.R",sep="/"))

### select time period
  Period    <- 2017:2022    # period with fishing data to calculate impact (updated data covers only 2017-2022)
  AssPeriod <- 2017:2022    # assessment period

### create list of marine reporting areas
  Sregions <- c("Greater North Sea", "Baltic Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
  NS_div <- c("Kattegat", "Norwegian Trench", "Central North Sea", "Southern North Sea", "Channel")
  BS_div <- c("Bothnian_BS", "GulfF_BS", "GulfR_BS" ,"ArkBor_BS", "Proper_BS", "Western_BS")
  CS_div <- c("Northern Celtic Sea" ,"Southern Celtic Sea")
  BoBIC_div <- c("Gulf of Biscay", "North-Iberian Atlantic", "South-Iberian Atlantic", "Gulf of Cadiz")
  divis <- c(NS_div,BS_div,CS_div,BoBIC_div)

### run all areas in a loop
  Assregion_index <- c(Sregions, divis) # get the reporting region
  EcoReg_index    <- c(Sregions, rep(Sregions[1],5),rep(Sregions[2],6),
                       rep(Sregions[3],2),rep(Sregions[4],4))  # get the (sub-)region for the reporting region
  Assunit_index   <- c(rep("(sub-)Region",4),rep("Division",17)) # is reporting region a "(sub-)Region" or "Division"?
  regions_with_impact <- c(1:5,7:21) # no sensitivity data for the Norwegian Trench (6)
  regions_with_corefishing <- c(1:10,13:18) # subdivisions: "Gulf of Finland"(11), "Gulf of Riga"(12), "North-Iberian Atlantic"(19), "South-Iberian Atlantic"(20), and "Gulf of Cadiz"(21) have no fishing values reported.

  for (p in 10:21){
   Assregion <- Assregion_index[p]
   EcoReg    <- EcoReg_index[p]
   Assunit <- Assunit_index[p]
   print(paste(p, Assregion, sep=": "))

### load processed file, with longevity and state/impact (and incl the gear modification data)
   if (p %in% regions_with_impact){
     load(paste(pathdir_nogit,paste(EcoReg,"state_PDsens.RData",sep="_"),sep="/"))
     State_reg_IL <- State_reg # rename the state from the PDsens
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PDsens_GMP0.1.RData",sep="_"),sep="/"))
     State_sens_GMP10 <- State_reg # rename the state from the PDsens
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PDsens_GMP0.2.RData",sep="_"),sep="/"))
     State_sens_GMP20 <- State_reg # rename the state from the PDsens
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PDsens_GMP0.05.RData",sep="_"),sep="/"))
     State_sens_GMP5 <- State_reg # rename the state from the PDsens
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PD_GMP0.1.RData",sep="_"),sep="/"))
     State_GMP10 <- State_reg # rename the state from the PD
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PD_GMP0.2.RData",sep="_"),sep="/"))
     State_GMP20 <- State_reg # rename the state from the PD
     load(paste(pathdir_nogit, "GM_scenarios", paste(EcoReg,"state_PD_GMP0.05.RData",sep="_"),sep="/"))
     State_GMP5 <- State_reg # rename the state from the PD
     load(paste(pathdir_nogit,paste(EcoReg,"state_PD.RData",sep="_"),sep="/"))
     }
   load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/"))
   load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/"))
   setwd(paste(pathdir,"1 - Input env",sep="/"))
   load(paste(EcoReg,"EU2region_grid_sensitivity.RData",sep="_"))
   load(paste(EcoReg,"MSFD2_per_csquare.RData",sep="_"))

### Run script to produce waterdepth figure
   setwd(paste(pathdir, "Utilities", sep="/"))
   source("WaterDepthMap.R")

### run script to process FBIT figures and tables
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("Processing_assessment.R")

### run script to make FBIT output
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("map_plot.R")
   source("Output_assessment.R")

### run script for spatial temporal analysis (takes some time)
   setwd(paste(pathdir,"Utilities",sep="/"))
   if (p %in% regions_with_corefishing){
     source("Core_fishing_by_metier_analysis_results_Value.R")
     }

### run habitat management option
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("Tradeoff_habitat_management_allRegions_KvdR.R")

### run gear modification scenario
   if(p %in% regions_with_impact){
     setwd(paste(pathdir,"Utilities",sep="/"))
     source("GMScenario_ExtentQualityThresholds.R")
     setwd(paste(pathdir,"Utilities",sep="/"))
     source("GMScenario_plotting.R")
    }
  }

  
# due to data limitations/errors, different outputs are potentially wrong 
# all (potentially) wrong numbers are changed to NA in tables with the below code
  
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Exceptions_in_tables_data_limitations.R")
  