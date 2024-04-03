
# ------------------------------------------------------------------------------
# The script removes output from tables where there is missing information
# or information is wrong
# ------------------------------------------------------------------------------

# issue 1 ----------------------------------------------------------------------
  # No sensitivity of benthic community is available in the greater North Sea 
  # in areas deeper than 200m 
  # Impact estimate from Greater North Sea 200-400 m and 400-800m is removed from table 1
    
    setwd(paste(pathdir,"5 - Output/(sub-)Region/Greater North Sea",sep="/"))
    dat <- read.csv(file = "Greater North Sea_Table_1.csv",check.names=FALSE)
    dat[6:9,3:4] <- NA
    write.csv(dat, file="Greater North Sea_Table_1.csv", row.names=FALSE)

# issue 2 ----------------------------------------------------------------------
  # No value of landings in Iberian Coast in ICES VMS datacall (2023)
  # and no fishing data included from Portugal at all.  
  # remove from all tables in subdivisions and sub-region
  # note that value of landings are available for Bay of Biscay
  # --- hence not removed from Bay of Biscay subdivision ---

    # subdivisions
    div <-   c("South-Iberian Atlantic","Gulf of Cadiz", "North-Iberian Atlantic")
    for (dix in 1:length(div)){
      setwd(paste(pathdir, "5 - Output/Division",div[dix],sep="/"))
      dat <- read.csv(file = paste(div[dix],"Table_3.csv",sep="_"),check.names=FALSE)
      dat[,5] <- NA
      write.csv(dat, file=paste(div[dix],"Table_3.csv",sep="_"), row.names=FALSE)

      dat <- read.csv(file = paste(div[dix],"Table_4.csv",sep="_"),check.names=FALSE)
      dat[c(3,5),2:ncol(dat)] <- NA
      write.csv(dat,file = paste(div[dix],"Table_4.csv",sep="_"), row.names=FALSE)
      
      dat <- read.csv(file = paste(div[dix],"Table_5.csv",sep="_"),check.names=FALSE)
      dat[c(2,4),2:ncol(dat)] <- NA
      write.csv(dat,file = paste(div[dix],"Table_5.csv",sep="_"), row.names=FALSE)
      
      dat <- read.table(file=paste0(div[dix], "_habitat_valuePCT2.txt"), sep=",", header=T)
      dat[,3:12] <- NA
      write.csv(dat, file=paste0(div[dix], "_habitat_valuePCT2.txt"), row.names=FALSE)
    }

    # (sub-)region BoBIC
    sreg <-   c("Bay of Biscay and the Iberian Coast")
    dix <- 1
    setwd(paste(pathdir,paste("5 - Output/(sub-)Region",sreg[dix],sep="/"),sep="/"))
    dat <- read.csv(file = paste(sreg[dix],"Table_3.csv",sep="_"),check.names=FALSE)
    dat[,5] <- NA
    write.csv(dat, file=paste(sreg[dix],"Table_3.csv",sep="_"), row.names=FALSE)

    dat <- read.csv(file = paste(sreg[dix],"Table_4.csv",sep="_"),check.names=FALSE)
    dat[c(3,5),2:ncol(dat)] <- NA
    write.csv(dat,file = paste(sreg[dix],"Table_4.csv",sep="_"), row.names=FALSE)
  
    dat <- read.csv(file = paste(sreg[dix],"Table_5.csv",sep="_"),check.names=FALSE)
    dat[c(2,4),2:ncol(dat)] <- NA
    write.csv(dat,file = paste(sreg[dix],"Table_5.csv",sep="_"), row.names=FALSE)
    
    dat <- read.table(file=paste0(sreg[dix], "_habitat_valuePCT2.txt"), sep=",", header=T)
    dat[,3:12] <- NA
    write.csv(dat, file=paste0(sreg[dix], "_habitat_valuePCT2.txt"), row.names=FALSE)
    