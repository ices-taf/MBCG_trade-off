# libraries needed to run WKTRADE3 output

# ICES libraries
  #remotes::install_github("ices-tools-prod/icesSharePoint")
  #library(icesSharePoint)

  #devtools::install_github("ices-tools-prod/icesVMS")
  #library(icesVMS), only needed to download VMS data from ICES data centre (data product will be on sharepoint)

# R libraries
  #library(rgdal) # outdated
  library(plyr)
  library(dplyr) # you need to load dplyr after plyr!
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(gridExtra)
  #library(maptools) # outdated
  library(sf)
  library(tidyr)
  library(mapview)
  library(data.table)
  library(english)

  