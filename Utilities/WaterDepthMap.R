# set directory for output
setwd(paste(pathdir,"5 - Output",sep="/"))  
dir.create(paste(Assunit), showWarnings = FALSE)
setwd(paste(pathdir,"5 - Output",Assunit,sep="/"))  
dir.create(paste(Assregion), showWarnings = FALSE)
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))


# select division from the region.
if (Assregion != EcoReg){
  Region <- subset(Region,Region@data$division == Assregion)
}

# remove NA-divisions from the region.
if(Assregion == EcoReg){
  Region <- subset(Region, is.na(Region@data$division) == FALSE)
}

# remove csquares with NA-depth
Region <- subset(Region, is.na(Region@data$Depth)==FALSE)

# Assign water depth classes
Region$depthclass <- ifelse(Region$Depth <= -800, "4. >= 800m",
                         ifelse(Region$Depth >-800 & Region$Depth <= -400, "3. 400 - 800m",
                                ifelse(Region$Depth >-400 & Region$Depth <= -200, "2. 200 - 400m",
                                       ifelse(Region$Depth > -200 & Region$Depth <= 0, "1. 0 - 200m", "Other"))))
Region <- subset(Region, depthclass %in% c("1. 0 - 200m", "2. 200 - 400m", "3. 400 - 800m", "4. >= 800m"))

if(Assregion == EcoReg){
  ## Determine extent of areas
  ERext <- aggregate(area_sqkm ~ depthclass+Ecoregion, data=Region@data, FUN="sum")
  ERext$division = "All"
  DIVext <- aggregate(area_sqkm ~ depthclass+division, data=Region@data, FUN="sum")
  DIVext$Ecoregion <- EcoReg
  AssAreaExt <- rbind(ERext, DIVext)
  AssAreaExt$area_sqkm <- round(AssAreaExt$area_sqkm, digits=2)
  AssAreaExt <- AssAreaExt[,c("Ecoregion", "division", "depthclass", "area_sqkm")]
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  write.table(AssAreaExt, file="AssAreaExt.txt", sep="\t", col.names = TRUE, row.names = FALSE)
  
  #### Start making figure water depth
  #
  # Load datasets
  worldMap <- map_data("world")
  reg <- st_as_sf(Region)
  reg <- st_transform(reg, 4326)
  load(paste0(pathdir, "/1 - Input env/RegDivShapes2.Rdata"))
  
  if(Assregion != EcoReg){
    AreaPoly <- st_make_valid(subset(RegDivShapes, Division == Assregion))
  }
  
  if(Assregion == EcoReg){
    AreaPoly <- st_make_valid(subset(RegDivShapes, EcoReg == Assregion))
  }
  
  APlab <- st_centroid(AreaPoly)
  APlab$label <- LETTERS[1:nrow(AreaPoly)]
  
  load(paste0(pathdir, "/1 - Input env/FAO/FAO.Rdata"))
  load(paste0(pathdir, "/1 - Input env/FAOlab.Rdata"))
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  
  # Figure with water depths
  fig_WD <- ggplot() + geom_sf(data=reg, aes(fill=depthclass), col=NA)
  fig_WD <- fig_WD + geom_map(data = worldMap, map = worldMap, aes(map_id = region), fill="lightgrey", col="grey")
  fig_WD <- fig_WD + geom_sf(data = FAO, fill=NA, col="darkred", linetype="dotted", linewidth=0.8)
  fig_WD <- fig_WD + geom_sf(data = AreaPoly, fill=NA, col="black", linewidth=1)
  fig_WD <- fig_WD + theme(plot.background=element_blank(),
                           panel.background=element_blank(),
                           axis.text.y   = element_text(size=12),
                           axis.text.x   = element_text(size=12),
                           axis.title.y  = element_text(size=16),
                           axis.title.x  = element_text(size=16),
                           panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                           legend.text   = element_text(size=12),
                           legend.title  = element_text(size=16))
  fig_WD <- fig_WD + scale_x_continuous(name = "Longitude", limits=c(st_bbox(AreaPoly)[c(1,3)]))
  fig_WD <- fig_WD + scale_y_continuous(name = "Latitude", limits=c(st_bbox(AreaPoly)[c(2,4)]))
  fig_WD <- fig_WD + scale_fill_manual(values=c("cadetblue1", "cadetblue3", "cadetblue4", "lavender"),
                                       labels= c("0 - 200m", "200 - 400m", "400 - 800m", ">= 800m"), name="Depth")
  mylegendDC <- g_legend(fig_WD)
  fig_WD <- fig_WD + theme(legend.position = "none")
  
  # Figure showing divisions and FAO regions
  fig_Div <- ggplot() + geom_map(data = worldMap, map = worldMap, aes(map_id = region), fill="lightgrey", col="grey")
  fig_Div <- fig_Div + geom_sf(data = AreaPoly, aes(fill=DivName), col="black", linewidth=0.8)
  fig_Div <- fig_Div + geom_sf(data = FAO, fill=NA, col="darkred", linewidth=0.8)
  fig_Div <- fig_Div + geom_sf(data=reg, fill="cadetblue4", col=NA, alpha = 0.3)
  fig_Div <- fig_Div + theme(plot.background=element_blank(),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=12),
                             axis.text.x   = element_text(size=12),
                             axis.title.y  = element_text(size=16),
                             axis.title.x  = element_text(size=16),
                             panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                             legend.text   = element_text(size=12),
                             legend.title  = element_text(size=16))
  fig_Div <- fig_Div + scale_x_continuous(name = "Longitude", limits=c(st_bbox(AreaPoly)[c(1,3)]))
  fig_Div <- fig_Div + scale_y_continuous(name = "", limits=c(st_bbox(AreaPoly)[c(2,4)]))
  fig_Div <- fig_Div + annotate("label",x=FAOlab$x, y=FAOlab$y, label=paste0(FAOlab$lab),
                                colour="red", size=3, fontface="italic")
  fig_Div <- fig_Div + scale_fill_manual(values=brewer.pal(n=length(unique(AreaPoly$DivName)), name="Set3"),
                                         name="Subdivisions")
  
  mylegendDV <- g_legend(fig_Div)
  fig_Div <- fig_Div + theme(legend.position = "none")
  
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  png(paste(Assregion,"figureAA1_Waterdepth.png",sep="_"),width=14,height=8, units = "in", res = 150)
  print(grid.arrange(arrangeGrob(grobs=list(fig_WD, fig_Div), ncol=2),
                     arrangeGrob(grobs=list(mylegendDC, mylegendDV), ncol=1),
                     widths=c(11,3)))
  dev.off()
  
  rm(reg, fig_WD, worldMap, AssAreaExt, ERext, DIVext, AreaPoly, FAO, RegDivShapes)
  
}




