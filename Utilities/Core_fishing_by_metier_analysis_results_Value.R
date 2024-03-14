## Copy of "Core_fishing_by_metier_analysis_results", adjusted by Karin to create fishing core grounds based on landing value.

pathdir_output <- paste(pathdir,"5 - Output",Assunit,Assregion,sep="/")
setwd(pathdir_output)  
dir.create("shapefiles", showWarnings = FALSE)
#dir.create("CSV", showWarnings = FALSE)

# subset all areas with longevity (north sea has no longevity prediction for Norwegian trench)
if (p %in% regions_with_impact){
  Region <- Region[!(is.na(Region$medlong)),]
}
reguni <- ifelse(Assunit == "Division","division","Ecoregion")

#Reformat data, define metiers
vmsValue <- select(FisheriesMet,csquares, contains("_value_"))
vmsValue <- cbind(vmsValue, Region@data[match(vmsValue$csquares,Region@data$csquares), c(reguni)])
colnames(vmsValue)[ncol(vmsValue)] <- reguni
vmsValue <-  subset(vmsValue,vmsValue[,reguni] == Assregion)
vmsValue <- vmsValue[,1:(ncol(vmsValue)-1)]
vmsValueLong <- gather(vmsValue, column, euro, 2:ncol(vmsValue), factor_key=TRUE)
vmsValueLong <- vmsValueLong[!is.na(vmsValueLong$euro),]
vmsValueLong$year <- sapply(strsplit(as.character(vmsValueLong$column), "_"), tail, 1)
vmsValueLong <- vmsValueLong[vmsValueLong$year %in% c(AssPeriod),]
vmsValueLong$wktradeMet <- substr(vmsValueLong$column, 1, nchar(as.character(vmsValueLong$column))-17)

# Only keep active metiers
vmsValueLong <- droplevels(vmsValueLong[vmsValueLong$wktradeMet %in% c(subset(ActiveMetiers, Activity == "Active")$gears),])
#wktradeMet <- as.data.frame(unique(vmsValueLong$wktradeMet))
metiers <- unique(vmsValueLong$wktradeMet)

#Find c-squares with 90% of landings
#Total
VMS_MBCG_sum <- vmsValueLong %>% 
  group_by(year, csquares, wktradeMet) %>% 
  summarise(euro_sum=sum(euro)) 

#Sort and make cumulated percentage
VMS_MBCG_sum2 <- VMS_MBCG_sum %>% 
  group_by(year, wktradeMet) %>% 
  arrange(year, wktradeMet, desc(euro_sum)) %>%
  mutate(cum=  cumsum(euro_sum)/sum(euro_sum))

VMS_MBCG_90pct <- VMS_MBCG_sum2[VMS_MBCG_sum2$cum <= 0.9,]
VMS_MBCG_90pct$t <- 1
VMS_MBCG_90pct <- VMS_MBCG_90pct[!is.na(VMS_MBCG_90pct$wktradeMet),]

#Sum number of years>90% by metier and c-square
VMS_MBCG_90pct2 <- VMS_MBCG_90pct %>% 
  group_by( wktradeMet, csquares) %>% 
  summarise(n_years=sum(t)) 

VMS_MBCG_90pct2$t <- 1
VMS_MBCG_90pct2 <- VMS_MBCG_90pct2 %>% 
  group_by( wktradeMet, n_years) %>% 
  summarise(n_csquares=sum(t)) 

# Determine number of csquares fished, but no core fishing ground, per metier. Add to VMS_MBCG_90pct2
VMS_MBCG_sum3 <- data.table(VMS_MBCG_sum2) # all fished grid cells
VMS_MBCG_sum3$ID <- paste(VMS_MBCG_sum3$csquares, VMS_MBCG_sum3$wktradeMet, sep="_")
VMS_MBCG_90pct$ID <- paste(VMS_MBCG_90pct$csquares, VMS_MBCG_90pct$wktradeMet, sep="_")
VMS_MBCG_sum3 <- VMS_MBCG_sum3[!ID %in% VMS_MBCG_90pct$ID] # remove the core fishing csquares (at least once), per metier
VMS_MBCG_sum3 <- VMS_MBCG_sum3[,.(n_csquares = length(unique(csquares))), by="wktradeMet"] # count left csquares - by metier.
VMS_MBCG_sum3$n_years=0
VMS_MBCG_90pct2 <- rbind(VMS_MBCG_90pct2, VMS_MBCG_sum3)

#Determine number of csquares as fishing ground per metier
VMS_MBCG_90pct2_tot <- VMS_MBCG_90pct2 %>% 
  group_by( wktradeMet) %>% 
  summarise(n_csquares_tot=sum(n_csquares)) 

VMS_MBCG_90pct2_totCF <- VMS_MBCG_90pct2 %>% 
  filter(n_years > 0) %>%
  group_by( wktradeMet) %>% 
  summarise(n_csquares_CF=sum(n_csquares))

VMS_MBCG_90pct2 <- merge(VMS_MBCG_90pct2,VMS_MBCG_90pct2_tot, by=c("wktradeMet"))
VMS_MBCG_90pct2 <- merge(VMS_MBCG_90pct2,VMS_MBCG_90pct2_totCF, by=c("wktradeMet"))
VMS_MBCG_90pct2$pct <- (VMS_MBCG_90pct2$n_csquares/VMS_MBCG_90pct2$n_csquares_tot)*100 

#Determine size of overall fishing ground
VMS_MBCG_90pct2$FG_size <- round(VMS_MBCG_90pct2$n_csquares_tot / length(unique(vmsValueLong$csquares)) * 100, digits=1)
VMS_MBCG_90pct2$CFG_size <- round(VMS_MBCG_90pct2$n_csquares_CF / VMS_MBCG_90pct2$n_csquares_tot * 100, digits=1)

# Order by decreasing FG-size
VMS_MBCG_90pct2 <- VMS_MBCG_90pct2 %>%
  arrange(desc(FG_size), n_years)
VMS_MBCG_90pct2$wktradeMet <- factor(VMS_MBCG_90pct2$wktradeMet, levels=rev(unique(VMS_MBCG_90pct2$wktradeMet)))

#Plot number of years by metier (Fig 5)
labtext1 <- VMS_MBCG_90pct2[,c("wktradeMet", "CFG_size")]
labtext1 <- labtext1[!duplicated(labtext1),]
labtext1$lab <- paste0(round(labtext1$CFG_size,digits=1), "%")

labtext2 <- VMS_MBCG_90pct2[,c("wktradeMet", "FG_size")]
labtext2 <- labtext2[!duplicated(labtext2),]
labtext2$lab <- paste0("(", round(labtext2$FG_size, digits=1), "%)")

core <- ggplot(data=VMS_MBCG_90pct2, aes(x= pct, y= wktradeMet)) +
  geom_col(position="stack", aes(fill=factor(n_years)), width=0.75) +
  theme_bw() + theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(x="Percent csquares",
       y="Fishing metier") +
  theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), 
        axis.title = element_text(size=10), legend.title = element_text(size=10), panel.grid.major.y= element_blank()) +
  scale_fill_manual(values=brewer.pal(7, "OrRd"), name = "Number of years as core fishing ground") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(limits = c(0,105), breaks = c(seq(0,100,20)), minor_breaks = seq(0,100,10)) +
  # scale_y_discrete(limits = c(levels(VMS_MBCG_90pct2$wktradeMet), ""),
  #                  breaks = c(levels(VMS_MBCG_90pct2$wktradeMet))) +
  annotate("text", label = labtext1$lab, x=labtext1$CFG_size, y=nrow(labtext1):1, size=2.5, fontface="italic", hjust=0) +
  annotate("text", label = labtext2$lab, x=105, y=nrow(labtext2):1, size=3, fontface="italic", hjust=0.5) 

jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig1Val.jpg",sep="_"),sep="/"),
     width=8,height=4.5, units = "in", res = 150 ) 
print(core)
dev.off()

# source CSquare2LonLat
source(paste(pathdir,"Utilities/CSquare2LonLat.R",sep="/"))
VMS_MBCG_90pct$Longitude=CSquare2LonLat(as.character(VMS_MBCG_90pct$csquares),0.05)$SI_LONG
VMS_MBCG_90pct$Latitude=CSquare2LonLat(as.character(VMS_MBCG_90pct$csquares),0.05)$SI_LAT         

# get polygons core regions
MBCG_90pct_poly <- 
  VMS_MBCG_90pct %>% 
  group_by(wktradeMet, csquares, Longitude, Latitude) %>% 
  summarise(n_years=sum(t), euro_sum=sum(euro_sum))  %>%
  mutate(
    wkt = paste0('POLYGON((',
                 Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                 Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                 Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                 Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                 Longitude + 0.025, ' ', Latitude + 0.025, '))')
  ) %>%
  st_as_sf(crs = 4326, wkt = "wkt")

# write_shape <- function(m){
#   st_write(MBCG_90pct_poly[MBCG_90pct_poly$wktradeMet==m,], dsn= paste0(pathdir_output,"/shapefiles"), layer=paste0("MBCG_90pct_poly_",Assregion,"_",m,".shp"), driver="ESRI Shapefile", append=F)
# }
# 
# for(m in metiers){
#   write_shape(m=m)
# }

# get polygons Total MBCG poly
VMS_MBCG_sum$Longitude=CSquare2LonLat(as.character(VMS_MBCG_sum$csquares),0.05)$SI_LONG
VMS_MBCG_sum$Latitude=CSquare2LonLat(as.character(VMS_MBCG_sum$csquares),0.05)$SI_LAT

MBCG_total_poly <- 
  VMS_MBCG_sum %>% 
  group_by(wktradeMet,csquares, Longitude, Latitude) %>% 
  summarise(euro_sum=sum(euro_sum))  %>%
  mutate(
    wkt = paste0('POLYGON((',
                 Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                 Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                 Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                 Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                 Longitude + 0.025, ' ', Latitude + 0.025, '))')
  ) %>%
  st_as_sf(crs = 4326, wkt = "wkt")
# 
# write_shape <- function(m){
#   st_write(MBCG_total_poly[MBCG_total_poly$wktradeMet==m,], dsn=paste0(pathdir_output,"/shapefiles"), layer=paste0("MBCG_total_poly_",Assregion,"_",m,".shp"), driver="ESRI Shapefile", append=F)
# }
# 
# for(m in metiers){
#   write_shape(m=m)
# }

# get map 
worldMap <- map_data("world")
reg <- st_as_sf(Region)
maplist <- vector(mode="list", length=length(metiers))
names(maplist) <- metiers

# obtain shapefile of region
load(file="D:/MBCG_trade-off/1 - Input env/RegDivShapes2.Rdata")
if(Assunit == "(sub-)Region"){
  Regshp <- subset(RegDivShapes, EcoReg == Assregion)}
if(Assunit == "Division"){
  Regshp <- subset(RegDivShapes, Division == Assregion)}

for(m in metiers){
  ## fix data
  plotdat <- MBCG_total_poly[MBCG_total_poly$wktradeMet == m,]
  coredat <- MBCG_90pct_poly[MBCG_90pct_poly$wktradeMet == m,]
  plotdat$CoreYears <- coredat$n_years [match(plotdat$csquares, coredat$csquares)]
  plotdat$CoreYears[is.na(plotdat$CoreYears)]<- 0
  
  ## Create metier-plots
  figmap <- ggplot() + geom_sf(data=reg, color=NA, fill="aliceblue") 
  figmap <- figmap + geom_sf(data=Regshp, color = "royalblue", fill=NA)
  figmap <- figmap + geom_sf(data=plotdat, aes(fill=as.factor(CoreYears)), color = NA) 
  figmap <- figmap + theme(plot.background=element_blank(),
                           plot.margin=unit(c(5,5,5,2), 'pt'),
                           panel.border = element_rect(color = 'black', linewidth = 0.5, linetype='solid', fill=NA),
                           panel.background = element_blank(),
                           plot.title.position = 'plot', 
                           plot.title = element_text(hjust = 0.5),
                           axis.text.y   = element_text(size=8),
                           axis.text.x   = element_text(size=8),
                           axis.title.y  = element_text(size=10),
                           axis.title.x  = element_text(size=10),
                           legend.position ="none")
  figmap <- figmap + scale_x_continuous(name = "Longitude", limits=c(st_bbox(Regshp)[c(1,3)]))
  figmap <- figmap + scale_y_continuous(name = "Latitude", limits=c(st_bbox(Regshp)[c(2,4)]))
  figmap <- figmap + scale_fill_manual(values=c("dimgrey", brewer.pal("OrRd", n=7)[2:7])) + labs(title=paste0(m))
  figmap <- figmap + geom_map(data = worldMap, map = worldMap, aes(map_id = region), fill="lightgrey", col="grey")
  
  ## store in list
  maplist[[m]] <- figmap
} # end m-loop


plotdat <- MBCG_total_poly[MBCG_total_poly$wktradeMet == metiers[1],]
coredat <- MBCG_90pct_poly[MBCG_90pct_poly$wktradeMet == metiers[1],]
plotdat$CoreYears <- coredat$n_years [match(plotdat$csquares, coredat$csquares)]
plotdat$CoreYears[is.na(plotdat$CoreYears)]<- 0

LEGmap <- 
  ggplot() + geom_sf(data=plotdat, aes(fill=as.factor(CoreYears)), color = NA) +
  theme(legend.text = element_text(size=8), legend.title = element_text(size=10), 
        legend.position = "top", legend.direction = "horizontal") +
  scale_fill_manual(values=c("dimgrey", brewer.pal("OrRd", n=7)[2:7]), limits=c("0", "1", "2", "3", "4", "5", "6"), name = "Number of years as core fishing ground") +
  guides(fill = guide_legend(nrow = 1))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(LEGmap)


## create overall figure
plotsize <- data.table(mets = 1:10, 
                       Nrows = c(1,1,1,2,2,2,3,3,3,4),
                       Ncols = c(1,2,3,2,3,3,3,3,3,3))
plotwidth = (13.5/3) * plotsize$Ncols[plotsize$mets == length(metiers)]
plotheight = 6 * plotsize$Nrows[plotsize$mets == length(metiers)]
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))

jpeg(paste(Assregion,"Fig_corefishinggroundsVAL.jpg",sep="_"),width=plotwidth,height=plotheight, units = "in", res = 120 ) 
print(grid.arrange(arrangeGrob(grobs = maplist, 
                               ncol=plotsize$Ncols[plotsize$mets == length(metiers)]),
                   mylegend, heights=c((6*plotsize[mets == length(metiers),]$Nrows),0.5)))
dev.off()


# now delete the two folders, too big for github
unlink(paste0(pathdir_output,"/shapefiles"), recursive = TRUE)
#unlink(list.files(pathdir_output, pattern="area_overlap", full.names=TRUE), recursive=TRUE)
unlink(paste(pathdir_output,"CSV",sep="/"), recursive = TRUE)

rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                            'EcoReg_index','GVA','GVAMet','Period','AssPeriod',"GVAPeriod",
                            "EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                            'Region','State_reg','State_reg_IL',"Assunit","Assregion",
                            "msfd_csq","regions_with_corefishing", "State_GMP10", "State_GMP20",
                            "State_GMP5", "State_sens_GMP10", "State_sens_GMP20", "State_sens_GMP5"))])
