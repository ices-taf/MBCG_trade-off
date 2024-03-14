
## Script to produce plot of Gear Modification Scenarios
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
load(file="GM_QETtable.Rdata")

GM_QETtable$extent <- GM_QETtable$extent * 100 # to make it into %

## Produce plot of Gear modifications
habs <- unique(GM_QETtable$ID)
for(iHab in 1:5){
  plotdat <- subset(GM_QETtable, ID == habs[iHab])
  plotdat$LineSize = ifelse(plotdat$ModificationLevel == 0, 0.8, 0.6)
  ymin = floor((min(plotdat$extent)*10))/10 # getting the lowest x
  
  
  if(ymin >= 80) {
    FigGM <- ggplot(plotdat, aes(y = extent, x = quality, group = ModificationLevel))+
      geom_line(aes(linetype=as.factor(ModificationLevel), colour=as.factor(ModificationLevel)), linewidth=plotdat$LineSize)+
      scale_y_continuous(limits = c(ymin-5, 100), breaks =  seq(80, 100, 5)) +
      geom_hline(yintercept=75, lty = 2) + scale_linetype_manual(values=c(1:4), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      scale_color_manual(values=c("black", "darkorange", "blue", "red"), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      theme_classic()+
      labs(x="Habitat quality (1 - impact)",
           y= "Haibtat extent (%)") +
      facet_grid(.~Indicator) +
      theme(panel.spacing = unit(3, "lines"), legend.text = element_text(size=10), legend.title = element_text(size=12),
            strip.text = element_text(size=14), axis.text = element_text(size=10), axis.title=element_text(size=12),
            axis.line=element_line())} # end ymin >0.8
  
  if(ymin >= 50 & ymin < 80) {
    FigGM <- ggplot(plotdat, aes(y = extent, x = quality, group = ModificationLevel))+
      geom_line(aes(linetype=as.factor(ModificationLevel), colour=as.factor(ModificationLevel)), linewidth=plotdat$LineSize)+
      scale_y_continuous(limits = c(ymin-5, 100), breaks =  rev(seq(100, ymin, -10))) +
      geom_hline(yintercept=75, lty = 2) + scale_linetype_manual(values=c(1:4), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      scale_color_manual(values=c("black", "darkorange", "blue", "red"), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      theme_classic()+
      labs(x="Habitat quality (1 - impact)",
           y= "Haibtat extent (%)") +
      facet_grid(.~Indicator) +
      theme(panel.spacing = unit(3, "lines"), legend.text = element_text(size=10), legend.title = element_text(size=12),
            strip.text = element_text(size=14), axis.text = element_text(size=10), axis.title=element_text(size=12),
            axis.line=element_line())} # end ymin 0.5 - 0.8
  
  if(ymin < 50) {
    FigGM <- ggplot(plotdat, aes(y = extent, x = quality, group = ModificationLevel))+
      geom_line(aes(linetype=as.factor(ModificationLevel), colour=as.factor(ModificationLevel)), linewidth=plotdat$LineSize)+
      scale_y_continuous(limits = c(ymin-5, 100), breaks =  rev(seq(100, ymin, -20))) +
      geom_hline(yintercept=75, lty = 2) + scale_linetype_manual(values=c(1:4), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      scale_color_manual(values=c("black", "darkorange", "blue", "red"), name=paste0("Broad habitat type:\n", habs[iHab], "\n\n\nGear modification level (%)"), labels=c("0", "5", "10", "20")) +
      theme_classic()+
      labs(x="Habitat quality (1 - impact)",
           y= "Haibtat extent (%)") +
      facet_grid(.~Indicator) +
      theme(panel.spacing = unit(3, "lines"), legend.text = element_text(size=10), legend.title = element_text(size=12),
            strip.text = element_text(size=14), axis.text = element_text(size=10), axis.title=element_text(size=12),
            axis.line=element_line())} # end ymin < 0.5
  
  png(filename=paste0(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"), "/", Assregion, "_GearModifications", iHab, ".png"), 
      width=10,height=5, units = "in", res=150)
  print(FigGM)
  dev.off()
} # end iHab-loop
