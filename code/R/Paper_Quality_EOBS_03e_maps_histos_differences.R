rm(list=ls())

library(plyr) #for join
library(scico) #scientific color maps by Fabio Crameri
library(rworldmap)
library(rworldxtra) #for high resolution country data
data(countriesHigh)
library(circlize) #for colorRamp2
library(maps) #for map
library(sf)

#map functions and parameters -----
north_arrow <- function(xmax,xmin,ymax,ymin,param1,param2){
  polygon(x=c(xmin+param1*(xmax-xmin),xmin+param1*(xmax-xmin),
              xmin+2*param1*(xmax-xmin)),
          y=c(ymax,ymax-param2*(ymax-ymin),ymax-2*param2*(ymax-ymin)),
          col="black")
  polygon(x=c(xmin+param1*(xmax-xmin),xmin+param1*(xmax-xmin),xmin),
          y=c(ymax,ymax-param2*(ymax-ymin),ymax-2*param2*(ymax-ymin)),
          border="black")
  text(x=xmin+param1*(xmax-xmin),y=ymax-2*param2*(ymax-ymin),
       labels=substitute(paste(bold("N"))),cex=8,pos=1)
}

map_scale <- function(xmax,scale,ymax,ymin,bar_length){
  lines(x=c(xmin+0.1*(xmax-xmin),xmin+0.1*(xmax-xmin)+scale),
        y=c(ymax-0.04*(ymax-ymin),ymax-0.04*(ymax-ymin)),lwd=1.2)
  lines(x=c(xmin+0.1*(xmax-xmin)+scale,xmin+0.1*(xmax-xmin)+scale),
        y=c(ymax-0.03*(ymax-ymin),ymax-0.05*(ymax-ymin)),lwd=1.5)
  lines(x=c(xmin+0.1*(xmax-xmin),xmin+0.1*(xmax-xmin)),
        y=c(ymax-0.03*(ymax-ymin),ymax-0.05*(ymax-ymin)),lwd=1.5)
  text(xmin+0.1*(xmax-xmin)+0.5*scale,ymax-0.035*(ymax-ymin),labels=paste(bar_length,"km"),pos=3,cex=5)
}

# ratio <- 2129.66/3224.65 #make nice ratios between the two axes (divide x-dist by y-dist)
xmin <- -9
xmax <- 22
ymin <- 37
ymax <- 66
bar_length <- 500
map_params <- c(0.025,0.03) #parameters for north arrow
R <- 6371 #equatorial earth radius in km
center_latitude <- mean(c(ymax,ymin))
mer_dist <- 2*pi*R*cos(center_latitude*pi/180)/360
scale <- bar_length/mer_dist

#basic data -----
main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"
topology <- read.csv(paste(main.path,"/topology.txt",sep=""),sep="\t")
filter <- read.csv(paste(main.path,"/list_filtered_3423.csv",sep=""),sep=",")
topology <- topology[topology$id%in%filter$basin_id,]

results <- read.csv(paste(main.path,"/Results/performances.txt",sep=""),sep="\t")
results <- results[results$id%in%filter$basin_id,]

differences <- data.frame(matrix(nrow=nrow(results),ncol=4))
names(differences) <- c("id","ESTR_min_CAM","P_from_CAM","EVAP_from_CAM")

differences$id <- results$id
differences$ESTR_min_CAM <- results$pt_q_camels.evap_estreams-results$ptq_camels.evap_camels
differences$P_from_CAM <- results$pt_q_camels.evap_estreams-results$p_t_estreams_q.evap_estreams
differences$EVAP_from_CAM <- results$pt_q_camels.evap_estreams-results$pt_q_camels.evap_camels
differences$T_from_CAM <- results$pt_q_camels.evap_estreams-results$p_estreams_tq.evap_estreams

fig_width <- 8/2.54 #in inches

#difference color palette -----
palette <- "bam" #color palette performance
direction <- 1
palette_min <- -0.3
palette_max <- 0.3
col_fun <- colorRamp2(seq(palette_min,palette_max,length.out=256),
                      colors=scico(256,palette=palette,direction=direction),space="RGB")

#difference maps -----
# ESTR_min_CAM: basic Estreams (with Q from CAMELS) minus CAMELS
# P_from_CAM: basic estreams (with Q from CAMELS) minus everything but P from Estreams
# EVAP_from_CAM: basic estreams (with Q from CAMELS) minus everything but evap from Estreams

map_width <- fig_width*0.85
map_height <- map_width/0.85 #nice ratio for Europe map

for (v in c("ESTR_min_CAM","P_from_CAM","EVAP_from_CAM","T_from_CAM")){
  data <- join(topology,differences[,c("id",v)],by="id")
  data$color <- col_fun(data[,v])
  svg(paste(main.path,"/Plots/differences/",v,"_difference_map.svg",sep=""),
      width=map_width,height=map_height,pointsize=1)
  par(mar=c(0.13,0.13,0.13,0.13))
  plot(data$lon,data$lat,pch=20,xlab="",ylab="",xaxt="n",yaxt="n",
       xlim=c(xmin,xmax),ylim=c(ymin,ymax),cex=0.5,col=data$color)
  plot(countriesHigh,add=TRUE,border="#838383")
  points(data$lon[order(abs(data[,v]))],data$lat[order(abs(data[,v]))],
         pch=20,col=data$color[order(abs(data[,v]))],cex=0.6) #plot low differences first
  north_arrow(xmax,xmin,ymax,ymin,map_params[1],map_params[2])
  map_scale(xmax,scale,ymax,ymin,bar_length)
  box(col="black")
  dev.off()
}

#performance histograms per country (two versions) -----
countries <- unique(substr(results$id,1,2))

step <- 0.03 #width of the buckets of the histogram
low <- -0.3 #minimum of histogram
high <- 0.3 #maximum of histogram

histo_height_portrait <- map_height*1/2
histo_height_landscape <- map_height

for (v in c("ESTR_min_CAM","P_from_CAM","EVAP_from_CAM","T_from_CAM")){
  for (c in countries){
    data_hist <- differences[substr(differences$id,1,2)==c,v]
    data_hist[data_hist<low] <- low
    data_hist[data_hist>high] <- high
    assign(paste("histo_",c,sep=""),
           hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                main="",xaxt="n",xlab="",yaxt="n",ylab=""))
  }
  #for all catchments (when even number of histos is required)
  data_hist <- differences[,v]
  data_hist[data_hist<low] <- low
  data_hist[data_hist>high] <- high
  histo_all <- hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                    breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                    main="",xaxt="n",xlab="",yaxt="n",ylab="")
  
  svg(paste(main.path,"/Plots/differences/",v,"_difference_histos_portrait.svg",sep=""),
      width=fig_width,height=histo_height_portrait)
  par(mfrow=c(3,3),mar=c(0.4,0.8,0.5,0.4))
  for (c in countries){
    histo <- get(paste("histo_",c,sep=""))
    barplot(histo$counts,xaxt="n",space=0,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),horiz=TRUE,border=NA,
            xlim=c(0,max(histo$counts)),main=paste(c,", n=",sum(histo$counts),sep=""),cex.main=0.6)
    axis(2,at=c(which(histo$mids==min(histo$mids))-0.5,which(round(histo$mids,digits=2)==0)-0.5,which(histo$mids==max(histo$mids))-0.5),
         labels=c(paste("≤",min(histo$mids),sep=""),0,paste("≥",max(histo$mids),sep="")),cex.axis=0.5,tick=FALSE,line=-0.9,las=2)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==min(histo$mids))-0.5,which(histo$mids==min(histo$mids))-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(round(histo$mids,digits=2)==0)-0.5,which(round(histo$mids,digits=2)==0)-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==max(histo$mids))-0.5,which(histo$mids==max(histo$mids))-0.5),lty="dotted",lwd=0.5)
    axis(1,at=c(round(sum(histo$counts)/10),max(histo$counts)),
         labels=c(round(sum(histo$counts)/10),max(histo$counts)),tick=FALSE,line=-1.5,cex.axis=0.5)
    lines(x=c(round(sum(histo$counts)/10),round(sum(histo$counts)/10)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
    lines(x=c(max(histo$counts),max(histo$counts)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
  }
  dev.off()
  
  svg(paste(main.path,"/Plots/differences/",v,"_difference_histos_landscape.svg",sep=""),
      width=fig_width,height=histo_height_landscape)
  par(mfrow=c(5,2),mar=c(0.4,1.2,0.5,0.4))
  for (c in c("all",countries)){
    histo <- get(paste("histo_",c,sep=""))
    barplot(histo$counts,xaxt="n",space=0,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),horiz=TRUE,border=NA,
            xlim=c(0,max(histo$counts)),main=paste(c,", n=",sum(histo$counts),sep=""),cex.main=0.7)
    axis(2,at=c(which(histo$mids==min(histo$mids))-0.5,which(round(histo$mids,digits=2)==0)-0.5,which(histo$mids==max(histo$mids))-0.5),
         labels=c(paste("≤",min(histo$mids),sep=""),0,paste("≥",max(histo$mids),sep="")),cex.axis=0.5,tick=FALSE,line=-0.9,las=2)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==min(histo$mids))-0.5,which(histo$mids==min(histo$mids))-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(round(histo$mids,digits=2)==0)-0.5,which(round(histo$mids,digits=2)==0)-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==max(histo$mids))-0.5,which(histo$mids==max(histo$mids))-0.5),lty="dotted",lwd=0.5)
    axis(1,at=c(round(sum(histo$counts)/10),max(histo$counts)),
         labels=c(round(sum(histo$counts)/10),max(histo$counts)),tick=FALSE,line=-1.5,cex.axis=0.55)
    lines(x=c(round(sum(histo$counts)/10),round(sum(histo$counts)/10)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
    lines(x=c(max(histo$counts),max(histo$counts)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
  }
  dev.off()
}

#legend -----
legend_width <- fig_width*0.15
legend_height <- map_height

incr <- step*2
#title <- expression("ΔR"["KGE"])
title <- "ΔKGE"

labels <- as.character(seq(as.numeric(palette_min),as.numeric(palette_max),by=incr))
labels[1] <- paste("≤",labels[1],sep="")
labels[length(labels)] <- paste("≥",labels[length(labels)],sep="")

svg(paste(main.path,"/Plots/differences/difference_legend.svg",sep=""),
    width=legend_width,height=legend_height,pointsize=6)
par(mar=c(0.05,0.05,1.3,2.3))
barplot(as.matrix(rep(1,length(scico(256,palette=palette,direction=direction)))),axes=FALSE,
        col=scico(256,palette=palette,direction=direction),border=NA)
axis(4,at=seq(1,256,by=(incr)*(255/(palette_max-palette_min)))-0.5,
     labels=labels,las=2,cex.axis=0.9,hadj=0.3,lwd=0,lwd.ticks=0.6,line=-0.05)
mtext(text=title,side=3,line=0.3,adj=0,cex=1.1)
dev.off()

#total effec vs. isolated P effect -----
differences$color[substr(differences$id,1,2)=="AT"] <- "#440154"
differences$color[substr(differences$id,1,2)=="CH"] <- "#472d7b"
differences$color[substr(differences$id,1,2)=="CZ"] <- "#3b528b"
differences$color[substr(differences$id,1,2)=="DE"] <- "#2c728e"
differences$color[substr(differences$id,1,2)=="DK"] <- "#21918c"
differences$color[substr(differences$id,1,2)=="ES"] <- "#28ae80"
differences$color[substr(differences$id,1,2)=="FR"] <- "#5ec962"
differences$color[substr(differences$id,1,2)=="GB"] <- "#addc30"
differences$color[substr(differences$id,1,2)=="SE"] <- "#fde725"

differences$ratio[differences$ESTR_min_CAM>=differences$P_from_CAM] <- 
  differences$ESTR_min_CAM[differences$ESTR_min_CAM>=differences$P_from_CAM]/differences$P_from_CAM[differences$ESTR_min_CAM>=differences$P_from_CAM]
differences$ratio[differences$P_from_CAM>differences$ESTR_min_CAM] <- 
  differences$P_from_CAM[differences$P_from_CAM>differences$ESTR_min_CAM]/differences$ESTR_min_CAM[differences$P_from_CAM>differences$ESTR_min_CAM]


svg(paste(main.path,"/Plots/differences/total_isoP_effect.svg",sep=""),
    width=fig_width,height=fig_width,pointsize=10)
par(mar=c(3,3,0.1,0.1))
plot(differences$ESTR_min_CAM[order(differences$ratio)],differences$P_from_CAM[order(differences$ratio)],
     col=differences$color[order(differences$ratio)],
     pch=20,cex=0.6,xlim=c(-1.3,5),ylim=c(-1.3,5),xaxt="n",yaxt="n",ylab="",xlab="")
lines(x=c(-2,12),y=c(-2,12),col="darkgrey",lty="dashed")
lines(x=c(-2,12),y=c(0,0),col="darkgrey",lty="dashed")
lines(x=c(0,0),y=c(-2,12),col="darkgrey",lty="dashed")
axis(1,at=c(-1,0,1,3,5),labels=c(-1,0,1,3,5),tck=-0.01,padj=-1)
axis(2,at=c(-1,0,1,3,5),labels=c(-1,0,1,3,5),tck=-0.01,hadj=0.4,las=2)
mtext("ΔKGE between scenario II and I",1,cex=1,line=1.8)
mtext("ΔKGE between scenario II and III",2,cex=1,line=1.8)
legend("topleft",legend=c("AT","CH","CZ","DE","DK","ES","FR","GB","SE"),
       col=c("#440154","#472d7b","#3b528b","#2c728e","#21918c","#28ae80","#5ec962","#addc30","#fde725"),ncol=3,pch=20,cex=0.8)
dev.off()











