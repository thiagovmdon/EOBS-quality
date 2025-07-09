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

fig_width <- 8/2.54 #in inches

#get hydmet data and calculate aridity -----
hydmet <- read.csv(paste(main.path,"/Results/hydmet.txt",sep=""),sep="\t")
hydmet <- hydmet[hydmet$id%in%filter$basin_id,]

hydmet <- hydmet[,c("id","P_sum_CAM","P_sum_Estr","Epot_sum_CAM","Epot_sum_Estr")]
hydmet$AI_CAM <- hydmet$Epot_sum_CAM/hydmet$P_sum_CAM
hydmet$AI_Estr <- hydmet$Epot_sum_Estr/hydmet$P_sum_Estr

#aridity color palette -----
palette <- "managua" #color palette performance
direction <- -1
palette_min <- 0
palette_max <- 2
col_fun <- colorRamp2(seq(palette_min,palette_max,length.out=256),
                      colors=scico(256,palette=palette,direction=direction),space="RGB")

#aridity maps -----
map_width <- fig_width*0.85
map_height <- map_width/0.85 #nice ratio for Europe map

for (v in c("AI_CAM","AI_Estr")){
  data <- join(topology,hydmet[,c("id",v)],by="id")
  data$color <- col_fun(data[,v])
  svg(paste(main.path,"/Plots/hydmet/",v,"_map.svg",sep=""),
      width=map_width,height=map_height,pointsize=1)
  par(mar=c(0.13,0.13,0.13,0.13))
  plot(data$lon,data$lat,pch=20,xlab="",ylab="",xaxt="n",yaxt="n",
       xlim=c(xmin,xmax),ylim=c(ymin,ymax),cex=0.5,col=data$color)
  plot(countriesHigh,add=TRUE,border="#838383")
  points(data$lon[order(abs(data[,v]))],data$lat[order(abs(data[,v]))],
         pch=20,col=data$color[order(abs(data[,v]))],cex=0.6) #plot smallest differences first
  north_arrow(xmax,xmin,ymax,ymin,map_params[1],map_params[2])
  map_scale(xmax,scale,ymax,ymin,bar_length)
  box(col="black")
  dev.off()
}

#histograms per country (two versions) -----
countries <- unique(substr(hydmet$id,1,2))

step <- 0.1 #width of the buckets of the histogram
low <- 0 #minimum of histogram
high <- 2 #maximum of histogram

histo_height_portrait <- map_height*1/2
histo_height_landscape <- map_height

for (v in c("AI_CAM","AI_Estr")){
  for (c in countries){
    data_hist <- hydmet[substr(hydmet$id,1,2)==c,v]
    data_hist[data_hist>high] <- high
    assign(paste("histo_",c,sep=""),
           hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                main="",xaxt="n",xlab="",yaxt="n",ylab=""))
  }
  #for all catchments (when even number of histos is required)
  data_hist <- hydmet[,v]
  data_hist[data_hist>high] <- high
  histo_all <- hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                    breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                    main="",xaxt="n",xlab="",yaxt="n",ylab="")
  
  svg(paste(main.path,"/Plots/hydmet/",v,"_histos_portrait.svg",sep=""),
      width=fig_width,height=histo_height_portrait)
  par(mfrow=c(3,3),mar=c(0.4,0.8,0.5,0.4))
  for (c in countries){
    histo <- get(paste("histo_",c,sep=""))
    barplot(histo$counts,xaxt="n",space=0,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),horiz=TRUE,border=NA,
            xlim=c(0,max(histo$counts)),main=paste(c,", n=",sum(histo$counts),sep=""),cex.main=0.6)
    axis(2,at=c(which(histo$mids==min(histo$mids))-0.5,which(round(histo$mids,digits=2)==1)-0.5,which(histo$mids==max(histo$mids))-0.5),
         labels=c(0,1,paste("≥",max(histo$mids),sep="")),cex.axis=0.5,tick=FALSE,line=-0.9,las=2)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==min(histo$mids))-0.5,which(histo$mids==min(histo$mids))-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(round(histo$mids,digits=2)==1)-0.5,which(round(histo$mids,digits=2)==1)-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==max(histo$mids))-0.5,which(histo$mids==max(histo$mids))-0.5),lty="dotted",lwd=0.5)
    axis(1,at=c(round(sum(histo$counts)/10),max(histo$counts)),
         labels=c(round(sum(histo$counts)/10),max(histo$counts)),tick=FALSE,line=-1.5,cex.axis=0.5)
    lines(x=c(round(sum(histo$counts)/10),round(sum(histo$counts)/10)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
    lines(x=c(max(histo$counts),max(histo$counts)),y=c(0,length(histo$counts)),lty="dotted",lwd=0.5)
  }
  dev.off()
  
  svg(paste(main.path,"/Plots/hydmet/",v,"_histos_landscape.svg",sep=""),
      width=fig_width,height=histo_height_landscape)
  par(mfrow=c(5,2),mar=c(0.4,0.8,0.5,0.4))
  for (c in c("all",countries)){
    histo <- get(paste("histo_",c,sep=""))
    barplot(histo$counts,xaxt="n",space=0,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),horiz=TRUE,border=NA,
            xlim=c(0,max(histo$counts)),main=paste(c,", n=",sum(histo$counts),sep=""),cex.main=0.7)
    axis(2,at=c(which(histo$mids==min(histo$mids))-0.5,which(round(histo$mids,digits=2)==1)-0.5,which(histo$mids==max(histo$mids))-0.5),
         labels=c(0,1,paste("≥",max(histo$mids),sep="")),cex.axis=0.5,tick=FALSE,line=-0.9,las=2)
    lines(x=c(0,max(histo$counts)),y=c(which(histo$mids==min(histo$mids))-0.5,which(histo$mids==min(histo$mids))-0.5),lty="dotted",lwd=0.5)
    lines(x=c(0,max(histo$counts)),y=c(which(round(histo$mids,digits=2)==1)-0.5,which(round(histo$mids,digits=2)==1)-0.5),lty="dotted",lwd=0.5)
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
title <- "[-]"

labels <- as.character(seq(as.numeric(palette_min),as.numeric(palette_max),by=incr))
labels[length(labels)] <- paste("≥",labels[length(labels)],sep="")

svg(paste(main.path,"/Plots/hydmet/hydmet_AI_legend.svg",sep=""),
    width=legend_width,height=legend_height,pointsize=6)
par(mar=c(0.05,0.05,1.3,2.3))
barplot(as.matrix(rep(1,length(scico(256,palette=palette,direction=direction)))),axes=FALSE,
        col=scico(256,palette=palette,direction=direction),border=NA)
axis(4,at=seq(1,256,by=(incr)*(255/(palette_max-palette_min)))-0.5,
     labels=labels,las=2,cex.axis=0.9,hadj=0.3,lwd=0,lwd.ticks=0.6,line=-0.05)
mtext(text=title,side=3,line=0.3,adj=0,cex=1.1)
dev.off()

#plot aridity indices against each other -----
hydmet$color[substr(hydmet$id,1,2)=="AT"] <- "#440154"
hydmet$color[substr(hydmet$id,1,2)=="CH"] <- "#472d7b"
hydmet$color[substr(hydmet$id,1,2)=="CZ"] <- "#3b528b"
hydmet$color[substr(hydmet$id,1,2)=="DE"] <- "#2c728e"
hydmet$color[substr(hydmet$id,1,2)=="DK"] <- "#21918c"
hydmet$color[substr(hydmet$id,1,2)=="ES"] <- "#28ae80"
hydmet$color[substr(hydmet$id,1,2)=="FR"] <- "#5ec962"
hydmet$color[substr(hydmet$id,1,2)=="GB"] <- "#addc30"
hydmet$color[substr(hydmet$id,1,2)=="SE"] <- "#fde725"

hydmet$AI_ratio[hydmet$AI_CAM>=hydmet$AI_Estr] <- hydmet$AI_CAM[hydmet$AI_CAM>=hydmet$AI_Estr]/hydmet$AI_Estr[hydmet$AI_CAM>=hydmet$AI_Estr]
hydmet$AI_ratio[hydmet$AI_Estr>hydmet$AI_CAM] <- hydmet$AI_Estr[hydmet$AI_Estr>hydmet$AI_CAM]/hydmet$AI_CAM[hydmet$AI_Estr>hydmet$AI_CAM]

svg(paste(main.path,"/Plots/hydmet/AI_comparison.svg",sep=""),width=fig_width,height=fig_width,pointsize=10)
par(mar=c(3,3,0.1,0.1))
plot(log(hydmet$AI_CAM[order(hydmet$AI_ratio)]),log(hydmet$AI_Estr[order(hydmet$AI_ratio)]),
     col=hydmet$color[order(hydmet$AI_ratio)],
     pch=20,cex=0.6,xlim=log(c(0.1,3.5)),ylim=log(c(0.1,3.5)),xaxt="n",yaxt="n",ylab="",xlab="")
lines(x=log(c(0.01,4)),y=log(c(0.01,4)),col="darkgrey",lty="dashed")
lines(x=log(c(0.01,4)),y=log(c(1,1)),col="darkgrey",lty="dashed")
lines(x=log(c(1,1)),y=log(c(0.01,4)),col="darkgrey",lty="dashed")
axis(1,at=log(c(0.1,0.2,0.5,1,2,3.5)),labels=c(0.1,0.2,0.5,1,2,3.5),tck=-0.01,padj=-1,cex.axis=0.8)
axis(2,at=log(c(0.1,0.2,0.5,1,2,3.5)),labels=c(0.1,0.2,0.5,1,2,3.5),tck=-0.01,hadj=0.4,las=2,cex.axis=0.8)
mtext("Aridity index from CAMELS",1,cex=0.8,line=1.8)
mtext("Aridity index from E-OBS",2,cex=0.8,line=1.8)
legend("topleft",legend=c("AT","CH","CZ","DE","DK","ES","FR","GB","SE"),
       col=c("#440154","#472d7b","#3b528b","#2c728e","#21918c","#28ae80","#5ec962","#addc30","#fde725"),ncol=3,pch=20,cex=0.8)
dev.off()





