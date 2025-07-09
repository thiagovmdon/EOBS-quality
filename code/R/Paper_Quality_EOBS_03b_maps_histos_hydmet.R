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

#get hydmet data and make comparison -----
# hydmet <- data.frame(matrix(nrow=nrow(topology),ncol=10))
# names(hydmet) <- c("id","P_sum_CAM","P_sum_Estr","rel_diff_P","T_mean_CAM","T_mean_Estr","T_diff",
#                    "Epot_sum_CAM","Epot_sum_Estr","rel_diff_Epot")
# hydmet$id <- topology$id
# 
# start.date <- as.Date("19951001",format="%Y%m%d")
# end.date <- as.Date("20150930",format="%Y%m%d")
# 
# for (c in hydmet$id){
#   data_cam <- read.csv(paste(main.path,"/Catchments/",c,"/Data/ptq_camels.txt",sep=""),sep="\t")
#   data_cam$epot <- read.csv(paste(main.path,"/Catchments/",c,"/Data/evap_camels.txt",sep=""))[,1]
#   data_cam <- data_cam[as.Date(as.character(data_cam$Date),format="%Y%m%d")%in%seq(start.date,end.date,by="day"),]
#   
#   data_estr <- read.csv(paste(main.path,"/Catchments/",c,"/Data/pt_q_camels.txt",sep=""),sep="\t")
#   data_estr$epot <- read.csv(paste(main.path,"/Catchments/",c,"/Data/evap_estreams.txt",sep=""))[,1]
#   data_estr <- data_estr[as.Date(as.character(data_estr$Date),format="%Y%m%d")%in%seq(start.date,end.date,by="day"),]
#   
#   hydmet$P_sum_CAM[hydmet$id==c] <- sum(data_cam$P)/20
#   hydmet$P_sum_Estr[hydmet$id==c] <- sum(data_estr$P)/20
#   hydmet$T_mean_CAM[hydmet$id==c] <- mean(data_cam$T)
#   hydmet$T_mean_Estr[hydmet$id==c] <- mean(data_estr$T)
#   hydmet$Epot_sum_CAM[hydmet$id==c] <- sum(data_cam$epot)/20
#   hydmet$Epot_sum_Estr[hydmet$id==c] <- sum(data_estr$epot)/20
# }
# 
# hydmet$rel_diff_P <- (hydmet$P_sum_Estr-hydmet$P_sum_CAM)/hydmet$P_sum_CAM*100
# hydmet$T_diff <- hydmet$T_mean_Estr-hydmet$T_mean_CAM
# hydmet$rel_diff_Epot <- (hydmet$Epot_sum_Estr-hydmet$Epot_sum_CAM)/hydmet$Epot_sum_CAM*100
# 
# write.table(hydmet,paste(main.path,"/Results/hydmet.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)

hydmet <- read.csv(paste(main.path,"/Results/hydmet.txt",sep=""),sep="\t")
hydmet <- hydmet[hydmet$id%in%filter$basin_id,]

#for P and Epot -----
#rel_diff color palette -----
palette <- "roma" #color palette performance
direction <- 1
palette_min <- -50
palette_max <- 50
col_fun <- colorRamp2(seq(palette_min,palette_max,length.out=256),
                      colors=scico(256,palette=palette,direction=direction),space="RGB")

#rel_diff maps -----
map_width <- fig_width*0.85
map_height <- map_width/0.85 #nice ratio for Europe map

for (v in c("rel_diff_P","rel_diff_Epot")){
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

step <- 5 #width of the buckets of the histogram
low <- -50 #minimum of histogram
high <- 50 #maximum of histogram

histo_height_portrait <- map_height*1/2
histo_height_landscape <- map_height

for (v in c("rel_diff_P","rel_diff_Epot")){
  for (c in countries){
    data_hist <- hydmet[substr(hydmet$id,1,2)==c,v]
    data_hist[data_hist<low] <- low
    data_hist[data_hist>high] <- high
    assign(paste("histo_",c,sep=""),
           hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                main="",xaxt="n",xlab="",yaxt="n",ylab=""))
  }
  #for all catchments (when even number of histos is required)
  data_hist <- hydmet[,v]
  data_hist[data_hist<low] <- low
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
  
  svg(paste(main.path,"/Plots/hydmet/",v,"_histos_landscape.svg",sep=""),
      width=fig_width,height=histo_height_landscape)
  par(mfrow=c(5,2),mar=c(0.4,0.8,0.5,0.4))
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
title <- "[%]"

labels <- as.character(seq(as.numeric(palette_min),as.numeric(palette_max),by=incr))
labels[1] <- paste("≤",labels[1],sep="")
labels[length(labels)] <- paste("≥",labels[length(labels)],sep="")

svg(paste(main.path,"/Plots/hydmet/hydmet_rel_legend.svg",sep=""),
    width=legend_width,height=legend_height,pointsize=6)
par(mar=c(0.05,0.05,1.3,2.3))
barplot(as.matrix(rep(1,length(scico(256,palette=palette,direction=direction)))),axes=FALSE,
        col=scico(256,palette=palette,direction=direction),border=NA)
axis(4,at=seq(1,256,by=(incr)*(255/(palette_max-palette_min)))-0.5,
     labels=labels,las=2,cex.axis=0.9,hadj=0.3,lwd=0,lwd.ticks=0.6,line=-0.05)
mtext(text=title,side=3,line=0.3,adj=0,cex=1.1)
dev.off()

#for T -----
#T_diff color palette -----
palette <- "vik" #color palette performance
direction <- 1
palette_min <- -3
palette_max <- 3
col_fun <- colorRamp2(seq(palette_min,palette_max,length.out=256),
                      colors=scico(256,palette=palette,direction=direction),space="RGB")

#rel_diff maps -----
map_width <- fig_width*0.85
map_height <- map_width/0.85 #nice ratio for Europe map

for (v in c("T_diff")){
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

step <- 0.3 #width of the buckets of the histogram
low <- -3 #minimum of histogram
high <- 3 #maximum of histogram

histo_height_portrait <- map_height*1/2
histo_height_landscape <- map_height

for (v in c("T_diff")){
  for (c in countries){
    data_hist <- hydmet[substr(hydmet$id,1,2)==c,v]
    data_hist[data_hist<low] <- low
    data_hist[data_hist>high] <- high
    assign(paste("histo_",c,sep=""),
           hist(data_hist,col=col_fun(seq(round(low/step)*step,round(high/step)*step,by=step)),
                breaks=seq(round(low/step)*step-step/2,round(high/step)*step+step/2,by=step),
                main="",xaxt="n",xlab="",yaxt="n",ylab=""))
  }
  #for all catchments (when even number of histos is required)
  data_hist <- hydmet[,v]
  data_hist[data_hist<low] <- low
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
  
  svg(paste(main.path,"/Plots/hydmet/",v,"_histos_landscape.svg",sep=""),
      width=fig_width,height=histo_height_landscape)
  par(mfrow=c(5,2),mar=c(0.4,0.8,0.5,0.4))
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
title <- "[°C]"

labels <- as.character(seq(as.numeric(palette_min),as.numeric(palette_max),by=incr))
labels[1] <- paste("≤",labels[1],sep="")
labels[length(labels)] <- paste("≥",labels[length(labels)],sep="")

svg(paste(main.path,"/Plots/hydmet/hydmet_T_diff_legend.svg",sep=""),
    width=legend_width,height=legend_height,pointsize=6)
par(mar=c(0.05,0.05,1.3,2.3))
barplot(as.matrix(rep(1,length(scico(256,palette=palette,direction=direction)))),axes=FALSE,
        col=scico(256,palette=palette,direction=direction),border=NA)
axis(4,at=seq(1,256,by=(incr)*(255/(palette_max-palette_min)))-0.5,
     labels=labels,las=2,cex.axis=0.9,hadj=0.3,lwd=0,lwd.ticks=0.6,line=-0.05)
mtext(text=title,side=3,line=0.3,adj=0,cex=1.1)
dev.off()

