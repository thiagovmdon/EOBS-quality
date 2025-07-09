rm(list=ls())

library(hydroGOF)

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"

catchments <- list.dirs(paste(main.path,"/Catchments",sep=""),recursive=FALSE)  
scenarios <- paste(c("p_camels_tq","p_estreams_tq","p_t_camels_q","p_t_estreams_q",
                     "pt_q_camels","pt_q_estreams","ptq_camels","ptq_estreams"),
                   rep(c("evap_camels","evap_estreams"),each=8),sep="/")

performances <- data.frame(matrix(nrow=length(catchments),ncol=length(scenarios)+1))
names(performances) <- c("id",scenarios)
performances$id <- basename(catchments)

for (c in gsub("/Catchments/","/Catchments_out/",catchments)){
  for (s in scenarios){
    data <- read.csv(paste(c,"/Batch_Results/",s,"/BatchQsimSummary.txt",sep=""),sep="\t")
    data$Qobs[data$Qobs<0] <- NA
    if (sum(is.na(data$Qobs))>730){
      print(paste(sum(is.na(data$Qobs)),c,s))
    }
    performances[performances$id==basename(c),s] <- KGE(data$Qmean,data$Qobs)
  }
}

write.table(performances,paste(main.path,"/Results/performances.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)

performances <- read.csv(paste(main.path,"/Results/performances.txt",sep=""),sep="\t")
names(performances) <- gsub("s.e","s/e",names(performances))
names(performances) <- gsub("q.e","q/e",names(performances))

performances$color[substr(performances$id,1,2)=="AT"] <- "pink"
performances$color[substr(performances$id,1,2)=="CH"] <- "red"
performances$color[substr(performances$id,1,2)=="CZ"] <- "purple"
performances$color[substr(performances$id,1,2)=="DE"] <- "gold"
performances$color[substr(performances$id,1,2)=="DK"] <- "lightblue"
performances$color[substr(performances$id,1,2)=="ES"] <- "orange"
performances$color[substr(performances$id,1,2)=="FR"] <- "blue"
performances$color[substr(performances$id,1,2)=="GB"] <- "darkgreen"
performances$color[substr(performances$id,1,2)=="SE"] <- "black"

performances$cam_min_estr <- performances$`ptq_camels/evap_camels`-performances$`pt_q_camels/evap_estreams`

#CAMELS performances
plot(which(performances$`ptq_camels/evap_camels`>(-1)),performances$`ptq_camels/evap_camels`[which(performances$`ptq_camels/evap_camels`>(-1))],
     col=performances$color[which(performances$`ptq_camels/evap_camels`>(-1))],
     pch=20,ylab="KGE CAMELS",ylim=c(-1,1),xlim=c(1,length(catchments)),xlab="Catchment indices")
points(which(performances$`ptq_camels/evap_camels`<(-1)),rep(-1,length(which(performances$`ptq_camels/evap_camels`<(-1)))),
       col=performances$color[which(performances$`ptq_camels/evap_camels`<(-1))],pch=17)
legend("topright",legend=c("AT","CH","CZ","DE","DK","ES","FR","GB","SE"),ncol=3,
       fill=c("pink","red","purple","gold","lightblue","orange","blue","darkgreen","black"),
       inset=c(0,-0.5),xpd=TRUE)

#ESTREAMS performances
plot(which(performances$`pt_q_camels/evap_estreams`>(-1)),performances$`pt_q_camels/evap_estreams`[which(performances$`pt_q_camels/evap_estreams`>(-1))],
     col=performances$color[which(performances$`pt_q_camels/evap_estreams`>(-1))],
     pch=20,ylab="KGE ESTREAMS",ylim=c(-1,1),xlim=c(1,length(catchments)),xlab="Catchment indices")
points(which(performances$`pt_q_camels/evap_estreams`<(-1)),rep(-1,length(which(performances$`pt_q_camels/evap_estreams`<(-1)))),
       col=performances$color[which(performances$`pt_q_camels/evap_estreams`<(-1))],pch=17)
legend("topright",legend=c("AT","CH","CZ","DE","DK","ES","FR","GB","SE"),ncol=3,
       fill=c("pink","red","purple","gold","lightblue","orange","blue","darkgreen","black"),
       inset=c(0,-0.5),xpd=TRUE)

#Performance differences
plot(which(performances$cam_min_estr>(-1)&performances$cam_min_estr<1),
     performances$cam_min_estr[which(performances$cam_min_estr>(-1)&performances$cam_min_estr<1)],
     col=performances$color[which(performances$cam_min_estr>(-1)&performances$cam_min_estr<1)],
     pch=20,ylab="ΔKGE (CAMELS-ESTREAMS)",ylim=c(-1,1),xlim=c(1,length(catchments)),xlab="Catchment indices")
lines(x=c(-1,length(catchments)+1),y=c(0,0))
points(which(performances$cam_min_estr<(-1)),rep(-1,length(which(performances$cam_min_estr<(-1)))),
       col=performances$color[which(performances$cam_min_estr<(-1))],pch=17)
points(which(performances$cam_min_estr>1),rep(1,length(which(performances$cam_min_estr>1))),
       col=performances$color[which(performances$cam_min_estr>1)],pch=17)
legend("topright",legend=c("AT","CH","CZ","DE","DK","ES","FR","GB","SE"),ncol=3,
       fill=c("pink","red","purple","gold","lightblue","orange","blue","darkgreen","black"),
       inset=c(0,-0.5),xpd=TRUE)

