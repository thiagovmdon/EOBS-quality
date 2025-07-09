rm(list=ls())

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"
catchments <- list.dirs(paste(main.path,"/Catchments",sep=""),full.names=FALSE,recursive=FALSE)

start.date <- as.Date("19901001",format="%Y%m%d")
end.date <- as.Date("20150930",format="%Y%m%d")

#unzip files from Thiago -----
zips <- list.files(paste(main.path,"/hbv_files_v02",sep=""),full.names=TRUE)

for (i in 1:length(zips)){
  unzip(zips[i],exdir=gsub(".zip","",zips[i]))
  file.remove(zips[i])
}

#copy the files to the Catchment folders ----
for (c in catchments){
  if (!file.exists(paste(main.path,"/hbv_files_v02/",c,"/ptq_estreams.txt",sep=""))){
    print(paste(c,"no estreams file!"))
  }
  file.copy(paste(main.path,"/hbv_files_v02/",c,"/ptq_estreams.txt",sep=""),
            paste(main.path,"/Catchments/",c,"/Data/ptq_estreams.txt",sep=""),overwrite=TRUE)

  file.copy(paste(main.path,"/hbv_files_v02/",c,"/evap_estreams.txt",sep=""),
            paste(main.path,"/Catchments/",c,"/Data/evap_estreams.txt",sep=""),overwrite=TRUE)
}

#make all ptq and evap files the same length -----
for (c in catchments){
  ptq_c <- read.csv(paste(main.path,"/Catchments/",c,"/Data/ptq_camels.txt",sep=""),sep="\t")
  evap_c <- read.csv(paste(main.path,"/Catchments/",c,"/Data/evap_camels.txt",sep=""),sep="\t")

  camels <- cbind(ptq_c,evap_c)

  ptq_e <- read.csv(paste(main.path,"/Catchments/",c,"/Data/ptq_estreams.txt",sep=""),sep="\t")
  evap_e <- read.csv(paste(main.path,"/Catchments/",c,"/Data/evap_estreams.txt",sep=""),sep="\t")

  estreams <- cbind(ptq_e,evap_e)

  camels <- camels[as.Date(as.character(camels$Date),format="%Y%m%d")>=start.date&
                     as.Date(as.character(camels$Date),format="%Y%m%d")<=end.date,]
  estreams <- estreams[as.Date(as.character(estreams$Date),format="%Y%m%d")>=start.date&
                     as.Date(as.character(estreams$Date),format="%Y%m%d")<=end.date,]

  ptq_c <- camels[,-5]
  evap_c <- camels[,5]

  ptq_e <- estreams[,-5]
  evap_e <- estreams[,5]
  
  #check for missing meteorological data
  if (sum(ptq_c$P==-9999)!=0 | sum(is.na(ptq_c$P))!=0){
    print(paste(c,": missing P value in CAMELS!",sep=""))
  }
  
  if (sum(ptq_e$P==-9999)!=0 | sum(is.na(ptq_e$P))!=0){
    print(paste(c,": missing P value in EStreams!",sep=""))
  }
  
  if (sum(ptq_c$T==-9999)!=0 | sum(is.na(ptq_c$T))!=0){
    print(paste(c,": missing T value in CAMELS!",sep=""))
  }
  
  if (sum(ptq_e$T==-9999)!=0 | sum(is.na(ptq_e$T))!=0){
    print(paste(c,": missing P value in EStreams!",sep=""))
  }
  
  if (sum(evap_c==-9999)!=0 | sum(is.na(evap_c))!=0){
    print(paste(c,": missing EVAP value in CAMELS!",sep=""))
  }
  
  if (sum(evap_e==-9999)!=0 | sum(is.na(evap_e))!=0){
    print(paste(c,": missing EVAP value in EStreams!",sep=""))
  }

  write.table(ptq_c,paste(main.path,"/Catchments/",c,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(ptq_e,paste(main.path,"/Catchments/",c,"/Data/ptq_estreams.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  write.table(evap_c,paste(main.path,"/Catchments/",c,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",c))
  write.table(evap_e,paste(main.path,"/Catchments/",c,"/Data/evap_estreams.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",c))
}

#mix ptq files -----
p_camels_tq <- data.frame(matrix(nrow=length(seq(start.date,end.date,by="day")),ncol=4))
names(p_camels_tq) <- c("Date","P","T","Q")
p_camels_tq$Date <- gsub("-","",as.character(seq(start.date,end.date,by="day")))

p_t_camels_q <- p_camels_tq
pt_q_camels <- p_camels_tq
p_estreams_tq <- p_camels_tq
p_t_estreams_q <- p_camels_tq
pt_q_estreams <- p_camels_tq

for (c in catchments){
  ptq_c <- read.csv(paste(main.path,"/Catchments/",c,"/Data/ptq_camels.txt",sep=""),sep="\t")
  ptq_e <- read.csv(paste(main.path,"/Catchments/",c,"/Data/ptq_estreams.txt",sep=""),sep="\t")
  
  p_camels_tq$P <- p_t_estreams_q$P <- pt_q_estreams$P <- ptq_c$P
  p_estreams_tq$P <- p_t_camels_q$P <- pt_q_camels$P <- ptq_e$P
  
  p_t_camels_q$T <- p_estreams_tq$T <- pt_q_estreams$T <- ptq_c$T
  p_t_estreams_q$T <- p_camels_tq$T <- pt_q_camels$T <- ptq_e$T
  
  pt_q_camels$Q <- p_estreams_tq$Q <- p_t_estreams_q$Q <- ptq_c$Q
  pt_q_estreams$Q <- p_camels_tq$Q <- p_t_camels_q$Q <- ptq_e$Q
  
  write.table(p_camels_tq,paste(main.path,"/Catchments/",c,"/Data/p_camels_tq.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(p_t_camels_q,paste(main.path,"/Catchments/",c,"/Data/p_t_camels_q.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(pt_q_camels,paste(main.path,"/Catchments/",c,"/Data/pt_q_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(p_estreams_tq,paste(main.path,"/Catchments/",c,"/Data/p_estreams_tq.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(p_t_estreams_q,paste(main.path,"/Catchments/",c,"/Data/p_t_estreams_q.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
  write.table(pt_q_estreams,paste(main.path,"/Catchments/",c,"/Data/pt_q_estreams.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
}



