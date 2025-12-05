rm(list=ls())

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"

catch <- list.dirs(paste(main.path,"/Catchments",sep=""),recursive=FALSE)

for (c in catch){
  file.rename(paste(c,"/Data/evap_estreams.txt",sep=""),paste(c,"/Data/evap_estreams_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/p_camels_tq.txt",sep=""),paste(c,"/Data/p_camels_tq_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/p_estreams_tq.txt",sep=""),paste(c,"/Data/p_estreams_tq_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/p_t_camels_q.txt",sep=""),paste(c,"/Data/p_t_camels_q_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/p_t_estreams_q.txt",sep=""),paste(c,"/Data/p_t_estreams_q_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/pt_q_camels.txt",sep=""),paste(c,"/Data/pt_q_camels_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/pt_q_estreams.txt",sep=""),paste(c,"/Data/pt_q_estreams_0-25.txt",sep=""))
  file.rename(paste(c,"/Data/ptq_estreams.txt",sep=""),paste(c,"/Data/ptq_estreams_0-25.txt",sep=""))
}

for (c in catch){
  ptq_estreams <- read.csv(paste(main.path,"/new_October2025/basins_3563_01deg/",basename(c),"/Data/ptq_estreams_01.txt",sep=""),sep="\t")
  evap_estreams <- read.csv(paste(main.path,"/new_October2025/basins_3563_01deg/",basename(c),"/Data/evap_estreams_01.txt",sep=""),sep="\t")
  ptq_estreams$evap <- evap_estreams[,1]
  rm(evap_estreams)
  
  ptq_camels <- read.csv(paste(c,"/Data/ptq_camels.txt",sep=""),sep="\t")
  
  ptq_estreams <- ptq_estreams[ptq_estreams$Date%in%ptq_camels$Date,]
  
  #evap_estreams
  write.table(ptq_estreams$evap,paste(c,"/Data/evap_estreams.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE,col.names=basename(c))
  
  #precipitation and streamflow from camels, temperature from estreams
  p_t_estreams_q <- data.frame(ptq_camels$Date,ptq_camels$P,ptq_estreams$T,ptq_camels$Q)
  names(p_t_estreams_q) <- c("Date","P","T","Q")
  write.table(p_t_estreams_q,paste(c,"/Data/p_t_estreams_q.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
  
  #precipitation and temperature from estreams, streamflow from camels
  pt_q_camels <- data.frame(ptq_camels$Date,ptq_estreams$P,ptq_estreams$T,ptq_camels$Q)
  names(pt_q_camels) <- c("Date","P","T","Q")
  write.table(pt_q_camels,paste(c,"/Data/pt_q_camels.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
  
  #precipitation from estreams, temperature and streamflow from camels
  p_estreams_tq <- data.frame(ptq_camels$Date,ptq_estreams$P,ptq_camels$T,ptq_camels$Q)
  names(p_estreams_tq) <- c("Date","P","T","Q")
  write.table(p_estreams_tq,paste(c,"/Data/p_estreams_tq.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
}


#get pure CAMELS results (from original runs)
for (c in catch[1708:length(catch)]){
  dir.create(paste(main.path,"/Catchments_out_0-1/",basename(c),sep=""))
  dir.create(paste(main.path,"/Catchments_out_0-1/",basename(c),"/Batch_Results",sep=""))
  dir.create(paste(main.path,"/Catchments_out_0-1/",basename(c),"/Batch_Results/ptq_camels",sep=""))
  dir.create(paste(main.path,"/Catchments_out_0-1/",basename(c),"/Results",sep=""))
  dir.create(paste(main.path,"/Catchments_out_0-1/",basename(c),"/Results/ptq_camels",sep=""))
  
  file.copy(paste(main.path,"/Catchments_out/",basename(c),"/Batch_Results/ptq_camels/evap_camels",sep=""),
            paste(main.path,"/Catchments_out_0-1/",basename(c),"/Batch_Results/ptq_camels/",sep=""),
            recursive=TRUE)
  file.copy(paste(main.path,"/Catchments_out/",basename(c),"/Results/ptq_camels/evap_camels",sep=""),
            paste(main.path,"/Catchments_out_0-1/",basename(c),"/Results/ptq_camels/",sep=""),
            recursive=TRUE)
}




