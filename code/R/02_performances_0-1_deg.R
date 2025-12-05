rm(list=ls())

library(hydroGOF)

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"

catchments <- list.dirs(paste(main.path,"/Catchments",sep=""),recursive=FALSE)  
scenarios <- c("ptq_camels/evap_camels","pt_q_camels/evap_estreams",
               "p_t_estreams_q/evap_estreams","pt_q_camels/evap_camels","p_estreams_tq/evap_estreams")

performances <- data.frame(matrix(nrow=length(catchments),ncol=length(scenarios)+1))
names(performances) <- c("id",scenarios)
performances$id <- basename(catchments)

PBIAS <- performances

for (c in gsub("/Catchments/","/Catchments_out_0-1/",catchments)){
  for (s in scenarios){
    data <- read.csv(paste(c,"/Batch_Results/",s,"/BatchQsimSummary.txt",sep=""),sep="\t")
    data$Qobs[data$Qobs<0] <- NA
    if (sum(is.na(data$Qobs))>730){
      print(paste(sum(is.na(data$Qobs)),c,s))
    }
    performances[performances$id==basename(c),s] <- KGE(data$Qmean,data$Qobs)
    PBIAS[PBIAS$id==basename(c),s] <- pbias(data$Qmean,data$Qobs)
  }
}

write.table(performances,paste(main.path,"/Results/performances.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
write.table(PBIAS,paste(main.path,"/Results/pbias.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
