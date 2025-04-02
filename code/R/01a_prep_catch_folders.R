rm(list=ls())

library(plyr) #for join

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"

#import the list of catchments
overview <- read.csv(paste(main.path,"/catchment_overview.csv",sep="")) 

#delete the catchments from DK that are in this lits (were updated later)
overview <- overview[-which(substr(overview$basin_id,1,2)=="DK"),]

#get the updated DK catchments file
overview_dk <- read.csv(paste(main.path,"/catchment_overview_DK.csv",sep="")) 

names <- c(overview$basin_id,overview_dk$basin_id)

#generate a catchment folder with a data folder and copy the "basic" Batch_Simulation file into it ----
for (n in names){
  if (!dir.exists(paste(main.path,"/Catchments/",n,sep=""))){
    dir.create(paste(main.path,"/Catchments/",n,sep=""))
    dir.create(paste(main.path,"/Catchments/",n,"/Data",sep=""))
    file.copy(paste(main.path,"/Batch_Simulation.xml",sep=""),
              paste(main.path,"/Catchments/",n,"/Data/Batch_Simulation.xml",sep=""))
  }
}


#catchments from LamaH-CE -----
at <- overview[substr(overview$basin,1,2)=="AT",]
#get the catchment areas for the calculation of the specific streamflow
areas <- read.csv(paste(main.path,"/LAMAH_CE/A_basins_total_upstrm/1_attributes/Catchment_attributes.csv",sep=""),
                  sep=";")
#get a taple to be able to link th different ids with each other
lookup <- read.csv(paste(main.path,"/LAMAH_CE/D_gauges/1_attributes/Gauge_attributes.csv",sep=""),sep=";")

catchments <- list.dirs(paste(main.path,"/Catchments",sep=""),recursive=FALSE,full.names=FALSE)
catchments <- catchments[substr(catchments,1,2)=="AT"]

at <- at[at$basin_id%in%catchments,]

for (c in at$gauge_id){
  basin <- at$basin_id[at$gauge_id==c]

  lamah.id <- lookup$ID[lookup$govnr==c]

  area_m2 <- areas$area_calc[areas$ID==lamah.id]*1000*1000

  timeseries <- read.csv(paste(main.path,
                               "/LAMAH_CE/A_basins_total_upstrm/2_timeseries/daily/ID_",lamah.id,".csv",
                               sep=""),sep=";")

  #zero-padding for month and day
  timeseries$MM[nchar(timeseries$MM)==1] <- paste("0",timeseries$MM[nchar(timeseries$MM)==1],sep="")
  timeseries$DD[nchar(timeseries$DD)==1] <- paste("0",timeseries$DD[nchar(timeseries$DD)==1],sep="")

  ptq <- data.frame(paste(timeseries$YYYY,timeseries$MM,timeseries$DD,sep=""),timeseries$prec,
                    timeseries$X2m_temp_mean)
  names(ptq) <- c("Date","P","T")

  qseries <- read.csv(paste(main.path,
                            "/LAMAH_CE/D_gauges/2_timeseries/daily/ID_",lamah.id,".csv",
                            sep=""),sep=";")
  qseries$q_spec <- qseries$qobs*60*60*24*1000/area_m2 #conversion from m3/s to mm/d

  qseries$MM[nchar(qseries$MM)==1] <- paste("0",qseries$MM[nchar(qseries$MM)==1],sep="")
  qseries$DD[nchar(qseries$DD)==1] <- paste("0",qseries$DD[nchar(qseries$DD)==1],sep="")

  qseries <- data.frame(paste(qseries$YYYY,qseries$MM,qseries$DD,sep=""),qseries$q_spec)
  names(qseries) <- c("Date","Q")

  ptq <- join(ptq,qseries,by="Date")
  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)
}
rm(areas,qseries,area_m2,ptq,timeseries,basin,c,evap)

for (c in at$gauge_id){
  basin <- at$basin_id[at$gauge_id==c]

  lamah.id <- lookup$ID[lookup$govnr==c]

  timeseries <- read.csv(paste(main.path,
                               "/LAMAH_CE/F_hydrol_model/2_timeseries/ID_",lamah.id,".csv",
                               sep=""),sep=";")

  #zero-padding for month and day
  timeseries$MM[nchar(timeseries$MM)==1] <- paste("0",timeseries$MM[nchar(timeseries$MM)==1],sep="")
  timeseries$DD[nchar(timeseries$DD)==1] <- paste("0",timeseries$DD[nchar(timeseries$DD)==1],sep="")

  timeseries <- data.frame(paste(timeseries$YYYY,timeseries$MM,timeseries$DD,sep=""),timeseries$PET_A)
  names(timeseries) <- c("Date","PET")

  ptq <- read.csv(paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),sep="\t")

  all_data <- join(ptq,timeseries,by="Date")
  all_data <- all_data[!is.na(all_data$PET),]

  ptq <- all_data[,c("Date","P","T","Q")]

  evap <- all_data$PET

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
                           sep="\t",quote=FALSE,row.names=FALSE)
  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}


#catchments from CAMELS-SE -----
se <- overview[substr(overview$basin,1,2)=="SE",]
catchments <- list.files(paste(main.path,"/Catchments",sep=""))
se <- se[se$basin_id%in%catchments,]

files <- list.files(paste(main.path,"/CAMELS_SE/timeseries",sep=""))

exclude <- character(0)
for (c in se$gauge_id){
  basin <- se$basin_id[se$gauge_id==c]

  if (length(grep(paste("_",c,"_",sep=""),files))!=0){
    timeseries <- read.csv(paste(main.path,"/CAMELS_SE/timeseries/",files[grep(paste("_",c,"_",sep=""),files)],sep=""),
                           sep=",")

    #zero-padding for month and day
    timeseries$Month[nchar(timeseries$Month)==1] <- paste("0",timeseries$Month[nchar(timeseries$Month)==1],sep="")
    timeseries$Day[nchar(timeseries$Day)==1] <- paste("0",timeseries$Day[nchar(timeseries$Day)==1],sep="")

    ptq <- data.frame(paste(timeseries$Year,timeseries$Month,timeseries$Day,sep=""),
                      timeseries$Pobs_mm,timeseries$Tobs_C,timeseries$Qobs_mm)
    names(ptq) <- c("Date","P","T","Q")
    ptq$Q[is.na(ptq$Q)] <- -9999

    write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
                sep="\t",quote=FALSE,row.names=FALSE)
  }else{
    exclude <- append(exclude,basin)
  }
}

#delete catchments that are not included in CAMELS-SE
for (d in exclude){
  unlink(paste(main.path,"/Catchments/",d,sep=""),recursive=TRUE)
  overview <- overview[-which(overview$basin_id==d),]
}
se <- overview[substr(overview$basin,1,2)=="SE",]

rm(ptq,timeseries,basin,c,evap,files,d,exclude)

files <- list.files(paste(main.path,"/cAMELS_SE/PET_Hamon",sep="")) #these are files provided by Claudia Teutschbein (not available in CAMELS-SE)
for (c in se$gauge_id){
  basin <- se$basin_id[se$gauge_id==c]

  evap <- read.csv(paste(main.path,"/CAMELS_SE/PET_Hamon/",files[grep(paste("_",c,"_",sep=""),files)],sep=""),sep=",")

  #zero-padding for month and day
  evap$Month[nchar(evap$Month)==1] <- paste("0",evap$Month[nchar(evap$Month)==1],sep="")
  evap$Day[nchar(evap$Day)==1] <- paste("0",evap$Day[nchar(evap$Day)==1],sep="")
  evap$Date <- paste(evap$Year,evap$Month,evap$Day,sep="")
  evap <- data.frame(evap$Date,evap$PET_Hamon_mm)
  names(evap) <- c("Date","PET")

  ptq <- read.csv(paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),sep="\t")

  evap <- evap$PET[evap$Date%in%ptq$Date]

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}

#catchment data from CAMELS-CH -----
ch <- overview[substr(overview$basin,1,2)=="CH",]

for (c in ch$gauge_id){
  basin <- ch$basin_id[ch$gauge_id==c]

  timeseries <- read.csv(paste(main.path,"/CAMELS_CH/timeseries/observation_based/CAMELS_CH_obs_based_",c,".csv",sep=""),
                         sep=",")
  ptq <- data.frame(timeseries$date,timeseries$precipitation.mm.d.,
                    timeseries$temperature_mean.degC.,timeseries$discharge_spec.mm.d.)
  names(ptq) <- c("Date","P","T","Q")
  ptq$Date <- gsub("-","",ptq$Date)
  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  timeseries <- read.csv(paste(main.path,"/CAMELS_CH/timeseries/simulation_based/CAMELS_CH_sim_based_",c,".csv",sep=""),
                         sep=",")
  if (sum(as.Date(timeseries$date,format="%Y-%m-%d")!=as.Date(ptq$Date,format="%Y%m%d"))>0){
    print("ptq and evap date do not fit")
  }
  evap <- timeseries$pet_sim.mm.d.

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}
rm(ptq,timeseries,basin,c,evap)

#catchment data from CAMELS-DE -----
#delete catchments that are not in CAMELS-DE
de <- overview[substr(overview$basin,1,2)=="DE",]
lookup <- read.csv(paste(main.path,"/CAMELS_DE/CAMELS_DE_topographic_attributes.csv",sep=""))

exclude <- character(0)
for (c in de$gauge_id){
  basin <- de$basin_id[de$gauge_id==c]

  if (substr(basin,1,4)=="DEBW"){ #for catchments in BW, there's no provider number but a name in the overview table
    name <- de$gauge_name[de$gauge_id==c]
    camels.id <- lookup$gauge_id[paste(lookup$gauge_name,lookup$water_body_name)==name]
  }else if (substr(basin,1,4)=="DETH"){ #for catchments in TH, 4 zeros need to be deleted from the gauge_ide in the overview table
    c <- substr(c,1,nchar(c)-4)
    camels.id <- lookup$gauge_id[lookup$provider_id==c]
  }else{
    camels.id <- lookup$gauge_id[lookup$provider_id==c]
  }
  if (length(camels.id)==0){
    exclude <- append(exclude,basin)
  }
}

for (d in exclude){
  unlink(paste(main.path,"/Catchments/",d,sep=""),recursive=TRUE)
  overview <- overview[-which(overview$basin_id==d),]
}

de <- overview[substr(overview$basin,1,2)=="DE",]

for (c in de$gauge_id[]){
  basin <- de$basin_id[de$gauge_id==c]

  if (substr(basin,1,4)=="DEBW"){ #for catchments in BW, there's no provider number but a name in the overview table
    name <- de$gauge_name[de$gauge_id==c]
    camels.id <- lookup$gauge_id[paste(lookup$gauge_name,lookup$water_body_name)==name]
  }else if (substr(basin,1,4)=="DETH"){ #for catchments in TH, 4 zeros need to be deleted from the gauge_ide in the overview table
    c <- substr(c,1,nchar(c)-4)
    camels.id <- lookup$gauge_id[lookup$provider_id==c]
  }else{
    camels.id <- lookup$gauge_id[lookup$provider_id==c]
  }

  timeseries <- read.csv(paste(main.path,"/CAMELS_DE/timeseries/CAMELS_DE_hydromet_timeseries_",camels.id,".csv",sep=""))
  ptq <- data.frame(timeseries$date,timeseries$precipitation_mean,
                    timeseries$temperature_mean,timeseries$discharge_spec_obs)
  names(ptq) <- c("Date","P","T","Q")
  ptq$Date <- gsub("-","",ptq$Date)
  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  timeseries <- read.csv(paste(main.path,"/CAMELS_DE/timeseries_simulated/CAMELS_DE_discharge_sim_",camels.id,".csv",sep=""),
                         sep=",")
  if (sum(as.Date(timeseries$date,format="%Y-%m-%d")!=as.Date(ptq$Date,format="%Y%m%d"))>0){
    print("ptq and evap date do not fit")
  }
  evap <- timeseries$pet_hargreaves

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}
rm(ptq,timeseries,basin,c,evap,lookup,exclude,d,name,camels.id,n)

#catchment data from CAMELS-GB -----
gb <- overview[substr(overview$basin,1,2)=="GB",]

for (c in gb$gauge_id){
  basin <- gb$basin_id[gb$gauge_id==c]

  timeseries <- read.csv(paste(main.path,"/CAMELS_GB/data/timeseries/CAMELS_GB_hydromet_timeseries_",c,
                               "_19701001-20150930.csv",sep=""),
                         sep=",")
  ptq <- data.frame(timeseries$date,timeseries$precipitation,
                    timeseries$temperature,timeseries$discharge_spec)
  names(ptq) <- c("Date","P","T","Q")
  ptq$Date <- gsub("-","",ptq$Date)
  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  evap <- timeseries$pet #peti would include a correction factor for interception on days with precipitation

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}
rm(ptq,timeseries,basin,c,evap)

#catchment data from CAMELS-FR -----
fr <- overview[substr(overview$basin,1,2)=="FR",]

exclude <- character(0)
for (c in fr$gauge_id){
  basin <- fr$basin_id[fr$gauge_id==c]

  if (file.exists(paste(main.path,"/CAMELS_FR/daily/CAMELS_FR_tsd_",c,".csv",sep=""))){
    timeseries <- read.csv(paste(main.path,"/CAMELS_FR/daily/CAMELS_FR_tsd_",c,".csv",sep=""),
                           sep=";",skip=7)
    ptq <- data.frame(timeseries$tsd_date,timeseries$tsd_prec,
                      timeseries$tsd_temp,timeseries$tsd_q_mm)
    names(ptq) <- c("Date","P","T","Q")
    ptq$Date <- gsub("-","",ptq$Date)
    ptq$Q[is.na(ptq$Q)] <- -9999

    write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
                sep="\t",quote=FALSE,row.names=FALSE)

    evap <- timeseries$tsd_pet_ou

    write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
                quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
  }else{
    exclude <- append(exclude,basin)
  }
}

#delete catchments that are not included in CAMELS-FR
for (d in exclude){
  unlink(paste(main.path,"/Catchments/",d,sep=""),recursive=TRUE)
  overview <- overview[-which(overview$basin_id==d),]
}
fr <- overview[substr(overview$basin,1,2)=="FR",]

rm(ptq,timeseries,basin,c,evap,d,exclude)

#catchments from LamaH-ICE -----
is <- overview[substr(overview$basin,1,2)=="IS",]
areas <- read.csv(paste(main.path,"/LAMAH_ICE/A_basins_total_upstrm/1_attributes/Catchment_attributes.csv",sep=""),
                  sep=";")

for (c in is$gauge_id){
  basin <- is$basin_id[is$gauge_id==c]

  area_m2 <- areas$area_calc[areas$id==c]*1000*1000

  timeseries <- read.csv(paste(main.path,
                               "/LAMAH_ICE/A_basins_total_upstrm/2_timeseries/daily/meteorological_data/ID_",c,".csv",
                               sep=""),sep=";")

  #zero-padding for month and day
  timeseries$MM[nchar(timeseries$MM)==1] <- paste("0",timeseries$MM[nchar(timeseries$MM)==1],sep="")
  timeseries$DD[nchar(timeseries$DD)==1] <- paste("0",timeseries$DD[nchar(timeseries$DD)==1],sep="")

  #RAV available from Sept 1958 to Aug 2019
  timeseries <- timeseries[as.Date(paste(timeseries$YYYY,timeseries$MM,timeseries$DD,sep=""),
                                   format="%Y%m%d")>as.Date("19580831",format="%Y%m%d"),]
  timeseries <- timeseries[as.Date(paste(timeseries$YYYY,timeseries$MM,timeseries$DD,sep=""),
                                   format="%Y%m%d")<as.Date("20190901",format="%Y%m%d"),]

  #RAV temperature and precipitation data
  ptq <- data.frame(paste(timeseries$YYYY,timeseries$MM,timeseries$DD,sep=""),timeseries$prec_rav,
                    timeseries$X2m_temp_rav)
  names(ptq) <- c("Date","P","T")

  #quality-filtered Q data
  qseries <- read.csv(paste(main.path,
                            "/LAMAH_ICE/D_gauges/2_timeseries/daily_filtered/ID_",c,".csv",
                               sep=""),sep=";")
  qseries$q_spec <- qseries$qobs*60*60*24*1000/area_m2 #conversion from m3/s to mm/d

  qseries$MM[nchar(qseries$MM)==1] <- paste("0",qseries$MM[nchar(qseries$MM)==1],sep="")
  qseries$DD[nchar(qseries$DD)==1] <- paste("0",qseries$DD[nchar(qseries$DD)==1],sep="")
  qseries <- qseries[as.Date(paste(qseries$YYYY,qseries$MM,qseries$DD,sep=""),
                                   format="%Y%m%d")>as.Date("19580831",format="%Y%m%d"),]
  qseries <- qseries[as.Date(paste(qseries$YYYY,qseries$MM,qseries$DD,sep=""),
                                   format="%Y%m%d")<as.Date("20190901",format="%Y%m%d"),]

  qseries <- data.frame(paste(qseries$YYYY,qseries$MM,qseries$DD,sep=""),qseries$q_spec)
  names(qseries) <- c("Date","Q")

  ptq <- join(ptq,qseries,by="Date")
  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  evap <- timeseries$pet

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}
rm(areas,qseries,area_m2,ptq,timeseries,basin,c,evap)

#catchments from BULL -----
es <- overview[substr(overview$basin,1,2)=="ES",]

exclude <- character(0)
for (c in es$gauge_id){
  basin <- es$basin_id[es$gauge_id==c]

  if (file.exists(paste(main.path,"/BULL/timeseries/csv/AEMET/AEMET_",c,".csv",sep=""))){
    timeseries <- read.csv(paste(main.path,"/BULL/timeseries/csv/AEMET/AEMET_",c,".csv",sep=""),
                           sep=",")
    qseries <- read.csv(paste(main.path,"/BULL/timeseries/csv/streamflow/streamflow_",c,".csv",sep=""))

    timeseries <- join(timeseries,qseries,by="date")

    ptq <- data.frame(timeseries$date,timeseries$total_precipitation,
                      timeseries$temperature_mean,timeseries$streamflow)
    names(ptq) <- c("Date","P","T","Q")
    ptq$Date <- gsub("-","",ptq$Date)

    ptq$Q[is.na(ptq$Q)] <- -9999

    write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
                sep="\t",quote=FALSE,row.names=FALSE)

    evap <- timeseries$potential_evapotranspiration

    write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
                quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
  }else{
    exclude <- append(exclude,basin)
  }
}

#delete catchments that are not included in BULL
for (d in exclude){
  unlink(paste(main.path,"/Catchments/",d,sep=""),recursive=TRUE)
  overview <- overview[-which(overview$basin_id==d),]
}
es <- overview[substr(overview$basin,1,2)=="ES",]

rm(ptq,timeseries,basin,c,evap,exclude,qseries)


#catchments from CAMELS-DK -----
dk <- overview_dk
areas <- read.csv(paste(main.path,"/CAMELS_DK/CAMELS_DK_topography.csv",sep=""))

for (c in dk$catch_id){
  basin <- dk$basin_id[dk$catch_id==c]

  area_m2 <- areas$catch_area[as.character(areas$catch_id)==as.character(c)]

  timeseries <- read.csv(paste(main.path,"/CAMELS_DK/timeseries/CAMELS_DK_obs_based_",c,".csv",sep=""),
                         sep=",")
  ptq <- data.frame(timeseries$time,timeseries$precipitation,
                    timeseries$temperature,timeseries$Qobs)
  names(ptq) <- c("Date","P","T","Q")
  ptq$Date <- gsub("-","",ptq$Date)

  #calculate specific discharge
  ptq$Q <- ptq$Q*3600*24/area_m2*1000

  ptq$Q[is.na(ptq$Q)] <- -9999

  write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
              sep="\t",quote=FALSE,row.names=FALSE)

  evap <- timeseries$pet

  write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
              quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
}
rm(ptq,timeseries,basin,c,evap,areas)


#catchments from CAMELS-CZ (provided by Michal) -----
cz <- overview[substr(overview$basin_id,1,2)=="CZ",]

exclude <- character(0)
for (c in cz$gauge_id){
  basin <- cz$basin_id[cz$gauge_id==c]

  if (file.exists(paste(main.path,"/CAMELS-CZ_HBV/catchments_dmr5g/",c,"/Data/PTQ.txt",sep=""))){
    ptq <- read.csv(paste(main.path,"/CAMELS-CZ_HBV/catchments_dmr5g/",c,"/Data/PTQ.txt",sep=""),
                           sep="\t",skip=1) #data already in HBV format (by Michal)
    
    write.table(ptq,paste(main.path,"/Catchments/",basin,"/Data/ptq_camels.txt",sep=""),
                sep="\t",quote=FALSE,row.names=FALSE)
    
    evap <- read.csv(paste(main.path,"/CAMELS-CZ_HBV/catchments_dmr5g/",c,"/Data/EVAP.txt",sep=""),
                     sep="\t") #data already in HBV format (by Michal)
    
    write.table(evap,paste(main.path,"/Catchments/",basin,"/Data/evap_camels.txt",sep=""),
                quote=FALSE,row.names=FALSE,col.names=paste("Evap",basin))
  }else{
    exclude <- append(exclude,basin)
  }
}
rm(ptq,timeseries,basin,c,evap,areas)

#delete catchments that are not included in CAMELS-SE
for (d in exclude){
  unlink(paste(main.path,"/Catchments/",d,sep=""),recursive=TRUE)
  overview <- overview[-which(overview$basin_id==d),]
}

rm(d,exclude)
