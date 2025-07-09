rm(list=ls())

main.path <- "C:/Users/fschwarz/Documents/PhD/E_OBS"

catch_list <- read.csv(paste(main.path,"/catchment_list_3563.txt",sep=""))
overview <- read.csv(paste(main.path,"/network_estreams_filtered_quality_eobs_with_DK_8297.csv",sep=""))

filter <- read.csv(paste(main.path,"/list_filtered_3423.csv",sep=""),sep=",")
catch_list <- data.frame(catch_list[catch_list$basin_id%in%filter$basin_id,])
names(catch_list) <- "basin_id"

catch_list$lon <- numeric(nrow(catch_list))
catch_list$lat <- numeric(nrow(catch_list))

for (c in catch_list$basin_id){
  catch_list$lon[catch_list$basin_id==c] <- overview$lon[overview$basin_id==c]
  catch_list$lat[catch_list$basin_id==c] <- overview$lat[overview$basin_id==c]
}

names(catch_list)[1] <- "id"

write.table(catch_list,paste(main.path,"/topology.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
