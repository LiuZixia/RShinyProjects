#Download link for raw data: http://ci5.iarc.fr/CI5-XI/CI5-XId.zip
#Set file path and read file names
file_name <- list.files(path = "./data/CI5-XId/raw", all.files = T, 
                        full.names = T)
file_name <- file_name[3:length(file_name)]

#Combine data from all csv files into one dataframe
combined_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(combined_df) <- c("registry_code", "sex", "age_group", 
                           "number_of_cases", "person_years_at_risk")

for(i in 1:length(file_name)){
  #Read files one by one 
  temp_all <- read.csv(file = file_name[i], header = F)
  #Select the lung cancer cases
  temp_lung <- temp_all[which(temp_all$V2 == 71),]
  registry_code <- rep(substr(file_name[i], 25, 33),length(temp_lung$V1))
  temp_df <- data.frame(registry_code = as.integer(registry_code), 
                        sex = temp_lung$V1, age_group = temp_lung$V3, 
                        number_of_cases = temp_lung$V4, 
                        person_years_at_risk = temp_lung$V5)
  combined_df <- rbind(combined_df, temp_df)
}

#Read and reformat the list of registry agencies
registry_info <- read.csv(file = "./data/CI5-XId/registry_detailed.txt",
                          header = F, sep = ";")
colnames(registry_info) <- c("registry_code", "discription", "start", 
                             "end")
registry_info$discription <- trimws(registry_info$discription, "l")

#Convert city names to coordinates
library(jsonlite)
region_list <- registry_info[grep(":", registry_info$discription, invert =T), ]
region_list$latitude <- NA
region_list$longitude <- NA
for(i in 1:length(region_list$discription)){
  tryCatch({
    region_info_raw <- fromJSON(gsub(" ", "%20", 
                                     paste0("https://api.opencagedata.com/geocode/v1/json?q=",
                                            region_list$discription[i],
                                            "&key=cd1b2dd087b3456b9ce01f0aaa9cbaf6"), fixed = T))
    region_list$latitude[i] <- region_info_raw$results$geometry[1,"lat"]
    region_list$longitude[i] <- region_info_raw$results$geometry[1,"lng"]
  }, error=function(e){cat("ERROR for", region_list$discription[i], i, ":", conditionMessage(e), "\n")})
}
#Some of the coordinate is not available, so I checked it manually
region_list$latitude[152] <- 26.1275551
region_list$longitude[152] <- 91.2998715
region_list$latitude[161] <- 37.1984436
region_list$longitude[161] <- 55.070672
#Write the registry agencies info to file
write.csv(region_list, 
          file = "./data/CI5-XId/converted/registry_info.csv", quote = T, 
          row.names = F)

#Merge registry agencies info and lung cancer incidence rate data
CI5_XId_converted <- merge(combined_df, region_list, by = "registry_code")
write.csv(CI5_XId_converted, 
          file = "./data/CI5-XId/converted/CI5_XId_converted.csv", 
          quote = T, row.names = F)
