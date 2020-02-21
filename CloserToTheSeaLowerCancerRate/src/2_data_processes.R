#----Read in converted data and registry info----
data_raw <- read.csv(file = "./data/CI5-XId/converted/CI5_XId_converted.csv", 
                     header =T)
region_list <- read.csv(file = "./data/CI5-XId/converted/registry_info.csv", 
                        header =T)[,c(1,5,6)]

#----Remove data with insufficient population----
data_removed_NA <- data_raw[which(data_raw$person_years_at_risk > 5000),]

#----Combine data based on groups----
for(sex in c("all","male", "female")){
  if(sex == "all"){
    data_removed_NA_with_sex <- data_removed_NA
  }else if(sex == "male"){
    data_removed_NA_with_sex <- data_removed_NA[which(data_removed_NA$sex == 1),]
  }else if(sex == "female"){
    data_removed_NA_with_sex <- data_removed_NA[which(data_removed_NA$sex == 2),]
  }
  DOI <- aggregate(data_removed_NA_with_sex[c("number_of_cases", "person_years_at_risk")], 
                   by = list(registry_code = data_removed_NA_with_sex$registry_code,
                             age_group = data_removed_NA_with_sex$age_group), FUN = sum)
  
  #----Calculate the incidence rate----
  DOI["incidence_rate_100000"] <- DOI$number_of_cases/DOI$person_years_at_risk*100000
  DOI <- merge(DOI, region_list, by = "registry_code")
  
  #----Save to processed data----
  write.csv(DOI, paste0("./data/CI5-XId/processed/",sex,"_sex_group.csv"), row.names = F)
}