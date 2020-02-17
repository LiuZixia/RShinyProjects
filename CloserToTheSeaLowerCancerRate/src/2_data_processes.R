#Read in converted data and registry info
data_raw <- read.csv(file = "./data/CI5-XId/converted/CI5_XId_converted.csv", 
                     header =T)
region_list <- read.csv(file = "./data/CI5-XId/converted/registry_info.csv", 
                        header =T)[,c(1,5,6)]

#Remove data with insufficient population
data_removed_NA <- data_raw[which(data_raw$person_years_at_risk > 10000),]

#Combine data based on groups
#Currently we don't concern about the gender so we combine the two sex group
data_all_sex <- aggregate(data_removed_NA[c("number_of_cases", "person_years_at_risk")], 
                          by = list(registry_code = data_removed_NA$registry_code,
                                    age_group = data_removed_NA$age_group), FUN = sum)

#Calculate the incidence rate
data_all_sex["incidence_rate_100000"] <- data_all_sex$number_of_cases/data_all_sex$person_years_at_risk*100000
data_all_sex <- merge(data_all_sex, region_list, by = "registry_code")

#Save to processed data
write.csv(data_all_sex, "./data/CI5-XId/processed/all_sex_group.csv", row.names = F)