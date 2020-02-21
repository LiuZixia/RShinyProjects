library(dplyr)

#Read in data
region_list <-
  read.csv(file = "./data/CI5-XId/region_info_with_distance.csv",
           header = T)

for(sex in c("all","male", "female")){
  data_raw <-
    merge(
      read.csv(file = paste0("./data/CI5-XId/processed/",sex,"_sex_group.csv"),
               header = T),
      region_list[, c("registry_code", "distance")],
      by = "registry_code"
    )
  
  #Global datasets
  Global_data_with_age <-
    data_raw
  write.csv(Global_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/Global_data_with_age.csv"))
  #Asia datasets
  Asia_data_with_age <-
    data_raw[data_raw$registry_code %in% grep("^4", data_raw$registry_code, value =
                                                T), ]
  write.csv(Asia_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/Asia_data_with_age.csv"))
  #North America datasets
  NA_data_with_age <-
    data_raw[data_raw$registry_code %in% grep("^3", data_raw$registry_code, value =
                                                T), ]
  write.csv(NA_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/NorthAmerica_data_with_age.csv"))
  #US datasets
  US_data_with_age <-
    data_raw[data_raw$registry_code %in% grep("^3840", data_raw$registry_code, value =
                                                T), ]
  write.csv(NA_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/US_data_with_age.csv"))
  #China datasets
  CN_data_with_age <-
    data_raw[data_raw$registry_code %in% grep("^4156", data_raw$registry_code, value =
                                                T), ]
  write.csv(NA_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/China_data_with_age.csv"))
  #EU datasets
  EU_data_with_age <-
    data_raw[data_raw$registry_code %in% grep("^5", data_raw$registry_code, value =
                                                T), ]
  write.csv(EU_data_with_age, file = paste0("./data/CI5-XId/processed/subset/",sex,"/Europe_data_with_age.csv"))
  
  #Seprate by age group
  age_group <-
    data.frame(threshold_youngest = c(0, 4, 9, 17),
               threshold_oldest = c(3, 8, 16, 20))
  for (g in c("Global", "Asia", "NorthAmerica", "Europe", "US", "China")) {
    if (g == "Global") {
      DOI_raw <- Global_data_with_age
    } else if (g == "Asia") {
      DOI_raw <- Asia_data_with_age
    } else if (g == "NorthAmerica") {
      DOI_raw <- NA_data_with_age
    } else if (g == "Europe") {
      DOI_raw <- EU_data_with_age
    } else if (g == "USA") {
      DOI_raw <- US_data_with_age
    } else if (g == "China") {
      DOI_raw <- CN_data_with_age
    }
    for (i in 1:4) {
      y <- age_group$threshold_youngest[i]
      o <- age_group$threshold_oldest[i]
      DOI <-
        DOI_raw[which(DOI_raw$age_group >= y &
                        DOI_raw$age_group < o), ]
      DOI <-
        aggregate(DOI[c("number_of_cases", "person_years_at_risk")],
                  by = list(registry_code = DOI$registry_code),
                  FUN = sum)
      DOI["incidence_rate_100000"] <-
        DOI$number_of_cases / DOI$person_years_at_risk *
        100000
      DOI <-
        merge(DOI, region_list, by = "registry_code")[, c(
          "incidence_rate_100000",
          "discription",
          "registry_code",
          "latitude",
          "longitude",
          "distance"
        )]
      write.csv(
        DOI,
        file = paste0(
          "./data/CI5-XId/processed/subset/",
          sex,
          "/",g,"_age_group_",
          as.character(y * 5),
          "-",
          as.character(o * 5 + 4),
          ".csv"
        )
      )
    }
  }
}