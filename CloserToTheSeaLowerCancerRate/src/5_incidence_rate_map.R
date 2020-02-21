#----Load Packages----
library(rgdal)
library(sf)
library(stars)
library(tmap)
library(tmaptools)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
#----Settings----
#Set the sex group, "all" "male" "female"
sex <- "male"
#Set the age group, "0-19" "20-44" "45-84" "85-104"
age_group <- "45-84"
#Set the dataset, "Global" "Asia" "Europe" "NorthAmerica" "China" "US"
dataset <- "NorthAmerica"

#----Prepare Maps----
data("World")
if (dataset == "Global") {
  DOI_map <-
    as(st_transform(World, "+proj=longlat +datum=WGS84"),
       "Spatial")
} else if (dataset == "Europe") {
  DOI_map <-
    as(st_transform(World[which(World$continent=="Europe"&World$name!="Russia"),], "+proj=longlat +datum=WGS84"),
       "Spatial")
} else if (dataset == "NorthAmerica") {
  DOI_map <-
    as(st_transform(World[which(World$continent=="North America"),], "+proj=longlat +datum=WGS84"),
       "Spatial")
} else if (dataset == "Asia") {
  DOI_map <-
    as(st_transform(World[which(World$continent=="Asia"),], "+proj=longlat +datum=WGS84"),
       "Spatial")
} else if (dataset == "China") {
  DOI_map <-
    as(st_transform(World[which(World$name=="China"),], "+proj=longlat +datum=WGS84"),
       "Spatial")
} else if (dataset == "US") {
  DOI_map <-
    as(st_transform(World[which(World$name=="United States"),], "+proj=longlat +datum=WGS84"),
       "Spatial")
}


#Read incidence rate data
incidence_data <- read.csv(
  file = paste0(
    "./data/CI5-XId/processed/subset/",
    sex,
    "/",dataset,"_age_group_",
    age_group,
    ".csv"
  ),
  header = T
)[, c("latitude", "longitude", "incidence_rate_100000", "distance")]
incidence_data$log_distance <- log(incidence_data$distance[])
incidence_data_sf <-
  st_as_sf(incidence_data,
           coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84")

#----Dot Plot----
tm_shape(DOI_map) + tm_polygons() +
  tm_shape(incidence_data_sf) +
  tm_dots(
    col = "incidence_rate_100000",
    midpoint = TRUE,
    title = paste0("Lung Cancer \nIncidence Rate\n", dataset , "\nAge", age_group),
    size = 0.1
  ) +
  tm_legend(legend.outside = TRUE)

#----Fit the variogram model----
# Define the 1st order polynomial equation
f.1 <- as.formula(incidence_rate_100000 ~ X + Y)

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the
# variogram on the de-trended data.
inc <- as(incidence_data_sf, "Spatial")
inc$X = coordinates(inc)[, 1]
inc$Y = coordinates(inc)[, 2]
var.smpl <- variogram(f.1, data = inc, cloud = FALSE)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <-
  fit.variogram(var.smpl,
                fit.ranges = FALSE,
                fit.sills = FALSE,
                vgm(model = "Sph", nugget = 0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit)

#----Generate Kriged surface----
# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
grd <- makegrid(DOI_map, n = 100000)
e = as.matrix(extent(DOI_map))
grd = grd[which(grd$x1 > e['x', 'min']),]
grd = grd[which(grd$x1 < e['x', 'max']),]
grd = grd[which(grd$x2 > e['y', 'min']),]
grd = grd[which(grd$x2 < e['y', 'max']),] #preventing overflow
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
proj4string(grd) <- proj4string(inc)
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

dat.krg <- krige(f.1,
                 location = inc,
                 newdata = grd,
                 model = dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, DOI_map)
# Plot the map

tm_shape(r.m) +
  tm_raster(
    n = 10,
    palette = "Reds",
    title = paste0(
      "Predicted Lung Cancer \nIncidence Rate\n",
      dataset ,
      "\nAge",
      age_group,
      "/nCases Per 100000"
    )
  ) +
  tm_shape(DOI_map) + tm_borders() +
  tm_legend(legend.outside = TRUE)
