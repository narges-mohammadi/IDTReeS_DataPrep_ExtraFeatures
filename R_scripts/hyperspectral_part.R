# analyse hyperspectral data using GDAL
# "neonhs" package to extract hyperspectral reflectance data 
# at the center of each tree crown polygon

library(here)
library(rgdal)
library(neonhs)
library(sf)
# to do: Find a way to remove "bad bands" with high noise due to water absorption 

# Extract hyperspectral reflectance data at the center of each tree crown polygon 
# (for each individual tree)
# Number of individual trees in train data: 732(MLBS) + 483(OSBS) = 1215
# 369 reflectance values from the hyperspectral data (one per wavelength)

# function to extract data at each individual tree from raster layers in HSI folder
tt <- function(path_to_file, focal_site){
  raster_hsi <- raster::brick(path_to_file)


  # create the "band names" to be added to the dataframe in the next lines
  bands <- seq(1, 369, by=1)
  band_names <- paste("band_", bands,  sep="")

  names(raster_hsi) <- band_names
  
  
  if (focal_site == "MLBS") {
    # Read in one plot(20m by 20m)
    train_sf <- sf::st_read(here("labdata_competition_data", "IDTREES_competition_train", 
                                 "ITC", "train_MLBS.shp"))
  } else if(focal_site == "OSBS") {
    train_sf <- sf::st_read(here("labdata_competition_data", "IDTREES_competition_train", 
                                 "ITC", "train_OSBS.shp"))
  } else {
    train_sf <- list()
  }


  sf_geometry <- sf::st_geometry(train_sf)


  cntrd <- sf::st_centroid(sf_geometry)
  
  
  centroid_coords <- st_coordinates(cntrd)

  centroid_spdf <- SpatialPointsDataFrame(
      coords = centroid_coords, 
      data = data.frame(id = 1:length(cntrd), indvdID = train_sf$indvdID),
      proj4string=raster_hsi@crs)

  rasValue <- raster::extract(raster_hsi, centroid_spdf, 
                             df = TRUE) # return a dataframe?

  rasValue$indvdID <- train_sf$indvdID

  rasValue <- na.omit(rasValue)
  
  return(rasValue)
}



#MLBS
list_hs_MLBS_files <- list.files(here("labdata_competition_data","IDTREES_competition_train", 
                                      "RemoteSensing", "HSI"), pattern = "MLBS_*.*", full.names = TRUE)

focal_site <- "MLBS"

list_result_mlbs <- lapply(list_hs_MLBS_files, tt, focal_site = focal_site)

# bind all the elements of list rowwise 
list_result_rowwise_mlbs <- dplyr::bind_rows(list_result_mlbs)


# OSBS
list_hs_OSBS_files <- list.files(here("labdata_competition_data","IDTREES_competition_train", 
                                      "RemoteSensing", "HSI"), pattern = "OSBS_*.*", full.names = TRUE)

focal_site <- "OSBS"

list_result_osbs <- lapply(list_hs_OSBS_files, tt, focal_site = focal_site)

# bind all the elements of list rowwise 
list_result_rowwise_osbs <- dplyr::bind_rows(list_result_osbs)

# Merge dataframes from "MLBS" and "OSBS" plots
two_plots_dataframes <- rbind(list_result_rowwise_mlbs, 
                              list_result_rowwise_osbs)
# Save the dataframe to csv
write.csv(two_plots_dataframes,here("output","hyperspectral_train.csv"), 
          row.names = FALSE)




# ------------------------------------------------------
# the scenario for just one tif file 
path_to_file <- here("labdata_competition_data","IDTREES_competition_train", 
                     "RemoteSensing", "HSI", "MLBS_16.tif")
raster_hsi <- raster::brick(path_to_file)


# create the "band names" to be added to the dataframe in the next lines
bands <- seq(1, 369, by=1)
band_names <- paste("band_", bands,  sep="")

names(raster_hsi) <- band_names


# center of each tree crown polygon that hs data needs to be extractd at
train_sf <- sf::st_read(here("labdata_competition_data", "IDTREES_competition_train", 
                             "ITC", "train_MLBS.shp"))

sf_geometry <- sf::st_geometry(train_sf)


plot(sf_geometry, border = 'grey')
cntrd <- sf::st_centroid(sf_geometry)
plot(cntrd, col = 'red', add = TRUE, cex = .5)


centroid_coords <- st_coordinates(cntrd)

centroid_spdf <- SpatialPointsDataFrame(
  coords = centroid_coords, 
  data = data.frame(id = 1:732, indvdID = train_sf$indvdID),
  proj4string=raster_hsi@crs)

rasValue <- raster::extract(raster_hsi, centroid_spdf, 
                            df = TRUE) # return a dataframe?

rasValue$indvdID <- train_sf$indvdID

rasValue <- na.omit(rasValue)


cent_max <- raster::extract(raster_hsi,             # raster layer
                            centroid_spdf,   # SPDF with centroids for buffer
                            buffer = 20,     # buffer size, units depend on CRS
                            fun=max,         # what to value to extract
                            df=TRUE)         # return a dataframe?
