

# Explanation: Code converts the las files of individual trees to csv
        #    BUT out of the total(1117) files, the csv has the information of 
        #    1071 trees (i.e. the "idtrees_pwave_indvdID.csv" has 1071 rows)
        #    In the function called "las_to_csv_individual_tree", 
        #    for normalizing, "normalize_height(las, tin())" is used.
        #    It could be the reason for the Error "Error: Internal error in C_interpolate_delaunay:
        #    cannot triangulate less than 3 points." 
        #    With "tryCatch" technique, the loop has been done for the rest of the files


# Valid lidar points within "each tree crown geometry" were used to create a
# pseudo-waveform for the tree crown which simulates the entire crown's footprint


# libraries
library(here)
library(lidR)
library(ggplot2)
library(dplyr)
library(stats)
library(gamlss)
library(MASS)
library(splines)
library(ISLR)

# Part I: "individual tree"
      #  create .las files for each individual tree and save on drive
      #  results in "output" folder of the project directory
#----------------------------------------------------------------------
# MLBS 
# (only 674 from the 732 individual trees in MLBS plot) 
# The difference might has sth to do with "readLAScatalog" function

# using "raster" library to read the shapefile
shp <- raster::shapefile(here("labdata_competition_data", "IDTREES_competition_train", 
                              "ITC", "train_MLBS.shp"))

folder_name <- list.files(here("labdata_competition_data", "IDTREES_competition_train", 
                               "RemoteSensing", "LAS"), 
                          pattern = "MLBS_*", full.names = TRUE)

path_names_vector <- as.vector(folder_name)

# read all the las files as lasCatalog
ctg <- readLAScatalog(
        path_names_vector,
        progress = TRUE,
        select = "*",
        filter = "",
        chunk_size = 0,
        chunk_buffer = 30
)


# based on this SOF post:
opt_independent_files(ctg) <- TRUE
opt_merge(ctg) = FALSE

plots <- clip_roi(ctg, shp)


# define the paths for the individual tree las files
output_file_paths <- here("output", "las", paste0(shp@data$indvdID, ".las"))
output_file_paths <- as.list(output_file_paths)

# write the las file for each individual tree to drive
apply_mlbs <- mapply(lidR::writeLAS, plots, file = output_file_paths)

# OSBS 
# (all the trees are 483 but for 443 of them .las files were created)
# This might give you a clue on the reason: "Be careful, some tiles seem to overlap each other. 
# lidR may return incorrect outputs with edge artifacts when processing this catalog."

# using "raster" library to read the shapefile
shp_osbs <- raster::shapefile(here("labdata_competition_data", "IDTREES_competition_train", 
                              "ITC", "train_OSBS.shp"))

folder_name_osbs <- list.files(here("labdata_competition_data", "IDTREES_competition_train", 
                               "RemoteSensing", "LAS"), 
                          pattern = "OSBS_*", full.names = TRUE)

path_names_vector_osbs <- as.vector(folder_name_osbs)

# read all the las files as lasCatalog
ctg_osbs <- readLAScatalog(
  path_names_vector_osbs,
  progress = TRUE,
  select = "*",
  filter = "",
  chunk_size = 0,
  chunk_buffer = 30
)

#plot(ctg_osbs)
# based on this SOF post:
opt_independent_files(ctg_osbs) <- TRUE
opt_merge(ctg_osbs) = FALSE

plots_osbs <- clip_roi(ctg_osbs, shp_osbs)


# define the paths for the individual tree las files
output_file_paths <- here("output", "las", paste0(shp_osbs@data$indvdID, ".las"))
output_file_paths <- as.list(output_file_paths)

# modified_writeLAS <- function(las, file, index = FALSE){
#   if (!is.empty(las)) {
#     # execute the original writeLAS function
#     writeLAS(las, file, index=FALSE)
#   } else {
#     train_sf <- list()
#   }
#   
# }

modified_writeLAS <- function (las, file, index = FALSE) {
  return(tryCatch(writeLAS(las, file, index=FALSE), error=function(e) NULL))
}

# write the .las file for each individual tree to drive
apply_osbs <- mapply(modified_writeLAS, plots_osbs, 
                     file = output_file_paths)#writeLAS


