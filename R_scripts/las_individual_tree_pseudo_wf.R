

# Explanation: Code converts the las files of individual trees to csv
        #    BUT out of the total(1117) files, the csv has the information of 
        #    1071 trees (i.e. the "idtrees_pwave_indvdID.csv" has 1071 rows)
        #    In the function called "las_to_csv_individual_tree", 
        #    for normalizing, "normalize_height(las, tin())" is used.
        #    It could be the reason for the Error "Error: Internal error in C_interpolate_delaunay:
        #    cannot triangulate less than 3 points." 
        #    With "tryCatch" technique, the loop has been done for the rest of the files

# Got help from this link: https://gis.stackexchange.com/questions/310758/visualizing-vertical-profiles-from-lidar-point-clouds-in-r

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
# https://gis.stackexchange.com/questions/386986/overlapping-las-tiles-and-edge-artifacts
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
# https://gis.stackexchange.com/questions/386986/overlapping-las-tiles-and-edge-artifacts
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





# Part II: " pseudo-waveform"
        #   create csv files from each individual las file 
        #   using cubic spline(pseudo-waveform) with density and bin_number
        #   & save on drive as csv called "idtrees_pwave_indvdID.csv"

#----------------------------------------------------------------------
# each .las file belongs to an individual trees (created in the previous lines of code)
LASfile_path <- here("output", "las", "MLBS0017A.las")


las_to_csv_individual_tree <- function(LASfile_path){
  
  las <- readLAS(LASfile_path)

  
  # create empty dataframe with specific column names
  df <- data.frame(Z = NA)
  
  
  # make a raster that encompass the point cloud
  #layout = raster(extent(las))
  #res(layout) = 5
  
  # Force to interpolate in these pixels
  #dtm = grid_terrain(las, res = layout, algorithm = kriging(k = 10))
  
  #dtm <- grid_terrain(las, 5, knnidw())
  las <- normalize_height(las, tin())#(las, dtm, method = "bilinear")# #normalize (changes elevations to heights)
  
  Z <- data.frame(Z = las@data$Z)
  las_frame <-  Z
  df <- las_frame 
    
    
  # Valid points lying between the 1st and 99th percentile of all height values 
  # within point cloud
    
  # Checking only for column a. Top 1% and bottom 1% is removed 
  df <- df %>% dplyr::filter(dplyr::between(Z, quantile(Z, .01), quantile(Z, .99)))
    
  # Checking for column a & b. Top 1% and bottom 1% is removed
  #df %>% dplyr::filter_all(all_vars(between(., quantile(., .01), quantile(., .99))))
    
    
  #-------------------------------------
  # smooth cubic splines
  # hh <- gamlss::histSmoC(df$Z, df=15, breaks=15, 
  #                        lower=0, upper =40, 
  #                        plot=TRUE)#, discrete=TRUE
    
  # saving results
  m1 <- gamlss::histSmoC(df$Z, lower=min(df$Z), upper = max(df$Z), 
                          df=5, breaks=max(df$Z)-min(df$Z))#, df=15
    
    # m2<- xts::lines(smooth.spline(dt, y, spar = 0.35), 
    #                 col = 'red', type = 'l'))
    
  # create the "bin names" to be added to the dataframe in the next lines
  bins <- seq(1, 39, by=1)
  col_bin_names <- paste("bin_", bins,  sep="")
    
  # export the histogram into csv file
  out  <- data.frame(x = m1$x, counts = m1$counts, density = m1$density)
  out_paper <- data.frame(x = c(0:40), counts = c(0:40), density = c(0:40))
  test <- as.data.frame(table(cut(c(0:39), b = 39)))
  test_2 <- merge(out, test, by = 0, all = TRUE)[-1]
    
  # rearrange(ascending order) the dataframe based on the x column
  test_2_ordered_ascending <- test_2 %>%
      dplyr::group_by(x) %>%
      dplyr::arrange(x)
    
  test_2_ordered_ascending$counts <- ifelse(is.na(test_2_ordered_ascending$x), 
                                              0, test_2_ordered_ascending$counts)
  test_2_ordered_ascending$density <- ifelse(is.na(test_2_ordered_ascending$x),
                                               0, test_2_ordered_ascending$density)
    
  # add the bin names
  test_2_ordered_ascending$bin_name <- col_bin_names
    
  # remove the "Freq" column from dataframe
  test_2_ordered_ascending <- subset(test_2_ordered_ascending, 
                                       select = -c(Freq,Var1,x) )
    
  test_2_ordered_ascending <- as.data.frame(test_2_ordered_ascending)
    
  # We create a new dataframe using the transpose of a dataframe
  test_2_transpose <- as.data.frame(t(as.matrix(test_2_ordered_ascending)))
  colnames(test_2_transpose) <- test_2_ordered_ascending$bin_name
  test_2_transpose$numpts <- sum(test_2_ordered_ascending$counts)
  test_2_transpose$indvdID <- gsub("*.las", "", basename(LASfile_path))
    
  # remove the unnecessary rows from dataframe
  test_2_transpose <- test_2_transpose[-c(1, 3), ] 
    
  return(test_2_transpose)
    

}

testFunction <- function (LASfile_path) {
  return(tryCatch(las_to_csv_individual_tree(LASfile_path), error=function(e) NULL))
}

# check if the function works for one ".las" file 
csv_from_las_indv_tree <- testFunction(LASfile_path) #las_to_csv_individual_tree(LASfile_path)

# create a list of all the paths of individual tree las files
list_path_las_indv_tree <- as.list(list.files(here("output", "las"),
                                      pattern = "*.las", 
                                      full.names = TRUE))


list_csv_individual_tree <- lapply(list_path_las_indv_tree,
                                   testFunction)

# bind all the elements of list rowwise 
list_result_rowwise_csv_indv_trees <- dplyr::bind_rows(list_csv_individual_tree)

write.csv(list_result_rowwise_csv_indv_trees,
          here("output","idtrees_pwave_indvdID.csv"), 
          row.names = FALSE)










## Part Unnecessary (Weibull densities)
#-----------------------------------------------------------------

# each .las file belongs to an individual trees (created in the previous lines of code)
LASfile <- here("output", "las", "MLBS00138A.las")

las <- readLAS(LASfile)


# create empty dataframe with specific column names
df <- data.frame(PointSourceID = NA,
                 Z = NA)

las <- normalize_height(las, tin()) #normalize (changes elevations to heights)
Z <- data.frame(Z = las@data$Z)
PointSourceID <- data.frame(PointSourceID = las@data$PointSourceID)
las_frame <- cbind(PointSourceID, Z)
df <- las_frame 

data_las <- las@data

# basic scatterplot
# ggplot(df, aes(x=data_las$X, y=Z)) + 
#   geom_point()


# Checking only for column a. Top 1% and bottom 1% is removed 
df<- df %>% dplyr::filter(dplyr::between(Z, quantile(Z, .01), quantile(Z, .99)))

# Checking for column a & b. Top 1% and bottom 1% is removed
df %>% dplyr::filter_all(all_vars(between(., quantile(., .01), quantile(., .99))))



#-------------------------------------
#Exploratory analysis of heights distribution.


min(df$Z)   #-0.661
max(df$Z)   #32.017

ggplot(df, aes(x = Z)) +
  geom_vline(xintercept = 1, color = 'red', linetype = 2) +
  geom_histogram(breaks = seq(0, 40, 2),
                 binwidth = 2,
                 aes(y = ..count..),
                 colour = "black",
                 fill = "white") +
  scale_y_continuous("Count of returns", limits = c(0, 40)) + 
  scale_x_continuous("Height (m)", limits = c(0, 40)) +
  #facet_wrap(~PointSourceID, ncol = 2, nrow = 1) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))


#-------------------------------------
#Get vertical profiles based on Weibull pdfs.

# Calculate Weibull densities to Z values.

shape <- 5.299524
scale <- 16.17665
weib_dens <- dweibull(x = df$Z, shape = shape, scale = scale)

ggplot(df, aes(x = Z)) +
  geom_histogram(breaks = seq(0,40, 1),
                 binwidth = 4,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  scale_y_continuous("Density") + 
  scale_x_continuous("Height (m)", limits = c(0,40)) +
  geom_line(data = df,
            aes(x = Z, y = weib_dens),
            colour = "red",
            size = 0.4) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  coord_flip()


#-------------------------------------

## spline

X <- df$Z#c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(X, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults
lines(density(X, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


