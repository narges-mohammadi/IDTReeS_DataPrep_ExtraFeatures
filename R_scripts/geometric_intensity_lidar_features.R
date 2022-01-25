#  Incorporating greater data volume and features 
#  (such as additional lidar point cloud metrics 
#  based on point height and intensity)


#  Attention: Out of the 1117 .las files for each tree, 
#             the csv file has 1,092 rows (in warning messages(No tree found. NULL returned.))


library(here)
library(lidR)

# path of a sample las file and reading it
las_tree_path <- here("output", "las", "MLBS00075.las")

# custom function to derive tree-based metrics
custom_tree_metrics <- function(z, i) { # user-defined function
  metrics <- list(
    z_max = max(z),     # max height
    z_sd = sd(z),       # vertical variability of points
    i_mean = mean(i), # mean intensity
    i_max  = max(i)   # max intensity
  )
  return(metrics) # output
}

las_to_csv_individual_tree_metrics <- function(las_tree_path){
  
  las <- readLAS(las_tree_path)

  las <- normalize_height(las, tin())

  # extract tree metrics
  # individual_tree_segmentation <- segment_trees(las,
  #                                             li2012(R = 3, speed_up = 5))


  # use the "indvdID" that appears in the name of each tree las file as "treeID"
  las@data$treeID <- gsub(".las", "", basename(las_tree_path))

  #individual_tree_metrics <- tree_metrics(individual_tree_segmentation)


  crowns_metrics <- delineate_crowns(las, #individual_tree_segmentation, 
                                   func = ~custom_tree_metrics(z = Z, 
                                                               i = Intensity)) # delineate crowns

  #crowns_metrics@data$indvdID <- gsub(".las", "", basename(las_tree_path))

  return(crowns_metrics@data)

}

tryCatchFunction <- function (las_tree_path) {
  return(tryCatch(las_to_csv_individual_tree_metrics(las_tree_path), error=function(e) NULL))
}

# create a list of all the paths of individual tree las files
list_path_las_indv_tree <- as.list(list.files(here("output", "las"),
                                              pattern = "*.las", 
                                              full.names = TRUE))


list_csv_individual_tree_metrics <- lapply(list_path_las_indv_tree,
                                   tryCatchFunction)

# bind all the elements of list rowwise 
result_rowwise_csv_indv_trees_metrics <- dplyr::bind_rows(list_csv_individual_tree_metrics)

write.csv(result_rowwise_csv_indv_trees_metrics,
          here("output","geometric_intensity_metrics_indvdID.csv"), 
          row.names = FALSE)
