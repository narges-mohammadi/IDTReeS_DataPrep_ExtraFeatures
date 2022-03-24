#  This script computes crownMetrics for las files of each tree 
library(here)
library(lidR)
library(lmom)
library(rLiDAR)

#-----------------calculate tree metrics for all the trees--------------------#

train_data <- read.csv(here("labdata_competition_data",
                            "IDTREES_competition_train", 
                            "Field", "train_data.csv"))


list_path_las_trees <- list.files(path = here("output","las"),
                                  pattern = "*.las",
                                  full.names = T)

list_pure_trees_indvID <- pbapply::pblapply(list.files(path = here("output","las"),
                                                       pattern = "*.las"), 
                                            function (y)sub(".las", y, replacement = "")
)

# Map individualID of trees to a list of numbers (used for "CrownMetrics" function)
df_indvID_Id <- data.frame(indvID = matrix(unlist(list_pure_trees_indvID), 
                                           nrow=1117, byrow=TRUE),
                           stringsAsFactors=FALSE)

treeId <- sort(unique(df_indvID_Id$indvID))
Id <- 1:length(treeId)
names(Id) <- treeId
df_indvID_Id$Id <- Id[df_indvID_Id$indvID]

# path : the path of each las file corresponding to each tree
# dataframe : A 2-column dataframe containing individualID("indvID"(str)) of trees and "Ids"(numeric) 

crownMetricTrees <- function(path, dataframe){
  
  # indivId of each tree based on path
  indvID <- sub(".las", basename(path), replacement = "")
  
  # Reading LAS file
  LAS <- readLAS(path,short=TRUE)
  
  # Setting the xyz coordinates and subsetting the data
  xyzi <- subset(LAS[,1:4],LAS[,3] >= 1.37)
  
  # Set the tree id vector
  Id <- dataframe[which(dataframe$indvID == indvID), "Id"]
  #Id <- as.factor("MLBS00083")# because tree segmentation has been done by data providers
  
  # Combining xyzi and tree id 
  xyziId<-cbind(xyzi,Id)
  
  ###############################################
  #  Computing individual tree LiDAR metrics 
  ###############################################
  
  TreesMetrics<-CrownMetrics(xyziId)
  
  TreesMetrics$indvID <- indvID
  
  specific_df <- train_data[which(train_data$indvdID == indvID),]
  
  TreesMetrics$taxonID <- specific_df$taxonID
  
  return(TreesMetrics)
  
}

crownMetricTrees_error_handler <- function (path, dataframe) {
  return(tryCatch(crownMetricTrees(path, dataframe), error=function(e) NULL))
}



list_crownMetrics_trees <- pbapply::pblapply(list_path_las_trees, 
                                             function(y)crownMetricTrees_error_handler(y, df_indvID_Id))


df_crownMetrics_all_trees <- do.call("rbind",list_crownMetrics_trees)


# Coerce the data.frame to all-character (to be able to write as csv)
df_crownMetrics_all_trees_new  <-  data.frame(lapply(df_crownMetrics_all_trees, 
                                                     as.character), 
                                              stringsAsFactors=FALSE)

write.csv(df_crownMetrics_all_trees_new,
          here("output", 
               "allTrees_crownMetric.csv"), 
          row.names = FALSE)
