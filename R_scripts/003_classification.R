library(caret)
library(here)


# Load From CSV (output of previous step)
allTrees_crownMetrics <- read.csv(here("output",
                                       "allTrees_crownMetric.csv"))

##################################################################
#--------------------Data (intensity metrics)--------------------#
##################################################################
# selection is based on table in the review ("https://doi.org/10.3390/rs13030353")
features <- subset(allTrees_crownMetrics, 
                   select = c(IMIN, IMAX, IMEAN, 
                              ISD, ICV, IMEDIAN, 
                              ISKE, IKUR, 
                              I10TH, I20TH, I30TH,       
                              I40TH, I50TH, I60TH,
                              I70TH, I80TH, I90TH,       
                              taxonID))

# Create a Validation Dataset
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(features$taxonID, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- features[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- features[validation_index,]

# 3. Summarize Dataset
# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)
dataset$taxonID <- as.factor(dataset$taxonID)  
dataset$IMIN <- as.numeric(dataset$IMIN)
dataset$IMODE <- as.numeric(dataset$IMODE)

# take a peek at the first 5 rows of the data
head(dataset)

# Levels of the Class
levels(dataset$taxonID)


# Class Distribution
# summarize the class distribution
percentage <- prop.table(table(dataset$taxonID)) * 100
percentag_table <- cbind(freq=table(dataset$taxonID), percentage=percentage)
write.csv(percentag_table,
          file=here("output", "taxon_percentage.csv"),
          row.names=TRUE) 


# Statistical Summary

# summarize attribute distributions
summary(dataset)

# Visualize Dataset
# split input and output
x <- dataset[,1:17]
y <- dataset[,18]

# # boxplot for the first 5 attributes on one tree
par(mfrow=c(1,5))
for(i in 1:5) {
  boxplot(x[,i], main=names(dataset)[i])
}


# barplot for class breakdown
plot(y)

#make "taxonID" into a factor with the levels in the desired order
cnt <- plyr::count(dataset$taxonID)
dataset$taxonID <- factor(dataset$taxonID, 
                          levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

p <- ggplot2::ggplot(data = dataset, aes(fill = taxonID)) +
  geom_bar(aes(x = taxonID))+
  coord_flip()+
  ggtitle("Number of samples in each class", 
          #subtitle = "Original dataset"
  )+
  xlab("Taxon (tree species)")+
  ylab("Frequency")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


png(here("output", "Tree_Species_Frequency.png"))
print(p)
dev.off()

#  Multivariate Plots
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
feature_box_plot<- featurePlot(x=x, y=y, plot="box")

png(here("output", "features_box_plot.png"))
print(feature_box_plot)
dev.off()


# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
feature_density_plot<- featurePlot(x=x, y=y, plot="density", scales=scales)
png(here("output", "features_density_plot.png"))
print(feature_density_plot)
dev.off()

#################################################################
#----------------Evaluate Some Algorithms------------------#
#################################################################
#Test Harness
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#  Build Models
# a) linear algorithms
set.seed(7)
fit.lda <- train(taxonID~., data=dataset, method="lda", 
                 metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(taxonID~., data=dataset, method="rpart", 
                  metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(taxonID~., data=dataset, method="knn", 
                 metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(taxonID~., data=dataset, method="svmRadial", 
                 metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(taxonID~., data=dataset, method="rf", 
                metric=metric, trControl=control)

# Select Best Model
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.rf)


