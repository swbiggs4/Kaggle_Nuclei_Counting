# Kaggle_Nuclei 

# Paul Smyth

# 30/01/2018 v1: First steps, examine intensities




# Load Libraries ----------------------------------------------------------


library(png)
library(dplyr)
library(ggplot2)



# Load training data without masks ----------------------------------------


# List all training .png files 
li.files<- list.files(path = "1_Data/stage1_train" ,pattern = ".png", recursive = TRUE)

# Make a dataframe that does not contain the masks 
df.trainingData_meta<- data.frame(Path = li.files) %>% filter(!grepl("masks",Path)) 
df.trainingData_meta$Path<- 
# Add full path
df.trainingData_meta$LoadPath<- paste0("1_Data/stage1_train/",df.trainingData_meta$Path)

# Load all the pngs - 3.1GB
li.images<- lapply(df.trainingData_meta$LoadPath,readPNG)

# Convert to char
df.trainingData_meta$Path<- as.character(df.trainingData_meta$Path)

# ID only
df.trainingData_meta$ID<- unlist(strsplit(df.trainingData_meta$Path,".png"))



# Explore -----------------------------------------------------------------


# Get various information
df.trainingData_meta$Dims<- rapply(li.images,dim, how = "list") # dimensions - some may be RGBA 
df.trainingData_meta$Intensity<- sapply(li.images,sum) # sum for intensity

# Convert to char
df.trainingData_meta$Dims<- as.character(df.trainingData_meta$Dims)

# Look at a few examples - overplots
grid::grid.raster(li.images[[1]]) # dark
grid::grid.raster(li.images[[3]]) # light


# Histogram of intensities

ggplot(data=df.trainingData_meta, aes(x=Intensity)) + geom_histogram() +theme_bw()

# Get bins
bins<- hist(df.trainingData_meta$Intensity)$breaks

# Bin
df.trainingData_meta$IntensityBin <- cut(df.trainingData_meta$Intensity, breaks = bins, labels = seq(1,length(bins)-1))

table(df.trainingData_meta$IntensityBin)

grid::grid.raster(li.images[[535]]) # zoom?
grid::grid.raster(li.images[[24]]) # clearly different


# Repeat for test data ----------------------------------------------------

# List all test .png files 
li.files_test<- list.files(path = "1_Data/stage1_test/" ,pattern = ".png", recursive = TRUE)

# Make a dataframe that does not contain the masks 
df.testData_meta<- data.frame(Path = li.files_test) %>% filter(!grepl("masks",Path)) 

# Add full path
df.testData_meta$LoadPath<- paste0("1_Data/stage1_test/",df.testData_meta$Path)

# Load all the pngs 
li.images_test<- lapply(df.testData_meta$LoadPath,readPNG)

# Get various information
df.testData_meta$Dims<- rapply(li.images_test,dim, how = "list") # dimensions - some may be RGBA 
df.testData_meta$Intensity<- sapply(li.images_test,sum) # sum for intensity

# Look at a few examples - overplots
grid::grid.raster(li.images_test[[1]]) # dark
grid::grid.raster(li.images_test[[6]]) # light and zoomed?


# Histogram of intensities

ggplot(data=df.testData_meta, aes(x=Intensity)) + geom_histogram() +theme_bw()

# Get bins
bins_test<- hist(df.testData_meta$Intensity)$breaks

# Bin data on intensity category from training - only entries in first two bins
df.testData_meta$IntensityBin_Test <- cut(df.testData_meta$Intensity, breaks = bins_test, labels = seq(1,length(bins_test)-1))

table(df.testData_meta$IntensityBin_Test) # train multiple models based on intensities - remove large intensities from training data?

# Look at a few examples - overplots
grid::grid.raster(li.images_test[[1]]) #1 dark, zoomed
grid::grid.raster(li.images_test[[21]]) #2 light
grid::grid.raster(li.images_test[[7]]) #4 dark, zoomed, blurry
grid::grid.raster(li.images_test[[13]]) #5 light, coloured, white background
grid::grid.raster(li.images_test[[25]]) #7 light, coloured, purple background
grid::grid.raster(li.images_test[[8]]) #8 unique, zoomed - issue to remove?
grid::grid.raster(li.images_test[[63]]) #9 zoomed - part of following, or cut?
grid::grid.raster(li.images_test[[23]])
grid::grid.raster(li.images_test[[45]])
grid::grid.raster(li.images_test[[57]])


# Bin data on intensity category from training - only entries in first two bins
df.testData_meta$IntensityBin_Training <- cut(df.testData_meta$Intensity, breaks = bins, labels = seq(1,length(bins)-1))

table(df.testData_meta$IntensityBin_Training) # train multiple models based on intensities - remove large intensities from training data?

# rebin training data using test bins?

# Look at a few examples - overplots
grid::grid.raster(li.images_test[[1]]) # dark
grid::grid.raster(li.images_test[[6]]) # light


# Save dataframe ----------------------------------------------------------




write.csv(df.trainingData_meta,"1_Data/Training_MetaData.csv")

