---
title: "postHocBinning"
author: "Alex M. Chubaty & Isolde Lane-Shaw"
date: "11 May 2021"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

This module calculates predicted bird densities from the mean of the most recent bootstrapped Boreal Avian Modelling (BAM) Project national models of bird density rasters (at 250m resolution), for each of a given set of cover and age classes according to two different methods:

- 1D, where a single variable, underlying cover class, is used to bin data points of predicted bird density, by calculating the mean.
  This method is used for both forested and non-forested cover classes.
- 2D, where a gbm is used to give a predicted bird density according the combination of two variables, cover class and age.
  The predicted bird densities are then further binned according to the mean value of the desired age classes. This method is used for forested cover classes only.
  
Model statistics, such as unimodality, normality and variance are generated.

# Usage


```r
library(Require)
Require("SpaDES.core")

setPaths(modulePath = file.path(".."),
         inputPath = file.path("data"),
         outputPath = "outputs")

times <- list(start = 0, end = 1)

parameters <- list(
  postHocBinning = list()
)
modules <- list("postHocBinning")
objects <- list()
inputs <- list()
outputs <- list()

mySimOut <- simInitAndSpades(times = times, params = parameters, modules = modules, objects = objects)
```
# Parameters

Provide a summary of user-visible parameters.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|paramName        |paramClass |default    |min |max |paramDesc                                                                                                                                        |
|:----------------|:----------|:----------|:---|:---|:------------------------------------------------------------------------------------------------------------------------------------------------|
|.plots           |character  |screen     |NA  |NA  |Used by Plots function, which can be optionally used here                                                                                        |
|.plotInitialTime |numeric    |start(sim) |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                        |
|.plotInterval    |numeric    |NA         |NA  |NA  |Describes the simulation time interval between plot events.                                                                                      |
|.saveInitialTime |numeric    |NA         |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                        |
|.saveInterval    |numeric    |NA         |NA  |NA  |This describes the simulation time interval between save events.                                                                                 |
|.useCache        |logical    |FALSE      |NA  |NA  |Should caching of events or module be activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName        |objectClass              |desc                                                    |sourceURL |
|:-----------------|:------------------------|:-------------------------------------------------------|:---------|
|forestClassRaster |RasterLayer              |forest class raster (e.g., derived from LandR Biomass). |NA        |
|landscapeRaster   |RasterLayer              |Land cover class raster, default LCC2005.               |NA        |
|rasterToMatch     |RasterLayer              |raster to match. default LCC2005.                       |NA        |
|studyArea         |SpatialPolygonsDataFrame |study area polygon                                      |NA        |

## Output data


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

This module uses the outputs from [bootRasterCombine](https://github.com/ilaneshaw/bootRasterCombine), so it's expected that you have run this module, or have access to its outputs.

<!-- TODO: move everything below here into module code, e.g. defaults -->


```r
birdsList <- c("BAWW", "OVEN") #specify the bird species
folderUrlBird <- "https://drive.google.com/drive/folders/1fCTr2P-3Bh-7Qh4W0SMJ_mT9rpsKvGEA" # give file location 

birdRasterStack <- load250mBootRasters(folderUrl = folderUrlBird,
                                       birdsList = birdsList,
                                       rastersPath = downloadFolderBird,
                                       rasterToMatch = landscapeRaster,
                                       studyArea = studyArea)

noBootsDFLocation <- "https://drive.google.com/file/d/1ldESo9gb6icRD8ZsuPgaEDSIwFjJEe4W"
noBootsDF <- drive_download(file = noBootsDFLocation, 
                            path = file.path(downloadFolderBird, "noBootsDF"),
                            type = "spreadsheet",
                            overwrite = TRUE)
```

# Create data table of bird density by landscape

A `data.table` is created, containing the number of cells of each land cover class that are found in the `landscapeRaster`, and the mean bird density for each land cover class, alongside the variance, and the standard error of this mean density. 

The first step is to gather the values from the rasters and create a clean dataset.
We then  calculate, for each cover class, the number of cells in this class, the mean bird density, and the variance and standard error for bird density. 

## Get bird dataset

Here the data for each cell's mean bird density and variance between bootstrap replicates is collected in a `data.table`.


```r
getBirdDataset <- function(birdRasterStack, categoriesRaster) {
  reproducible::Require("raster")
  
  meanBirdRasters <- names(birdRasterStack) %>%
    str_detect('mean') %>%
    keep(birdRasterStack, .)
  namesMeanBirdRasters <- names(meanBirdRasters)
  
  birdDatasets <- lapply(X = meanBirdRasters, FUN = function(birdRasterLayer) {
    landBirdRasterStack <- raster::stack(categoriesRaster, birdRasterLayer)
    ## take the values from the rasters and input them to a data table calledcellValues
    cellValues <- data.table(getValues(landBirdRasterStack))
    cellValues <- setnames(cellValues, c("landCoverClass", "birdDensity"))
    
    cellValues <- na.omit(cellValues) ## remove any rows with NA
    
    ## make landCoverClass categorical rather than numerical
    cellValues$landCoverClass <- as.factor(cellValues$landCoverClass) 
    
    return(cellValues)
  })
  
  names(birdDatasets) <- meanBirdRasters

  return(birdDatasets)
}
```


```r
birdDatasets <- getBirdDataset(birdRasterStack = birdRasterStack,
                               categoriesRaster = landscapeRaster)

for (i in names(birdDatasets)) {
  attr(birdDatasets[[i]], "Species") <- i ## attr(birdDatasets$OVEN, "Species")
}
```


```r
birdStatsByClass <- getBirdStatsByClass(birdDatasets = birdDatasets)

for (i in names(birdStatsByClass)) {
  attr(birdStatsByClass[[i]], "Species") <- i
}
```


```r
assumptionsSummary <- getAssumptionsSummary(birdStatsTables = birdStatsByClass)
```


```r
kernelDensityData <- getKernelDensityData(birdDatasets = birdDatasets)
```


```r
kernelDensityPlot <- getKernelDensityPlot(birdName = "Ovenbird", #sp name for title
                                          coverType = "7, forested", #cover type name for title
                                          birdCoverDensity = kernelDensityData$meanOVEN$"1_7")
```

### Plot variance in bird density by cover class 


```r
plotsVarBirdDensity <- lapply(X = birdStatsByClass,
                              FUN = function(singleBirdStats){
plotVarBirdDensity <- ggplot(data = singleBirdStats, aes(x = uniqueClasses, y = varBirdDensity, fill = forestedStatus)) + 
   theme_classic() +
  ggtitle(paste0("Variance in bird density by cover class")) +
  xlab("Cover Class") +
    theme(axis.text = element_text(size = 6)) +
  geom_bar(stat = "identity", width = 0.7) 
return(plotVarBirdDensity)
})
plotsVarBirdDensity$meanOVEN #show an example
```


# 2D problem - Using BRTs to create a matrix of predicted bird density by age and cover class

This is carried out for forested cover classes only.
 

```r
#define how you want the age classes to be (parameter)
ageGrouping <- 10 #how many years included per age class
maxAgeClass <- 15 #what the oldest age class will be (everything older will be included in this class)
birdMatricies <- lapply(X = birdDatasets, FUN = function(birdDF) {
#separate out data table rows that are forested
forestedDF <- birdDF[forestedStatus == "1"]
forestedDF <- droplevels(forestedDF)
#fit gbm
birdData <- forestedDF
gbmFitted <- gbm::gbm(formula = birdDensity ~ landForClass + age, distribution = "gaussian", data = birdData, interaction.depth = 2, n.trees = 100) #same number of trees as used in predict.gbm
#generate prediction df using expand(?)
maxAge <- max(birdData$age)
allAges <- c(1:maxAge)
birdPredictDF <- birdData %>% expand(landForClass, allAges)
names(birdPredictDF) <- c("landForClass", "age")
#do prediction 
#(object, newdata, n.trees, type = "link", single.tree = FALSE,...)
gbmPred <- gbm::predict.gbm(object = gbmFitted,
                 newdata = birdPredictDF,
                 n.trees = 100, 
                 type = "link", 
                 single.tree = FALSE)  
  
#bin ages into age classes according to the value of ageGrouping 
#I need to get average of every ten columns into one
noAgeClasses <- maxAge/ageGrouping
ageClasses <- rep(1:noAgeClasses, each = ageGrouping)
ageClasses <- ifelse(ageClasses < maxAgeClass, ageClasses, maxAgeClass)
gbmPredDF <- cbind(birdPredictDF, gbmPred, ageClasses)
#TODO in the future add weight averages by gbm prediction variance
gbmPredDF <- aggregate( gbmPred ~ ageClasses + landForClass, gbmPredDF, mean )
 
#form matrix with landForClass as y axis and age as x axis
birdMatrix <- reshape2::acast(gbmPredDF, 
                              landForClass~ageClasses, 
                              value.var = "gbmPred")
      return(birdMatrix)
   
    })
```

### Gather together data to be output by module


```r
#get non-Forest 1D data together
nonforBirdPreds <- lapply(X = birdStatsByClass, FUN = function(singleBirdDF) {
#separate out data table rows that are forested, get rid of unnecessary forestedStatus column
nonforestedDF <- singleBirdDF[forestedStatus == "0"]
nonforestedDF <- subset(nonforestedDF, select = c("landForClass", "meanBirdDensity", "varBirdDensity", "seBirdDensity", "normality", "unimodality"))
nonforestedDF <- droplevels(nonforestedDF)
return(nonforestedDF)
})
```


```r
# create table defining age classes
ages <- rep(1:(maxAgeClass*ageGrouping))
ageClasses <- rep(1:maxAgeClass, each = ageGrouping)
ageClassDefs <- cbind(ages,ageClasses)
#make list object of all module outputs needed for Mapping Module 
birdPreds <- list(birdMatricies, nonforBirdPreds, ageGrouping) 
names(birdPreds) <- c("birdMatrices", "nonforBirdPreds", "ageClassDefs")
```


# References

Boreal Avian Modelling Project, 2020. BAM Generalized National Models Documentation, Version 4.0. Available at https://borealbirds.github.io/. DOI: 10.5281/zenodo.4018335.
