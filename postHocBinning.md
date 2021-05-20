---
title: "postHocBinning"
author: "Alex M. Chubaty and Isolde Lane-Shaw"
date: "15 March 2021"
output: 
  html_document: 
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

This module predicts expected bird densities based on land cover classification.

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



|objectName    |objectClass              |desc                              |sourceURL |
|:-------------|:------------------------|:---------------------------------|:---------|
|rasterToMatch |RasterLayer              |raster to match. default LCC2005. |NA        |
|studyArea     |SpatialPolygonsDataFrame |study area polygon                |NA        |

## Output data

Description of the module outputs.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Describe any anticipated linkages to other modules.

<!-- TODO: move everything below here into module code, e.g. defaults, and remove -->

# OLD -- do not use


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

birdDatasets <- getBirdDataset(birdRasterStack = birdRasterStack,
                               categoriesRaster = landscapeRaster)

for (i in names(birdDatasets)) {
  attr(birdDatasets[[i]], "Species") <- i ## attr(birdDatasets$OVEN, "Species")
}

birdStatsByClass <- getBirdStatsByClass(birdDatasets = birdDatasets)

for (i in names(birdStatsByClass)) {
  attr(birdStatsByClass[[i]], "Species") <- i
}

assumptionsSummary <- getAssumptionsSummary(birdStatsTables = birdStatsByClass)

kernelDensityData <- getKernelDensityData(birdDatasets = birdDatasets)

kernelDensityPlot <- getKernelDensityPlot(birdName = "Ovenbird", #sp name for title
                                          coverType = "7", #cover type name for title
                                          birdCoverDensity = kernelDensityData$OVEN$"7", #which kernel density cover type and bird species to plot
                                          meanData = birdStatsByClass$OVEN[7, meanBirdDensity] #show a red line with the mean density
)
#which sp and cover type to graph 

# plotsVarBirdDensity <- lapply(X = birdStatsByClass,
#                               FUN = function(singleBirdStats){
#                                 
# plotVarBirdDensity <- ggplot(data = singleBirdStats, aes(x =landCoverClass, y = varBirdDensity)) + geom_bar(stat="identity")
# 
# return(plotVarBirdDensity)
# 
# })
```
