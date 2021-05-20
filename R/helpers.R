download250mBootRasters <- function(folderUrl, birdsList, rastersPath) {
  ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
  filesToDownload <-
    googledrive::drive_ls(path = as_id(folderUrl)) #note: has to be changed if filenaming system changes
  ## grepl function searches for all items in the filesToDownload that are on birdList & stores their names in rastersforBirdList
  rastersForBirdList <-
    filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"),
                               x = filesToDownload$name)]

  ## for each item in turn from rastersForBirdlist the following function is applied:
  downloadedRasters <-
    lapply(
      X = rastersForBirdList,
      FUN = function(rasterFile) {
        ## if the item in rastersForBirdList is not already present at rastersPath, googledrive package downloads it
        if (!file.exists(file.path(rastersPath, rasterFile))) {
          googledrive::drive_download(
            file = as_id(filesToDownload[filesToDownload$name %in% rasterFile,]$id),
            #rasterFile,
            path = file.path(rastersPath, rasterFile),
            overwrite = TRUE
          )
        }

        ## otherwise, if it is already present and downloaded, just get the name of the item
        return(raster(file.path(rastersPath, rasterFile), verbose = TRUE))
      }
    )

  #get the species codes as names for the downloadedRasters object, rather than using the whole filepath
  X <-
    lapply(rastersForBirdList, substr, 1, 8) #works for strings of the form "varsXXXX" or "meanXXXX"
  names(downloadedRasters) <- X
  #the downloadBirdDensityRasters function returns/loads the downloaded rasters
  return(downloadedRasters)
}

load250mBootRasters <- function(birdsList,
                                folderUrl,
                                rastersPath,
                                rasterToMatch,
                                studyArea) {
  reproducible::Require("raster")
  ## check that there is a folder at the given rastersPath. if not, create it.
  rastersPath <- checkPath(file.path(rastersPath), create = TRUE)
  ## download the rasters on the birdList. Return List of downloaded files.
  downloadedRasters <- download250mBootRasters(folderUrl = folderUrl,
                                               birdsList = birdsList,
                                               rastersPath = rastersPath)
  ## lapply applys the custom function to each raster in turn
  postprocessedRasters <- lapply(X = downloadedRasters, FUN = function(RasterLayer) {
    ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path
    proRaster <- postProcess(RasterLayer,
                             studyArea = studyArea,
                             rasterToMatch = rasterToMatch,
                             destinationPath = rastersPath)
    ## each layer is returned into the object proRaster
    return(proRaster)
  })
  ## Finally the whole function returns the birdRasterStacks
  return(postprocessedRasters)
}

getBirdDataset <- function(birdRasterStack, landscapeClassesRasterStack) {
  reproducible::Require("raster")
  meanBirdRasters <- names(birdRasterStack) %>%
    str_detect('mean') %>%
    keep(birdRasterStack, .)
  namesMeanBirdRasters <- names(meanBirdRasters)

  birdDatasets <- lapply(X = meanBirdRasters, FUN = function(birdRasterLayer) {

    landBirdRasterStack <- raster::addLayer(birdRasterLayer, landscapeClassesRasterStack)


    ## take the values from the rasters and input them to a data table called cellValues
    cellValues <- data.table(getValues(landBirdRasterStack))
    cellValues <- setnames(cellValues, c( "birdDensity", "landForClass", "age", "forestedStatus"))
    cellValues <- unite(cellValues, uniqueClasses, c(forestedStatus, landForClass), remove = FALSE)

    #get rid of any rows with NA values
    cellValues <- na.omit(cellValues)

    ## make sure landForClass and forestedStatus are categorical rather than numerical
    cellValues$landForClass <- as.factor(cellValues$landForClass)
    cellValues$forestedStatus <- as.factor(cellValues$forestedStatus)
    cellValues$uniqueClasses <- as.factor(cellValues$uniqueClasses)

    return(cellValues)
  })

  names(birdDatasets) <- namesMeanBirdRasters
  return(birdDatasets)
}

getBirdStatsByClass <- function(birdDatasets) {
  namesBirdsAnalysed <- names(birdDatasets)
  #base::attr()
  birdStatsByClass <- lapply(X = birdDatasets, FUN = function(singleBirdDataset) {
    print(attr(singleBirdDataset, "Species")) ## TODO: use messages
    flush.console() ## TODO: remove!
    birdStats <- singleBirdDataset[order(-forestedStatus, landForClass) # order the rows by the land cover class
    ][,list(classCount = .N, # get the number of cells each cover class
            #meanBirdDensity = mean(birdDensity), # get the mean bird density for each cover class
            meanBirdDensity = mean(birdDensity), #try log of mean bird density
            varBirdDensity = var(birdDensity), # get the variance for bird density for each cover class
            seBirdDensity = std.error(birdDensity), # get the standard error for bird density for each cover class
            normality = tryCatch(ad.test(birdDensity)$p.value,error = function(cond) { return(NaN) }), #ifelse(mean(birdDensity) > 0, tryCatch(ad.test(birdDensity)$p.value,error = function(cond){return(NA)}), NA),
            unimodality =   dip.test(birdDensity)$p.value),
      by = list(forestedStatus, landForClass)]

    birdStats <- unite(birdStats, uniqueClasses, c(forestedStatus, landForClass), remove = FALSE)
    return(birdStats)
  })

  names(birdStatsByClass) <- namesBirdsAnalysed
  return(birdStatsByClass)
}

getAssumptionsSummary <- function(birdStatsTables) {
  namesBirdsAnalysed <- names(birdStatsTables)

  byBirdAssumptions <- lapply(birdStatsTables, FUN = function(x) {
    print(attr(x, "Species")) ## TODO: use messages
    flush.console() ## TODO: remove

    ## Normality Proportion
    norm <- length(x$normality[!is.na(x$normality)])
    if (norm == "0") {
      propNormal <- NA
    } else {
      noNormal <- sum(!(x$normality[!is.na(x$normality)]) > 0.05)
      totalClassesNorm <- length(x$normality[!is.na(x$normality)])
      propNormal <- noNormal/totalClassesNorm
    }

    ## unimodal proportion
    noUnimodal <- sum(!(x$unimodality > 0.05))
    totalClassesUni <- length(x$unimodality)
    propUnimodal <- noUnimodal/totalClassesUni

    # assumptions <- data.table(propUnimodal)
    assumptions <- data.table(propNormal, propUnimodal)
    return(assumptions)
  })

  names(byBirdAssumptions) <- namesBirdsAnalysed
  numberOfSpecies <- length(byBirdAssumptions)

  birdAssumpTab <- rbindlist(byBirdAssumptions, use.names = TRUE, idcol = "birdSp")

  ## proportion of species where > 50% cover types are normal
  numberOfSpeciesNorm <- length(birdAssumpTab$propNormal[!is.na(birdAssumpTab$propNormal)])
  numberSpNormal <-  sum(!(birdAssumpTab$propNormal[!is.na(birdAssumpTab$propNormal)] < 0.5))
  propSpNormal <- numberSpNormal/numberOfSpeciesNorm

  ## proportion of species where > 50% cover types are unimodal
  numberSpUnimodal <- sum(!(birdAssumpTab$propUnimodal < 0.5))
  propSpUnimodal <- numberSpUnimodal / numberOfSpecies
  birdAssumptions <- data.table(propSpNormal, propSpUnimodal)
  return(birdAssumptions)
}

getKernelDensityData <- function(birdDatasets) {
  namesBirdsAnalysed <- names(birdDatasets)

  birdKernelDensities <- lapply(X = birdDatasets, FUN = function(singleBirdDataset) {
    namesClasses <- levels(singleBirdDataset$uniqueClasses)
    classKernelDensities <- lapply(X = namesClasses, FUN = function(coverType) {
      dataForDensity <- singleBirdDataset[uniqueClasses == coverType]
      singleClassDensity <- density(dataForDensity[,birdDensity])

      return(singleClassDensity)
    })

    names(classKernelDensities) <- namesClasses
    return(classKernelDensities)
  })
  names(birdKernelDensities) <- namesBirdsAnalysed
  return(birdKernelDensities)
}

getKernelDensityPlot <- function(birdCoverDensity, birdName, coverType, meanData) {
  densityPlot <- plot(birdCoverDensity,
                      main = paste0("Kernel Density for ", paste0(birdName), " in CC", coverType),
                      xlab = "Predicted Bird Density",
                      ylab = "Density of predictions")
  return(densityPlot)
}
