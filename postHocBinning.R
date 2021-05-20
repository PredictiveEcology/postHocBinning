defineModule(sim, list(
  name = "postHocBinning",
  description = "",
  keywords = "",
  authors = c(
    person("Alex M.", "Chubaty", role = c("aut", "cre"), email = "achubaty@for-cast.ca"),
    person("Isolde", "Lane-Shaw", role = "aut", email = "") ## TODO: need email
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6.9018", postHocBinning = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "postHocBinning.Rmd")),
  reqdPkgs = list("data.table", "diptest", "dplyr", "gbm", "ggplot2", "ggpubr", "googledrive",
                  "nortest", "plotrix", "raster", "rgdal", "sf", "sp",
                  "PredictiveEcology/LandR@development"), ## TODO: are all these used?
  parameters = rbind(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "forestClassRaster", objectClass = "RasterLayer",
                 desc = "forest class raster (e.g., derived from LandR Biomass).", sourceURL = NA),
    expectsInput(objectName = "landscapeRaster", objectClass = "RasterLayer",
                 desc = "Land cover class raster, default LCC2005.", sourceURL = NA),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "raster to match. default LCC2005.", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area polygon", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.postHocBinning = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "postHocBinning", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "postHocBinning", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # schedule future event(s)
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "postHocBinning", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "postHocBinning", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "postHocBinning", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "postHocBinning", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  if (!suppliedElsewhere("studyArea")) {
    ## needs to align with default forestClassRaster extent below
    bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"
    bcrshp <- Cache(prepInputs,
                    url = bcrzip,
                    destinationPath = dPath,
                    targetCRS = mod$targetCRS,
                    fun = "sf::st_read")
    bcr6 <- bcrshp[bcrshp$BCR == 6, ]

    canProvs <- Cache(prepInputs,
                      "GADM",
                      fun = "base::readRDS",
                      dlFun = "raster::getData",
                      country = "CAN", level = 1, path = dPath,
                      #targetCRS = targetCRS, ## TODO: fails on Windows
                      targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                      destinationPath = dPath) %>%
      st_as_sf(.) %>%
      st_transform(., mod$targetCRS)
    bcab <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta"), ]

    sim$studyArea <- postProcess(bcab, studyArea = bcr6, useSAcrs = TRUE,
                                 filename2 = NULL, overwrite = TRUE) %>%
      as_Spatial(.)
  }

  if (!suppliedElsewhere("rasterToMatch")) {
    sim$rasterToMatch <- Cache(prepInputsLCC, year = 2005, destinationPath = dPath,
                               studyArea = sim$studyArea, filename2 = NULL)
  }

  if (!suppliedElsewhere("forestClassRaster")) {
    urlForestClassRaster <- "https://drive.google.com/file/d/1cfrb-RhiwMD4XlS_yzRSQYPVh8ffwC0L" ## ABBC
    sim$forestClassRaster <- Cache(prepInputs,
                                   url = urlForestClassRaster,
                                   destinationPath = dPath,
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$rasterToMatch,
                                   fun = "raster::raster")
    sim$forestClassRaster[forestClassRaster == 0, ] <- NA
  }

  browser()
  if (!suppliedElsewhere("nonForestClassRaster")) {
     ## using LCC
  }

  if (!suppliedElsewhere("landscapeRaster")) {
    sim$landscapeRaster <- Cache(prepInputsLCC, year = 2005, destinationPath = dPath,
                                 studyArea = sim$studyArea, filename2 = NULL) ## TODO: overlay F/NF ?????
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
