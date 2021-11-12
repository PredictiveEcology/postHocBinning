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
  reqdPkgs = list("data.table", "diptest", "dplyr", "ggplot2", "ggpubr", "googledrive",
                  "nortest", "plotrix", "raster", "rgdal", "sf", "sp",
                  "PredictiveEcology/LandR@development"), ## TODO: are all these used?
  parameters = rbind(
    defineParameter("ageClassWidth", "integer", 10L, NA, NA,
                    "width of each age class/bin."),
    defineParameter("ageMax", "integer", 200L, NA, NA,
                    "Maximum forest age."),
    defineParameter("birdSpecies", "character", NA, NA, NA,
                    "vector of bird species codes (default NA, means 'all species available')"),
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
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "raster to match.", sourceURL = NA),
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

      plotFun(sim) # example of a plotting function
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

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

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
    bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"
    bcrshp <- Cache(prepInputs,
                    url = bcrzip,
                    destinationPath = dPath,
                    targetCRS = mod$targetCRS,
                    fun = "sf::st_read")

    sim$studyArea <- bcrshp[bcrshp$BCR == 11, ] ## 10 and 11 are relatively small
  }

  if (!suppliedElsewhere("rasterToMatch")) {
    sim$rasterToMatch <- LandR::prepInputsLCC(year = 2005, destinationPath = dPath,
                                              studyArea = sim$studyArea, filename2 = NULL)
  }

  if (!suppliedElsewhere("landscapeRaster")) {
    sim$landscapeRaster <- LandR::prepInputsLCC(year = 2005, destinationPath = dPath,
                                                studyArea = sim$studyArea, filename2 = NULL)
    ## TODO: confirm this is cropped & masked to stuydArea
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
