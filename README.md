---
title: "postHocBinning"
author: "Alex M. Chubaty and Isolde Lane-Shaw"
date: "11th May 2021"
output: 
  html_document: 
    keep_md: yes
editor_options:
  chunk_output_type: console
---


# Overview


This module calculates predicted bird densities from the mean of the most recent bootstrapped Boreal Avian Modelling Project national models of bird density rasters (at 250m resolution), for each of a given set of cover and age classes according to two different methods:
  - 1D, where a single variable, underlying cover class, is used to bin data points of predicted bird density, by calculating the mean. This method is used for both forested and non-forested cover classes.
  - 2D, where a gbm is used to give a predicted bird density according the combination of two variables, cover class and age. The predicted bird densities are then further binned according to the mean value of the desired age classes. This method is used for forested cover classes only.
  - Model statistics, such as unimodality, normality and variance are generated.
  


# Parameters

Provide a summary of user-visible parameters.


# Events

Describe what happens for each event type.

## Init

Everything the module does happens in the init event.


# Data dependencies

## Input data

rasterToMatch - a raster file of Canada to set the crs 
studyArea - a shapefile of the desired area of Canada to be examined
forClassRaster - a raster map giving the desired land cover classes for forested areas found in the study area
nonForRaster - a raster map giving the desired land cover classes for non-forested areas found in the study area
forAgeRaster - a raster map giving the ages of forested areas found in the study area
kNNAgeRaster - a raster map giving the ages of non-forested areas found in the study area

## Output data

Description of the module outputs.

### 1D binning


### 2D binning

birdPreds - a list that includes birdMatricies (matricies giving binned predictions for each age and forested cover class for each bird species), nonforBirdPreds (table giving binned 1D predictions for non-forested cover classes for each bird species), ageClassDefs (a table giving the definitions of the age classes) 



# Links to other modules

This module uses data layers prepared by BAM bootRasterCombine module (<https://github.com/ilaneshaw/bootRasterCombine>).

