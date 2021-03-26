# SOM DESTABILIZATION
# KAIZAD F. PATEL
# Aug 31, 2020

#################### #
#################### #

# 0-packages

## This script contains packages and file paths needed to run the scripts.
## `source` this script in the script you want to run. 

#################### #
#################### #
# 1. LOAD PACKAGES --------------------------------------------------------
library(tidyverse)
library(drake)
library(PNWColors)
library(outliers)
library(patchwork)
library(ggforce)

# 2. SET FILE NAMES/PATHS -------------------------------------------------
COREKEY = "data/core_key.csv"
COREWEIGHTS = "data/core_weights.csv"

## IRMS & TOTAL C
IRMS_SOIL_REPORT = "data/irms/irms_soil_report.csv"
IRMS_WEOC_REPORT = "data/irms/irms_weoc_report.csv"
IRMS_WEOC_PELLETS_REPORT = "data/irms/irms_weoc_pellets_report.csv"
IRMS_SOIL_TRAYKEY = "data/irms/irms_soil_traykey.csv"
IRMS_WEOC_TRAYKEY = "data/irms/irms_weoc_traykey.csv"
IRMS_WEOC_PELLETS_TRAYKEY = "data/irms/irms_weoc_pellets_traykey.csv"

TC_SOIL_REPORT = "data/irms/tc_soil_report.csv"
TC_WEOC_REPORT = "data/irms/tc_weoc_report.csv"
TC_WEOC_PELLETS_REPORT = "data/irms/tc_weoc_pellets_report.csv"

WEOC_SUBSAMPLING = "data/irms/irms_weoc_subsampling.csv"
WEOC_CAPSULES = "data/irms/weoc_capsuleweights.csv"

SOIL_PROCESSED = "data/processed/soil_processed.csv"
WEOC_PROCESSED = "data/processed/weoc_processed.csv"
WEOC_PELLETS_PROCESSED = "data/processed/weoc_pellets_processed.csv"

## RESPIRATION
RESPIRATION_PROCESSED = "data/processed/respiration_processed.csv"

# 3. SET GGPLOT THEME -----------------------------------------------------

theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

