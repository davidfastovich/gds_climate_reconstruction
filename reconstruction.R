#############
# ENVIRONMENT
#############

# This information isn't very important right now, but maybe if this script is
# referenced years after it is written because R packages get updated.
# 
# Operating System: Ubuntu 20.04.4 LTS x86_64
# R Version: 4.1.2 (2021-11-01) "Bird Hippie"
# 
# Author: David Fastovich, dfastovi@syr.edu
# Date: 3/17/2022

library(neotoma) #1.7.4
library(openxlsx) # 4.2.5
library(rioja) # 0.9-26
library(parallel) # 4.1.2
library(gstat) # 2.0-9
library(sp) # 1.4-7
library(geosphere) # 1.5-14
library(tidyverse) # 1.3.1

# Set working directory
setwd("~/Documents/GitHub/dismal_swamp_analyses/")

#####################
# READ IN EXCEL FILES
#####################

# New pollen counts
ds49 <- read.xlsx("pollen_counts/DS49_for_tilia.xlsx")
gds83 <- read.xlsx("pollen_counts/GDS83_for_tilia.xlsx")
gds519 <- read.xlsx("pollen_counts/GDS519_for_tilia.xlsx")
gds520 <- read.xlsx("pollen_counts/GDS520_for_tilia.xlsx")
gds528 <- read.xlsx("pollen_counts/GDS528_for_tilia.xlsx")
gds542 <- read.xlsx("pollen_counts/GDS542_for_tilia.xlsx")
gdsw1 <- read.xlsx("pollen_counts/GDSW1_for_tilia.xlsx")

# Data needed for analog calculations and climate reconstructions
# Note these files are ALREADY SPLIT. They are split according to the shapefiles
# from Williams and Shuman (2008, QSR) in the pollen_split.py script.
modern_pollen <- read.csv("results/whitmore_surface_expanded.csv")
modern_pollen_picea_east <- read.csv("results/whitmore_surface_expanded_east.csv")
modern_pollen_pinus_ne <- read.csv("results/whitmore_surface_expanded_ne.csv")
modern_pollen_pinus_se <- read.csv("results/whitmore_surface_expanded_se.csv")
modern_climate <- read.csv("results/whitmore_climate_expanded.csv")

# Climate
modern_pollen_climate <- read.csv("results/whitmore_climate_expanded.csv")

####################################
# COMPILE TAXA NAMES TO COMMON NAMES
####################################

# Whitmore uses a non-standard naming convention so I'm going to change it to
# standard names usig a self-made conversion list.
# 
# NOT COMPILING PINUS SITES SINCE WE'RE ONLY USING THEM AS IDENTIFIERS

from_whitmore <- read.csv("taxa_conversion_list/from_whitmore.csv")
whitmore_picea_east_common_names <- as.data.frame(
  compile_taxa(modern_pollen_picea_east[,14:147], alt.table = from_whitmore, list.name = "from_whitmore")
)
rownames(whitmore_picea_east_common_names) <- modern_pollen_picea_east$ID2

############################
# COMPILE TAXA NAMES TO WS64
############################

# Now that Whitmore has standard names we can use my extended WS64 list to
# compile taxa to the recommended list from Williams and Shuman (2008, QSR).

to_ws64 <- read.csv("taxa_conversion_list/ws64_extended.csv")
whitmore_picea_east_ws64 <- as.data.frame(
  compile_taxa(whitmore_picea_east_common_names, alt.table = to_ws64, list.name = "WS64")
) # Errors are okay because those taxa don't belong ot WS64
rownames(whitmore_picea_east_ws64) <- modern_pollen_picea_east$ID2

######################################
# RENAME SPLIT TAXA ACCORDING TO SPLIT
######################################

# WE DON'T NEED TO RENAME PICEA SINCE WE'RE ONLY ANALYZING PICEA SITES THAT ARE
# PICEA EAST. HENCE, ALL PICEA IS THE SAME TYPE.
# 
# Not all Picea sites within the Pinus subgroups are eastern type so I'm
# removing western Picea sites.
modern_pollen_pinus_ne_picea_east <- modern_pollen_pinus_ne[(modern_pollen_pinus_ne$ID2 %in% modern_pollen_picea_east$ID2), ]
modern_pollen_pinus_se_picea_east <- modern_pollen_pinus_se[(modern_pollen_pinus_se$ID2 %in% modern_pollen_picea_east$ID2), ]

# Lets confirm that all Pinus split sites are within the Picea sites
all(modern_pollen_pinus_ne_picea_east$ID2 %in% modern_pollen_picea_east$ID2)
all(modern_pollen_pinus_se_picea_east$ID2 %in% modern_pollen_picea_east$ID2)

# Now lets build a Pinus split data frame to split Pinus within the larger Picea
# data frame
northeast_pinus <- as.integer(modern_pollen_picea_east$ID2 %in% modern_pollen_pinus_ne_picea_east$ID2)
southeast_pinus <- as.integer(modern_pollen_picea_east$ID2 %in% modern_pollen_pinus_se_picea_east$ID2)

# Apply these Pinus splits
whitmore_picea_east_ws64$`Pinus diploxylon.NE` <- whitmore_picea_east_ws64$`Pinus diploxylon` * northeast_pinus
whitmore_picea_east_ws64$`Pinus haploxylon.NE` <- whitmore_picea_east_ws64$`Pinus haploxylon` * northeast_pinus
whitmore_picea_east_ws64$`Pinus undifferentiated.NE` <- whitmore_picea_east_ws64$`Pinus undifferentiated` * northeast_pinus

whitmore_picea_east_ws64$`Pinus diploxylon.SE` <- whitmore_picea_east_ws64$`Pinus diploxylon` * southeast_pinus
whitmore_picea_east_ws64$`Pinus haploxylon.SE` <- whitmore_picea_east_ws64$`Pinus haploxylon` * southeast_pinus
whitmore_picea_east_ws64$`Pinus undifferentiated.SE` <- whitmore_picea_east_ws64$`Pinus undifferentiated` * southeast_pinus

# After applying these splits we need to set the unsplit column to 0 for sites
# that have a Pinus split. This will retain the unsplit Pinus column names for
# sites that do not have a Pinus split identifier. This is accomplished by adding
# the boolean Pinus split sites together and every site that has a value of 0
# means it does not have a split identifier. I'm also using this opportunity to
# remove sites that have both a southeast and northeast identifier - 48 sites
# in total.
split_pinus_sites <- northeast_pinus + southeast_pinus

# Sites that have a Pinus split have the unsplit columns set to 0
whitmore_picea_east_ws64$`Pinus diploxylon`[split_pinus_sites != 0] <- 0
whitmore_picea_east_ws64$`Pinus haploxylon`[split_pinus_sites != 0] <- 0
whitmore_picea_east_ws64$`Pinus undifferentiated`[split_pinus_sites != 0] <- 0

# Remove sites that are identified as NE and SE Pinus
whitmore_picea_east_ws64 <- whitmore_picea_east_ws64[split_pinus_sites != 2,]

##############################
# TEST THE SPLITTING PROCEDURE
##############################

# Splits are tested to make sure that the Pinus rowsums are unchanged
pinus_columns <- grep("Pinus", colnames(whitmore_picea_east_ws64))

# Applying sample filter as Line 125 to the unmodinfied pollen counts - All true!
# To explain the test: if I performed the splits incorrectly then when I rowSum
# all of the columns "Pinus diploxylon", "Pinus haploxylon", 
# "Pinus undifferentiated", "Pinus diploxylon.NE", "Pinus haploxylon.NE", 
# "Pinus undifferentiated.NE", "Pinus diploxylon.SE", "Pinus haploxylon.SE", 
# "Pinus undifferentiated.SE" would presumably not add up to the same exact
# values from the unsplit counts (likely double counts).
# 
# ALL TRUE
filtered_unmofidifed_counts <- modern_pollen_picea_east[split_pinus_sites != 2,]
all(rowSums(whitmore_picea_east_ws64[, pinus_columns]) == rowSums(filtered_unmofidifed_counts[,c("PINDIPLO", "PINHAPLO", "PINUSX")]))

###########################
# COMPILE THE FOSSIL COUNTS
###########################

# DS49 -------------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
ds49_t <- as.data.frame(
  t(
    ds49[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(ds49_t) <- ds49[, 1]

# New pollen counts
ds49_ws64 <- as.data.frame(
  compile_taxa(ds49_t, alt.table = to_ws64, list.name = "WS64")
)

# GDS83 ------------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gds83_t <- as.data.frame(
  t(
    gds83[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gds83_t) <- gds83[, 1]

# New pollen counts
gds83_ws64 <- as.data.frame(
  compile_taxa(gds83_t, alt.table = to_ws64, list.name = "WS64")
) # Polypodiophyta  (monolete) error is okay - not in WS64

# GDS519 -----------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gds519_t <- as.data.frame(
  t(
    gds519[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gds519_t) <- gds519[, 1]

# New pollen counts
gds519_ws64 <- as.data.frame(
  compile_taxa(gds519_t, alt.table = to_ws64, list.name = "WS64")
) # Polypodiophtya (zonate_ error okay - not in WS64

# GDS520 -----------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gds520_t <- as.data.frame(
  t(
    gds520[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gds520_t) <- gds520[, 1]

# New pollen counts
gds520_ws64 <- as.data.frame(
  compile_taxa(gds520_t, alt.table = to_ws64, list.name = "WS64")
)

# GDS528 -----------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gds528_t <- as.data.frame(
  t(
    gds528[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gds528_t) <- gds528[, 1]

# New pollen counts
gds528_ws64 <- as.data.frame(
  compile_taxa(gds528_t, alt.table = to_ws64, list.name = "WS64")
)

# GDS542 -----------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gds542_t <- as.data.frame(
  t(
    gds542[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gds542_t) <- gds542[, 1]

# New pollen counts
gds542_ws64 <- as.data.frame(
  compile_taxa(gds542_t, alt.table = to_ws64, list.name = "WS64")
)

# GDSW1 ------------------------------------------------------------------------
# Need to transpose data frame first to get species as columns
gdsw1_t <- as.data.frame(
  t(
    gdsw1[, -1]
  )
)
# Assign species names as columns while preserving the correct order
names(gdsw1_t) <- gdsw1[, 1]

# New pollen counts
gdsw1_ws64 <- as.data.frame(
  compile_taxa(gdsw1_t, alt.table = to_ws64, list.name = "WS64")
) # Polypodiophyta (monolete with perine) and Unknown (stephanocolpate - not in WS64

########################
# CONVERT TO PROPORTIONS
########################

ds49_ws64_pct <- ds49_ws64/rowSums(ds49_ws64)
gds83_ws64_pct <- gds83_ws64/rowSums(gds83_ws64)
gds519_ws64_pct <- gds519_ws64/rowSums(gds519_ws64)
gds520_ws64_pct <- gds520_ws64/rowSums(gds520_ws64)
gds528_ws64_pct <- gds528_ws64/rowSums(gds528_ws64)
gds542_ws64_pct <- gds542_ws64/rowSums(gds542_ws64)
gdsw1_ws64_pct <- gdsw1_ws64/rowSums(gdsw1_ws64)

whitmore_picea_east_ws64_pct <- whitmore_picea_east_ws64/rowSums(whitmore_picea_east_ws64)

################################
# GET RID OF 'OTHER' AND POACEAE
################################

ds49_ws64_pct <- ds49_ws64_pct[,-grep("Other", colnames(ds49_ws64_pct))]
gds83_ws64_pct <- gds83_ws64_pct[,-grep("Other", colnames(gds83_ws64_pct))]
gds519_ws64_pct <- gds519_ws64_pct[,-grep("Other", colnames(gds519_ws64_pct))]
gds520_ws64_pct <- gds520_ws64_pct[,-grep("Other", colnames(gds520_ws64_pct))]
gds528_ws64_pct <- gds528_ws64_pct[,-grep("Other", colnames(gds528_ws64_pct))]
gds542_ws64_pct <- gds542_ws64_pct[,-grep("Other", colnames(gds542_ws64_pct))]
gdsw1_ws64_pct <- gdsw1_ws64_pct[,-grep("Other", colnames(gdsw1_ws64_pct))]

whitmore_picea_east_ws64_pct <- whitmore_picea_east_ws64_pct[,-grep("Other", colnames(whitmore_picea_east_ws64_pct))]

ds49_ws64_pct <- ds49_ws64_pct[,-grep("Poaceae", colnames(ds49_ws64_pct))]
gds83_ws64_pct <- gds83_ws64_pct[,-grep("Poaceae", colnames(gds83_ws64_pct))]
gds519_ws64_pct <- gds519_ws64_pct[,-grep("Poaceae", colnames(gds519_ws64_pct))]
gds520_ws64_pct <- gds520_ws64_pct[,-grep("Poaceae", colnames(gds520_ws64_pct))]
gds528_ws64_pct <- gds528_ws64_pct[,-grep("Poaceae", colnames(gds528_ws64_pct))]
gds542_ws64_pct <- gds542_ws64_pct[,-grep("Poaceae", colnames(gds542_ws64_pct))]
gdsw1_ws64_pct <- gdsw1_ws64_pct[,-grep("Poaceae", colnames(gdsw1_ws64_pct))]

whitmore_picea_east_ws64_pct <- whitmore_picea_east_ws64_pct[,-grep("Poaceae", colnames(whitmore_picea_east_ws64_pct))]

##################################################################
# SPLIT FOSSIL COUNTS ACCORDING TO WORK FROM MARSICECK ET AL. 2018
##################################################################

# DS49 -------------------------------------------------------------------------
# Southeastern from 0-220.5 cm all else northeastern
depth <- as.numeric(rownames(ds49_ws64_pct)) * 100 # To convert to cm from m

ds49_ws64_pct$`Pinus haploxylon.SE` <- ds49_ws64_pct$`Pinus haploxylon`
ds49_ws64_pct$`Pinus undifferentiated.SE` <- ds49_ws64_pct$`Pinus undifferentiated`
ds49_ws64_pct$`Pinus haploxylon.NE` <- ds49_ws64_pct$`Pinus haploxylon`
ds49_ws64_pct$`Pinus undifferentiated.NE` <- ds49_ws64_pct$`Pinus undifferentiated`

ds49_ws64_pct$`Pinus haploxylon.SE`[depth > 220.5] <- 0
ds49_ws64_pct$`Pinus undifferentiated.SE`[depth > 220.5] <- 0

ds49_ws64_pct$`Pinus haploxylon.NE`[depth <= 220.5] <- 0
ds49_ws64_pct$`Pinus undifferentiated.NE`[depth <= 220.5] <- 0

ds49_ws64_pct <- ds49_ws64_pct[,-c(26, 27)] # Get rid of unsplit columns

# GDS519 -----------------------------------------------------------------------
# Southeastern from 0-336 cm all else northeastern
depth <- as.numeric(rownames(gds519_ws64_pct))

gds519_ws64_pct$`Pinus haploxylon.SE` <- gds519_ws64_pct$`Pinus haploxylon`
gds519_ws64_pct$`Pinus undifferentiated.SE` <- gds519_ws64_pct$`Pinus undifferentiated`
gds519_ws64_pct$`Pinus haploxylon.NE` <- gds519_ws64_pct$`Pinus haploxylon`
gds519_ws64_pct$`Pinus undifferentiated.NE` <- gds519_ws64_pct$`Pinus undifferentiated`

gds519_ws64_pct$`Pinus haploxylon.SE`[depth > 336] <- 0
gds519_ws64_pct$`Pinus undifferentiated.SE`[depth > 336] <- 0

gds519_ws64_pct$`Pinus haploxylon.NE`[depth <= 336] <- 0
gds519_ws64_pct$`Pinus undifferentiated.NE`[depth <= 336] <- 0

gds519_ws64_pct <- gds519_ws64_pct[,-c(25, 26)] # Get rid of unsplit columns

# GDS520 -----------------------------------------------------------------------
# Southeastern from 0-280 cm all else northeastern
depth <- as.numeric(rownames(gds520_ws64_pct))

gds520_ws64_pct$`Pinus haploxylon.SE` <- gds520_ws64_pct$`Pinus haploxylon`
gds520_ws64_pct$`Pinus diploxylon.SE` <- gds520_ws64_pct$`Pinus diploxylon`
gds520_ws64_pct$`Pinus undifferentiated.SE` <- gds520_ws64_pct$`Pinus undifferentiated`
gds520_ws64_pct$`Pinus haploxylon.NE` <- gds520_ws64_pct$`Pinus haploxylon`
gds520_ws64_pct$`Pinus diploxylon.NE` <- gds520_ws64_pct$`Pinus diploxylon`
gds520_ws64_pct$`Pinus undifferentiated.NE` <- gds520_ws64_pct$`Pinus undifferentiated`

gds520_ws64_pct$`Pinus haploxylon.SE`[depth > 280] <- 0
gds520_ws64_pct$`Pinus diploxylon.SE`[depth > 280] <- 0
gds520_ws64_pct$`Pinus undifferentiated.SE`[depth > 280] <- 0

gds520_ws64_pct$`Pinus haploxylon.NE`[depth <= 280] <- 0
gds520_ws64_pct$`Pinus diploxylon.NE`[depth <= 280] <- 0
gds520_ws64_pct$`Pinus undifferentiated.NE`[depth <= 280] <- 0

gds520_ws64_pct <- gds520_ws64_pct[,-c(26, 27, 28)] # Get rid of unsplit columns

# GDS528 -----------------------------------------------------------------------
# Southeastern from 0-200.5 cm all else northeastern
depth <- as.numeric(rownames(gds528_ws64_pct))

gds528_ws64_pct$`Pinus haploxylon.SE` <- gds528_ws64_pct$`Pinus haploxylon`
gds528_ws64_pct$`Pinus undifferentiated.SE` <- gds528_ws64_pct$`Pinus undifferentiated`
gds528_ws64_pct$`Pinus haploxylon.NE` <- gds528_ws64_pct$`Pinus haploxylon`
gds528_ws64_pct$`Pinus undifferentiated.NE` <- gds528_ws64_pct$`Pinus undifferentiated`

gds528_ws64_pct$`Pinus haploxylon.SE`[depth > 200.5] <- 0
gds528_ws64_pct$`Pinus undifferentiated.SE`[depth > 200.5] <- 0

gds528_ws64_pct$`Pinus haploxylon.NE`[depth <= 200.5] <- 0
gds528_ws64_pct$`Pinus undifferentiated.NE`[depth <= 200.5] <- 0

gds528_ws64_pct <- gds528_ws64_pct[,-c(24, 25)] # Get rid of unsplit columns

# GDS542 -----------------------------------------------------------------------
# Southeastern from 0-102.5 cm all else northeastern
depth <- as.numeric(rownames(gds542_ws64_pct))

gds542_ws64_pct$`Pinus haploxylon.SE` <- gds542_ws64_pct$`Pinus haploxylon`
gds542_ws64_pct$`Pinus undifferentiated.SE` <- gds542_ws64_pct$`Pinus undifferentiated`
gds542_ws64_pct$`Pinus haploxylon.NE` <- gds542_ws64_pct$`Pinus haploxylon`
gds542_ws64_pct$`Pinus undifferentiated.NE` <- gds542_ws64_pct$`Pinus undifferentiated`

gds542_ws64_pct$`Pinus haploxylon.SE`[depth > 102.5] <- 0
gds542_ws64_pct$`Pinus undifferentiated.SE`[depth > 102.5] <- 0

gds542_ws64_pct$`Pinus haploxylon.NE`[depth <= 102.5] <- 0
gds542_ws64_pct$`Pinus undifferentiated.NE`[depth <= 102.5] <- 0

gds542_ws64_pct <- gds542_ws64_pct[,-c(24, 25)] # Get rid of unsplit columns

# GDSW1 ------------------------------------------------------------------------
# Southeastern from 0-349 cm all else northeastern
depth <- as.numeric(rownames(gdsw1_ws64_pct))

gdsw1_ws64_pct$`Pinus haploxylon.SE` <- gdsw1_ws64_pct$`Pinus haploxylon`
gdsw1_ws64_pct$`Pinus undifferentiated.SE` <- gdsw1_ws64_pct$`Pinus undifferentiated`
gdsw1_ws64_pct$`Pinus haploxylon.NE` <- gdsw1_ws64_pct$`Pinus haploxylon`
gdsw1_ws64_pct$`Pinus undifferentiated.NE` <- gdsw1_ws64_pct$`Pinus undifferentiated`

gdsw1_ws64_pct$`Pinus haploxylon.SE`[depth > 349] <- 0
gdsw1_ws64_pct$`Pinus undifferentiated.SE`[depth > 349] <- 0

gdsw1_ws64_pct$`Pinus haploxylon.NE`[depth <= 349] <- 0
gdsw1_ws64_pct$`Pinus undifferentiated.NE`[depth <= 349] <- 0

gdsw1_ws64_pct <- gdsw1_ws64_pct[,-c(26, 27)] # Get rid of unsplit columns

#################################
# 'JOIN' MODERN AND FOSSIL POLLEN
#################################

ds49_ws64_pct_join <- analogue::join(ds49_ws64_pct, whitmore_picea_east_ws64_pct)
gds83_ws64_pct_join <- analogue::join(gds83_ws64_pct, whitmore_picea_east_ws64_pct)
gds519_ws64_pct_join <- analogue::join(gds519_ws64_pct, whitmore_picea_east_ws64_pct)
gds520_ws64_pct_join <- analogue::join(gds520_ws64_pct, whitmore_picea_east_ws64_pct)
gds528_ws64_pct_join <- analogue::join(gds528_ws64_pct, whitmore_picea_east_ws64_pct)
gds542_ws64_pct_join <- analogue::join(gds542_ws64_pct, whitmore_picea_east_ws64_pct)
gdsw1_ws64_pct_join <- analogue::join(gdsw1_ws64_pct, whitmore_picea_east_ws64_pct)

#################
# BUILD MAT MODEL
#################

# Filter climate to only keep sites that we are analyzing
modern_climate_east <- modern_climate[modern_climate$ID2 %in% as.numeric(rownames(whitmore_picea_east_ws64_pct)), ]

# Testing filter - SUCCESS
all(modern_climate_east$ID2 == as.numeric(rownames(whitmore_picea_east_ws64_pct)))

# There's a single new surface sample site without coordinates that has NA's
# for climate variables. Removing now.
whitmore_picea_east_ws64_pct <- whitmore_picea_east_ws64_pct[!is.na(modern_climate_east$tave),]
modern_climate_east <- modern_climate_east[!is.na(modern_climate_east$tave),]

# Build models
mat_model_tave <- rioja::MAT(
  y = whitmore_picea_east_ws64_pct,
  x = modern_climate_east$tave,
  dist.method ="sq.chord",
  k = 7,
  lean = FALSE
)

mat_model_tmin <- rioja::MAT(
  y = whitmore_picea_east_ws64_pct,
  x = modern_climate_east$tmin,
  dist.method ="sq.chord",
  k = 7,
  lean = FALSE
)

mat_model_tmax <- rioja::MAT(
  y = whitmore_picea_east_ws64_pct,
  x = modern_climate_east$tmax,
  dist.method ="sq.chord",
  k = 7,
  lean = FALSE
)

mat_model_annp <- rioja::MAT(
  y = whitmore_picea_east_ws64_pct,
  x = modern_climate_east$annp,
  dist.method ="sq.chord",
  k = 7,
  lean = FALSE
)

##########################
# H-BLOCK CROSS VALIDATION
##########################

# Methods are from: https://cran.r-project.org/web/packages/palaeoSig/vignettes/h-block-crossvalidation.html
# Data prep - getting long/lat pairs and site names and project
whitmore_site_name <- modern_pollen_picea_east[modern_pollen_picea_east$ID2 %in% as.numeric(rownames(whitmore_picea_east_ws64_pct)), c("ID2", "SITENAME", "LATDD", "LONDD")]

# Create distance matrix and convert to kilometers
dist_matrix <- distm(whitmore_site_name[,c("LONDD", "LATDD")])/1000

# Project using `sp` for autofitVariogram
coordinates(whitmore_site_name) <- ~ LONDD + LATDD
proj4string(whitmore_site_name) <- CRS("+proj=longlat +datum=WGS84")

# TAVE -------------------------------------------------------------------------

# Get residuals from MAT model
mat_tave_resid <- resid(mat_model_tave)

# Detrend with a Loess smoother to remove edge effects
detrended_resid <- resid(loess(mat_tave_resid[, 2] ~ modern_climate_east$tave, span = 0.1))

# Fit variogram to get range estimate
v <- variogram(detrended_resid ~ 1, data = whitmore_site_name)
vm <- fit.variogram(v, vgm(psill = 1, model = "Exp", range = 500, nugget =  1))
plot(v, vm)

mat_model_tave_cross <- rioja::crossval(mat_model_tave, cv.method="h-block", h.cutoff = vm$range[2], h.dist = dist_matrix)

# TMIN -------------------------------------------------------------------------

# Get residuals from MAT model
mat_tmin_resid <- resid(mat_model_tmin)

# Detrend with a Loess smoother to remove edge effects
detrended_resid <- resid(loess(mat_tmin_resid[, 2] ~ modern_climate_east$tmin, span = 0.1))

# Fit variogram to get range estimate
v <- variogram(detrended_resid ~ 1, data = whitmore_site_name)
vm <- fit.variogram(v, vgm(psill = 2.5, model = "Gau", range = 100, nugget =  6))
plot(v, vm)

mat_model_tmin_cross <- rioja::crossval(mat_model_tmin, cv.method="h-block", h.cutoff = vm$range[2], h.dist = dist_matrix)

# TMAX -------------------------------------------------------------------------

# Get residuals from MAT model
mat_tmax_resid <- resid(mat_model_tmax)

# Detrend with a Loess smoother to remove edge effects
detrended_resid <- resid(loess(mat_tmax_resid[, 2] ~ modern_climate_east$tmax, span = 0.1))

# Fit variogram to get range estimate
v <- variogram(detrended_resid ~ 1, data = whitmore_site_name)
vm <- fit.variogram(v[-1,], vgm(psill = 2.5, model = "Gau", range = 750, nugget =  1))
plot(v, vm)

mat_model_tmax_cross <- rioja::crossval(mat_model_tmax, cv.method="h-block", h.cutoff = vm$range[2], h.dist = dist_matrix)

# ANNP -------------------------------------------------------------------------

# Get residuals from MAT model
mat_annp_resid <- resid(mat_model_annp)

# Detrend with a Loess smoother to remove edge effects
detrended_resid <- resid(loess(mat_annp_resid[, 2] ~ modern_climate_east$annp, span = 0.1))

# Fit variogram to get range estimate
v <- variogram(detrended_resid ~ 1, data = whitmore_site_name)
vm <- fit.variogram(v, vgm(psill = 7000, model = "Sph", range = 500, nugget =  4000))
plot(v, vm)

mat_model_annp_cross <- rioja::crossval(mat_model_annp, cv.method="h-block", h.cutoff = vm$range[2], h.dist = dist_matrix)

# Check model performance
performance(mat_model_tave_cross)
performance(mat_model_tmin_cross)
performance(mat_model_tmax_cross)
performance(mat_model_annp_cross)

#######################################
# PREDICT TEMPERATURE AND PRECIPITATION
#######################################

# mclapply helper function
predict_mat <- function(model, newdata) {
  return(predict(model, newdata = newdata, k = 7, sse = TRUE, n.boot = 100))
}

# These calculations take a bit of time so I'm doing them in parallel on 6 cores
# using mclapply. Since mclapply requires a list, we begin the prediction by 
# wrangling the fossil pollen counts into a list
fossil_pollen_list <- list(
  ds49 = ds49_ws64_pct_join$ds49_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gds83 = gds83_ws64_pct_join$gds83_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gds519 = gds519_ws64_pct_join$gds519_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gds520 = gds520_ws64_pct_join$gds520_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gds528 = gds528_ws64_pct_join$gds528_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gds542 = gds542_ws64_pct_join$gds542_ws64_pct[,names(whitmore_picea_east_ws64_pct)],
  gdsw1 = gdsw1_ws64_pct_join$gdsw1_ws64_pct[,names(whitmore_picea_east_ws64_pct)]
)

# tave
tave_all <- mclapply(X = fossil_pollen_list, FUN = predict_mat, model = mat_model_tave_cross, mc.cores = 7)

# tmin
tmin_all <- mclapply(X = fossil_pollen_list, FUN = predict_mat, model = mat_model_tmin_cross, mc.cores = 7)

# tmax
tmax_all <- mclapply(X = fossil_pollen_list, FUN = predict_mat, model = mat_model_tmax_cross, mc.cores = 7)

# annp
annp_all <- mclapply(X = fossil_pollen_list, FUN = predict_mat, model = mat_model_annp_cross, mc.cores = 7)

#############################
# BUILD DATA FRAME OF RESULTS
#############################

# Need to get a filtered version of the Whitmore pollen counts to extract site
# names for analog matching
whitmore_site_name <- modern_pollen_picea_east[modern_pollen_picea_east$ID2 %in% as.numeric(rownames(whitmore_picea_east_ws64_pct)), c("ID2", "SITENAME", "LATDD", "LONDD")]

# Testing filter - SUCCESS
all(whitmore_site_name$ID2 == as.numeric(rownames(whitmore_picea_east_ws64_pct)))

# DS49 -------------------------------------------------------------------------
ds49_results <- data.frame(
  depth = as.numeric(rownames(tave_all$ds49$fit)),
  tave = tave_all$ds49$fit.boot[,"MAT"],
  tave_errors = tave_all$ds49$SEP.boot[,"MAT"],
  tmin = tmin_all$ds49$fit.boot[,"MAT"],
  tmin_errors = tmin_all$ds49$SEP.boot[,"MAT"],
  tmax = tmax_all$ds49$fit.boot[,"MAT"],
  tmax_errors = tmax_all$ds49$SEP.boot[,"MAT"],
  annp = annp_all$ds49$fit.boot[,"MAT"],
  annp_errors = annp_all$ds49$SEP.boot[,"MAT"],
  min_dissim = tave_all$ds49$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$ds49$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$ds49$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$ds49$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$ds49$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$ds49$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$ds49$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$ds49$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$ds49$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$ds49$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$ds49$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$ds49$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$ds49$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$ds49$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$ds49$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$ds49$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$ds49$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$ds49$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$ds49$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$ds49$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$ds49$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$ds49$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$ds49$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$ds49$dist.n[,"N07"]
)

# GDS83 ------------------------------------------------------------------------
gds83_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gds83$fit)),
  tave = tave_all$gds83$fit.boot[,"MAT"],
  tave_errors = tave_all$gds83$SEP.boot[,"MAT"],
  tmin = tmin_all$gds83$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gds83$SEP.boot[,"MAT"],
  tmax = tmax_all$gds83$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gds83$SEP.boot[,"MAT"],
  annp = annp_all$gds83$fit.boot[,"MAT"],
  annp_errors = annp_all$gds83$SEP.boot[,"MAT"],
  min_dissim = tave_all$gds83$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gds83$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gds83$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gds83$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gds83$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gds83$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gds83$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gds83$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gds83$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gds83$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gds83$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gds83$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gds83$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gds83$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gds83$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gds83$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gds83$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gds83$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gds83$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gds83$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gds83$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gds83$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gds83$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gds83$dist.n[,"N07"]
)

# GDS519 -----------------------------------------------------------------------
gds519_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gds519$fit)),
  tave = tave_all$gds519$fit.boot[,"MAT"],
  tave_errors = tave_all$gds519$SEP.boot[,"MAT"],
  tmin = tmin_all$gds519$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gds519$SEP.boot[,"MAT"],
  tmax = tmax_all$gds519$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gds519$SEP.boot[,"MAT"],
  annp = annp_all$gds519$fit.boot[,"MAT"],
  annp_errors = annp_all$gds519$SEP.boot[,"MAT"],
  min_dissim = tave_all$gds519$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gds519$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gds519$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gds519$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gds519$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gds519$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gds519$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gds519$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gds519$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gds519$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gds519$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gds519$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gds519$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gds519$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gds519$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gds519$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gds519$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gds519$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gds519$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gds519$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gds519$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gds519$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gds519$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gds519$dist.n[,"N07"]
)

# GDS520 -----------------------------------------------------------------------
gds520_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gds520$fit)),
  tave = tave_all$gds520$fit.boot[,"MAT"],
  tave_errors = tave_all$gds520$SEP.boot[,"MAT"],
  tmin = tmin_all$gds520$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gds520$SEP.boot[,"MAT"],
  tmax = tmax_all$gds520$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gds520$SEP.boot[,"MAT"],
  annp = annp_all$gds520$fit.boot[,"MAT"],
  annp_errors = annp_all$gds520$SEP.boot[,"MAT"],
  min_dissim = tave_all$gds520$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gds520$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gds520$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gds520$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gds520$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gds520$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gds520$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gds520$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gds520$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gds520$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gds520$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gds520$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gds520$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gds520$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gds520$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gds520$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gds520$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gds520$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gds520$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gds520$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gds520$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gds520$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gds520$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gds520$dist.n[,"N07"]
)

# GDS528 -----------------------------------------------------------------------
gds528_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gds528$fit)),
  tave = tave_all$gds528$fit.boot[,"MAT"],
  tave_errors = tave_all$gds528$SEP.boot[,"MAT"],
  tmin = tmin_all$gds528$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gds528$SEP.boot[,"MAT"],
  tmax = tmax_all$gds528$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gds528$SEP.boot[,"MAT"],
  annp = annp_all$gds528$fit.boot[,"MAT"],
  annp_errors = annp_all$gds528$SEP.boot[,"MAT"],
  min_dissim = tave_all$gds528$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gds528$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gds528$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gds528$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gds528$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gds528$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gds528$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gds528$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gds528$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gds528$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gds528$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gds528$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gds528$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gds528$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gds528$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gds528$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gds528$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gds528$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gds528$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gds528$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gds528$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gds528$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gds528$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gds528$dist.n[,"N07"]
)

# GDS542 -----------------------------------------------------------------------
gds542_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gds542$fit)),
  tave = tave_all$gds542$fit.boot[,"MAT"],
  tave_errors = tave_all$gds542$SEP.boot[,"MAT"],
  tmin = tmin_all$gds542$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gds542$SEP.boot[,"MAT"],
  tmax = tmax_all$gds542$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gds542$SEP.boot[,"MAT"],
  annp = annp_all$gds542$fit.boot[,"MAT"],
  annp_errors = annp_all$gds542$SEP.boot[,"MAT"],
  min_dissim = tave_all$gds542$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gds542$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gds542$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gds542$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gds542$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gds542$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gds542$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gds542$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gds542$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gds542$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gds542$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gds542$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gds542$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gds542$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gds542$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gds542$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gds542$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gds542$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gds542$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gds542$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gds542$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gds542$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gds542$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gds542$dist.n[,"N07"]
)

# GDSW1 ------------------------------------------------------------------------
gdsw1_results <- data.frame(
  depth = as.numeric(rownames(tave_all$gdsw1$fit)),
  tave = tave_all$gdsw1$fit.boot[,"MAT"],
  tave_errors = tave_all$gdsw1$SEP.boot[,"MAT"],
  tmin = tmin_all$gdsw1$fit.boot[,"MAT"],
  tmin_errors = tmin_all$gdsw1$SEP.boot[,"MAT"],
  tmax = tmax_all$gdsw1$fit.boot[,"MAT"],
  tmax_errors = tmax_all$gdsw1$SEP.boot[,"MAT"],
  annp = annp_all$gdsw1$fit.boot[,"MAT"],
  annp_errors = annp_all$gdsw1$SEP.boot[,"MAT"],
  min_dissim = tave_all$gdsw1$diagnostics$minD,
  analog_ID2_1 = as.numeric(tave_all$gdsw1$match.name[,"N01"]),
  analog_1 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N01"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_1_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N01"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_1_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N01"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_1 = tave_all$gdsw1$dist.n[,"N01"],
  analog_ID2_2 = as.numeric(tave_all$gdsw1$match.name[,"N02"]),
  analog_2 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N02"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_2_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N02"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_2_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N02"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_2 = tave_all$gdsw1$dist.n[,"N02"],
  analog_ID2_3 = as.numeric(tave_all$gdsw1$match.name[,"N03"]),
  analog_3 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N03"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_3_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N03"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_3_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N03"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_3 = tave_all$gdsw1$dist.n[,"N03"],
  analog_ID2_4 = as.numeric(tave_all$gdsw1$match.name[,"N04"]),
  analog_4 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N04"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_4_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N04"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_4_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N04"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_4 = tave_all$gdsw1$dist.n[,"N04"],
  analog_ID2_5 = as.numeric(tave_all$gdsw1$match.name[,"N05"]),
  analog_5 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N05"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_5_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N05"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_5_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N05"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_5 = tave_all$gdsw1$dist.n[,"N05"],
  analog_ID2_6 = as.numeric(tave_all$gdsw1$match.name[,"N06"]),
  analog_6 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N06"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_6_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N06"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_6_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N06"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_6 = tave_all$gdsw1$dist.n[,"N06"],
  analog_ID2_7 = as.numeric(tave_all$gdsw1$match.name[,"N07"]),
  analog_7 = sapply(as.numeric(tave_all$gdsw1$match.name[,"N07"]), function(x) whitmore_site_name$SITENAME[x == whitmore_site_name$ID2]),
  analog_7_LATDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N07"]), function(x) whitmore_site_name$LATDD[x == whitmore_site_name$ID2]),
  analog_7_LONDD = sapply(as.numeric(tave_all$gdsw1$match.name[,"N07"]), function(x) whitmore_site_name$LONDD[x == whitmore_site_name$ID2]),
  dissim_7 = tave_all$gdsw1$dist.n[,"N07"]
)

#############
# SAVE AS CSV
#############

write.csv(ds49_results, "results/ds49_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gds83_results, "results/gds83_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gds519_results, "results/gds519_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gds520_results, "results/gds520_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gds528_results, "results/gds528_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gds542_results, "results/gds542_results_hblock_no_poaceae.csv", row.names = FALSE)
write.csv(gdsw1_results, "results/gdsw1_results_hblock_no_poaceae.csv", row.names = FALSE)

######
# PLOT
######

# Depth appears to be in meters for DS49, so a quick fix
ds49_results$depth <- ds49_results$depth * 100

# Add site ID
ds49_results$site <- "DS49"
gds83_results$site <- "GDS83"
gds519_results$site <- "GDS519"
gds520_results$site <- "GDS520"
gds528_results$site <- "GDS528"
gds542_results$site <- "GDS542"
gdsw1_results$site <- "GDSW1"

# Bind into a single data frame
all_recon <- rbind(
  ds49_results,
  gds83_results,
  gds519_results,
  gds520_results,
  gds528_results,
  gds542_results,
  gdsw1_results
)

# Setting theme
theme <- 
  theme_bw() + 
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.key = element_blank(),
        panel.spacing.y = unit(-.1, "cm"))
theme_set(theme)

# Create data frame of depth where Pine changes from north to south
pine_split <- data.frame(site = c("DS49", "GDS83", "GDS519", "GDS520", "GDS528", "GDS542", "GDSW1"),
           depth = c(220.5, NA, 336, 280, 200.5, 102.5, 349),
           value = NA) %>% 
  crossing(variable = c("tave", "tmin", "tmax", "annp"))

# Plot - first pivot to a narrow data format for friendlier ggploting
all_recon_plot <- all_recon %>% 
  select(depth, tave, tmin, tmax, annp, site) %>% 
  pivot_longer(!c(site, depth), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = depth, y = value)) + 
  geom_line() + 
  geom_vline(data = pine_split, aes(xintercept = depth), color = "gray", linetype = "dashed") + 
  xlab("Depth (cm)") +
  facet_wrap(~ site + variable, ncol = 4, scales = "free")

# Save
ggsave(all_recon_plot, filename = "diagnostics/all_recon.pdf", height = 10, width = 10, dpi = 300)
