#############
# ENVIRONMENT
#############

# Operating System: Ubuntu 20.04.4 LTS x86_64
# R Version: 4.1.2 (2021-11-01) "Bird Hippie"
# 
# Author: David Fastovich, fastovich@wisc.edu
# Date: 3/17/2022

library(openxlsx) # 4.2.5
library(neotoma) # 1.7.4
library(dplyr) # 1.14.2

# Set working directory
setwd("~/Documents/GitHub/dismal_swamp_analyses/")

##############
# READ IN DATA
##############

whitmore_counts <- read.xlsx(
  "pollen_counts/whitmoreetal2005_v1-8.xlsx",
  sheet = "POLLEN DATA"
  )
to_whitemore <- read.csv("taxa_conversion_list/to_whitmore.csv")
roanoke_surface <- read.xlsx("pollen_counts/Roanoke_surface_for_tilia.xlsx")
dismal_swamp_surface <- read.xlsx("pollen_counts/GDS_surface_for_tilia.xlsx")

# Replace NA's with 0's
roanoke_surface[is.na(roanoke_surface)] <- 0
dismal_swamp_surface[is.na(dismal_swamp_surface)] <- 0

############################
# RESHAPE NEW SURFACE COUNTS
############################
#
# The Whitmore modern pollen database has taxa as columns and sites as rows. To
# merge the new surface coutns with the Whitmore surface counts we need to
# massage the new data into the correct shape.

# Transpose and then convert to data frame
dismal_swamp_surface_t <- as.data.frame(
  t(
    dismal_swamp_surface[, -1]
    )
  )

# Assign species names as columns while preserving the correct order
names(dismal_swamp_surface_t) <- dismal_swamp_surface[, 1]

# Transpose and then convert to data frame
roanoke_surface_t <- as.data.frame(
  t(
    roanoke_surface[, -1]
    )
  )
# Assign species names as columns while preserving the correct order
names(roanoke_surface_t) <- roanoke_surface[, 1]

#########################
# COMPILE SURFACE SAMPLES
#########################
# 
# There are taxonomic resolution mismatches between the the Whitmore modern
# pollen database and the surface counts. Our counts are higher resolution than
# the Whitmore database. To fix this, I am using `compile_taxa` from the
# `neotoma` R package to rename our taxa to the Whitmore naming schema and
# combine any taxa that belong to higher orders in the Whitmore database.
# I accomplish this by providing my own conversion list to aggregate taxa in
# from Roanoke and Dismal Swamp.
# 
# I have gone through all errors and taxa that are "errors" do not exist in
# the Whitmore database and are therefore removed in this process.

# This chunk performs the operation outlined above and saves the warnings as a
# text file
warning_message <- list()
withCallingHandlers(
  {
    dismal_whitmore <- as.data.frame(
      compile_taxa(dismal_swamp_surface_t, alt.table = to_whitemore, list.name = "to_whitmore")
    )
  },
  warning = function(w) {
    warning_message[[1]] <<- w$message
    invokeRestart("muffleWarning")
  }
) # Added this wrapper to capture the warnings from compile_taxa for review

write.table(warning_message, file = "dismal_surface_taxa_not_in_whitmore.txt")

warning_message <- list()
withCallingHandlers(
  {
    roanake_whitmore <- as.data.frame(
      compile_taxa(roanoke_surface_t, alt.table = to_whitemore, list.name = "to_whitmore")
    )
  },
  warning = function(w) {
    warning_message[[1]] <<- w$message
    invokeRestart("muffleWarning")
    }
  ) # Added this wrapper to capture the warnings from compile_taxa for review

write.table(warning_message, file = "roanoke_surface_taxa_not_in_whitmore.txt")

#######################
# MERGING WITH WHITMORE
#######################
# 
# Now that taxon names and data shape match we can combine our new surface
# samples into the Whitmore database

# Adding SITE column to new surface samples for analog tracking
dismal_whitmore$SITENAME <- paste("Dismal Swamp Surface", names(dismal_swamp_surface)[-1])
roanake_whitmore$SITENAME <- paste("Roanoke Surface Treetag", names(roanoke_surface)[-1])

# Adding ID2 column
dismal_whitmore$ID2 = seq_len(length(dismal_whitmore$SITENAME)) + max(whitmore_counts$ID2)
roanake_whitmore$ID2 = seq_len(length(roanake_whitmore$SITENAME)) + max(dismal_whitmore$ID2)

# bind_rows so we don't have to create missing columns in dismal_whitmore
whitmore_expanded <- bind_rows(whitmore_counts, dismal_whitmore)
whitmore_expanded <- bind_rows(whitmore_expanded, roanake_whitmore)

# bind_rows fills with NAs, lets make those 0's
whitmore_expanded[is.na(whitmore_expanded)] <- 0

# Save as CSV file
write.csv(whitmore_expanded, file = "results/whitmore_surface_expanded.csv", row.names = FALSE)