#############
# ENVIRONMENT
#############

# This information isn't very important right now, but maybe if this script is
# referenced years after it is written because R packages get updated.
# 
# Operating System: Ubuntu 20.04.4 LTS x86_64
# R Version: 4.1.2 (2021-11-01) "Bird Hippie"
# 
# Author: David Fastovich, fastovich@wisc.edu
# Date: 3/17/2022

library(vegan) # 2.5-7

# Set working directory
setwd("~/Documents/GitHub/dismal_swamp_analyses/")

#####################
# READ IN EXCEL FILES
#####################

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

# Want a data frame of just the counts only and a separate data frame with all
# of the metadata
modern_pollen_picea_east_counts <- modern_pollen_picea_east[,14:147]
rownames(modern_pollen_picea_east_counts) <- modern_pollen_picea_east$ID2

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
modern_pollen_picea_east_counts$PINDIPLO.NE <- modern_pollen_picea_east_counts$PINDIPLO * northeast_pinus
modern_pollen_picea_east_counts$PINHAPLO.NE <- modern_pollen_picea_east_counts$PINHAPLO * northeast_pinus
modern_pollen_picea_east_counts$PINUSX.NE <- modern_pollen_picea_east_counts$PINUSX * northeast_pinus

modern_pollen_picea_east_counts$PINDIPLO.SE <- modern_pollen_picea_east_counts$PINDIPLO * southeast_pinus
modern_pollen_picea_east_counts$PINHAPLO.SE <- modern_pollen_picea_east_counts$PINHAPLO * southeast_pinus
modern_pollen_picea_east_counts$PINUSX.SE <- modern_pollen_picea_east_counts$PINUSX * southeast_pinus

# After applying these splits we need to set the unsplit column to 0 for sites
# that have a Pinus split. This will retain the unsplit Pinus column names for
# sites that do not have a Pinus split identifier. This is accomplished by adding
# the boolean Pinus split sites together and every site that has a value of 0
# means it does not have a split identifier. I'm also using this opportunity to
# remove sites that have both a southeast and northeast identifier - 48 sites
# in total.
split_pinus_sites <- northeast_pinus + southeast_pinus

# Sites that have a Pinus split have the unsplit columns set to 0
modern_pollen_picea_east_counts$PINDIPLO[split_pinus_sites != 0] <- 0
modern_pollen_picea_east_counts$PINHAPLO[split_pinus_sites != 0] <- 0
modern_pollen_picea_east_counts$PINUSX[split_pinus_sites != 0] <- 0

# Remove sites that are identified as NE and SE Pinus
modern_pollen_picea_east_counts <- modern_pollen_picea_east_counts[split_pinus_sites != 2,]

##############################
# TEST THE SPLITTING PROCEDURE
##############################

# Splits are tested to make sure that the Pinus rowsums are unchanged
pinus_columns <- grep("PIN", colnames(modern_pollen_picea_east_counts))

filtered_unmofidifed_counts <- modern_pollen_picea_east[split_pinus_sites != 2,]
all(rowSums(modern_pollen_picea_east_counts[, pinus_columns]) == rowSums(filtered_unmofidifed_counts[,c("PINDIPLO", "PINHAPLO", "PINUSX")]))

##############################################
# EXTRACT CLIMATE DATA FOR CORRESPONDING SITES
##############################################

# Filter climate to only keep sites that we are analyzing
modern_climate_east <- modern_climate[modern_climate$ID2 %in% as.numeric(rownames(modern_pollen_picea_east_counts)), ]

# Testing filter - SUCCESS
all(modern_climate_east$ID2 == as.numeric(rownames(modern_pollen_picea_east_counts)))

# There's a single new surface sample site without coordinates that has NA's
# for climate variables. Removing now.
modern_pollen_picea_east_counts <- modern_pollen_picea_east_counts[!is.na(modern_climate_east$tave),]
modern_climate_east <- modern_climate_east[!is.na(modern_climate_east$tave),]

################################
# ASSESS RECONSTRUCTED VARIABLES
################################

# Hellinger transform the abundance data per Legendre and Gallagher (2001)
whitmore_picea_east_hellinger <- decostand(modern_pollen_picea_east_counts, method = "hellinger")

# Perform RDA
whitmore_rda <- rda(
  whitmore_picea_east_hellinger ~ 
    modern_climate_east$tave + 
    modern_climate_east$tmin + 
    modern_climate_east$tmax + 
    modern_climate_east$annp
  )

# Plot results
# 
# Takeaways:
# 
# 1. Average temperature is primarily loaded onto RDA1 but is positively
# correlated with all other climate variables
# 2. Tmin is loaded heavily onto RDA1 and somewhat on RDA2. Tmax is loaded in
# the opposite direction on RDA1 and 2 which suggests that RDA2 is seasonality.
# This interpretation is made all the stronger by the fact that Tave has near 0
# loading on RDA2, since its the average temperature with 0 seasonality.
# 3. It looks like RDA1 is a general "climate" axis, but most heavily influenced
# by average temperature then seasonality, the precipitation.
# 4. Precipitation does not appear to be loaded very much on any of the axes.
pdf(height = 7, width = 7, "modern_pollen_rda.pdf")
plot(whitmore_rda, scaling = 2, choices = c(1, 2), type = "text")
dev.off()

# Test for significance
# 
# All significant! But interpretation is complicated given the findings from
# above.
anova.cca(whitmore_rda, permutations = 999, by = "terms")

# Test for mulitcollineary
# 
# High multicollinearity...
vif.cca(whitmore_rda)

# Variance partitioning
# 
# Most variance is shared, likely because we're using three tempeature variables
whitmore_varpart <- varpart(
  modern_pollen_picea_east_counts,
  modern_climate_east$tave,
  modern_climate_east$tmin,
  modern_climate_east$tmax,
  modern_climate_east$annp,
  transfo = "hellinger"
  )

variance_partition <- data.frame(
  variance_explained = whitmore_varpart$part$fract[1:4,3],
  unique_variance_explained = whitmore_varpart$part$indfract[1:4,3],
  variable = c("tave", "tmin", "tmax", "annp")
  )

# Create stacked barplot
pdf(height = 5, width = 5, "modern_pollen_variance_partitioning.pdf")
barplot(
  t(
    as.matrix(
      variance_partition[,1:2]
      )
    ),
  names.arg = variance_partition$variable,
  ylim = c(0, 0.2),
  ylab = "Variance Explained (%)",
  xlab = "Climate Variable",
  col = c("#4D4D4D", "#E6E6E6")
  )

legend(
  x = "topright",
  legend = c("Independent", "Shared"),
  fill = c("#4D4D4D", "#E6E6E6"),
  border = "black"
  )
dev.off()