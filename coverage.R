
# Reading rasters
require(raster)

#read rasters
wmask <- raster("./mask_world.tif")

sdm_model<- raster("./durum_sdm_all.tif")

trait_rast <- raster("./ST.S.Raster.tif")

#load data
load(".")

# Define the threshold
lower <- mean(ST.S.output$Prediction) + sd(ST.S.output$Prediction)
upper <- mean(ST.S.output$Prediction) 


# Preparing rasters
trait_rast<- resample(trait_rast,wmask,method='ngb')
plot(trait_rast)

# sumRas <- summary(trait_rast)
# sdRas <- calc(trait_rast, sd)
sdm_model<- resample(sdm_model,wmask,method='ngb')

# Calculate the coverage Coverage
rast_area <- raster::area(wmask) * wmask
sdm_mask <- raster::mask(wmask, sdm_model)

sdm_area <- sdm_mask * rast_area
total_sdm_area <- sum(sdm_area[], na.rm = TRUE)


#Calculate the High Confidence coverage

high_conf <- trait_rast
high_conf[which(high_conf[] >= upper)] <- 1
high_conf[which(high_conf[] < upper)] <- NA
high_conf <- high_conf * rast_area
total_hg_conf <- sum(high_conf[], na.rm = TRUE)

high_conf <-  (total_hg_conf/total_sdm_area)

rm(high_conf);g <- gc(); rm(g)


#Calculate the Low Confidence coverage 

low_conf <- trait_rast
low_conf[which(low_conf[] >= lower)] <- 1
low_conf[which(low_conf[] <lower)] <- NA
low_conf <- low_conf * rast_area
total_lw_conf <- sum(low_conf[], na.rm = TRUE)

low_conf <-  (total_lw_conf/total_sdm_area)

rm( rast_area, sdm_mask );g <- gc(); rm(g)

#Print the results
cat("High Confidence coverage: ",high_conf, sep = '')
cat("Low Confidence coverage: ",low_conf, sep = '')
