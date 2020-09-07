
# Reading rasters
require(raster)

#read rasters
wmask <- raster("./mask_world.tif")

sdm_model<- raster("./durum_sdm_all.tif")

trait_rast <- raster("./ST.S.Raster.tif")

# Load RData
load(".")
data <- '' #put the loaded data in this value
# Define the threshold
xmin <- extent(sdm_model)[1]
xmax <- extent(sdm_model)[2]
ymin <- extent(sdm_model)[3]
ymax <- extent(sdm_model)[4]

data <- data[data$Longitude <= xmax & data$Longitude >= xmin ,]
data <- data[data$Latitude <= ymax & data$Latitude >= ymin ,]

lower <- mean(data$Prediction) + sd(data$Prediction)
upper <- mean(data$Prediction) 

# Preparing rasters
trait_rast<- resample(trait_rast,wmask,method='ngb')

sdm_model<- resample(sdm_model,wmask,method='ngb')


# Calculate the SDM area
rast_area <- raster::area(wmask) * wmask
sdm_mask <- raster::mask(wmask, sdm_model)
sdm_area <- sdm_mask * rast_area
total_sdm_area <- sum(sdm_area[], na.rm = TRUE)


#Calculate the High Confidence coverage
high_conf <- trait_rast
high_conf[which(high_conf[] >= upper)] <- 1
high_conf[which(high_conf[] < upper)] <- NA
high_conf <- high_conf * rast_area
high_conf <- raster::mask(high_conf, mask = sdm_mask)
total_hg_conf <- sum(high_conf[], na.rm = TRUE)

high_conf_percent <-  (total_hg_conf/total_sdm_area)*100

g <- gc(); rm(g)


#Calculate the Low Confidence coverage 
low_conf <- trait_rast
low_conf[which(low_conf[] >= lower)] <- 1
low_conf[which(low_conf[] <lower)] <- NA
low_conf <- low_conf * rast_area
low_conf <- raster::mask(low_conf, mask = sdm_mask)
total_lw_conf <- sum(low_conf[], na.rm = TRUE)

low_conf_percent <-  (total_lw_conf/total_sdm_area)*100

g <- gc(); rm(g)

cat("High Confidence coverage: ",high_conf_percent,'%', sep = '')
cat("Low Confidence coverage: ",low_conf_percent,'%', sep = '')

#plot the Trait in SDM area
#TraitSDM <- raster::mask(trait_rast, mask = sdm_mask)
#then plot the traitSDM using mapPNG function
