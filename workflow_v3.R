#setwd("D:/")


### load required packages ###
require(raster)

### reading rasters
trait  <- raster("D:/ICARDA/durum/Mapping/cold.tolerant.tif")

#sdm <- raster("Coverage/SDM Gap Rasters/asian rice/sdm_japonica.tif")

gap <- raster("D:/SDM Gap Rasters/durum/durum_gap_class_all.tif")


## trait raster
trait[trait[] < 0.5] <- NA
trait[trait[] < 0.7] <- 0 #Medium
trait[trait[] >= 0.7] <- 1 #High
table(trait[])

### sdm raster
# sdm[sdm[] <= 0.7] <- NA
# sdm[!is.na(sdm[]) ] <- 1
# table(sdm[])

### gap raster
gap[gap[] == 0] <- NA
gap[gap[] != 0] <- 1
table(gap[])

### intersection
#sdm <- raster::crop(sdm, extent(trait))
gap <- raster::crop(gap, extent(trait))

#hotspost <- sum(trait, sdm, na.rm = F )
#final_raster <- sum(hotspost, gap, na.rm = F ) # level 2 = medium, level 3 = high
final_ras <- sum(trait, gap, na.rm = F )
table(final_ras[])
# plot(final_raster)

###
trait1_name <- 'coldTolerance'
crop_name <- 'durum'
#writeRaster(hotspost, filename=paste('D:/ICARDA/trait analysis results/trait analysis/durum/', crop_name,'/raster/',trait1_name,'_SDM.tif',sep = ''), overwrite=TRUE)
writeRaster(final_ras, filename=paste('D:/ICARDA/trait analysis results/trait analysis/', crop_name,'/raster/',trait1_name,'_GAP.tif',sep = ''), overwrite=TRUE)

save.image(file = paste('D:/ICARDA/trait analysis results/trait analysis/', crop_name,'/RData/',trait1_name,'.RData',sep = ''))
