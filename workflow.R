
workflow <- function(trait_path , sdm_path , gap_path){
    
    ### Install and load required packages ###
    suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
    suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
    suppressMessages(if(!require(rgeos)){install.packages("rgeos"):library(rgeos)}else{library(rgeos)})
    suppressMessages(if(!require(rworldmap)){install.packages("rworldmap"):library(rworldmap)}else{library(rworldmap)})
    
    
    cat("Reading raters...", "\n")
    trait  <- raster(trait_path)
    
    sdm <- raster(sdm_path)
    
    gap <- raster(gap_path)
    
    
    cat("Transform the SDM raster to plygons...", "\n")
    r2 <- sdm > -Inf
    rclass <- rasterToPolygons(r2, fun = function(x){x < 150}, na.rm=TRUE)
    rclass <- as(rclass, "sf")
    rclass <- sf::st_union(rclass)
    rclass <- as(rclass, "Spatial")
    sdmShp <- rclass
    
    g <- gc(); rm(g)
    
    #Remove first level of the Gap and keep other levels
    #gap[which(gap[] != 0)] <- 1
    gap[which(gap[] == 0)] <- NA
    
    cat("Transform the Gap raster to plygons...", "\n")
    r3 <- gap > -Inf
    rclass <- raster::rasterToPolygons(r3, fun = function(x){x < 150}, na.rm=TRUE)
    rclass <- as(rclass, "sf")
    rclass <- sf::st_union(rclass)
    rclass <- as(rclass, "Spatial")
    gapShp <- rclass
    
    g <- gc(); rm(g)
    
    ### Selecting the intersections ###
    cat("Intersection...", "\n")
    hotspot <- intersect(trait, sdmShp)
    Finalintersection <- intersect(hotspot, gapShp)
    
    g <- gc(); rm(g)
    
    #writeRaster(Finalintersection, filename='.Raster.tif', overwrite=TRUE)
    return(Finalintersection)
    
}
