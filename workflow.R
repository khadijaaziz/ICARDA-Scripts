#' @title Intersection of trait raster with SDM and gap rasters
#' @description Return a plot of the intersection.
#' @param trait_path expression. The path to the trait raster.
#' @param sdm_path expression. The path to the SDMraster.
#' @param gap_path expression. The path to the Gap raster.
#' @return A list that contains a plot of the intersection.
#' @details \code{workflow} 
#' 
#' 
#' 
#' @author Zakaria Kehel, Khadija Aziz
#' @examples
#' \dontrun{
#' p <- workflow(trait_path = "./CoveredRaster.tif",sdm_path = "./sdm_six-rowed.tif",gap_path = "./gap_class_six-rowed.tif")
#' }
#' @rdname workflow


workflow <- function(trait_path , sdm_path , gap_path
){
    
    ### Install and load required packages ###
    suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
    suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
    suppressMessages(if(!require(rgeos)){install.packages("rgeos"):library(rgeos)}else{library(rgeos)})
    suppressMessages(if(!require(rworldmap)){install.packages("rworldmap"):library(rworldmap)}else{library(rworldmap)})
    
    
    cat("Reading raters...", "\n")
    trait  <- raster(trait_path)
    
    sdm <- raster(sdm_path)
    
    gap <- raster(gap_path)
    
    cat("Transform the trait raster to plygons...", "\n")
    r1 <- trait > -Inf
    traiShp <- rasterToPolygons(r1, fun=NULL,n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
    proj4string(traiShp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    cat("Transform the SDM raster to plygons...", "\n")
    r2 <- sdm > -Inf
    sdmShp <- rasterToPolygons(r2, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
    proj4string(sdmShp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    cat("Transform the Gap raster to plygons...", "\n")
    r3 <- gap > -Inf
    gapShp <- rasterToPolygons(r3, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
    proj4string(gapShp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    ### Selecting the intersections ###
    cat("Intersection...", "\n")
    hotspot0 <- gIntersection(traiShp, sdmShp, byid=TRUE,drop_lower_td=F)
    Finalintersection <- gIntersection(hotspot0,gapShp, byid=TRUE,drop_lower_td=F)
    
    ### crop and mask ###
    crp <- crop(trait, extent(Finalintersection))
    msk <- mask(crp, Finalintersection)
    
    ### Plot the final result ###
    worldmap <- getMap(resolution = "coarse")
    plot(worldmap, col = "lightgrey",
                fill = T, border = "darkgray",
                xlim = c(-180, 180), ylim = c(-90, 90),
                bg = "aliceblue",main = "World Map",
                asp = 1, wrap=c(-180,180),lwd=0.5,xlab="Longitude",ylab="Latitude")
    
    plot(msk,add=TRUE,zoom=3)
    p <- recordPlot()
    

    return (list(`p`=p))
}
