
grph_dir <- "D:/ICARDA/trait analysis results/coverage/durum/PNG/"
traitName <- "coldTolerance.GAP"

#require packages
pacman::p_load(raster, rasterVis, maptools, tidyverse, xlsx, latticeExtra, sp)


# Loading country shapefile
data(wrld_simpl)

# Creating reference lines
grat <<- sp::gridlines(wrld_simpl, easts = seq(-180, 180, by = 10), norths = seq(-90, 90, by = 15))

# Crop raster according with the extent function
r_temp <- TraitGAP
xyp <- raster::rasterToPoints(r_temp)
ext <- raster::extent(xyp[,1:2])
ext <- raster::alignExtent(ext, r_temp, snap = 'out')
r_temp <- raster::crop(r_temp, ext)

# theme
library(RColorBrewer)
mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                    panel.background = list(col = "#f5f5dc"))


if(r_temp@data@max <= 1){
    p <- rasterVis::levelplot(r_temp,
                              at = seq(0, 1, .05),
                              margin = F,
                              par.settings = mThm,
                              maxpixels = ncell(r_temp)) + 
        latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
        latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#717171"), under = T)
    
} else {
    p <- rasterVis::levelplot(r_temp,
                              att = 'level',
                              margin = F,
                              par.settings = mThm,
                              maxpixels = ncell(r_temp)) +
        latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
        latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#717171"), under = T)

}
g <- gc(); rm(g)

png(paste0(grph_dir, "/", traitName, ".png"), height = 7, width = 10, units = "in", res = 300)
print(p)
dev.off()
