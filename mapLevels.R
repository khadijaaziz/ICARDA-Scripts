
pacman::p_load(raster, rasterVis, maptools, tidyverse, xlsx, latticeExtra, sp)

rst  <- final_ras


# rst[rst[] >= 0.7] <- 1 #high
# rst[rst[] < 0.7 & rst[] >= 0.5] <- 0 #medium
# rst[rst[] != 1 & rst[] != 0] <- NA
# table(rst[])

# Loading country shapefile
data(wrld_simpl)

# Creating reference lines
grat <<- sp::gridlines(wrld_simpl, easts = seq(-180, 180, by = 10), norths = seq(-90, 90, by = 15))


#prepare raster
rst <- ratify(rst)
rat <- levels(rst)[[1]]
rat$level <- c('Medium prob + GAP', 'High prob + GAP')
levels(rst) <- rat

#figure details
ht <- 12
fct <- (rst@extent@xmin-rst@extent@xmax)/(rst@extent@ymin-rst@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=10), norths=seq(-90,90,by=15))

#control colors
colors = list(one_cols =  c('red2'), two_cols =  c('grey70', 'red2') , three_cols = c('grey70', 'goldenrod3', 'red2'))

#control the color panel denpending of the number of levels
#if(nrow(rat) == 1){cols <- colors$one_cols}
if(nrow(rat) == 2){cols <- colors$two_cols}
#if(nrow(rat) == 3){cols <- colors$three_cols}

# theme
library(RColorBrewer)
mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                    panel.background = list(col = "#f5f5dc"))

#produce levelplot
p <- rasterVis:::levelplot(rst,
                           att = 'level',
                           margin = F,
                           par.settings = mThm,
                           col.regions = cols,
                           maxpixels = ncell(rst)) + 
    latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
    latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#4B4B4B"), under = T)

png(paste0("D:/ICARDA/trait analysis results/trait analysis/", crop_name,'/PNG/',trait1_name, "_GAP.png"), height = 7, width = 10, units = "in", res = 300)
print(p)
dev.off()

