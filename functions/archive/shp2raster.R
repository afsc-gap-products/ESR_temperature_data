shp2raster <- function(shp, mask.raster, label, value = 0, transform = FALSE, proj.from = NA,
    proj.to = NA, map = TRUE) {

    require(raster, rgdal)
 
    # use transform==TRUE if the polygon is not in the same coordinate system as
    # the output raster, setting proj.from & proj.to to the appropriate
    # projections
    if (transform == TRUE) {
        proj4string(shp) <- proj.from
        shp <- spTransform(shp, proj.to)
    }
 
    # convert the shapefile to a raster based on a standardised background
    # raster
    r <- rasterize(shp, mask.raster)
    # set the cells associated with the shapefile to the specified value
    r[!is.na(r)] <- value
    # merge the new raster with the mask raster and export to the working
    # directory as a tif file
    r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
        overwrite = T)
 
    # plot map of new raster
    if (map == TRUE) {
        plot(r, main = label, axes = F, box = F)
    }
 
    names(r) <- label
    return(r)
}

library(maptools)
library(raster)
 
## example: import world raster from package biomod2 and set the background
## values to zero
worldRaster <- raster(system.file("external/bioclim/current/bio3.grd", package = "biomod2"))
worldRaster[!is.na(worldRaster)] <- 0
plot(worldRaster, axes = F, box = F, legend = F, main = "The world")
 
# import world polygon shapefile from package maptools
data(wrld_simpl, package = "maptools")
plot(wrld_simpl, add = T)

# extract all Australian polygons and convert to a world raster where cells
# associated with Australia have a value of 1 and everything else has a
# value of 0.
australia <- shp2raster(shp = wrld_simpl[grepl(c("Australia";), wrld_simpl$NAME), ],
    mask.raster = worldRaster, label = "Where Amy currently lives", transform = FALSE, value = 1)
 
## Found 1 region(s) and 97 polygon(s)

# extract Australia, NZ & USA and convert to a world raster where cells
# associated with these countries have a value of 3 and everything
# else has a value of 0.
aus.nz.us <- shp2raster(shp = wrld_simpl[grepl(c("Australia|New Zealand|United States"),
    wrld_simpl$NAME), ], mask.raster = worldRaster, label = "All countries Amy has lived in",
    transform = FALSE, value = 3)
 
## Found 5 region(s) and 384 polygon(s)

# set relevant projections
GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
GDA94 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
 
LH.mask <- raster("LH.mask.tif")
# set the background cells in the raster to 0
LH.mask[!is.na(LH.mask)] <- 0
 
NPWS.reserves <- readShapePoly("NPWSReserves.shp", proj4 = GDA94)
 
# convert the NPWS.reserves polygon data for National Parks & Nature
# Reserves to a raster, after changing the projection.
NPWS.raster <- shp2raster(shp = NPWS.reserves[grepl(c("NP|NR"), NPWS.reserves$reservetyp),],
    mask.raster = LH.mask, label = "National Parks & Nature Reserves", value = 3,
    transform = TRUE, proj.from = GDA94, proj.to = GDA94.56)
 
## Found 111 region(s) and 837 polygon(s)
