# establish projection
aea.prj <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

AI.area <- raster("G:/AI-GOA/rasters/aiarea")

# read in shapefile
test.mask <- readShapePoly("G:/AI-GOA/shapefiles/AIdissolved_erase_dnr.shp", proj4 = aea.prj)

# convert shape file to raster
converted.mask <- shp2raster(shp = test.mask, mask.raster = AI.bathy)

plot(AI.bathy, add = TRUE)

AI.area <- raster("G:/AI-GOA/rasters/aiarea")
	