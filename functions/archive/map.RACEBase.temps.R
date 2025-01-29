map.RACEBase.temps <- function (channel = NA){

	#############################################################################
	## Function name:  map.RACEBase.temps
	## Author: N. Laman
	## BornOnDate: 09/21/2018
	##
	## What it does:
	## Queries RACEBase.HAUL for GEAR_TEMPERATURE (BOTTOM_TEMPERATURE) and
	## SURFACE_TEMPERATURE by REGION.
	## Collects all years' data for that region from a pre-determined min(CRUISE)
	## to present.
	## Maps median-corrected gear and surface temperatures on globe.  
	##
	## What it doesn't do:
	## The query is not constrained to good PERFORMANCE or standard HAUL_TYPE so does
	## not eliminate "bad" tows. It has built in year minima by survey REGION 
	##
	## Dependencies:
	## Libraries - RODBC, mgcv, 
	## Functions - get.RACEBase.temps
	## 
	## Returns:  Geo-referenced Gear and Surface temperatures on maps
	##
	###################################################################################

	## Libraries
	pkgTest("mgcv")
	pkgTest("raster")
	pkgTest("rgdal")
	pkgTest("sp")
	pkgTest("maptools")
	pkgTest("rgeos")
	pkgTest("maps")
	pkgTest("gstat")
	pkgTest("proj4")
	pkgTest("viridis")
	
	## Enter Survey REGION
	survey <- readline("Enter Survey Designation (AI, BS, or GOA):  "); survey <- toupper(survey)
	usr.nm <- readline("Enter Oracle user name: "); usr.nm <- tolower(usr.nm)
	pwd <- readline("Enter Oracle password: "); pwd <- tolower(pwd)

	## select and assign global variable for filepath to where output will be stored
	# file.target <<- choose.dir(caption = "Select destination for program output: ")
	
	# for testing
	file.target <<- "F:/AI_GOA/EcosystemConsiderationsChapter/2018"
	
	# establish projection for maps
	aea.prj <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"

	# test for valid regions and set minimum year for data by region
	if(survey %ni% c("AI", "GOA", "BS")){
		cat(paste(" \n", survey, "survey is not supported in this function", " \n", sep = " "))
		break
	}else{
		if(survey == "AI"){
			min.yr = 1991
			# AI map extent INFO
			AI.xaxis<-c(174,176,178,180,-178,-176,-174,-172,-170,-168,-166)
			AI.yaxis<-c(47,49,51,53)
			AI.yaxis.ticks<-cbind(c(174,172.5,170.75,168.75),AI.yaxis)
			AI.xaxis.ticks<-cbind(AI.xaxis,c(46.9,47.55,48.25,48.85,49,49.5,50.25,50.5,50.75,51.25,51.5))
			AI.yaxis.ticks<-project(AI.yaxis.ticks,aea.prj)
			AI.xaxis.ticks<-project(AI.xaxis.ticks,aea.prj)
			AI.yaxis.ticks<-AI.yaxis.ticks[,2]
			AI.xaxis.ticks<-AI.xaxis.ticks[,1]
			AI.ext.x<-c(-164.95,-164.95,171,171,-164.95)
			AI.ext.y<-c(54.5,50.5,50.5,54.5,54.5)
			AI.ext.pol<-Polygon(cbind(AI.ext.x,AI.ext.y))
			AI.ext.pol<-Polygons(list(AI.ext.pol),"AI")
			AI.ext.pol<-SpatialPolygons(list(AI.ext.pol), proj4string=CRS("+proj=longlat +datum=WGS84"))
			AI.ext.pol.proj<-spTransform(AI.ext.pol,CRS(aea.prj))
			}
			
		if(survey == "GOA"){
			min.yr = 1991
			# GOA map extent INFO
			GOA.xaxis<-c(-166,-162,-158,-154,-150,-146,-142,-138,-134,-130)
			GOA.yaxis<-c(55,57,59,61)
			GOA.yaxis.ticks<-cbind(c(-163,-163.5,-164,-165),GOA.yaxis)
			GOA.xaxis.ticks<-cbind(GOA.xaxis,c(52.75,53.25,53.5,53.75,54.15,53.15,53,52.35,51.75,54.5))
			GOA.yaxis.ticks<-project(GOA.yaxis.ticks,aea.prj)
			GOA.xaxis.ticks<-project(GOA.xaxis.ticks,aea.prj)
			GOA.yaxis.ticks<-GOA.yaxis.ticks[,2]
			GOA.xaxis.ticks<-GOA.xaxis.ticks[,1]
			GOA.ext.x<-c(-560000,1380000,1380000,-560000,-560000)
			GOA.ext.y<-c(450000,450000,1330000,1330000,450000)
			GOA.ext.pol<-Polygon(cbind(GOA.ext.x,GOA.ext.y))
			GOA.ext.pol<-Polygons(list(GOA.ext.pol),"GOA")
			GOA.ext.pol<-SpatialPolygons(list(GOA.ext.pol), proj4string=CRS(aea.prj))
			GOA.ext.pol.proj<-spTransform(GOA.ext.pol,CRS(aea.prj))
			}
			
		if(survey == "BS"){
			min.yr = 1982
			# BS map extent INFO
			EBS.xaxis<-c(-176,-174,-172,-170,-168,-166,-164,-162,-160,-158,-156)
			EBS.yaxis<-c(53,55,57,59,61,63)
			EBS.yaxis.ticks<-cbind(c(-177,-178.25,-179.5,178.65,176.75,174.75),EBS.yaxis)
			EBS.xaxis.ticks<-cbind(EBS.xaxis,c(52.25,52.75,53.25,53.5,53.75,54.25,54.15,54.15,54.5,54.5,54.5))
			EBS.yaxis.ticks<-project(EBS.yaxis.ticks,aea.prj)
			EBS.xaxis.ticks<-project(EBS.xaxis.ticks,aea.prj)
			EBS.yaxis.ticks<-EBS.yaxis.ticks[,2]
			EBS.xaxis.ticks<-EBS.xaxis.ticks[,1]
			EBS.ext.x<-c(-1500000,-220000,-220000,-1500000,-1500000)
			EBS.ext.y<-c(510000,510000,1830000,1830000,510000)
			EBS.ext.pol<-Polygon(cbind(EBS.ext.x,EBS.ext.y))
			EBS.ext.pol<-Polygons(list(EBS.ext.pol),"EBS")
			EBS.ext.pol<-SpatialPolygons(list(EBS.ext.pol), proj4string=CRS(aea.prj))
			EBS.ext.pol.proj<-spTransform(EBS.ext.pol,CRS(aea.prj))
			}
		}

	# get temperature data
	t.dat <- get.RACEBase.temps(survey, usr.nm, pwd) 
	
	## normalize temps by median collection day
	# create a data frame to accept temperature data and add columns for values from models to be fitted below
	predict.data <- data.frame(matrix(ncol = ncol(t.dat) + 5))    
	names(predict.data) <- c(names(t.dat), "fitted", "residuals", "estimated_temp", "est_sd",
		"type")
	col.names <- names(predict.data)
	
		## for each unique year test the model
		for(Y in sort(unique(t.dat$yr))) {

			print(Y)
			# subset data by year (Y)
			Y.dat <- t.dat[t.dat$yr == Y,  ]
			b.dat <- Y.dat[!is.na(Y.dat$btemp), ]
			s.dat <- Y.dat[!is.na(Y.dat$stemp), ]
			
			# btemp models
			# test for N to determine if/which model			
			if(length(b.dat$btemp) < 3) 
				next  ## next skips that estimate and advances the loop if there are less than 3 values
			if(length(b.dat$btemp) < 20){  ## less than 20 points (but > 3), run a glm
				btemp.gam <- glm(btemp ~ day_of_year, data = b.dat, na.action = na.exclude)
			}else{  ## more than 20 points and run a GAM with a smooth term
				btemp.gam <- gam(btemp ~ s(day_of_year), data = b.dat, na.action = na.exclude)
				}
				
			# stemp models
			# test for N to determine if/which model			
			if(length(s.dat$stemp) < 3) 
				next  ## next skips that estimate and advances the loop if there are less than 3 values
			if(length(s.dat$stemp) < 20){  ## less than 20 points (but > 3), run a glm
				stemp.gam <- glm(stemp ~ day_of_year, data = s.dat, na.action = na.exclude)
			}else{  ## more than 20 points and run a GAM with a smooth term
				stemp.gam <- gam(stemp ~ s(day_of_year), data = s.dat, na.action = na.exclude)
				}
			
			## note that the value for median day is actually July 10 which is historic median survey day of year for GOA and AI
			## need to do this differently and more dynamically to incorporate BS
			# convert actualy day of year to median day of year
			median_day_B <- rep(190, length(b.dat[,1]))
			median_day_S <- rep(190, length(s.dat[,1]))
			b.dat$day_of_year <- median_day_B
			s.dat$day_of_year <- median_day_S
				
			## create vector of temperatures predicted from the temp.gam model using the 10July median date and then add
			## residuals from the temp.gam back to these temperatures to get the estimated temperature used in maps
			## NOTE that for each observation there is a distinct predicted temperature
			btemp.est <- predict(btemp.gam, newdata = b.dat) + residuals(btemp.gam)
			stemp.est <- predict(stemp.gam, newdata = s.dat) + residuals(stemp.gam)
			btemp.resids <- residuals(btemp.gam)
			stemp.resids <- residuals(stemp.gam)
			btemp.est.sd <- (btemp.resids - mean(btemp.resids))/sd(btemp.resids)
			stemp.est.sd <- (stemp.resids - mean(stemp.resids))/sd(stemp.resids)
			btemp.fit <- fitted(btemp.gam)
			stemp.fit <- fitted(stemp.gam)
			btype <- rep("btemp", length(btemp.fit))
			stype <- rep("stemp", length(stemp.fit))
			b.dat <- data.frame(b.dat, btemp.fit, btemp.resids, btemp.est, btemp.est.sd, btype)
			s.dat <- data.frame(s.dat, stemp.fit, stemp.resids, stemp.est, stemp.est.sd, stype)
			names(b.dat) <- col.names; names(s.dat) <- col.names
			p.dat <- rbind(b.dat, s.dat)
			predict.data <- rbind(predict.data, p.dat)  # accumulates data frame rows as proceed through the Y loop
			}

		predict.data <- predict.data[!is.na(predict.data$yr),  ]
		
	# get mapping layers
	# get degree symbol and Celsius C together for plot labels
	deg.symbol <- expression(paste("Temperature (",degree,"C)"))

	wrld_p <- map("world", interior = FALSE,plot = FALSE)
	llCRS <- CRS("+proj=longlat +ellps=WGS84")
	wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS)
	prj_new <- CRS("+proj=moll")
	wrld_proj <- spTransform(wrld_sp, prj_new)

	# get coastlines
	ak.coast <- readShapePoly("G:/AI-GOA/shapefiles/namerica_dcw", proj4string = CRS(aea.prj))
	russia.coast <- readShapePoly("G:/AI-GOA/shapefiles/russia_dcw", proj4string = CRS(aea.prj))
	
	# establish gridlines for different regions
	if(survey == "AI" | survey == "BS"){
		# common gridlines for AI and BS
		wrld_grd <- gridlines(wrld_sp, easts = c(-178, seq(-176,178, 2), 180), norths = seq(-75, 75, 2), ndiscr = 100)
		wrld_grd_proj2 <- spTransform(wrld_grd, CRS(aea.prj))
	}else{
		# for GOA gridlines
		wrld_grd <- gridlines(wrld_sp, easts = c(-130, seq(-166,-134, 2), -170), norths = seq(-75, 75, 2), ndiscr = 100)
		wrld_grd_proj3 <- spTransform(wrld_grd, CRS(aea.prj))
		}

	# get bathymetry for different regions
	AI.bathy <- raster("G:/Rooper/EFH-Descriptions Modeling/EFH Descriptions 2015_Revisions December 2016/Variables/Variables_AI_1km/Bathy")
	BS.bathy <- raster("G:/Rooper/EFH-Descriptions Modeling/EFH Descriptions 2015_Revisions December 2016/Variables/Variables_EBS_1km/Bathy")
	GOA.bathy <- raster("G:/Rooper/EFH-Descriptions Modeling/EFH Descriptions 2015_Revisions December 2016/Variables/Variables_GOA_1km/Bathy")
	
	## make a raster out of temperature variable
	## kriging for GOA and AI because irregular grid
	## interpolate EBS because of regular grid
	
	# remember that we have already established a single region for this run when entering parameters at the start
	
		# krig temperatures in AI and GOA
		# Temperature kriging to raster
	for(ty in unique(predict.data$type)){
		ty.dat <- predict.data[predict.data$type == ty, ]
		z.range <- range(ty.dat$estimated_temp)
		
		for(yy in sort(unique(ty.dat$yr))){
			# shaping and reprojecting data
			tempdata <- ty.dat[ty.dat$yr == yy, ]
			tempdata <- tempdata[ , c("lon", "lat", "estimated_temp")]
			temp1 <- cbind(tempdata[,1],tempdata[,2])
			tempdata.project <- project(temp1, aea.prj)
			tempdata.pos <- data.frame(cbind(tempdata.project[,1], tempdata.project[,2]))
			tempdata.pos <- SpatialPoints(tempdata.pos, proj4string=CRS(aea.prj)) 
			tempdata.project <- data.frame(cbind(tempdata.project, tempdata[,3]))
			tempdata.project <- SpatialPointsDataFrame(coords = c(tempdata.project["X1"], tempdata.project["X2"]), 
				data = tempdata.project, proj4string=CRS(aea.prj)) 
		
			# since each region has unique extent and grids each plot script is wrapped in an IF statement
			if(survey == "AI"){
				#Interpolate to with IDW
				temp.idw <- gstat(id = "X3", formula = X3~1, data = tempdata.project, nmax = 7, set = list(idp = 2.5))
				temp.raster.idw <- interpolate(AI.bathy, temp.idw, overwrite = TRUE, xyOnly = TRUE, progress = "text")
				masked.idw.temp <-mask(temp.raster.idw, AI.bathy, filename = paste(file.target, "/", survey, "_", ty, "idw_", yy, sep = ""), overwrite = TRUE)

				## kriging from gstat
				# v <- variogram(X3~1, tempdata.project)
				# plot(v)
				# m <- fit.variogram(v, vgm(.2625, "Sph", 80000, .06))
				# plot(v, model=m)
				# g <- gstat(NULL, "X3", X3~1, tempdata.project, model = m, nmax=50)
				# temp.raster.kr <- interpolate(AI.bathy, g, xyOnly=TRUE, progress = "text", overwrite = TRUE )
				# masked.kr.temp <- mask(temp.raster.kr, AI.bathy, filename = paste(file.target, "/", survey, "_", ty, "_", yy, sep = ""), overwrite = TRUE)
				
				png(filename = paste(paste(file.target, "/", survey, "_", ty, "_", yy, ".png", sep = "")),
					width = 7.1, height = 5, res = 300, units = "in")
				par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
				plot(masked.idw.temp, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(256), ext = AI.ext.pol.proj,
					legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = deg.symbol, 
					cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", 
					zlim = c(0, z.range[2]))
				plot(ak.coast, col = "black", add = TRUE)
				plot(russia.coast, col = "black", add = TRUE)
				plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
				text(-1600000, 1000000, paste(yy, ty, sep = " "), font = 2, cex = 2)
				axis(1, at=AI.xaxis.ticks, labels=AI.xaxis)
				axis(2,at=AI.yaxis.ticks, labels=AI.yaxis)
				# a = locator(n=1)
				dev.off()
				}
				
				if(survey == "BS"){
				#Interpolate to with IDW
				temp.idw <- gstat(id = "X3", formula = X3~1, data = tempdata.project, nmax = 7, set = list(idp = 2.5))
				temp.raster.idw <- interpolate(BS.bathy, temp.idw, overwrite = TRUE, xyOnly = TRUE, progress = "text")
				masked.idw.temp <-mask(temp.raster.idw, BS.bathy, filename = paste(file.target, "/", survey, "_", ty, "idw_", yy, sep = ""), overwrite = TRUE)

				## kriging from gstat
				# v <- variogram(X3~1, tempdata.project)
				# plot(v)
				# m <- fit.variogram(v, vgm(.2625, "Sph", 80000, .06))
				# plot(v, model=m)
				# g <- gstat(NULL, "X3", X3~1, tempdata.project, model = m, nmax=50)
				# temp.raster.kr <- interpolate(AI.bathy, g, xyOnly=TRUE, progress = "text", overwrite = TRUE )
				# masked.kr.temp <- mask(temp.raster.kr, AI.bathy, filename = paste(file.target, "/", survey, "_", ty, "_", yy, sep = ""), overwrite = TRUE)
				
				png(filename = paste(paste(file.target, "/", survey, "_", ty, "_", yy, ".png", sep = "")),
					width = 7.1, height = 5, res = 300, units = "in")
				par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
				plot(masked.idw.temp, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(256), ext = BS.ext.pol.proj,
					legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = deg.symbol, 
					cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", 
					zlim = c(0, z.range[2]))
				plot(ak.coast, col = "black", add = TRUE)
				plot(russia.coast, col = "black", add = TRUE)
				plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
				text(-1100000, 980000, paste(yy, ty, sep = " "), font = 2, cex = 1)
				axis(1, at=BS.xaxis.ticks, labels=BS.xaxis)
				axis(2,at=BS.yaxis.ticks, labels=BS.yaxis)
				# a = locator(n=1)
				dev.off()
				}
				
				if(survey == "GOA"){
				#Interpolate to with IDW
				temp.idw <- gstat(id = "X3", formula = X3~1, data = tempdata.project, nmax = 7, set = list(idp = 2.5))
				temp.raster.idw <- interpolate(GOA.bathy, temp.idw, overwrite = TRUE, xyOnly = TRUE, progress = "text")
				masked.idw.temp <-mask(temp.raster.idw, GOA.bathy, filename = paste(file.target, "/", survey, "_", ty, "idw_", yy, sep = ""), overwrite = TRUE)

				## kriging from gstat
				# v <- variogram(X3~1, tempdata.project)
				# plot(v)
				# m <- fit.variogram(v, vgm(.2625, "Sph", 80000, .06))
				# plot(v, model=m)
				# g <- gstat(NULL, "X3", X3~1, tempdata.project, model = m, nmax=50)
				# temp.raster.kr <- interpolate(AI.bathy, g, xyOnly=TRUE, progress = "text", overwrite = TRUE )
				# masked.kr.temp <- mask(temp.raster.kr, AI.bathy, filename = paste(file.target, "/", survey, "_", ty, "_", yy, sep = ""), overwrite = TRUE)
				
				png(filename = paste(paste(file.target, "/", survey, "_", ty, "_", yy, ".png", sep = "")),
					width = 7.1, height = 5, res = 300, units = "in")
				par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
				plot(masked.idw.temp, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(256), ext = GOA.ext.pol.proj,
					legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = deg.symbol, 
					cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", 
					zlim = c(0, z.range[2]))
				plot(ak.coast, col = "black", add = TRUE)
				plot(wrld_grd_proj3, lty = 3, col = "lightgrey", add = T)
				text(-1100000, 980000, paste(yy, ty, sep = " "), font = 2, cex = 1)
				axis(1, at=GOA.xaxis.ticks, labels=GOA.xaxis)
				axis(2,at=GOA.yaxis.ticks, labels=GOA.yaxis)
				# a = locator(n=1)
				dev.off()
				}
				
			removeTmpFiles(h=.5)
			
			}
		}			
	}

