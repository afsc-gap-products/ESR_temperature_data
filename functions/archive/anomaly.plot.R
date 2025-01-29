anomaly.plot <- function(){

	####################################################################################
	################################### anomaly.plot ###################################
	#
	# October 2018
	# N. Laman
	#
	# Description
	#	This program plots the anomaly from the INPFC area-specific long term average
	#	gear temperature of per annum INPFC area-specific gear temperatures.
	#
	# Dependencies
	#
	#	Oracle: AIGOA_WORK_DATA.BTD_THERMOCLINE has to have been populated from
	#	make.temperature.array() in ecosystem.chapter().
	#	R: get.thermocline() function that collects the inversion data
	#	
	# Output
	#
	#	A .png of the anomaly plot
	#
	# IF USING FOR GOA NEED TO DO THE FOLLOWING: BIFURCATE THE PROGRAM BY REGION SINCE
	# GOA HAS 5 REGIONS AND AI HAS 4 WHICH MEANS THE COLOR AND SYMBOL INDEXING AS 
	# WRITTEN FOR THE AI WON'T WORK FOR THE GULF. THIS ALSO MEANS THAT ANOMALY.PLOT
	# WILL NEED TO ACCEPT SURVEY AND PASS IT TO GET.THERMOCLINE OR VICE VERSA OR PERHAPS
	# COULD ASSIGN IT AS A GLOBAL VARIABLE FROM GET.THERMOCLINE?
	#
	####################################################################################

	# get data
	dat2 <- get.thermocline()

	######################### THERMAL ANOMALIES ########################################
	
	# expression to get degree symbol in axis label
	deg.symbol <- expression(paste("Temperature Anomaly ( ",degree,"C)"))

	# summarizing thermocline data to get annual and long term averages of RACEBASE.GEAR_TEMPERATURE
	yr.area.avg <- tapply(dat2$gear_temperature, list(dat2$inpfc_area, dat2$yr), mean)
	area.avg <- tapply(dat2$gear_temperature, dat2$inpfc_area, mean)

	# get vectors of values for plotting
	x <- unlist(dimnames(yr.area.avg)[2])
	y <- yr.area.avg[rownames(yr.area.avg) == "Southern Bering Sea"]

	# working directory to write plot into
	working.directory <- getwd()
	
	# open png object
	png(filename=paste(working.directory, "/anomaly_plot.png", sep = ""), width = 8.5, height = 11, 
		res = 300, units = "in")
	
	# blank plot that will be filled in below
	plot(x = x, y = y, ylim = c(-1,1), xlim = c(1990, 2020), xlab = "Survey Year", 
		ylab = deg.symbol, type = "n")
	# represents the long term average
	abline(h = 0, lty = 2, lwd = 2, col = "navyblue")

	# accounting for changing symbols and colors using indexes and vectors
	symbols <- c(0,1,2,6)
	symbol.index <- 1
	colors <- c("darkgoldenrod1", "darkseagreen", "deeppink1", "darkorchid2")
	color.index <- 1
	
	# ordering INFPC areas from west to east
	areas <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")

	# by INPFC area
	for(j in areas){

		long.avg <- as.vector(area.avg[rownames(area.avg) == j])
		anom <- yr.area.avg[rownames(yr.area.avg) == j] - long.avg

		# adding data points to blank plots one area at a time
		points(x, anom, pch = symbols[symbol.index], cex = 2.0, col = colors[color.index], lwd = 2)

		# tracking color and symbol indexes
		color.index = color.index + 1
		symbol.index <- symbol.index + 1
		# a <- locator(n=1)
		}
	
	# Contrary to the rest of the surveys, 2002 and 2006 in the Aleutians did not sweep from east to west
	text(2002, -0.54, "*", font = 2, cex = 3.0); text(2006, -0.27, "*", font = 2, cex = 3.0)
	
	# legend
	legend("topleft", areas, pch = symbols, cex = 1.25, lwd = 2, lty = 0, col = colors, bty = "n")
	 
	dev.off()
	
	################################### THERMOCLINE DEPTH ANOMALIES #####################################
browser()	
	dat3 <- dat2[!is.na(dat2$inversion_depth),]
	
	# summarizing thermocline data to get annual and long term averages of AIGOA_WORK_DATA.BTD_THERMOCLINE
	# thermocline depths
	yr.area.avg <- tapply(dat3$inversion_depth, list(dat3$inpfc_area, dat3$yr), mean)
	area.avg <- tapply(dat3$inversion_depth, dat3$inpfc_area, mean)

	# get vectors of values for plotting
	x <- unlist(dimnames(yr.area.avg)[2])
	y <- yr.area.avg[rownames(yr.area.avg) == "Southern Bering Sea"]

	# working directory to write plot into
	working.directory <- getwd()
	
	# open png object
	# png(filename=paste(working.directory, "/anomaly_plot_2.png", sep = ""), width = 8.5, height = 11, 
	#	res = 300, units = "in")
	
	#  
	# blank plot that will be filled in below
	plot(x = x, y = y, ylim = c(-100,100), xlim = c(1990, 2020), xlab = "Survey Year", 
		ylab = "Depth Anomaly (m)", type = "n")
	# represents the long term average
	abline(h = 0, lty = 2, lwd = 2, col = "navyblue")

	# accounting for changing symbols and colors using indexes and vectors
	symbols <- c(0,1,2,6)
	symbol.index <- 1
	colors <- c("darkgoldenrod1", "darkseagreen", "deeppink1", "darkorchid2")
	color.index <- 1
	
	# ordering INFPC areas from west to east
	areas <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")

	# by INPFC area
	for(j in areas){

		long.avg <- as.vector(area.avg[rownames(area.avg) == j])
		anom <- yr.area.avg[rownames(yr.area.avg) == j] - long.avg

		# adding data points to blank plots one area at a time
		points(x, anom, pch = symbols[symbol.index], cex = 2.0, col = colors[color.index], lwd = 2)

		# tracking color and symbol indexes
		color.index = color.index + 1
		symbol.index <- symbol.index + 1
a <- locator(n=1)
		}
	
	# Contrary to the rest of the surveys, 2002 and 2006 in the Aleutians did not sweep from east to west
	text(2002, -0.54, "*", font = 2, cex = 3.0); text(2006, -0.27, "*", font = 2, cex = 3.0)
	
	# legend
	legend("topleft", areas, pch = symbols, cex = 1.25, lwd = 2, lty = 0, col = colors, bty = "n")
	 
	# dev.off()
	}

