require(RODBC)

channel <- odbcConnect(dsn = "AFSC", uid = "lamane", pwd = "sept_4_2018", believeNRows = F)

dat <- sqlQuery(channel, "select floor(a.cruise/100) yr, inpfc_area, min(start_time) min_date, max(start_time) max_date, 
round(avg(b.depth),1) mean_inversion_depth,
round(stddev(b.depth)/sqrt(count(*)),3) se_inversion_depth,
round(avg(b.temperature),1) mean_inversion_temperature, 
round(stddev(b.temperature)/sqrt(count(*)),3) se_inversion_temperature,
round(avg(a.gear_temperature),1) mean_bottom_temperature, 
round(stddev(a.gear_temperature)/sqrt(count(*)),3) se_bottom_temperature,
round(avg(a.surface_temperature),1) mean_surface_temperature, 
round(stddev(a.surface_temperature)/sqrt(count(*)),3) se_surface_temperature
from racebase.haul a,
aigoa_work_data.btd_thermocline b,
goa.goa_strata c
where a.region = 'AI'
and a.region = c.survey
and a.vessel = b.vessel 
and a.cruise = b.cruise
and a.haul = b.haul
and a.stratum = c.stratum
and a.gear_temperature is not null
group by floor(a.cruise/100), inpfc_area
order by floor(a.cruise/100), inpfc_area")

dat2 <- sqlQuery(channel, "select floor(a.cruise/100) yr, start_time, start_longitude lon, start_latitude lat,
b.depth inversion_depth, round(b.temperature,1) inversion_temperature, 
b.max_downcast_temp, b.min_downcast_temp,
a.gear_temperature, a.surface_temperature,
inpfc_area
from racebase.haul a,
aigoa_work_data.btd_thermocline b,
goa.goa_strata c
where a.region = 'AI'
and a.region = c.survey
and a.vessel = b.vessel 
and a.cruise = b.cruise
and a.haul = b.haul
and a.stratum = c.stratum
and a.gear_temperature is not null")

names(dat) <- casefold(names(dat))
names(dat2) <- casefold(names(dat2))

close(channel)

deg.symbol <- expression(paste("Temperature (",degree,"C)"))

min.y <- min(c(dat$mean_bottom_temperature, dat$mean_surface_temperature, dat$mean_inversion_temperature))
max.y <- max(c(dat$mean_bottom_temperature, dat$mean_surface_temperature, dat$mean_inversion_temperature))

par(mfrow = c(3,4))

for(j in c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")){

	dat.area <- subset(dat, dat$inpfc_area == j)
	area.avg <- mean(subset(dat2$surface_temperature, dat2$inpfc_area == j), na.rm = T)

	plot(x = dat$yr, y = dat$mean_surface_temperature, ylim = c(-2, 2), xlim = c(1990, 2020), 
		xlab = "year", ylab = deg.symbol, main = paste(j, "\nsurface temp", sep = ""), type = "n")

	for (i in sort(unique(dat.area$yr))){

		dat.yr <- dat.area[dat.area$yr == i, ]

		# segments(i, (dat.yr$mean_surface_temperature-2*dat.yr$se_surface_temperature),
		#	i, (dat.yr$mean_surface_temperature+2*dat.yr$se_surface_temperature))
		abline(h = 0, lty = 2, lwd = 2, col = "green")
		points(i, dat.yr$mean_surface_temperature - area.avg, pch = 19)
		
		}
	}

for(j in c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")){

	dat.area <- subset(dat, dat$inpfc_area == j)
	area.avg <- mean(subset(dat2$inversion_temperature, dat2$inpfc_area == j), na.rm = T)

	plot(x = dat$yr, y = dat$mean_inversion_temperature, ylim = c(-2,2), xlim = c(1990, 2020), 
		xlab = "year", ylab = deg.symbol, main = "inversion temp", type = "n")

	for (i in sort(unique(dat.area$yr))){

		dat.yr <- dat.area[dat.area$yr == i, ]

		# segments(i, (dat.yr$mean_inversion_temperature-2*dat.yr$se_inversion_temperature),
		#	i, (dat.yr$mean_inversion_temperature+2*dat.yr$se_inversion_temperature))
		abline(h = 0, lty = 2, lwd = 2, col = "green")
		points(i, dat.yr$mean_inversion_temperature - area.avg, pch = 19)
		
		}
	}

par(mfrow = c(1,4))

for(j in c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")){

	dat.area <- subset(dat, dat$inpfc_area == j)
	area.avg <- mean(subset(dat2$gear_temperature, dat2$inpfc_area == j), na.rm = T)

	plot(x = dat$yr, y = dat$mean_bottom_temperature, ylim = c(-2,2), xlim = c(1990, 2020), 
		xlab = "year", ylab = deg.symbol, main = "bottom temp", type = "n")

	for (i in sort(unique(dat.area$yr))){

		dat.yr <- dat.area[dat.area$yr == i, ]

		# segments(i, (dat.yr$mean_bottom_temperature-2*dat.yr$se_bottom_temperature),
		#	i, (dat.yr$mean_bottom_temperature+2*dat.yr$se_bottom_temperature))
		abline(h = 0, lty = 2, lwd = 2, col = "green")
		points(i, dat.yr$mean_bottom_temperature - area.avg, pch = 19)
		
		}
	}

####################################################################################
####################################################################################
###############  FOR PUBLICATION

deg.symbol <- expression(paste("Average Temperature Anomaly ( ",degree,"C)"))

yr.area.avg <- tapply(dat2$gear_temperature, list(dat2$inpfc_area, dat2$yr), mean)
area.avg <- tapply(dat2$gear_temperature, dat2$inpfc_area, mean)

x <- unlist(dimnames(yr.area.avg)[2])
y <- yr.area.avg[rownames(yr.area.avg) == "Southern Bering Sea"]

plot(x = x, y = y, ylim = c(-1,1), xlim = c(1990, 2020), 
		xlab = "Survey Year", ylab = deg.symbol, main = "Bottom Temperature Anomaly", type = "n")
abline(h = 0, lty = 2, lwd = 2, col = "navyblue")

symbols = 15
colors <- c("green", "purple", "orange", "darkblue")
color.index = 1
areas <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea")

for(j in areas){

long.avg <- as.vector(area.avg[rownames(area.avg) == j])
anom <- yr.area.avg[rownames(yr.area.avg) == j] - long.avg

	points(x, anom, pch = symbols, col = colors[color.index])
		
	symbols = symbols + 1
	color.index = color.index + 1

	}

legend("topleft", areas, pch = c(15:18), col = colors)
 
# method
#
# multi-year averages are calculated by INPFC area
# the long term average is subtracted from the annual average to yield
# “average anomlies from the longterm bottom temperature average”
# Plots are conducted in order from the WAI to SBS
# colors and symbols are assigned with counting vectors
# the legend co-opts the ordered construction of the graph 
#
# need to bring color coding into spec with viridis