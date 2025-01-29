get.thermocline <- function(){
#########################################################################################
############################ get.thermocline ############################################
#
# October 2018
# N. Laman
#
# Description:
#	Queries data from RACEBASE.HAUL, AIGOA_WORK_DATA.BTD_THERMOCLINE, and GOA.GOA_STRATA
#	AIGOA_WORK_DATA.BTD_THERMOCLINE is populated when make.temperature.array() is run as
#	part of the preparation for the Ecosystem Considerations chapter. Data collected are
#	start_time, start lon/lat, inversion_depth, inversion temperature, max_downcast_temp,
#	min_downcast_temp, gear_temperature, surface_temperature, and INPFC area.
#	Will work for GOA or AI.	
#
# Dependencies:
#
#	Process: Have to run ecosystem.chapter() first to populate the BTD_THERMOCLINE table.
#	Oracle: 
#	R: RODBC
#
# Output:
#
#	Returns data frame as described above.
#
##########################################################################################

# load libraries
pkgTest("RODBC")

# connect to Oracle
channel <- odbcConnect(dsn = "AFSC", uid = "lamane", pwd = "sept_4_2018", believeNRows = F)

# query data
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

return(dat2)

}