get.sgt.data <- function(survey, vessel, cruise, channel = NA, user.name = NA, AFSC.pwd = NA){

	# accept channel passed by time.offset()
	if(is.na(channel)){
		require(RODBC)
		channel <- odbcConnect(dsn = "AFSC", uid = user.name, pwd = AFSC.pwd, case = "nochange", believeNRows = F)
		close.channel = TRUE
	}else{
		close.channel <- FALSE
		}

	# make sure survey in uppercase
	survey <- toupper(survey)

	## cruise year
	year = floor(cruise/100)

	## RACE_DATA survey definitions
	if(survey == "GOA") survey.name <- "Gulf of Alaska Bottom Trawl Survey"
	if(survey == "AI") survey.name <- "Aleutian Islands Bottom Trawl Survey"
	if(survey == "BS" | survey == "EBS") survey.name <- "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey"
	if(survey == "SLOPE") survey.name <- "Eastern Bering Sea Slope Bottom Trawl Survey"
	if(survey == "NBS") survey.name <- "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey - Triennial Extension"

	if(year == 2004){

		sgt = sqlQuery(channel, paste("select * from race_edit.rb2_sgt where vessel = ",vessel,"
			and cruise = ",cruise," and time_flag in(1,3,5,6,7) order by date_time"),
			errors = TRUE,  rows_at_time = 1)

	}else{

		# get data from RACE_DATA edit tables
		start.times1 <- sqlQuery(channel, query = paste("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
			c.edit_date_time date_time, c.event_type_id time_flag, c.edit_latitude latitude, c.edit_longitude longitude
			from race_data.cruises a, race_data.edit_hauls b, race_data.edit_events c, race_data.surveys d,
			race_data.survey_definitions e, race_data.datum_codes f where e.survey_definition_id = d.survey_definition_id
			and d.survey_id = a.survey_id and year = ", year, " and e.survey_name = '", survey.name, "' and
			b.cruise_id = a.cruise_id and c.haul_id = b.haul_id and a.vessel_id = ", vessel, " and
			c.event_type_id in (1,3,5,6,7,15) and c.datum_code = f.datum_code
			and f.use_in_analysis = 'Y' order by c.edit_date_time", sep = ""))

		# get data from RACE_DATA final tables
		start.times2 <- sqlQuery(channel, query = paste("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
			c.edit_date_time date_time, c.event_type_id time_flag, c.latitude, c.longitude from race_data.cruises a,
			race_data.hauls b, race_data.events c, race_data.surveys d, race_data.survey_definitions e, race_data.datum_codes f
			where e.survey_definition_id = d.survey_definition_id and d.survey_id = a.survey_id and year = ", year,
			" and e.survey_name = '", survey.name, "' and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id and a.vessel_id = ",
			vessel, " and c.event_type_id in (1,3,5,6,7,15) and c.datum_code = f.datum_code and f.use_in_analysis = 'Y'
			order by c.edit_date_time", sep = ""))

		# test if start.times are available in RACE_DATA edit or final tables
		if (length(start.times2[,1]) != 0) {
			sgt <- start.times2
		}else{
			sgt <- start.times1
			}

		}

		if(close.channel) close(channel)

		names(sgt) <- casefold(names(sgt))

		sgt
	}
