get.mbt.data <- function(survey, vessel, cruise, channel = NA, user.name = NA, AFSC.pwd = NA){

	# test channel hand off from time.offset()
	if(is.na(channel)){
		require(RODBC)
		channel <- odbcConnect(dsn="AFSC", uid = user.name, pwd = AFSC.pwd, case = "nochange", believeNRows=F)
		close.channel = TRUE
	}else{
		close.channel <- FALSE
	}

	## select Survey and Cruise to populate Oracle queries
	#survey <- readline("Enter Survey Designation (e.g., AI, BS, GOA, NBS, or Slope):  ");
	survey <- toupper(survey)
	#vessel <- readline("Enter vessel code (e.g., 143 for Sea Storm):  "); vessel <- as.numeric(vessel)
	#cruise <- readline("Enter Cruise ID (e.g., 200501):  "); cruise <- as.numeric(cruise)

	## cruise year
	year = floor(cruise/100)

	## RACE_DATA survey definitions
	if(survey == "GOA") survey.name <- "Gulf of Alaska Bottom Trawl Survey"
	if(survey == "AI") survey.name <- "Aleutian Islands Bottom Trawl Survey"
	if(survey == "BS" | survey == "EBS") survey.name <- "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey"
	if(survey == "SLOPE") survey.name <- "Eastern Bering Sea Slope Bottom Trawl Survey"
	if(survey == "NBS") survey.name <- "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey - Triennial Extension"

	if(year == 2004){

		mbt = sqlQuery(channel, paste("select vessel, cruise, haul, date_time, depth
			from race_edit.rb2_btd where vessel = ",vessel,"  and cruise = ",cruise,"
			and datum_code <> 0 order by haul, date_time"), errors = TRUE,  rows_at_time = 1)

	}else{

		# get data from RACE_DATA edit tables
		bt.data1 <- sqlQuery(channel, query = paste("select a.vessel_id vessel, a.cruise cruise,
			b.haul haul, d.edit_date_time date_time, d.edit_depth depth
			from race_data.cruises a, race_data.edit_hauls b, race_data.edit_bathythermic_headers c,
			race_data.edit_bathythermics d, race_data.datum_codes e, race_data.surveys f,
			race_data.survey_definitions g where g.survey_definition_id = f.survey_definition_id
			and year = ", year, " and g.survey_name = '", survey.name, "' and f.survey_id = a.survey_id
			and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id and a.vessel_id = ", vessel,
			" and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code
			and e.use_in_analysis = 'Y' order by vessel, haul", sep = ""))

		# get data from RACE_DATA final tables
		bt.data2 <- sqlQuery(channel, query = paste("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
			d.edit_date_time date_time, d.edit_depth depth
			from race_data.cruises a, race_data.hauls b, race_data.bathythermic_headers c,
			race_data.bathythermics d, race_data.datum_codes e, race_data.surveys f,
			race_data.survey_definitions g where g.survey_definition_id = f.survey_definition_id
			and year = ", year, " and g.survey_name = '", survey.name, "' and f.survey_id = a.survey_id
			and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id and a.vessel_id = ", vessel,
			" and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code
			and e.use_in_analysis = 'Y' order by vessel, haul", sep = ""))

		# test if data come from the edit or final tables
		if(length(bt.data2[,1]) != 0) {
			mbt <- bt.data2
		}else{
			mbt <- bt.data1
			}

		}

		if(close.channel) close(channel)

		names(mbt) <- casefold(names(mbt))

		mbt

	}
