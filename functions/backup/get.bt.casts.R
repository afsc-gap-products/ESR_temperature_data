get.bt.casts <- function(channel = NA, region = "GOA",survey.name = "Gulf of Alaska Bottom Trawl Survey", year = 2021,  new.years = new.years, old.years = old.years)
     {
     #############################################################################
     ## Function name:  get.bt.casts
     ##
     ## What it does:
     ## Queries BT data from Oracle RACE_DATA EDIT and Final tables (2005-present)
     ## and from RACE_EDIT RB2 reconciliation tables (1991-2004). This is set up
     ## to query a vector of years and return all BT cast data available from the
     ## beginning of the record to the survey year entered.
     ## Tests resulting objects so that return contains data. Logic for inclusion is...
     ## IF in RACE_DATA there are any data in Final Tables then use them, otherwise,
     ## if there are no data in Final Tables but some in Edit Tables then use them,
     ## otherwise, get wrecked!
     ##
     ## What it does and doesn't do:
     ## The query is not constrained to good PERFORMANCE or standard HAUL_TYPE so does
     ## not eliminate "bad" tows. It does tap into the datum codes in RACE_DATA.
	 ## It limits data returned to "summer months" between May and September
	 ## It is initially written for AI and GOA but could be modified to run for BS Shelf & Slope, and NBS
     ##
     ## Dependencies:
     ## Libraries - RODBC
     ##
     ## Returns:  BT data for all years of interest
     ##
     ###################################################################################
    require(RODBC)
  
     if(is.na(channel)){
        require(RODBC)
         schema <- 'afsc'
         username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
         password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
         channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                        uid = paste(username),
                                        pwd = paste(password),
                                        believeNRows = FALSE)
       close.channel = TRUE
     }else{
          close.channel <- FALSE
     }

     # build regional vectors of years
     if(survey.name == "Gulf of Alaska Bottom Trawl Survey"){
          # GOA triennial in the 1990s, biennial in 2000s. 1993 is earliest BT cast year available for GOA
          new.years <- noquote(paste(seq(2005, year, by = 2), collapse = ","))
          old.years <- noquote(paste(c(seq(1993, 1999, by = 3),c(2001,2003)), collapse = ","))
          years <- noquote(paste(c(old.years, new.years), collapse = ","))
          region = "GOA"
          }
     if(survey.name == "Aleutian Islands Bottom Trawl Survey"){
          # AI triennial in the 1990s, biennial in 2000s. 1994 is earliest BT cast year available for AI
          new.years <- noquote(paste(seq(2006, year, by = 2), collapse = ","))
          old.years <- noquote(paste(c(seq(1994, 2000, by = 3),seq(2002, 2006, by = 2)), collapse = ","))
          years <- noquote(paste(c(old.years, new.years), collapse = ","))
          region = "AI"
     }
    if(survey.name == "Bering Sea Bottom Trawl Survey"){
      # BS annual. 2005 is earliest BT cast year available for BS in racebase. data goes back to 1993 but earlier data
      # were not reconciled
      new.years <- noquote(paste(seq(2005, year, by = 1), collapse = ","))
      years <- new.years
      region = "EBS"
    }
     ################### need queries to parse out Bering Shelf, Slope, and NBS BT casts here ######################

     print(paste0("Getting BT casts from ", region, " survey years ", new.years, "..."))
  if(survey.name == "Gulf of Alaska Bottom Trawl Survey" | survey.name == "Aleutian Islands Bottom Trawl Survey" ){
   # get data from RACE_DATA EDIT tables
     bt.data1 <- RODBC::sqlQuery(channel = channel, query = paste0("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
		d.edit_date_time date_time, d.edit_temperature temperature, d.edit_depth depth
		from race_data.cruises a, race_data.edit_hauls b, race_data.edit_bathythermic_headers c,
		race_data.edit_bathythermics d, race_data.datum_codes e, race_data.surveys f, race_data.survey_definitions g
		where g.survey_definition_id = f.survey_definition_id and year in (", new.years, ") and g.survey_name = '", survey.name,
         "' and f.survey_id = a.survey_id and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id
		and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code and e.use_in_analysis = 'Y'
		and extract(month from d.edit_date_time) between 5 and 9 order by vessel, haul"))
     # get data from RACE_DATA FINAL tables
     bt.data2 <- RODBC::sqlQuery(channel = channel, query = paste0("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
		d.edit_date_time date_time, d.edit_temperature temperature, d.edit_depth depth
		from race_data.cruises a, race_data.hauls b, race_data.bathythermic_headers c, race_data.bathythermics d,
		race_data.datum_codes e, race_data.surveys f, race_data.survey_definitions g
		where g.survey_definition_id = f.survey_definition_id and year in (", new.years, ") and g.survey_name = '", survey.name,
         "' and f.survey_id = a.survey_id and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id
		and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code and e.use_in_analysis = 'Y'
		and extract(month from d.edit_date_time) between 5 and 9 order by vessel, haul"))
  }
  
  if(survey.name == "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey"){
    # get data from RACE_DATA EDIT tables
    bt.data1 <- sqlQuery(channel = channel, query = paste0("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
		d.edit_date_time date_time, d.edit_temperature temperature, d.edit_depth depth
		from race_data.cruises a, race_data.edit_hauls b, race_data.edit_bathythermic_headers c,
		race_data.edit_bathythermics d, race_data.datum_codes e, race_data.surveys f, race_data.survey_definitions g
		where g.survey_definition_id = f.survey_definition_id and year in (", new.years, ") and g.survey_name = '", survey.name,
                                                           "' and f.survey_id = a.survey_id and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id
		and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code and e.use_in_analysis = 'Y'
		and extract(month from d.edit_date_time) between 5 and 9 order by vessel, haul"))
    # get data from RACE_DATA FINAL tables
    bt.data2 <- sqlQuery(channel = channel, query = paste0("select a.vessel_id vessel, a.cruise cruise, b.haul haul,
		d.edit_date_time date_time, d.edit_temperature temperature, d.edit_depth depth
		from race_data.cruises a, race_data.hauls b, race_data.bathythermic_headers c, race_data.bathythermics d,
		race_data.datum_codes e, race_data.surveys f, race_data.survey_definitions g
		where g.survey_definition_id = f.survey_definition_id and year in (", new.years, ") and g.survey_name = '", survey.name,
                                                           "' and f.survey_id = a.survey_id and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id
		and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code and e.use_in_analysis = 'Y'
		and extract(month from d.edit_date_time) between 5 and 9 order by vessel, haul"))
  }
     # test if data come from the edit or final tables using the logic, if there are any data in final state, then use them
     if(length(bt.data2[,1]) > 1) {
          bt.data <- bt.data2
          # if there are no data in final and some in edit use the edit data
     }else if(length(bt.data2[,1]) == 0 & length(bt.data1[,1]) > 1){
          bt.data <- bt.data1
     } else {
          print(paste0("There are no data available for cruise year ", year, "."))
          stop
     }
     # names(bt.data) <- casefold(names(bt.data))

     print(paste0("Getting BT casts from ", region, " survey years ", old.years, "..."))
     RB2.bt.data <- sqlQuery(channel = channel, query = paste0("select a.vessel, a.cruise, a.haul, a.date_time, a.temperature, a.depth from race_edit.rb2_btd a, racebase.haul h
          where a.vessel = h.vessel and a.cruise = h.cruise and a.haul = h.haul and h.region = '",region,"' and floor(a.cruise/100) in (",
          old.years, ") and datum_code != 0 and extract(month from a.date_time) between 5 and 9 order by a.cruise, a.vessel, a.date_time"))
     # names(RB2.bt.data) <- casefold(names(RB2.bt.data))

     bt.casts <- rbind(bt.data, RB2.bt.data)

     if(close.channel)close(channel)

     names(bt.casts) <- casefold(names(bt.casts)); bt.casts

     }
