###############################################################################################################################
# make.esr.temperatures
###############################################################################################################################

make.esr.temperatures <- function(){
     #############################################################################
     ## Function name:  make.esr.temperatures
     ##
     ## What it does:
     ## 1. Takes input of Region and Cruise Year from user and uses those to set up
     ## 2. Survey.Name and sampling year stanzas for queries.
     ## 3. Opens RODBC channel to Oracle
     ## 4. Calls series of queries to get BT casts, OB-FB times, and HAUL data
     ## 5. Processes BT cast data. At present (Oct2021) it focuses on downcasts
     ##    and AI/GOA data streams. After excising the downcast through a series
     ##    of steps that identify the shallowest depth and associated time at the
     ##    beginning of the downcast and ending the downcast at OB time, the
     ##    script then conditionally computes average temperatures and SDs over
     ##    depth ranges of 1-5m, 95-105m, and 195-205m. Then it assembles those
     ##    outcomes along with vessel,cruise,haul,location (lat/lon), and a subregion
     ##    (in GOA these were < -165W = wgoa, btw -165 & -147 = cgoa, and
     ##    > -147=egoa), finally writing those out into a CSV to be delivered to
     ##    the ESR editors.
     ##
     ## How is it constrained?
     ## The query is not constrained to good PERFORMANCE or standard HAUL_TYPE so does
     ## not eliminate "bad" tows. It does tap into the datum codes in RACE_DATA.
     ## It limits data returned to "summer months" between May and September
     ## It is limited to hauls with not null start_lat, start_lon, and bottom_depth
     ##
     ## What could it do?
     ## It needs to be modified to produce subregion assigments for the Aleutians
     ## It could be modified to produce temperature summaries for the Bering shelf,
     ## slope, and NBS.
     ## It needs to be modified to accept Oracle username and password as input
     ##
     ## Dependencies:
     ## Libraries - RODBC, data.table
     ## Other functions - get.bt.casts, get.ob.fb.time, get.haul.pos
     ##
     ## Returns:  Haul-specific geo-referenced avg temps and sds at 3 depths from BT downcasts
     ##
     ###################################################################################

     require(data.table)

     begin.time <- proc.time()

     # enter the max year to collect data from
     region <- readline("Enter abbreviation for region of interest (e.g., AI or GOA): ")
     region <- toupper(region)
     year <- readline("Enter latest survey year (e.g., 2021): ")

     # build regional vectors of years
     if(region %ni% c("GOA","AI")){
          print("This program is presently written to only run for AI or GOA! Choose one of those regions.")
          stop
     }
     if(region == "GOA"){
          # GOA triennial in the 1990s, biennial in 2000s. 1993 is earliest BT cast year available for GOA
          new.years <- noquote(paste(seq(2005, year, by = 2), collapse = ","))
          old.years <- noquote(paste(c(seq(1993, 1999, by = 3),c(2001,2003)), collapse = ","))
          years <- noquote(paste(c(old.years, new.years), collapse = ","))
          survey.name = "Gulf of Alaska Bottom Trawl Survey"
     }
     if(region == "AI"){
          # AI triennial in the 1990s, biennial in 2000s. 1991? is earliest BT cast year available for AI
          # 1991 seems to be earliest year but does not get pulled in and previous reports use only 1994 onward
          # Currently just using 1994-present even with this script
          new.years <- noquote(paste(seq(2006, year, by = 2), collapse = ","))
          old.years <- noquote(paste(c(seq(1991, 2000, by = 3),seq(2002, 2006, by = 2)), collapse = ","))
          years <- noquote(paste(c(old.years, new.years), collapse = ","))
          survey.name = "Aleutian Islands Bottom Trawl Survey"
     }

     print("Getting data from Oracle...")

     schema <- 'afsc'
     username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
     password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
     channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                    uid = paste(username),
                                    pwd = paste(password),
                                    believeNRows = FALSE)

     dat <- get.bt.casts(channel = channel, survey.name = survey.name, year = year,
                         region = region, new.years = new.years, old.years = old.years)

     pos.dat <- get.haul.pos(channel = channel, region = region, year = year)

     obfb.dat <- get.ob.fb.time(channel = channel, region = region, new.years = new.years)

     pos.time.dat <- merge(pos.dat, obfb.dat)

     # data frame for temperature summary data
     temperature.summary <- data.frame()
     temp.summary.names <- c("vessel","cruise","haul","subregion","surface_temp","surface_temp_sd","surface_temp_n",
                             "temp100m","temp100m_sd","temp100m_n","temp200m","temp200m_sd","temp200m_n")

     for(Cr in sort(unique(dat$cruise))){

          cr.dat <- dat[dat$cruise == Cr, ]

          for(V in sort(unique(cr.dat$vessel))){

               v.cr.dat <- cr.dat[cr.dat$vessel == V, ]

               for(H in sort(unique(v.cr.dat$haul))){
                    print(paste0("Getting downcast for Vessel/Cruise/Haul ", V, "/", Cr, "/", H, "..."))
                    h.v.cr.dat <- v.cr.dat[v.cr.dat$haul == H, ]
                    inpfc_area <- pos.time.dat$inpfc_area[pos.time.dat$cruise == Cr & pos.time.dat$vessel == V & pos.time.dat$haul == H]
                    # subregion <- pos.time.dat$subregion[pos.time.dat$cruise == Cr & pos.time.dat$vessel == V & pos.time.dat$haul == H]
                    ob.time <- pos.time.dat$ob_time[pos.time.dat$cruise == Cr & pos.time.dat$vessel == V & pos.time.dat$haul == H]
                    fb.time <- pos.time.dat$fb_time[pos.time.dat$cruise == Cr & pos.time.dat$vessel == V & pos.time.dat$haul == H]
                    bottom.depth <- pos.time.dat$bottom_depth[pos.time.dat$vessel == V & pos.time.dat$cruise == Cr & pos.time.dat$haul == H]
                    raw.downcast <- h.v.cr.dat[h.v.cr.dat$date_time < ob.time, ]
                    raw.upcast <- h.v.cr.dat[h.v.cr.dat$date_time > fb.time, ]
                    # order cast data by time in chronological order
                    downcast.ordered <- raw.downcast[order(raw.downcast$date_time, na.last = NA),  ]
                    upcast.ordered <- raw.upcast[order(raw.upcast$date_time, na.last = NA),  ]

                    ## establishing shallowest depth in downcast data and, setting it to 1 if it is <1 or setting it
                    ## to 5 if it is >5
                    shallowest.depth <- min(downcast.ordered$depth)
                    if(shallowest.depth < 1){shallowest.depth <- 1}
                    if(shallowest.depth > 5){shallowest.depth <- 5}

                    ## this is the most recent time preceding the occurrence of find.depth. it says return the date_time
                    ## where the depth is <= find.depth at the last position in the ordered list of data ordered by date_time
                    ## the notation below basically says give me all of the date_times from this haul that are at
                    ## depths <= find.depth and then the second set of square brackets provides the address of the last
                    ## record in the list
                    last.surface.time <- downcast.ordered$date_time[downcast.ordered$depth <= shallowest.depth][length(downcast.ordered$depth[downcast.ordered$depth <= shallowest.depth])]
                    ## this uses the last.surface.time to refine the start of the downcast by selecting the date_times
                    ## that correspond to records where the depth is <= shallowest.depth while being <= last.surface.time.
                    ## It then reverses the list of date_times and picks the first element of the reversed list which
                    ## happens to correspond to the shallowest depth in the list
                    first.obs <- rev(downcast.ordered$date_time[downcast.ordered$date_time <= last.surface.time & downcast.ordered$depth <= shallowest.depth])[1]
# browser()
                    if(is.na(first.obs)) {
                         cat(paste("No date_time available to start downcast for haul", H, "\n"))

                         next
                    }

                    ## truncate data set to the downcast between first.obs and ob.time
                    downcast <- downcast.ordered[downcast.ordered$date_time >= first.obs,  ]
                    min.depth <- min(downcast$depth)
                    print(paste0("downcast starts on surface at depth ", min.depth, "..."))
                    surface.temp <- mean(downcast$temperature[downcast$depth >= min.depth & downcast$depth < 5])
                    surface.temp.sd <- sd(downcast$temperature[downcast$depth >= min.depth & downcast$depth < 5])
                    surface.temp.n <- length(downcast$temperature[downcast$depth >= min.depth & downcast$depth < 5])

                    print(paste0("bottom depth for this haul is ", bottom.depth, "..."))
                    # testing for truncated bottom depth rel to 95-100m average temperature target
                    if(bottom.depth < 95){
                         print("Insufficient temperature records between 95m and 105m to calculate 100m average temp")
                         temp.100 <- NA
                         temp.100.sd <- NA
                         temp.100.n <- 0
                    }else{
                         temp.100 <- mean(downcast$temperature[downcast$depth >= 95 & downcast$depth <= 105])
                         temp.100.sd <- sd(downcast$temperature[downcast$depth >= 95 & downcast$depth <= 105])
                         temp.100.n <- length(downcast$temperature[downcast$depth >= 95 & downcast$depth <= 105])
                    }

                    # testing for truncated bottom depth rel to 195-205m average temperature target
                    if(bottom.depth < 195){
                         print("Insufficient temperature records between 195m and 205m to calculate 100m average temp")
                         temp.200 <- NA
                         temp.200.sd <- NA
                         temp.200.n <- 0
                    }else{
                         temp.200 <- mean(downcast$temperature[downcast$depth >= 195 & downcast$depth <= 205])
                         temp.200.sd <- sd(downcast$temperature[downcast$depth >= 195 & downcast$depth <= 205])
                         temp.200.n <- length(downcast$temperature[downcast$depth >= 195 & downcast$depth <= 205])
                    }

                    # Currently needs changed depending on region
                    # out.vec <- c(V,Cr,H, subregion, surface.temp, surface.temp.sd, surface.temp.n, temp.100, temp.100.sd, temp.100.n,
                    #              temp.200, temp.200.sd, temp.200.n)
                    out.vec <- c(V,Cr,H, inpfc_area, surface.temp, surface.temp.sd, surface.temp.n, temp.100, temp.100.sd, temp.100.n,
                                 temp.200, temp.200.sd, temp.200.n)
                    temperature.summary <- rbind(temperature.summary,out.vec)
               }
          }
     }

     names(temperature.summary) <- temp.summary.names
     temperature.summary <- merge(temperature.summary, pos.time.dat)
     print("Writing temperature summary...")
     write.csv(temperature.summary, "temperature_summary.csv", row.names = F)
     # return(temperature.summary)
     library(data.table)

     print(paste0("It took this long to run this script: ", data.table::timetaken(begin.time)))
}


###############################################################################################################################
# get.bt.casts
###############################################################################################################################
get.bt.casts <- function(channel = NA, survey.name = "Gulf of Alaska Bottom Trawl Survey", year = year, new.years = new.years,
                         old.years = old.years, region = region){
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

########################
# NEED TO EXCLUDE NON SUMMER (MAY-SEPT) MONTHS FROM DATA TO AVOID WINTER SURVEYS
# FOR AI AND GOA ELIMINATE RECORDS WITH TEMPERATURES <= 0
# FOR ALL ELIMINATE NA TEMPS OR DEPTHS
# MAKE SURE A CALCULATED TEMPERATURE RETURNS A NULL NOT A ZERO UNLESS IT SHOULD BE ZERO
########################

     # build regional vectors of years
     # if(survey.name == "Gulf of Alaska Bottom Trawl Survey"){
          # GOA triennial in the 1990s, biennial in 2000s. 1993 is earliest BT cast year available for GOA
     #      new.years <- noquote(paste(seq(2005, year, by = 2), collapse = ","))
     #      old.years <- noquote(paste(c(seq(1993, 1999, by = 3),c(2001,2003)), collapse = ","))
     #     years <- noquote(paste(c(old.years, new.years), collapse = ","))
     #      region = "GOA"
     # }
     # if(survey.name == "Aleutian Islands Bottom Trawl Survey"){
          # AI triennial in the 1990s, biennial in 2000s. 1994 is earliest BT cast year available for AI
     #      new.years <- noquote(paste(seq(2006, year, by = 2), collapse = ","))
     #      old.years <- noquote(paste(c(seq(1994, 2000, by = 3),seq(2002, 2006, by = 2)), collapse = ","))
     #      years <- noquote(paste(c(old.years, new.years), collapse = ","))
     #      region = "AI"
     # }
     ################### need queries to parse out Bering Shelf, Slope, and NBS BT casts here ######################

     print(paste0("Getting BT casts from ", region, " survey years ", new.years, "..."))
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

     print(paste0("Getting BT casts from ", region, " survey years ", old.years, "..."))
     RB2.bt.data <- RODBC::sqlQuery(channel = channel, query = paste0("select a.vessel, a.cruise, a.haul, a.date_time, a.temperature, a.depth from race_edit.rb2_btd a, racebase.haul h
          where a.vessel = h.vessel and a.cruise = h.cruise and a.haul = h.haul and h.region = '",region,"' and floor(a.cruise/100) in (",
          old.years, ") and datum_code != 0 and extract(month from a.date_time) between 5 and 9 order by a.cruise, a.vessel, a.date_time"))

     bt.casts <- rbind(bt.data, RB2.bt.data)

     names(bt.casts) <- casefold(names(bt.casts))
     return(bt.casts)

}

###############################################################################################################################
# get.ob.fb.time
###############################################################################################################################
get.ob.fb.time <- function(channel = NA, region = region, new.years = new.years){
     # accept channel
     # accept survey or region
     # accept user.name and password
     # eliminates non-summer months

     # note that in these queries the years for RB2 and RACEBase data are fixed because we know them for GOA
     # need to alter this query to reflect known years for AI and have it run conditionally by region using
     # new.years and old.years as input
  if(region == "GOA"){
    dat.query <- paste0("select a.vessel_id vessel, a.cruise, b.haul, x.edit_date_time ob_time,
          y.edit_date_time fb_time from race_data.cruises a, race_data.hauls b, race_data.events c,
          race_data.events x, race_data.events y, race_data.surveys e, race_data.survey_definitions f
          where a.cruise_id = b.cruise_id and a.survey_id = e.survey_id and b.haul_id = c.haul_id
          and c.haul_id = x.haul_id and x.haul_id = y.haul_id and c.event_type_id = 4
          and b.start_timelog = x.event_type_id and b.end_timelog = y.event_type_id
          and e.survey_definition_id = f.survey_definition_id and c.edit_date_time is not null
          and extract(month from c.edit_date_time) between 5 and 9
          and x.edit_date_time is not null and y.edit_date_time is not null
          and f.survey_name = 'Gulf of Alaska Bottom Trawl Survey'
          and floor(a.cruise/100) in (", new.years, ") ",
          "union
          select a.vessel, a.cruise, a.haul, b.date_time ob_time, c.date_time fb_time from race_edit.rb2_hpden a,
          race_edit.rb2_sgt b, race_edit.rb2_sgt c where a.region = 'GOA' and a.vessel = b.vessel
          and b.vessel = c.vessel and a.cruise = b.cruise and b.cruise = c.cruise
          and a.haul = b.haul and b.haul = c.haul and b.time_flag = a.start_timelog
          and c.time_flag = a.end_timelog and a.cruise < 200501 and a.vessel not in (21,159)
          and extract(month from b.date_time) between 5 and 9
          union
          select vessel,cruise,haul, start_time ob_time, start_time + duration/(24) fb_time
          from racebase.haul where region = 'GOA' and cruise between 199301 and 199601
          and vessel != 21 and extract(month from start_time) between 5 and 9 order by cruise,vessel,haul")
    }
  
  if(region == "AI"){
    dat.query <- paste0("select a.vessel_id vessel, a.cruise, b.haul, x.edit_date_time ob_time,
          y.edit_date_time fb_time from race_data.cruises a, race_data.hauls b, race_data.events c,
          race_data.events x, race_data.events y, race_data.surveys e, race_data.survey_definitions f
          where a.cruise_id = b.cruise_id and a.survey_id = e.survey_id and b.haul_id = c.haul_id
          and c.haul_id = x.haul_id and x.haul_id = y.haul_id and c.event_type_id = 4
          and b.start_timelog = x.event_type_id and b.end_timelog = y.event_type_id
          and e.survey_definition_id = f.survey_definition_id and c.edit_date_time is not null
          and extract(month from c.edit_date_time) between 5 and 9
          and x.edit_date_time is not null and y.edit_date_time is not null
          and f.survey_name = 'Aleutian Islands Bottom Trawl Survey'
          and floor(a.cruise/100) in (", new.years, ") ",
                        "union
          select a.vessel, a.cruise, a.haul, b.date_time ob_time, c.date_time fb_time from race_edit.rb2_hpden a,
          race_edit.rb2_sgt b, race_edit.rb2_sgt c where a.region = 'AI' and a.vessel = b.vessel
          and b.vessel = c.vessel and a.cruise = b.cruise and b.cruise = c.cruise
          and a.haul = b.haul and b.haul = c.haul and b.time_flag = a.start_timelog
          and c.time_flag = a.end_timelog and a.cruise < 200501 and a.vessel not in (21,159)
          and extract(month from b.date_time) between 5 and 9
          union
          select vessel,cruise,haul, start_time ob_time, start_time + duration/(24) fb_time
          from racebase.haul where region = 'AI' and cruise between 199101 and 199801
          and vessel != 21 and extract(month from start_time) between 5 and 9 order by cruise,vessel,haul")
    } # changed to include 1991 & 1997, need to figure out why this was excluded
  
    pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
    names(pos.dat) <- casefold(names(pos.dat))
     print("Getting on-bottom and off-bottom times...")
     obfb.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
     names(obfb.dat) <- casefold(names(obfb.dat))

     return(obfb.dat)

  }

###############################################################################################################################
# get.haul.pos
###############################################################################################################################
get.haul.pos <- function(channel = NA, region = region, year = year){
     # accept channel
     # accept region or survey name
     # accept user.name and password
     # eliminates non-summer months

     print("Getting haul location, bottom depth, and assigning subregion...")

     if(region == "GOA"){

     # institutes updated subregion appellations provided by Bridget Ferriss in 2021 from the Oracle side
     # It would be of benefit to move this assignment of regions to the R code so easier to change in the future
     # or could be allowed to revert to older appellations (e.g., INPFC areas used in GOA samplign design)

     dat.query <- paste0("select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth,
          'wgoa' subregion from racebase.haul where region = 'GOA' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and start_longitude < -165 and extract(month from start_time) between 5 and 9
          union
          select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth,
          'cgoa' subregion from racebase.haul where region = 'GOA' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and start_longitude between -165 and -147 and extract(month from start_time) between 5 and 9
          union
          select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth,
          'egoa' subregion from racebase.haul where region = 'GOA' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and start_longitude > -147 and extract(month from start_time) between 5 and 9")

     pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
     names(pos.dat) <- casefold(names(pos.dat))

     }

  if(region == "AI"){
    # dat.query <- paste0("select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth
    #       from racebase.haul where region = 'AI' and floor(cruise/100) between 1993 and ", year, " and vessel not in
    #       (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
    #       and extract(month from start_time) between 5 and 9")
    
    dat.query <- paste0("select a.vessel, a.cruise, a.haul, a.stationid, a. stratum, b.inpfc_area, a.start_longitude longitude, a.start_latitude latitude, a.bottom_depth
          from racebase.haul a, goa.goa_strata b 
          where region = 'AI' 
          and floor(cruise/100) between 1991 and ", year, " 
          and vessel not in (21,159) 
          and start_longitude is not null 
          and start_latitude is not null 
          and bottom_depth is not null
          and extract(month from start_time) between 5 and 9
          and a.region = b.survey
          and a.stratum = b.stratum")

    pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
    names(pos.dat) <- casefold(names(pos.dat))
  }
  
  if(region == "BS" | region == "EBS"){
    dat.query <- paste0("select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth
            from racebase.haul where region = 'BS' and floor(cruise/100) between 2005 and ", year, " and vessel not in
            (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
            and extract(month from start_time) between 5 and 9")
    
    pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
    names(pos.dat) <- casefold(names(pos.dat))
  }
     return(pos.dat)

}

###############################################################################################################################
# Not In
###############################################################################################################################
'%ni%' <- Negate('%in%')
