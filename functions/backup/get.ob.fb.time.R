get.ob.fb.time <- function(channel = NA, new.years = new.years){
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
          from racebase.haul where region = 'AI' and cruise between 199301 and 199601
          and vessel != 21 and extract(month from start_time) between 5 and 9 order by cruise,vessel,haul")
  }
  
  if(region == "BS" | region == "EBS"){
    dat.query <- paste0("select a.vessel_id vessel, a.cruise, b.haul, x.edit_date_time ob_time,
          y.edit_date_time fb_time from race_data.cruises a, race_data.hauls b, race_data.events c,
          race_data.events x, race_data.events y, race_data.surveys e, race_data.survey_definitions f
          where a.cruise_id = b.cruise_id and a.survey_id = e.survey_id and b.haul_id = c.haul_id
          and c.haul_id = x.haul_id and x.haul_id = y.haul_id and c.event_type_id = 4
          and b.start_timelog = x.event_type_id and b.end_timelog = y.event_type_id
          and e.survey_definition_id = f.survey_definition_id and c.edit_date_time is not null
          and extract(month from c.edit_date_time) between 5 and 9
          and x.edit_date_time is not null and y.edit_date_time is not null
          and f.survey_name = 'Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey'
          and floor(a.cruise/100) in (", new.years, ") ",
                        "union
          select a.vessel, a.cruise, a.haul, b.date_time ob_time, c.date_time fb_time from race_edit.rb2_hpden a,
          race_edit.rb2_sgt b, race_edit.rb2_sgt c where a.region = 'BS' and a.vessel = b.vessel
          and b.vessel = c.vessel and a.cruise = b.cruise and b.cruise = c.cruise
          and a.haul = b.haul and b.haul = c.haul and b.time_flag = a.start_timelog
          and c.time_flag = a.end_timelog and a.cruise < 200501 and a.vessel not in (21,159)
          and extract(month from b.date_time) between 5 and 9
          union
          select vessel,cruise,haul, start_time ob_time, start_time + duration/(24) fb_time
          from racebase.haul where region = 'BS' 
          and extract(month from start_time) between 5 and 9 order by cruise,vessel,haul")
  }
     print("Getting on-bottom and off-bottom times...")
     obfb.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
     names(obfb.dat) <- casefold(names(obfb.dat))

     return(obfb.dat)

     }
