
region <- "AI"

year <- 2022

# AI triennial in the 1990s, biennial in 2000s. 1994 is earliest BT cast year available for AI
new.years <- noquote(paste(seq(2006, 2022, by = 2), collapse = ","))
old.years <- noquote(paste(c(seq(1994, 2000, by = 3),seq(2002, 2006, by = 2)), collapse = ","))
years <- noquote(paste(c(old.years, new.years), collapse = ","))
survey.name = "Aleutian Islands Bottom Trawl Survey"


dat <- get.bt.casts(channel = channel, survey.name = survey.name, year = year,
                    region = region, new.years = new.years, old.years = old.years)

pos.dat <- get.haul.pos(channel = channel, region = region, year = 2022)

obfb.dat <- get.ob.fb.time(channel = channel, region = region, new.years = new.years)

pos.time.dat <- merge(pos.dat, obfb.dat)

temperature.summary <- data.frame()
temp.summary.names <- c("vessel","cruise","haul","inpfc_area","surface_temp","surface_temp_sd","surface_temp_n",
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
          from racebase.haul where region = 'AI' and cruise between 199301 and 199601
          and vessel != 21 and extract(month from start_time) between 5 and 9 order by cruise,vessel,haul")
  }
  
  pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
  names(pos.dat) <- casefold(names(pos.dat))
  print("Getting on-bottom and off-bottom times...")
  obfb.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
  names(obfb.dat) <- casefold(names(obfb.dat))
  
  return(obfb.dat)
  
}
