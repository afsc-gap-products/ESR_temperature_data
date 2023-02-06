function(){
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
        ## Libraries - RODBC
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
          # AI triennial in the 1990s, biennial in 2000s. 1994 is earliest BT cast year available for AI
          new.years <- noquote(paste(seq(2006, year, by = 2), collapse = ","))
          old.years <- noquote(paste(c(seq(1994, 2000, by = 3),seq(2002, 2006, by = 2)), collapse = ","))
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

        obfb.dat <- get.ob.fb.time(channel = channel, region = region, year = year, new.years = new.years)

        pos.time.dat <- merge(pos.dat, obfb.dat)

        # interim for testing
        write.csv(pos.time.dat, "pos_time_data.csv", row.names = F)
        write.csv(dat, "goa_bt_casts.csv", row.names = F)

        # data frame for temperature summary data
        temperature.summary <- data.frame()
        temp.summary.names <- c("vessel","cruise","haul","surface_temp","surface_temp_sd","surface_temp_n",
                                "temp100m","temp100m_sd","temp100m_n","temp200m","temp200m_sd","temp200m_n")

        for(Cr in sort(unique(dat$cruise))){

                cr.dat <- dat[dat$cruise == Cr, ]

                for(V in sort(unique(cr.dat$vessel))){

                        v.cr.dat <- cr.dat[cr.dat$vessel == V, ]

                        for(H in sort(unique(v.cr.dat$haul))){
                                print(paste0("Getting downcast for Vessel/Cruise/Haul ", V, "/", Cr, "/", H, "..."))
                                h.v.cr.dat <- v.cr.dat[v.cr.dat$haul == H, ]
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

                                if(is.na(first.obs)) {
                                        cat(paste("No date_time available to start downcast for haul", H, "\n"))
                                        # browser()
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

                                out.vec <- c(V,Cr,H, surface.temp, surface.temp.sd, surface.temp.n, temp.100, temp.100.sd, temp.100.n,
                                             temp.200, temp.200.sd, temp.200.n)
                                temperature.summary <- rbind(temperature.summary,out.vec)
                        }
                }
        }

        if(close.channel) close(channel)

        names(temperature.summary) <- temp.summary.names
        temperature.summary <- merge(temperature.summary, pos.time.dat)
        write.csv(temperature.summary, "temperature_summary.csv", row.names = F)
        # return(temperature.summary)

}
