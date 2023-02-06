make.temperature.array <- function(year.vector = c(2014), survey = "AI", vessel = 0, haul = 0, channel = NA, file.target = file.target){
     #############################################################################
     ## Function name:  make.temperature.array
     ##
     ## What it does:
     ## Brings in BT data and On and Off Bottom times from Oracle and uses these data to
     ## identify temperature inversions from the downcasts.
     ##
     ## Dependencies:
     ## Libraries - RODBC
     ## Subroutines - get.bt.data, get.start.times, get.end.times, get.bt.data.old,
     ## get.start.times.old, get.end.times.old, find.inversion.tops, combined.temperature.analysis.nl
     ##
     ## Returns:  Populates Oracle Table BTD_BY_DEPTH & BTD_BY_DEPTH_LATLONG_RACE_DATA plus
     ## BTD_THERMOCLINES & BTD_INVERSIONS, PDF of downcasts by haul
     ##
     ## Note that the BTD_BY_DEPTH... tables accumulate data...means that if you want to just
     ## plot AI up to 2012 you would still get 2014 and 2016 (Sept. 2017) because they are stored
     ## in those tables
     ##
     ## One of the places this program could be enhanced is by including code to identify
     ## thermoclines and inversions during the upcast and then to store both down- and up-
     ## cast identified data
     ##
     ###################################################################################

     if(is.na(channel)){
          require(RODBC)
          channel <- odbcConnect(dsn = "AFSC", uid = "AIGOA_WORK_DATA", believeNRows = FALSE)
          close.channel = TRUE
     }else{
          close.channel <- FALSE
     }

     if(survey == "GOA") survey.name <- "Gulf of Alaska Bottom Trawl Survey"
     if(survey == "AI") survey.name <- "Aleutian Islands Bottom Trawl Survey"

     # get degree symbol and Celsius C together for plot labels
     deg.symbol <- expression(paste("Temperature (",degree,"C)"))

     pdf(file = paste(file.target, "/", survey, year.vector, ".pdf", sep = ""))

     for(year in year.vector) {
          ## Year referenced here will change as earlier data are Reconciled and moved into RACE_DATA.
          ## At present (10/12) data older than 2007 are to be found in RACE_EDIT.RB2_* tables while data
          ## collected since 2007 could be in one of two places or three states in RACE_DATA.  In RACE_DATA
          ## data could either be in the EDIT_ or final tables and could either be in the editing state, the
          ## final state, or the transitional state when final data are being re-edited.
          ## NOTE that while much of the data in RACE_EDIT.RB2_tables goes back to 1999, the RB2_BTD table
          ## contains data going back to 1993 thanks to Skip Zenger's recovery efforts.

          if(year >= 2007){
               ## btd is vessel, cruise, haul, date_time, depth, temperature
               ## bt data are NOT constrained to PERFORMANCE >= 0 or HAUL_TYPE = 3
               ## start and end times are equivalent to start_timelog and end_timelog
               btd <- get.bt.data(channel = channel, survey.name = survey.name, year = year)
               start.times <- get.start.times(channel = channel, survey.name = survey.name, year = year)
               end.times <- get.end.times(channel = channel, survey.name = survey.name, year = year)
          }else{
               btd <- get.bt.data.old(channel = channel, survey = survey, year = year)
               start.times <- get.start.times.old(channel = channel, survey = survey, year = year)
               end.times <- get.end.times.old(channel = channel, survey = survey, year = year)
          }

          # eliminate records at depths > 1200 m
          btd <- btd[btd$depth < 1200,]

          ## the pipe symbol operates as an 'or' statement
          if(length(btd$temperature[(btd$temperature < 0 | btd$temperature > 20) & btd$depth > 1]) > 1){
               cat("Here are some suspicious temperature records:\n")
               print(btd[(btd$temperature < 0 | btd$temperature > 20) & btd$depth > 1,])
          }

          # arbitrary but systematic depth breaks intended to provide better resolution in shallower water
          target.depths <- c(3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100,
                             110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290,
                             300, 325, 350, 375, 400, 450, 500, 600, 700, 800, 900, 1000)

          # create midpoints between target cut depths
          cut.depths <- (target.depths[1:length(target.depths) - 1] + target.depths[2:length(target.depths)])/2

          # add a 0 to the cut.depths vector
          cut.depths <- c(0, cut.depths, max(target.depths))

          # create empty data frames to hold data created below
          out.data <- data.frame(matrix(ncol = 6))
          names(out.data) <- toupper(c("vessel", "cruise", "haul", "date_time", "depth", "temperature"))
          out.thermocline <- data.frame(matrix(ncol = 8))
          names(out.thermocline) <- toupper(c("vessel", "cruise", "haul", "depth", "temperature", "temp_rate_change",
                                              "min_downcast_temp", "max_downcast_temp"))
          out.inversions <- data.frame(matrix(ncol = 7))
          names(out.inversions) <- toupper(c("vessel", "cruise", "haul", "top_depth", "top_temperature", "bottom_depth",
                                             "bottom_temperature"))

          ## isolate the Cruise
          # consider changing these nested loops to an array output with tapply
          for(Cr in unique(btd$cruise)){

               print(paste("Cruise", Cr))

               # get BT data where Cruise
               cruise.btd <- btd[btd$cruise == Cr,  ]

               # get only Cruise BT data where Temperature and Depth are not null
               cruise.btd <- cruise.btd[!is.na(cruise.btd$temperature) & !is.na(cruise.btd$depth),  ]
               cruise.starts <- start.times[start.times$cruise == Cr,  ]
               cruise.ends <- end.times[end.times$cruise == Cr,  ]

               ## isolate a Vessel and loop through all hauls under that Vessel
               for(v in sort(unique(cruise.btd$vessel))) {
                    ## this anticipates weird vessel numbers but not sure why
                    if(vessel > 0) {
                         if(v < vessel) next
                    }
                    print(paste("Vessel", v))
                    vessel.btd <- cruise.btd[cruise.btd$vessel == v,  ]
                    vessel.starts <- cruise.starts[cruise.starts$vessel == v,  ]
                    vessel.ends <- cruise.ends[cruise.ends$vessel == v,  ]

                    ## here is the meat where per haul operations take place
                    for(h in sort(unique(vessel.btd$haul))) {
                         if(haul > 0) {
                              if(h < haul) next
                         }
                         print(h)
                         haul.btd <- vessel.btd[vessel.btd$haul == h,  ]
                         haul.start <- vessel.starts[vessel.starts$haul == h,  ]
                         haul.end <- vessel.ends[vessel.ends$haul == h,  ]

                         if(length(haul.start$haul) == 0) {
                              cat(paste("No start time available for haul", h, "\n"))
                              next
                         }
                         ob.time <- haul.start$date_time
                         fb.time <- haul.end$date_time

                         if(any(haul.btd$temperature < 0)) {
                              haul.btd <- haul.btd[haul.btd$temperature >= 0,  ]
                              cat(paste("Negative temperatures found in data for haul", h,
                                        " and eliminated. Rest of data may be suspect.\n", sep = ""))
                         }
                         ## isolate data before on bottom
                         downcast.btd <- haul.btd[haul.btd$date_time < ob.time,  ]
                         ## isolate on bottom data
                         bottom.btd <- haul.btd[haul.btd$date_time >= ob.time & haul.btd$date_time <= fb.time,  ]
                         ## range of on bottom temperatures
                         bottom.range <- range(bottom.btd$temperature)
                         ## equivalent to RACEBase GEAR_TEMPERATURE
                         bottom.avg <- round(mean(bottom.btd$temperature), 1)
                         downcast.btd <- downcast.btd[order(downcast.btd$date_time),  ]

                         if(length(downcast.btd$depth) < 2) {
                              cat(paste("No good bt data available during downcast period for haul",h, "\n"))
                              next
                         }
                         ## establishing shallowest depth in downcast data and, setting it to 2 if it is <2 or setting it
                         ## to 5 if it is >5
                         shallowest.depth <- min(downcast.btd$depth)

                         if(shallowest.depth < 2) {
                              shallowest.depth <- 2
                         }
                         find.depth <- 5
                         if(shallowest.depth > 5)
                              find.depth <- shallowest.depth
                         ## this is the most recent time preceding the occurrence of find.depth. it says return the date_time
                         ## where the depth is <= find.depth at the last position in the ordered list of data ordered by date_time
                         ## the notation below basically says give me all of the date_times from this haul that are at
                         ## depths <= find.depth and then the second set of square brackets provides the address of the last
                         ## record in the list
                         last.surface.time <- downcast.btd$date_time[downcast.btd$depth <= find.depth][length(downcast.btd$depth[downcast.btd$depth <= find.depth])]
                         ## this uses the last.surface.time to refine the start of the downcast by selecting the date_times
                         ## that correspond to records where the depth is <= shallowest.depth while being <= last.surface.time.
                         ## It then reverses the list of date_times and picks the first element of the reversed list which
                         ## happens to correspond to the shallowest depth in the list
                         first.obs <- rev(downcast.btd$date_time[downcast.btd$date_time <= last.surface.time & downcast.btd$depth <= shallowest.depth])[1]

                         if(is.na(first.obs)) {
                              cat(paste("No date_time available to start downcast for haul", h, "\n"))
                              browser()
                              next
                         }
                         ## truncate data set to the downcast between first.obs and ob.time
                         downcast.btd <- downcast.btd[downcast.btd$date_time >= first.obs,  ]

                         if(length(downcast.btd$depth) < 2) {
                              cat(paste("No good bt data available during downcast period for haul", h, "\n"))
                              next
                         }
                         ## takes a mean of temperature binned by unique raw depths.  looks like in most cases n=1 for these means
                         unique.depths <- tapply(downcast.btd$temperature, downcast.btd$depth, mean)
                         ## create a dataframe where unique depths are both rownames and a numeric field "depth"
                         downcast.approx <- data.frame(as.numeric(names(unique.depths)), unique.depths)
                         names(downcast.approx) <- c("depth", "temperature")
                         ## identify possible temperature inversions below 10 m using cumulative minima of temp diffs < -.31
                         possible.inversions <- downcast.approx[(cummin(downcast.approx$temperature) -
                                                                      downcast.approx$temperature) < -0.31 & downcast.approx$depth > 10,  ]
                         low.temp.depths <- downcast.approx[(cummin(downcast.approx$temperature) - downcast.approx$temperature) == 0,  ]
                         # the 0-value notation in the plot call below sets up depth so that the y-axis is surface to depth
                         ## from top to bottom
                         plot(downcast.approx$temperature, 0 - downcast.approx$depth, xlab = deg.symbol, ylab = "Depth (m)",
                              main = paste("Vessel", v, "Cruise", Cr, "Haul", h))

                         if(length(possible.inversions$temperature) > 0) {
                              inversion.tops <- data.frame(matrix(nrow = length(possible.inversions$temperature), ncol = 2))
                              names(inversion.tops) <- c("inversion.top.depth", "inversion.top.temperature")

                              for(p in 1:length(possible.inversions$temperature)) {
                                   inversion.tops[p,  ] <- find.inversion.tops(possible.inversions$depth[p], low.temp.depths)
                              }

                              possible.inversions <- data.frame(inversion.tops, possible.inversions)
                              names(possible.inversions) <- c("top.depth", "top.temperature", "bottom.depth", "bottom.temperature")
                              possible.inversions <- possible.inversions[order(possible.inversions$top.depth,
                                                                               - possible.inversions$bottom.temperature), ]
                              possible.inversions <- possible.inversions[!duplicated(possible.inversions$top.depth),  ]
                              abline(h =  - possible.inversions$top.depth, col = 4)
                              abline(h =  - possible.inversions$bottom.depth, col = 4)
                              print(possible.inversions)
                         }

                         loc <- list(100, 100)
                         names(loc) <- c("x", "y")
                         depth.range <- range(downcast.approx$depth)
                         min.loc <- depth.range[1] - (depth.range[2] - depth.range[1]) * 0.05

                         if(any(is.na(downcast.approx)) | length(downcast.approx$depth) < 10) {
                              cat(paste("Not enough good bt data available during downcast period for haul", h, "\n"))
                              next
                         }
                         ## fitting cubic smooth spline to supplied data
                         smooth.temps <- smooth.spline(downcast.btd$depth, downcast.btd$temperature)
                         ## populate smooth.time with just the y-values from the smooth
                         smooth.time <- smooth.spline(downcast.btd$depth, downcast.btd$date_time)$y
                         ## convert the y-s above to calendar time
                         smooth.time <- as.POSIXct(smooth.time, origin = "1970-01-01", TZ=Sys.timezone())
                         approx.downcast <- data.frame(smooth.time, smooth.temps$x, smooth.temps$y)
                         names(approx.downcast) <- c("date_time", "depth", "temperature")
                         ## create a dataframe of target.depths and the y-values from a predict.smooth.spline of smooth.temps and
                         ## those target depths

                         approx.btd <- data.frame(target.depths[target.depths <= max(approx.downcast$depth)], predict(smooth.temps,
                                                                                                                      target.depths[target.depths <= max(approx.downcast$depth)])$y)
                         names(approx.btd) <- c("depth", "temperature")
                         max.depth <- max(approx.downcast$depth)
                         min.depth <- 5
                         tc.search.data <- approx.downcast[approx.downcast$depth <= max.depth & approx.downcast$depth >= min.depth,]
                         depth.changes <- diff(tc.search.data$depth)
                         time.changes <- as.numeric(diff(tc.search.data$date_time, units="secs"))
                         depth.change.rate <- c(depth.changes/time.changes,0)
                         tc.search.data <- tc.search.data[depth.change.rate > .12,]

                         ## if temperature change is > 0.4 you have identified the thermocline
                         if((max(tc.search.data$temperature) - min(tc.search.data$temperature)) < .4){
                              this.thermocline <- data.frame(matrix(c(v,Cr,h,NA,NA,NA, min(downcast.btd$temperature),
                                                                      max(downcast.btd$temperature)), nrow = 1))
                              cat(paste("Insufficient temperature change for thermocline\n"))
                              mtext("No thermocline detected", side = 3)
                         }else{
                              depth.changes <- diff(tc.search.data$depth)
                              temp.changes <- diff(tc.search.data$temperature)
                              rate.temp.change <- temp.changes/depth.changes
                              rate.temp.change[depth.changes == 0] <- 0
                              depth.bins <- findInterval(tc.search.data$depth, seq(min(tc.search.data$depth), max(tc.search.data$depth),
                                                                                   (max(tc.search.data$depth) - min(tc.search.data$depth))/10))
                              depth.bin.means <- tapply(rate.temp.change, depth.bins[1:(length(depth.bins)-1)], mean)
                              max.change.bin <- as.numeric(names(depth.bin.means))[depth.bin.means == min(depth.bin.means)]
                              max.rate.temp.change <- min(rate.temp.change[depth.bins[(1:length(depth.bins)-1)] == max.change.bin])
                              tc.search.data <- tc.search.data[1:(length(tc.search.data$depth)-1),]
                              tc.depth <- tc.search.data[depth.bins[(1:length(depth.bins)-1)] == max.change.bin &
                                                              rate.temp.change == max.rate.temp.change,][1,]
                              abline(h = -tc.depth$depth, col = "red")
                              mtext(paste("Thermocline Depth =",  tc.depth$depth, "m temp change rate =",
                                          round(max.rate.temp.change,3)), side = 3)
                              cat(paste("Estimated thermocline is at", tc.depth$depth, "meters\n"))
                              lines(approx.downcast$temperature,  - approx.downcast$depth, col = "green")
                              approx.time <- approx(downcast.btd$depth, downcast.btd$date_time, xout = approx.btd$depth)
                              approx.time <- ISOdatetime(1970,1,1,0,0,0) + approx.time$y - 28800
                              vch <- matrix(rep(c(v, Cr, h), length(approx.time)), ncol = 3, byrow = T)
                              this.data <- data.frame(vch, approx.time, approx.btd)
                              this.data <- this.data[!is.na(this.data$temperature),  ]
                              names(this.data) <- toupper(c("vessel", "cruise", "haul", "date_time", "depth", "temperature"))
                              this.data$DATE_TIME <- as.character(this.data$DATE_TIME)
                              out.data <- rbind(out.data, this.data)
                              vch <- c(v, Cr, h)
                              this.thermocline <- data.frame(matrix(c(vch, tc.depth[,c("depth","temperature")], max.rate.temp.change,
                                                                      min(downcast.btd$temperature), max(downcast.btd$temperature)), nrow = 1))
                         }

                         names(this.thermocline) <- names(out.thermocline)
                         out.thermocline <- rbind(out.thermocline, this.thermocline)

                         if(length(possible.inversions$top.depth) > 0) {
                              vch <- matrix(rep(c(v, Cr, h), length(possible.inversions$top.depth)), ncol = 3, byrow = T)
                              this.inversions <- data.frame(vch, possible.inversions)
                              names(this.inversions) <- names(out.inversions)
                              out.inversions <- rbind(out.inversions, this.inversions)
                         }
                         #locator(n=1)
                    }
               }
          }

          out.data <- out.data[!is.na(out.data$VESSEL),]
          out.thermocline <- out.thermocline[!is.na(out.thermocline$VESSEL),]
          out.inversions <- out.inversions[!is.na(out.inversions$VESSEL),]

          ## table management in Oracle.
          ## Create a new temporary BTD_BY_DEPTH table with sqlSave
          varTypes <- c(rep("int",3),"date",rep("float",2))
          names(varTypes) <- names(out.data)
          sqlSave(channel = channel, dat = out.data, tablename = paste("BTD_BY_DEPTH_", Cr, sep = ""), rownames = F,
                  varTypes = varTypes)

          ## identify conflicting data in target table and delete when present
          dat.test1 <- sqlQuery(channel = channel, query = paste("select count(*) records from BTD_BY_DEPTH where cruise = ",
                                                                 Cr, sep = ""), errors = T, rows_at_time = 1)

          if (dat.test1$RECORDS > 0) {
               sqlQuery(channel = channel, query = paste("delete from BTD_BY_DEPTH where cruise = ", Cr, sep = ""),
                        errors = T, rows_at_time = 1)
               sqlQuery(channel = channel, query = "commit")
          }
          ## this will append records to BTD_BY_DEPTH but should fail to insert duplicate records because
          ## of table constraints in Oracle
          ## temperatures inserted into BTD_BY_DEPTH and ultimately into BTD_BY_DEPTH_LATLONG_RACE_DATA are
          ## predicted from a cubic spline smooth
          sqlQuery(channel = channel, query = paste("insert into BTD_BY_DEPTH (VESSEL, CRUISE, HAUL, DATE_TIME, DEPTH, TEMPERATURE)
			select VESSEL, CRUISE, HAUL, DATE_TIME, DEPTH, TEMPERATURE from BTD_BY_DEPTH_", Cr, sep = ""),
                   errors = T, rows_at_time = 1)
          ## clean up temporary table
          sqlDrop(channel = channel, paste("BTD_BY_DEPTH_", Cr, sep = ""), errors = T)
          # sqlQuery(channel = channel, query = paste("drop table BTD_BY_DEPTH_", Cr, sep = ""))
          # sqlQuery(channel = channel, query = "commit")

          ## Create a new temporary THERMOCLINE table with sqlSave
          varTypes <- c(rep("int",3),rep("float",5))
          names(varTypes) <- names(out.thermocline)
          sqlSave(channel = channel, dat = out.thermocline, tablename = paste("BTD_THERMOCLINE_", Cr, sep = ""), rownames = F,
                  varTypes = varTypes, fast = F, nastring = NULL)
          ## identify conflicting data in target table and delete when present
          dat.test2 <- sqlQuery(channel = channel, query = paste("select count(*) records from BTD_THERMOCLINE where cruise = ",
                                                                 Cr, sep = ""), errors = T, rows_at_time = 1)

          if (dat.test2$RECORDS > 0) {
               sqlQuery(channel = channel, query = paste("delete from BTD_THERMOCLINE where cruise = ", Cr, sep = ""),
                        errors = T, rows_at_time = 1)
               sqlQuery(channel = channel, query = "commit")
          }
          ## this will append records to BTD_THERMOCLINE but should fail to insert duplicate records
          ## because of table constraints in Oracle
          sqlQuery(channel = channel, query = paste("insert into BTD_THERMOCLINE (VESSEL, CRUISE, HAUL, DEPTH,
			TEMPERATURE, TEMP_RATE_CHANGE, MIN_DOWNCAST_TEMP, MAX_DOWNCAST_TEMP) select VESSEL, CRUISE, HAUL,
			DEPTH, TEMPERATURE, TEMP_RATE_CHANGE, MIN_DOWNCAST_TEMP, MAX_DOWNCAST_TEMP from BTD_THERMOCLINE_",
                                                    Cr, sep = ""), errors = T, rows_at_time = 1)
          ## clean up temporary table
          sqlDrop(channel = channel, paste("BTD_THERMOCLINE_", Cr, sep = ""), errors = T)

          ## Create a new temporary INVERSIONS table with sqlSave
          varTypes <- c(rep("int",3),rep("float",4))
          names(varTypes) <- names(out.inversions)
          sqlSave(channel = channel, dat = out.inversions, tablename = paste("BTD_INVERSIONS_", Cr, sep = ""), rownames = F,
                  varTypes = varTypes, fast = F, nastring = NULL)
          ## identify conflicting data in target table and delete when present
          dat.test3 <- sqlQuery(channel = channel, query = paste("select count(*) records from BTD_INVERSIONS where cruise = ",
                                                                 Cr, sep = ""), errors = T, rows_at_time = 1)

          if (dat.test3$RECORDS > 0) {
               sqlQuery(channel = channel, query = paste("delete from BTD_INVERSIONS where cruise = ", Cr, sep = ""),
                        errors = T, rows_at_time = 1)
               sqlQuery(channel = channel, query = "commit")
          }
          ## append records to BTD_INVERSIONS but should fail to insert duplicate records because of table constraints in Oracle
          sqlQuery(channel = channel, query = paste("insert into BTD_INVERSIONS (VESSEL, CRUISE, HAUL, TOP_DEPTH,
			TOP_TEMPERATURE, BOTTOM_DEPTH, BOTTOM_TEMPERATURE) select VESSEL, CRUISE, HAUL, TOP_DEPTH,
			TOP_TEMPERATURE, BOTTOM_DEPTH, BOTTOM_TEMPERATURE from BTD_INVERSIONS_", Cr, sep = ""), errors = T, rows_at_time = 1)
          ## clean up temporary table
          sqlDrop(channel = channel, paste("BTD_INVERSIONS_", Cr, sep = ""), errors = T)

          ## identify conflicting data in target table and delete when present
          dat.test4 <- sqlQuery(channel = channel, query = paste("select count(*) records from BTD_BY_DEPTH_LATLONG_RACE_DATA
			where cruise = ", Cr, sep = ""), errors = T, rows_at_time = 1)

          if (dat.test4$RECORDS > 0) {
               sqlQuery(channel = channel, query = paste("delete from BTD_BY_DEPTH_LATLONG_RACE_DATA where cruise = ", Cr, sep = ""),
                        errors = T, rows_at_time = 1)
               sqlQuery(channel = channel, query = "commit")
          }
          ## Populate BTD_BY_DEPTH_LATLONG_RACE_DATA table with array to use in Temperature Analyses
          ## This script has the potential to append records to the target table but should be blocked
          ## by unique table constraints from Oracle
          sqlQuery(channel = channel, query = paste("insert into BTD_BY_DEPTH_LATLONG_RACE_DATA (HAULJOIN, VESSEL,
			CRUISE, HAUL, DATE_TIME, JULIAN_DATE, YEAR, DEPTH, TEMPERATURE, REGION, START_LATITUDE, START_LONGITUDE,
			REGULATORY_AREA_NAME, STRATUM_TYPE) select B.HAUL_ID HAULJOIN, A.VESSEL_ID VESSEL, A.CRUISE, B.HAUL,
			C.DATE_TIME, to_number(to_char(C.DATE_TIME,'DDD')) JULIAN_DATE, floor(A.CRUISE/100) YEAR, C.DEPTH,
			C.TEMPERATURE, H.SURVEY, J.LATITUDE, J.LONGITUDE, H.REGULATORY_AREA_NAME, H.STRATUM_TYPE
			from RACE_DATA.CRUISES A,RACE_DATA.HAULS B, BTD_BY_DEPTH C, RACE_DATA.SURVEYS F,
			RACE_DATA.SURVEY_DEFINITIONS G, GOA.GOA_STRATA H, RACE_DATA.EVENTS J
			where G.SURVEY_DEFINITION_ID = F.SURVEY_DEFINITION_ID and A.CRUISE = C.CRUISE and A.VESSEL_ID = C.VESSEL
			and B.HAUL = C.HAUL and G.REGION = H.SURVEY and B.STRATUM = H.STRATUM and F.SURVEY_ID = A.SURVEY_ID
			and B.CRUISE_ID = A.CRUISE_ID and J.HAUL_ID = B.HAUL_ID and B.START_TIMELOG = J.EVENT_TYPE_ID
			and G.REGION = '", survey,"' and A.CRUISE = ", Cr, sep = ""), errors = T, rows_at_time = 1)
     }

     dev.off()

}
