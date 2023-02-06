combined.temperature.analysis <- function(survey = "AI", file.path = file.target, channel = NA, yr = 2022){
  #############################################################################
  ## Function name:  combined.temperature.analysis
  ##
  ## General Comments:
  ## 		Conducts GLMs and GAMs to predict temperatures at depth corrected for median day of year July 10.
  ##  
  ##
  ## Dependencies:
  ## 		Required libraries
  ##			RODBC
  ##			mgcv
  ##		Oracle tables
  ##			BTD_RESIDS, BTD_MEAN_RESIDS
  ## 
  ## Returns:  
  ## 		Profile.PNG of mean annual cubic spline smoothed temperature profiles, stores intermediate products 
  ## 		(e.g., residuals and mean residuals) in Oracle tables.
  ##
  ###################################################################################
  
  require(mgcv)
  
  if(is.na(channel)){
    require(RODBC)
    schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
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
  
  ## decide whether you want to reload array for analysis or use last version of the data
  replace.haul.data <- readline("Do you want to replace the btd_data object?(y/n)")
  
  # if Yes then queries data from Oracle table just populated from make.temperature.array.  
  # Else it uses what is
  # available from that table for that survey.
  if(replace.haul.data == "Y" | replace.haul.data == "y") {
    survey.btd <- sqlQuery(channel, query = paste("select * from btd_by_depth_latlong_race_data 
                                                  where region = '", survey,
                                                  "' and year <= ", yr, sep = ""), errors = T)
    names(survey.btd) <- casefold(names(survey.btd))
    haul.names <- names(survey.btd)
    # creating long longitudes
    survey.btd$start_longitude[survey.btd$start_longitude > 0] <- survey.btd$start_longitude[survey.btd$start_longitude > 0] - 360
    # make survey.btd available to other programs in this session
    assign("survey.btd", survey.btd, immediate = T, envir = .GlobalEnv)
  }

  # get degree symbol and Celsius C together for plot labels
  deg.symbol <- expression(paste("Temperature (",degree,"C)"))
  
  profile.data <- survey.btd
  
  ## create a data frame to accept profile.data and add columns for values from models to be fitted below
  predict.data <- data.frame(matrix(ncol = ncol(profile.data) + 4))    
  names(predict.data) <- c(names(profile.data),"fitted_values", "residuals", "estimated_temperature", "est_stdev")
  
  ## for each unique temperature test the model
  for(dep in sort(unique(profile.data$depth))) {
    print(dep)
    dep.dat <- profile.data[profile.data$depth == dep,  ]
    
    if(length(dep.dat$depth) < 3) 
      next  ## next skips that estimate and advances the loop if there are less than 3 values
    if(length(dep.dat$depth) < 20){  ## less than 20 points and run a glm
      temp.gam <- glm(temperature ~ julian_date, data = dep.dat, na.action = na.exclude)
    }else{  ## more than 20 points and run a GAM with a smooth term
      temp.gam <- gam(temperature ~ s(julian_date), data = dep.dat, na.action = na.exclude)
    }
    ## note that the value for julian_date is actually day-of-year and not a true julian date		
    dep.dat.2 <- dep.dat
    dep.dat.2$julian_date <- rep(190, length(dep.dat.2$haul))  ## forces day of year to be 10July
    ## create vector of temperatures predicted from the temp.gam model using the 10July median date and then add
    ## residuals from the temp.gam back to these temperatures to get the estimated temperature used in image.color.rects
    ## NOTE that for each distinct depth there is a distinct predicted temperature
    estimated.temperature <- predict(temp.gam, newdata = dep.dat.2) + residuals(temp.gam)  
    resids <- residuals(temp.gam)
    est.stdev <- (resids - mean(resids))/sd(resids)
    fitted <- fitted(temp.gam)
    name.vec <- names(dep.dat.2)
    dep.dat.2 <- data.frame(dep.dat.2, fitted, resids, estimated.temperature, est.stdev)
    names(dep.dat.2) <- c(name.vec, "fitted_values", "residuals", "estimated_temperature", "est_stdev")
    predict.data <- rbind(predict.data, dep.dat.2)  # accumulates rows as proceed through the dep loop
  }
  
  predict.data <- data.frame(predict.data)
  predict.data <- predict.data[!is.na(predict.data$haul),  ]
  names(predict.data) <- toupper(names(predict.data))
  varTypes <- c(rep("int",8), "float", "varchar(255)", rep("float",2), rep("varchar(255)",2), rep("float",4))
  ## note that the date_time field is set to an integer type to bypass difficulties with passing dates through sqlSave
  names(varTypes) <- names(predict.data)
  
  ## all of the table swapping below achieves several things.  target temporary tables are emptied out so that there are no duplicate data.
  ## sqlSave is used to save a new table in Oracle from the R dataframe while taking advantage of the variable typing options for this fxn.
  ## sqlSave is not used to directly append records to the target table because the operation is unstable at this time 10/12.
  ## finally, the Oracle-flavored data in the sqlSaved table can be inserted into the temporary table so it is accessible by the next program.
  
  ## empty or drop temporary storage tables
  sqlClear(channel = channel, "BTD_RESIDS", errors = T)
  sqlClear(channel = channel, "BTD_MEAN_RESIDS", errors = T)
  ## create temporary table, insert records from it to permanent table, then clean up temporary table
  sqlSave(channel = channel, dat = predict.data, 
          tablename = "BTD_RESIDS_TEMP", 
          rownames = FALSE,
          varTypes = varTypes) 
  sqlQuery(channel = channel, query = "insert into BTD_RESIDS select * from BTD_RESIDS_TEMP", errors = T, 
           rows_at_time = 1)
  sqlDrop(channel = channel, "BTD_RESIDS_TEMP", errors = F)
  names(predict.data) <- tolower(names(predict.data))
  
  ## create mean residuals
  # computes stdevs of (estimated temperatures + model residuals) by depth bin
  stdevs <- tapply(predict.data$estimated_temperature, predict.data$depth, sd)  
  # matches all of the observations of depth bins with the stdevs above (e.g., the 120 depth bin stdev above
  # is paired with all observations within an observed 120m depth bin throughout the data set); these become weights.
  w <- stdevs[match(predict.data$depth, as.numeric(names(stdevs)))]  
  # mean of the weighted estimated model stdev 
  wmeans <- tapply(predict.data$est_stdev * w, predict.data$hauljoin, sum)/tapply(w, predict.data$hauljoin, sum)	# calculate weighted means
  predict.means <- cbind(predict.data[match(names(wmeans), predict.data$hauljoin), c("region", "hauljoin",
                                                                                     "year", "vessel", "cruise", "haul", "start_latitude", "start_longitude")], wmeans)
  names(predict.means) <- c("region", "hauljoin", "year", "vessel", "cruise", "haul", "start_latitude",
                            "start_longitude", "mean_res")
  varSpec <- c("varchar(255)", rep("int",5), rep("float",3))
  names(varSpec) <- names(predict.means)
  
  ## create temporary table, insert records from it to permanent table, then clean up temporary table
  sqlSave(channel = channel, tablename = "BTD_MEAN_RESIDS_TEMP", dat = predict.means, rownames = FALSE,
          varTypes = varSpec, fast = T, nastring = NULL) 
  sqlQuery(channel = channel, query = paste("insert into BTD_MEAN_RESIDS select * from BTD_MEAN_RESIDS_TEMP"), 
           error = T, rows_at_time = 1)
  sqlDrop(channel = channel, "BTD_MEAN_RESIDS_TEMP", errors = F)
  
  names(predict.means) <- toupper(names(predict.means))
  varTypes <- c("varchar(5)", rep("int",5), rep("float",3))
  names(varTypes) <- names(predict.means)
  names(predict.means) <- tolower(names(predict.means))
  # predict.data$temperature are smoothed temperatures (cubic spline) within depth bins within years
  # mean.profile is the average of those temperatures for a depth bin within a year
  mean.profile <- tapply(predict.data$temperature, list(predict.data$depth, predict.data$year), mean)
  years <- dimnames(mean.profile)[[2]]
  depths <- dimnames(mean.profile)[[1]]
  
  # open output png to accept cubic spline smoothed annual profile plots
  png(filename = paste(file.path, "/", survey, "profile.png", sep = ""), width = 1920, height = 1900, 
      pointsize = 12, bg = "white", res = 300)
  par(mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(5, 4, 4, 2) + 0.1)
  plot(mean.profile[, 1],  - as.numeric(dimnames(mean.profile)[[1]]), xlim = range(mean.profile, na.rm = TRUE), type = "n", 
       xlab = deg.symbol, ylab = "Depth (m)", col = 1, main = "Normalized Annual Thermoclines")
  
  for(y in 1:length(years)) {
    year <- years[y]
    lines(mean.profile[, year],  - as.numeric(dimnames(mean.profile)[[1]]), lty = y, col = y, lwd = 3)
    # locator(n=1)
  }
  
  legend("bottomright", legend = years, lty = 1:length(years), col = 1:length(years), lwd = 2)
  
  dev.off()
  
}