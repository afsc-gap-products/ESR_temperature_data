image.color.rects <- function (survey = "AI", file.path = file.target, channel = NA, yr = 2022){
  #############################################################################
  ## Function name:  image.color.rects
  ##
  ## General Comment:
  ##		This program derived from plot.color.rects.  It differs in that it utilizes the graphics(image) 
  ##		function to create the plots because this function provides a more direct method for mapping
  ##		colors onto temperatures than did the method employed in its predecesor.
  ##
  ## What it does:
  ## 		plots the temperatures estimated by depth across longitudes by year from temperature
  ## 		estimates. 
  ##
  ## Dependencies:
  ## 		Required Libraries
  ##			RODBC 
  ##			"RColorBrewer" (may not be necessary anymore) 
  ##			mgcv
  ##			fields
  ##			graphics
  ## 
  ## Returns:  Publication-quality graphic of annual waterfall temperature plots for Ecosystem Considerations 
  ## chapter of SAFE document.
  ##
  ## Version Notes -
  ##		Sept 2017 - struggled with image.plot where the length of the "at" and "label" vectors !=
  ## 					solution was to reduce the specifications in the image.plot call (see older script
  ##					commented out below).
  ##					Instituted a region-specific upper limit to the false array of temperatures to further
  ##					expand mid-range temperatures in shallow GOA water. The previous upper limit of 7.5 in 
  ##					the Gulf was masking some details and hiding others. Feel that upper limit of 9.5 yields
  ##					more realistic plots.
  ##					The max.figs.per.page is 12; can't fit any more on the page and still read them...Means
  ## 					next GOA chapter will have to drop 1993 or go to 2 pg format. For AI, need to add some
  ##					script that detects the number of years to be plotted and then scales par accordingly
  ## 					to maximize graph size on single page. E.g., if 8 years then put 8 on page, but if 9 put
  ##					10 on page.
  ##					Added a constraint to the survey.btd data call to limit plotting data to cruise years
  ##					to those <= to the cruise year entered for the program run
  ##
  ###################################################################################
  
  require("RColorBrewer")
  require(mgcv)
  require(fields)
  require(graphics)
  require(viridis)
  
  if(is.na(channel)){
    require(RODBC)
    channel <- odbcConnect(dsn = "AFSC", uid = "AIGOA_WORK_DATA", believeNRows = FALSE)
    close.channel = TRUE
  }else{
    close.channel <- FALSE
  }
  
  survey.btd <- sqlQuery(channel, query = paste("select * from btd_resids where region = '", survey, "' 
                                                and year <= ", yr, sep = ""))
  names(survey.btd) <- casefold(names(survey.btd))
  survey.btd <- survey.btd[!is.na(survey.btd$estimatedtemperature) | !is.na(survey.btd$fitted_values),]
  
  if(survey == "AI"){
    longs <- seq(-189.5, -165, 0.5)
    x.long <- seq(-189.25, -165.25, 0.5)
    xlab <- c(-189.5, -185, -180, -175, -170, -165.5)  
    xlab.corr <- c("170 E", "175 E", "180", "175 W", "170 W", "165 W")
    y.lim <- -400
    fit.y.lim <- -150	## expands upper shallow depths of fitted values plot
    false.temp.max <- 7.5
  }
  
  if(survey == "GOA"){
    longs <- seq(-170, -132.5, 0.5)
    x.long <- seq(-167.25, -130.25, 0.5)
    xlab <- c(-167.5, -160, -150, -140, -130.5)  
    xlab.corr <- c("170 W", "160 W", "150 W", "140 W", "130 W")
    y.lim <- -450
    fit.y.lim <- -150
    false.temp.max <- 9.5
  }
  
  years <- sort(unique(survey.btd$year))
  n.years <- length(years)
  
  ## max.figs.per.page dynamic
  # max.figs.per.page <- 2*ceiling(n.years/2)
  max.figs.per.page <- 12
  
  ## median date and mean residual normalized temperatures traditionally reported for SAFE document
  temp.range <- range(survey.btd$estimatedtemperature, na.rm = T)
  temp.list <- quantile(survey.btd$estimatedtemperature, probs = seq(0,1,by=1/64), na.rm = T)
  if(temp.list[1]<0) temp.list[1] = 0
  temp.array <- tapply(survey.btd$estimatedtemperature, list(survey.btd$year, survey.btd$depth, 
                                                             cut(survey.btd$start_longitude, longs)), mean, na.rm = T)
  ## for plotting purposes to avoid plotting NAs where values are out of range of fixed temperature limits
  ## basically this is done to expand the color range to increase visual differences in the resulting plots
  false.array <- temp.array 
  false.array[false.array <= 3.5] <- 3.5; false.array[false.array >= false.temp.max] <- false.temp.max
  
  y.depth <- as.numeric(dimnames(temp.array)[[2]])
  y.lab <- dimnames(temp.array)[[2]]
  
  ## median date corrected fitted temperature values
  fitted.range <- range(survey.btd$fitted_values, na.rm = T)
  fitted.list <- quantile(survey.btd$fitted_values, probs = seq(0,1,by=1/64), na.rm = T)
  fitted.array <- tapply(survey.btd$fitted_values, list(survey.btd$year, survey.btd$depth, 
                                                        cut(survey.btd$start_longitude, longs)), mean, na.rm = T)
  ## for plotting purposes to avoid plotting NAs where values are out of range of fixed temperature limits
  false.fit.array <- fitted.array 
  false.fit.array[false.fit.array <= 3.5] <- 3.5; false.fit.array[false.fit.array >= false.temp.max] <- false.temp.max
  
  depths <- as.numeric(dimnames(temp.array)[[2]])
  depths <- c(1,(depths[1:length(depths) - 1] + depths[2:length(depths)])/2, 800)
  pages <- ceiling(n.years/max.figs.per.page)
  
  if(pages > 1){
    max.figs.per.page <- ceiling(n.years/pages)
  }
  
  fig.num <- 1
  y <- 1
  # this doesn't seem to do anything for the AI data but also doesn't screw anything up so leaving it
  # will re-evaluate next year for GOA
  # ylab = pretty(depths, n = 9) # added by NLaman 10/23/13 to get increments of 50 on yaxis of thermal profiles
  
  # get degree symbol and Celsius C together for plot labels
  deg.symbol <- expression(paste("Temperature (",degree,"C)"))
  
  ## plot median date, mean residual corrected temperature estimates for Ecosystem Considerations chapter of SAFE document
  for (p in (1:pages)){
    
    png(filename = paste(file.path, "/", survey, p, ".png", sep = ""), width = 6.5, height = 8, 
        units = "in", pointsize = 14, bg = "white", res = 300) 
    
    ## max.figs.per.page/2 implies max always must be even
    par(mfrow = c(max.figs.per.page/2,2), mgp = c(0,0.5,0), mar = c(1.75,1.5,1.75,1), oma = c(8,3,0,0))
    set.panel(max.figs.per.page/2,2)
    
    while (fig.num <= max.figs.per.page & y <= n.years){
      print(c(p, fig.num, y))
      yr <- as.character(years[y])
      print(yr)
      year.dat <- false.array[y,,]
      
      image(x.long, y.depth, t(as.matrix(year.dat)), zlim = c(3.5,false.temp.max), ylim = c(-y.lim,0), col = plasma(256), 
            xaxt = "n", xlab = " ", ylab = " ", main = yr, cex.main = 1.5)
      axis(side = 1, at = xlab, labels = xlab.corr, cex.axis = 0.8)
      box()
      # locator(n=1)
      
      if(fig.num == max.figs.per.page | y == length(years)){
        mtext("Longitude", side = 1, line = 1, outer = T)
        mtext("Depth (m)", side = 2, line = 1, outer = T)
      }
      
      y <- y + 1
      fig.num <- fig.num + 1
    }
    
    fig.num <- 1
    
    ## inside the loop so each page gets its own legend and label
    par(mfrow = c(1,1), mgp = c(0,0.2,0), mar = c(1.8,1.5,1.8,1), oma = c(1,0,14,0))
    temp.list.df <- data.frame(temp.list)
    
    image.plot(legend.only = TRUE, zlim = c(3.5,false.temp.max), col = plasma(256),
               horizontal = TRUE, legend.shrink = 0.5,
               legend.args = list(deg.symbol, col = "black", cex = 1.0, side = 1, line = 2.0))
    
  }
  
  set.panel()
  dev.off(); dev.off()
  
  ## plot fitted values only
  y <- 1
  
  for (p in (1:pages)){
    
    png(filename = paste(file.path, "/", survey, "fitted", p, ".png", sep = ""), width = 6.5, height = 8, 
        units = "in", pointsize = 14, bg = "white", res = 300) 
    
    ## max.figs.per.page/2 implies max always must be even
    # par(mfrow = c(max.figs.per.page/2,2), mgp = c(0,0.5,0), mar = c(1.75,1.5,1.75,1), oma = c(8,3,0,0))
    # set.panel(max.figs.per.page/2,2)
    
    while (fig.num <= max.figs.per.page & y <= n.years){
      print(c(p, fig.num, y))
      yr <- as.character(years[y])
      print(yr)
      year.dat <- false.fit.array[y,,]		
      image(x.long, y.depth, t(as.matrix(year.dat)), zlim = c(3.5,false.temp.max), ylim = c(-y.lim,0), col = tim.colors(64), 
            xaxt = "n", xlab = " ", ylab = " ", main = yr, cex.main = 1.5)
      axis(side = 1, at = xlab, labels = xlab.corr, cex.axis = 0.8)
      box()
      locator(n=1)
      
      if(fig.num == max.figs.per.page | y == length(years)){
        mtext("Longitude", side = 1, line = 1, outer = T)
        mtext("Depth (m)", side = 2, line = 1, outer = T)
      }
      
      y <- y + 1
      fig.num <- fig.num + 1
    }
    
    fig.num <- 1
    
    ## inside the loop so each page gets its own legend and label
    par(mfrow = c(1,1), mgp = c(0,0.2,0), mar = c(1.8,1.5,1.8,1), oma = c(1,0,14,0))
    fit.list.df <- data.frame(fitted.list)
    
    image.plot(legend.only = TRUE, zlim = c(3.5,false.temp.max), col = tim.colors(n = 64),
               horizontal = TRUE, legend.shrink = 0.5,
               legend.args = list(deg.symbol, col = "black", cex = 1.0, side = 1, line = 2.0))
    
  }
  
  set.panel()
  
  # if 2 page output shut off two devices
  if(pages > 1){
    dev.off(); dev.off()
  }else{
    dev.off()
  }
  
  close(channel)
  # rm(file.target)
  
}