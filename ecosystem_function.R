ecosystem.chapter <- function(){
  #############################################################################
  ## Function name:  ecosystem.chapter
  ##		Original code project:  nlaman (F:\AI_GOA\AI2014\EcosystemConsiderations)
  ##		Formal code:  RACE GF (G:\GOA\R\ecosystem considerations)
  ##
  ## General comments:
  ##		Runs from GOA schema (10/09/14) but administers both GOA and AI surveys
  ## 		calls "make.temperature.array", "combined.temperature.analysis", and "image.color.rects"
  ## 		which provide a variety of Oracle and R objects that ultimately lead to the color waterfall
  ## 		graphs of modeled temperature by depth to include in the Ecosystem Considerations chapter
  ## 		of the SAFE document 
  ##
  ## Dependencies:
  ## 		Required packages
  ##			RODBC
  ##			mgcv
  ##			graphics
  ##			fields
  ##
  ##		Oracle Objects
  ##
  ## 		Calls
  ##			make.temperature.array, 
  ##			combined.temperature.analysis
  ##			image.color.rects
  ## 			get.bt.data 
  ##			get.start.times
  ##			get.end.times
  ##			get.bt.data.old
  ##			get.start.times.old 
  ##			get.end.times.old 
  ##			find.inversion.tops
  ## 
  ## Returns:  
  ##			Median date standardized temperature profiles across AI and GOA longitudes
  ##			in the form of a waterfall or heatmap style graphic (png).  Also produces
  ##			Haul-by-haul downcast PDF with thermoclines and inversions highlighted and
  ##			an annual smoothed temperature profile (png) for each survey year.
  ##
  ###################################################################################
  
  # load library
  require(RODBC)
  
  ## Enter Survey and Cruise Year to populate Oracle queries
  survey <- readline("Enter Survey Designation (either GOA or AI):  "); survey <- toupper(survey)
  year <- readline("Enter Cruise Year (e.g., 2005):  "); year.vector <- c(year)
  
  ## select and assign global variable for filepath to where output will be stored
  file.target <<- choose.dir(caption = "Select destination for program output: ")
  # "<<-" indicates assignment of global variable
  
  # open Oracle connection
  schema <- "afsc"#getPass::getPass(msg = "Enter ORACLE schema: ")
  username <-  "AIGOA_WORK_DATA" #getPass::getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                 uid = paste(username),
                                 pwd = paste(password),
                                 believeNRows = FALSE)
  #channel <- odbcConnect(dsn = "AFSC", uid = "olearyc", believeNRows = FALSE)
  
  # collects and processes raw data from Oracle tables and creates cubic spline smoothed temperatures used in annual profile plot
  print("Creating downcasts by haul and detecting thermoclines")
  # browser()	
  make.temperature.array(survey = survey, file.target = file.target, year.vector = year.vector, channel = channel)
  
  # creates data array used to populate waterfall graphs and is dependent on products created in make.temperature.array
  print("Creating annual temperature profiles from smoothed temperature data")
  # browser()	
  source("~/GitHub/ESR_temperature_data/combined_temperature_analysis.R")
  combined.temperature.analysis(survey = survey, file.path = file.target, channel = channel, yr = year)
  
  # creates waterfall graphs of modeled temperatures and fitted values before residuals are added back in
  # modified Oct2014 to use function 'image' so that waterfall colors are mapped directly onto modeled temperature values
  print("Creating waterfall graphs for Ecosystem Considerations chapter")
  # browser()	
  image.color.rects(survey = survey, file.path = file.target, channel = channel, yr = year)
  
  print("The program run has ended.")
  
}