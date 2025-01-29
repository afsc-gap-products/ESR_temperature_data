get.haul.pos <- function(channel = NA, region = region, year = year){
     # accept channel
     # accept region or survey name
     # accept user.name and password
     # eliminates non-summer months

     print("Getting haul location, bottom depth, and assigning subregion...")

     if(region == "GOA"){

     # institutes updated subregion appellations provided by Bridget Ferriss in 2021 from the Oracle side
     # Interestingly, they want the data broken up into WGOA (< -147W) and EGOA (>= -147W)
     # It would be of benefit to move this assignment of regions to the R code so easier to change in the future
     # or could be allowed to revert to older appellations (e.g., INPFC areas used in GOA samplign design)

     dat.query <- paste0("select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth,
          'wgoa' subregion from racebase.haul where region = 'GOA' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and start_longitude < -147 and extract(month from start_time) between 5 and 9
          union
          select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth,
          'egoa' subregion from racebase.haul where region = 'GOA' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and start_longitude >= -147 and extract(month from start_time) between 5 and 9")

     pos.dat <- RODBC::sqlQuery(channel = channel, query = dat.query)
     names(pos.dat) <- casefold(names(pos.dat))

     }

     if(region == "AI"){
       dat.query <- paste0("select vessel, cruise, haul, start_longitude longitude, start_latitude latitude, bottom_depth
          from racebase.haul where region = 'AI' and floor(cruise/100) between 1993 and ", year, " and vessel not in
          (21,159) and start_longitude is not null and start_latitude is not null and bottom_depth is not null
          and extract(month from start_time) between 5 and 9")
        
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
