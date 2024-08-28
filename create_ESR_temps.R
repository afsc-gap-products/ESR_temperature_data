source(paste0(getwd(),"/R/make.esr.temperature.data.R"))
make.esr.temperatures()

##### GULF OF ALASKA #######
# 
# temperature_summary <- read.csv(paste0(getwd(),"/temperature_summary.csv"))
# temperature_summary$year <- as.integer(substring(temperature_summary$ob_time,1,4))
# 
# library(dplyr)
# library(tidyverse)
# total_mean_SST <- mean(temperature_summary$surface_temp[which(temperature_summary$year >= 1993 & temperature_summary$year <= 2013)], na.rm=TRUE)
# total_mean_BT <- mean(temperature_summary$temp200m[which(temperature_summary$year >= 1993 & temperature_summary$year <= 2013)], na.rm=TRUE)
# 
# mean_temp <- temperature_summary   %>%
#   group_by(year,subregion) %>%
#   #group_by(year,inpfc_area) %>%
#   summarise(mean_SST = mean(surface_temp , na.rm = TRUE),
#             mean_BT = mean(temp200m, na.rm=TRUE),
#             mean_SST_SD = mean(surface_temp_sd, na.rm=TRUE),
#             mean_BT_SD = mean(temp200m_sd, na.rm=TRUE),
#             SST_anomaly = mean_SST - total_mean_SST,
#             BT_anomaly = mean_BT - total_mean_BT)
# 
# all_years <- data.frame(year = c(1993:2023))
# mean_temp_all_years <- bind_rows(mean_temp, all_years) %>% arrange(year)
# mean_temp$subregion[which(mean_temp$subregion == "wgoa")] <- "western GOA"
# mean_temp$subregion[which(mean_temp$subregion == "egoa")] <- "eastern GOA"
# mean_temp$subregion[which(mean_temp$subregion == "cgoa")] <- "central GOA"
# 
# library(ggplot2)
# ggplot2::ggplot(mean_temp, aes(x=year, y = mean_SST)) +
#   geom_point(col = "orange", size = 6) +
#   geom_errorbar(aes(ymin=mean_SST-mean_SST_SD, ymax=mean_SST+mean_SST_SD), width=2,
#                 position=position_dodge(0.9), col = "orange") +
#   geom_point(aes(x=year,y=mean_BT), col = "purple", size = 6) +
#   geom_errorbar(aes(ymin=mean_BT-mean_BT_SD, ymax=mean_BT+mean_BT_SD), width=2,
#                 position=position_dodge(0.9), col = "purple") +
#   geom_hline(yintercept = mean(mean_temp$mean_SST, na.rm=TRUE), col = "orange", size = 2) +
#   geom_hline(yintercept = mean(mean_temp$mean_BT, na.rm=TRUE), col = "purple", size = 2) +
#   ggtitle("Mean SST and Bottom Temp") +
#   ylab("mean temperature (°C)") +
#   theme_bw() +
#   scale_x_continuous(breaks = round(seq(min(mean_temp$year), 2025, by = 4),1)) + #max(mean_temp$year)
#   #scale_x_discrete(limits=c("1994","2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022")) +
#   theme(text = element_text(size = 23),
#         legend.text = element_text(size = 4),
#         legend.title = element_text(size = 17),
#         axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 25),
#         axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5, size = 21),
#         axis.ticks=element_line(size=2),
#         axis.ticks.length=unit(0.25, "cm")) +
#   #facet_grid(~factor(inpfc_area, levels=c('Western Aleutians', 'Central Aleutians', 'Eastern Aleutians', "Southern Bering Sea")))
#   facet_grid(~factor(subregion, levels=c('western GOA', 'central GOA', 'eastern GOA')))
# 
# ##anomaly
# ggplot2::ggplot(mean_temp, aes(x=year, y = SST_anomaly)) +
#   geom_point(col = "orange", size = 5) +
#   geom_point(aes(x=year,y=BT_anomaly), col = "purple", size = 5) +
#   geom_hline(yintercept = mean(mean_temp$SST_anomaly, na.rm=TRUE), col = "orange", size = 1) +
#   geom_hline(yintercept = mean(mean_temp$BT_anomaly, na.rm=TRUE), col = "purple", size = 1) +
#   ggtitle("Mean SST and Bottom Temp Anomalies") +
#   ylab("mean temperature anomaly (°C)") +
#   theme_bw() +
#   scale_x_continuous(breaks = round(seq(min(mean_temp$year), max(mean_temp$year), by = 4),1)) +
#   #scale_x_discrete(limits=c("1994","2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022")) +
#   theme(text = element_text(size = 23),
#         legend.text = element_text(size = 17),
#         legend.title = element_text(size = 17),
#         axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 25),
#         axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5, size = 21),
#         axis.ticks=element_line(size=2),
#         axis.ticks.length=unit(0.25, "cm")) +
#   #facet_grid(~factor(inpfc_area, levels=c('Western Aleutians', 'Central Aleutians', 'Eastern Aleutians', "Southern Bering Sea")))
#   facet_grid(~factor(subregion, levels=c('western GOA', 'central GOA', 'eastern GOA')))
# 
# 
# 
# temperature_summary_2023 <- temperature_summary[which(temperature_summary$year == 2023),]
# 
# sst <- ggplot2::ggplot(temperature_summary, aes(x=year, y = surface_temp)) +
#   geom_point(col = "orange", size = 3) +
#   ylab("SST") +
#   theme_bw() +
#   theme(text = element_text(size = 23),
#         legend.text = element_text(size = 17),
#         legend.title = element_text(size = 17))+
#   facet_grid(~subregion)
# 
# 
# bt <- ggplot2::ggplot(temperature_summary, aes(x=year, y = temp200m)) +
#   geom_point(col = "purple", size = 3) +
#   ylab("BT") +
#   theme_bw() +
#   theme(text = element_text(size = 23),
#         legend.text = element_text(size = 17),
#         legend.title = element_text(size = 17))+
#   facet_grid(~subregion)
# 
# gridExtra::grid.arrange(sst,bt,nrow=1)


##### ALEUTIANS #######

temperature_summary <- read.csv(paste0(getwd(),"/temperature_summary.csv"))
temperature_summary$year <- as.integer(substring(temperature_summary$ob_time,1,4))

library(dplyr)
library(tidyverse)
total_mean_SST <- mean(temperature_summary$surface_temp[which(temperature_summary$year >= 1994 &
                                                                temperature_summary$year <= 2014)], na.rm = TRUE)
total_mean_BT <- mean(temperature_summary$temp200m[which(temperature_summary$year >= 1994 & 
                                                           temperature_summary$year <= 2014)], na.rm = TRUE)
area_mean_BT <- temperature_summary %>%
  group_by(inpfc_area) %>%
  summarise(mean_SST_20y = mean(surface_temp[which(year >= 1994 &
                                                     year <= 2014)], na.rm = TRUE),
            mean_BT_20y = mean(temp200m[which(year >= 1994 &
                                                year <= 2014)], na.rm = TRUE),
            meansd_SST_20y = mean(surface_temp_sd[which(year >= 1994 &
                                                           year <= 2014)], na.rm = TRUE),
            meansd_BT_20y = mean(temp200m_sd[which(year >= 1994 &
                                                        year <= 2014)], na.rm = TRUE))

temperature_match <- left_join(temperature_summary, area_mean_BT, "inpfc_area")
  
# Need to change to use 1994-2014 (where is 1991?)
mean_temp <- temperature_match %>%
  group_by(year, inpfc_area) %>% # not in original script, changed in the GOA
  summarise(mean_SST = mean(surface_temp , na.rm = TRUE),
            mean_BT = mean(temp200m, na.rm = TRUE),
            mean_SST_SD = mean(surface_temp_sd, na.rm = TRUE),
            mean_BT_SD = mean(temp200m_sd, na.rm = TRUE),
            SST_20y = mean(mean_SST_20y, na.rm = TRUE),
            BT_20y = mean(mean_BT_20y, na.rm = TRUE)) %>% # changed to use 20 year mean instead
  mutate(SST_anomaly = mean_SST - SST_20y,
         BT_anomaly = mean_BT - BT_20y)


all_years <- data.frame(year = c(1994:2022))
mean_temp_all_years <- bind_rows(mean_temp, all_years) %>% arrange(year)

library(ggplot2)
ggplot2::ggplot(mean_temp, aes(x = year, y = mean_SST)) +
  geom_point(col = "orange", size = 6) +
  geom_errorbar(aes(ymin = mean_SST - mean_SST_SD, 
                    ymax = mean_SST + mean_SST_SD), 
                width = 2, 
                position = position_dodge(0.9), 
                col = "orange") +
  geom_point(aes(x = year, 
                 y = mean_BT), 
             col = "purple", 
             size = 6) +
  geom_errorbar(aes(ymin = mean_BT - mean_BT_SD,
                    ymax = mean_BT + mean_BT_SD), 
                width = 2, 
                position = position_dodge(0.9), 
                col = "purple") +
  geom_hline(yintercept = mean(mean_temp$mean_SST, na.rm = TRUE), 
             col = "orange", 
             size = 2) +
  geom_hline(yintercept = mean(mean_temp$mean_BT, na.rm = TRUE), 
             col = "purple", 
             size = 2) +
  ggtitle("Mean SST and Bottom Temp") +
  ylab("mean temperature (°C)") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(mean_temp$year), 2025, by = 4), 1)) + # max(mean_temp$year)
  # scale_x_discrete(limits=c("1994","2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022")) +
  theme(text = element_text(size = 23),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 17),
        axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 25),
        axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.5, size = 21),
        axis.ticks = element_line(size = 2), 
        axis.ticks.length = unit(0.25, "cm")) +
  facet_grid(~ factor(inpfc_area,
                      levels = c('Western Aleutians', 'Central Aleutians', 'Eastern Aleutians', "Southern Bering Sea")))

## anomaly plot
ggplot2::ggplot(mean_temp, aes(x = year, y = SST_anomaly)) +
  geom_point(col = "orange", size = 5) +
  geom_point(aes(x = year, y = BT_anomaly),
             col = "purple",
             size = 5) +
  geom_hline(yintercept = mean(mean_temp$SST_anomaly, na.rm = TRUE),
             col = "orange",
             size = 1) +
  geom_hline(yintercept = mean(mean_temp$BT_anomaly, na.rm = TRUE),
             col = "purple",
             size = 1) +
  ggtitle("Mean SST and Bottom Temp Anomalies") +
  ylab("mean temperature anomaly (°C)") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(mean_temp$year), max(mean_temp$year), by = 4), 1)) +
  # scale_x_discrete(limits = c("1994", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")) +
  theme(text = element_text(size = 23),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 25),
        axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5, size = 21),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm")) +
  # facet_grid(~factor(inpfc_area, levels=c('Western Aleutians', 'Central Aleutians', 'Eastern Aleutians', "Southern Bering Sea")))
  facet_grid(~ factor(inpfc_area, levels=c('Western Aleutians', 'Central Aleutians', 'Eastern Aleutians', "Southern Bering Sea")))



temperature_summary_2022 <- temperature_summary[which(temperature_summary$year == 2022),]

sst <- ggplot2::ggplot(temperature_summary, 
                       aes(x = year, y = surface_temp)) +
  geom_point(col = "orange", size = 3) +
  ylab("SST") +
  theme_bw() +
  theme(text = element_text(size = 23),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17))+
  facet_grid(~ inpfc_area)


bt <- ggplot2::ggplot(temperature_summary, 
                      aes(x = year, y = temp200m)) +
  geom_point(col = "purple", size = 3) +
  ylab("BT") +
  theme_bw() +
  theme(text = element_text(size = 23),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17))+
  facet_grid(~ inpfc_area)

gridExtra::grid.arrange(sst, bt, nrow=1)
