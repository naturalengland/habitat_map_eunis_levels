# some analysis
library(tidyverse)


# read in the file
english_waters <- read_rds("./outputs/english_waters_area_calcs.RDS")


#area calcs
total_area <- sum(english_waters$area_km2)

total_areas_dat <- english_waters %>% 
        dplyr::select(level, area_km2) %>% 
        dplyr::group_by(level) %>% 
        dplyr::summarise(total_areas_km2 = sum(area_km2))

prop_areas <- total_areas_dat %>% 
        group_by(level) %>% 
        summarise(area_percent =total_areas_km2/eval(total_area)*100)


#eunis mapped habitat AREA without assessments
prop_areas %>% dplyr::filter(level < 4) %>% 
        summarise(proportion_below_l3 = sum(area_percent))
#eunis mapped habitat AREA at level where assessmentstake place
100 - prop_areas %>% dplyr::filter(level < 4) %>% 
        summarise(proportion_below_l3 = sum(area_percent))


#change display - for easier reading of percentages
options("scipen" = 10)
options()$scipen
prop_areas



#poly calcs

(total_poly <- english_waters %>%
        tally())

(total_polys <- english_waters %>% 
        group_by(level) %>% 
        dplyr::tally())

prop_poly <- total_polys %>% 
        dplyr::group_by(level) %>% 
        dplyr::summarise(percent_poly = (n/eval(total_poly$n))*100)

#eunis mapped habitat without assessments
prop_poly %>% dplyr::filter(level < 4) %>% 
        summarise(proportion_below_l3 = sum(percent_poly))
#proportion above L4
100- prop_poly %>% dplyr::filter(level < 4) %>% 
        summarise(proportion_below_l3 = sum(percent_poly))


# distinct mapped habitats
## convert to cahracter
english_waters$hab.1 <- as.character(english_waters$hab.1)
english_waters$hab.2 <- as.character(english_waters$hab.2)
english_waters$hab.3 <- as.character(english_waters$hab.3)

hab_categories <- english_waters %>% 
        dplyr::select(hab.1, hab.2, hab.3) %>% 
        replace_na(list(hab.1 = "", hab.2 = "", hab.3 = "")) %>% 
        tidyr::unite("habs", c(hab.1, hab.2, hab.3), sep = "", remove = TRUE) %>% 
        distinct()
