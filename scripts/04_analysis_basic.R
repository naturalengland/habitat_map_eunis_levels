# some analysis
library(tidyverse)

# read in the file
english_waters <- read_rds("./outputs/english_waters_area_calcs.RDS")


#area calcs
total_area <- sum(english_waters$area)

total_areas_dat <- english_waters %>% dplyr::group_by(level) %>% 
        dplyr::summarise(total_areas_km2 = sum(area))

prop_areas <- total_areas_dat %>% 
        group_by(level) %>% 
        summarise(area_percent =total_areas_km2/eval(total_area)*100)

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
