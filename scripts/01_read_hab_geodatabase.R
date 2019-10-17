library(sf)
#library(sp)
library(tidyverse)
library(data.table)
library(cowplot)
library(units)


# Define gis input directory/geopackage for habitat map(s)
input_habitat_map <- "F:/projects/marine_biotope_sensitivity/outputs/habitat_sensitivity_fishing_mosaic_unfiltered.gpkg" 
#or habitat_sensitivity_fishing_multiple_sbgr_mosaic.gpkg
#GIS data for output non-sbgr filter - as it contains habitat info and should be correct otherwise for this calculation: "F:\\projects\\offshore_sensitivity\\marine_offshore_sensitivity\\outputs\\habitat_sensitivity_fishing_sbgr_no_biotope_filter.GPKG"

# Run this to see the available layers in the gis file
sf::st_layers(input_habitat_map)
# Now supply the layer name that you are interest in
offshore_layer <- "offshore_sens"
inshore_layer <- "inshore_sens"

# now read the data in
inshore <- sf::st_read(dsn=input_habitat_map, layer = inshore_layer)
offshore <- sf::st_read(dsn=input_habitat_map, layer = offshore_layer)
#or for English seas -> 12 NM waters
#fgdb <- st_read(dsn="D:/projects/fishing_displacement/2_subprojects_and_data/2_GIS_DATA/Marine habitat/Phil_Fish_Project.gdb", layer = "Input_BSH_Polys_WGS84_Internal_Selection_Clip")

offshore <- offshore %>% dplyr::select(HAB_TYPE, hab.1, hab.2, hab.3, bgr_subreg_id, level, geom)
inshore <- inshore %>% dplyr::select(HAB_TYPE, hab.1, hab.2, hab.3, bgr_subreg_id, level, geom)

#add inshore_offshore_marker_prior to merge
offshore$zone <- "offshore"
inshore$zone <- "inshore"

# Merge data into single data layer
english_waters <- rbind(offshore, inshore)

# add area calculation (km squared)
english_waters$area_m2 <- st_area(english_waters) %>% set_units(km^2)
english_waters <- english_waters %>% dplyr::rename(area_km2 = area_m2)
english_waters$area <- as.numeric(english_waters$area_km2)
english_waters$level <- as.integer(english_waters$level)

rm(inshore, offshore) #frees up a lot of memory

# Data prepared - save enlish_waters as R obj
saveRDS(english_waters, "./outputs/english_waters_area_calcs.RDS")

# stop here and start plotting and analyses scripts in new file.
#-------------------------------------



#for spatial  
#y <- st_drop_zm(fgdb) # drop z dimension from geodatabase
#z <- as(y, "Spatial")

#to allow for easy editing, convert to char
#english_waters$HAB_TYPE <- as.character(english_waters$HAB_TYPE) # convert hab_type to character



#-------------------
#OLDER CODE - I THINK YOU CAN REDO THIs a LOt EASIER WITH THE NEW DATA
#clean data

hab.type.dat$HAB_TYPE <- gsub(" or ", "/", hab.type.dat$HAB_TYPE) # replace ; with / to make consistent
hab.type.dat$HAB_TYPE <- gsub(";", "/", hab.type.dat$HAB_TYPE) # replace ; with / to make consistent
hab.type.dat$HAB_TYPE <- gsub("(8)", "", hab.type.dat$HAB_TYPE) # remove (8) to make consistent
hab.type.dat$HAB_TYPE <- gsub(" #", "", hab.type.dat$HAB_TYPE) # remove (8) to make consistent

# split HAB_TYPE into multiple columns, separating where or statements, or "/" appears to allow for the next step
#hab.type.dat.2 <- str_split(string = hab.type.dat$HAB_TYPE, pattern = " or ", n = Inf, simplify = FALSE) #split by" or "
hab.type.dat.2 <- str_split(string = hab.type.dat$HAB_TYPE, pattern = "/", n = Inf, simplify = FALSE) #split by" or "

# convert lists to data.table so that you can isolate a single row for counting (massive list, so will take  a while to run.)
n.obs <- sapply(hab.type.dat.2, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(hab.type.dat.2, "[", i = seq.max))
rm(hab.type.dat.2, n.obs, seq.max)



#hab.type.dat.3 <- str_split(string = mat[,1], pattern = "/", n = Inf, simplify = FALSE) # split by /
# convert lists to data.table so that you can isolate a single row for counting (massive list, so will take  a while to run.)
#n.obs <- sapply(hab.type.dat.3, length)
#seq.max <- seq_len(max(n.obs))
#mat.2 <- t(sapply(hab.type.dat.3, "[", i = seq.max))
#rm(hab.type.dat.3, n.obs, seq.max, mat) # housekeeping, free up memory by removing unneccessary lists (don't remove hab.type yet - you may need it and then you will have to start from scratch
#(mat.2)

#remove white space
mat <- trimws(mat, which = c("both"))

#count the number of characters in the column - this should refelct the EUNIS habitat level
nchar.hab <- nchar(mat[,1], type = "chars", allowNA = T, keepNA = T)
summary(nchar.hab)



#Quality Assurace Check: NAs
sum(is.na(mat[,1])) # 1257 records are NA
which(is.na(mat[,1]))
length(mat[,1]) # total number of records
sum(is.na(mat[,1]))/length(mat[,1])*100 # percentage polygons with NA in HAB_TYPE

#QA: Any greater than 7?
nchar.hab <- nchar(mat[,1], type = "chars", allowNA = F, keepNA = F)
which(nchar.hab > 7)
summary(nchar.hab)
which(nchar.hab == 3) # there should not be any 3 character long as far as I understand

plot.dat <- data.frame(matrix(nchar.hab, nrow = length(nchar.hab), ncol = 1))
names(plot.dat) <- c("n.char")
plot.dat.2 <- as.data.frame(cbind(plot.dat$n.char, hab.type.dat$Shape_Area))
names(plot.dat.2) <- c("n.char", "area")
head(plot.dat.2)


#Eunis level: e.g. codes: number of chars
#---------------------------------------
#Eunis level 1  : A : 1
#Eunis level 2  : A1 : 2
#Eunis level 3  : A1.2 : 4
#Eunis level 4 : A1.21 : 5
#Eunis level 5 : A1.123 : 6
#Eunis level 6 : A5.5331 : 7

#now add into data

plot.dat.2$EUNIS.level <- plot.dat.2$n.char # add column EUNIS level and assign n.char as its value
plot.dat.2$EUNIS.level[plot.dat.2$n.char == 0] <- NA # #assign 0 a absurd value
plot.dat.2$EUNIS.level[plot.dat.2$n.char == 3] <- NA # remove 3 letter polys for now
plot.dat.2$EUNIS.level[plot.dat.2$n.char >= 4] <- (plot.dat.2$EUNIS.level[plot.dat.2$n.char >= 4]-1)
plot.dat.2$HAB_TYPE <- hab.type.dat$HAB_TYPE # add the HAB_TYPE value

dim(plot.dat.2)
unique(plot.dat.2$n.char)
unique(plot.dat.2$EUNIS.level)
str(plot.dat.2)


#data sumamry by area
total.area <- sum(plot.dat.2$area)
plot.dat.2 %>% group_by(EUNIS.level) %>% 
  summarise(area = (sum(area)/total.area)*100)

#number (percent) of polygons
plot.dat.2 %>% group_by(EUNIS.level) %>% 
  tally()#/(nrow(plot.dat.2))*100



#summary at level
area.E.l.4 <- plot.dat.2 %>% filter(EUNIS.level == 4) %>% summarise(sum(area)) # total area of level
hab.results <- as.tibble(plot.dat.2) %>%  #filter(EUNIS.level == 4) %>% 
  select(EUNIS.level, HAB_TYPE, area)  %>%
  group_by(HAB_TYPE) %>% 
  summarise(sum.area = sum(area), mean.area = mean(area), sd.area = sd(area)) %>% 
  arrange(desc(sum.area))
tmp <- hab.results$sum.area/total.area*100

#plot.dat <- match EUNIS level to nchar

#plot
#mat[,1] <- as.factor(nchar.hab) #convert to factor
#data=subset(iris, !is.na(Sepal.Length))

area.plot.dat <- subset(plot.dat.2, !is.na(EUNIS.level)) %>%
  # Create the variable you need for the plot
  mutate(EUNIS.level,
         fill_col = case_when(
           EUNIS.level < 4 ~ "grey",
           EUNIS.level >= 4 ~ "red"
         ))


#df %>%
#  ggplot(aes(x = factor, y = mean, fill = concern)) + 
#  geom_col() + 
#  coord_flip() +
#  scale_x_discrete(limits = rev(levels(df$factor))) +
#  scale_fill_manual(
#    values = pal,
#    limits = names(pal)
#  )


plot_area <- ggplot(data = area.plot.dat, aes(x = EUNIS.level, y = area, fill = as.integer(EUNIS.level), alpha = 0.5))+
  geom_col() +
  theme(axis.title = element_text(size = 28),
        axis.text.x = element_text(color="black", 
                                   size=20,
                                   margin = margin(t = 0, r = 20, b = 0, l = 1)),
        axis.text.y = element_text(color = "black",
                                   size = 20,
                                   margin = margin(t = 0, r = 20, b = 0, l = 10)))+
    scale_x_continuous() +      
        scale_fill_manual(
        values = fill_col#,
        #limits = names(fill_col)
        ) +
  labs(x = "EUNIS level", y = "Area") +
  theme(legend.title = element_blank(),
        legend.position = "none")


plot_area <- ggplot(data = subset(plot.dat.2, !is.na(EUNIS.level)), aes(x = EUNIS.level, y = area))+
  geom_col() +
  theme(axis.title = element_text(size = 28),
        axis.text.x = element_text(color="black", 
                                   size=20,
                                   margin = margin(t = 0, r = 20, b = 0, l = 1)),
        axis.text.y = element_text(color = "black",
                                   size = 20,
                                   margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  labs(x = "EUNIS level", y = "Area") +
  theme(legend.title = element_blank())


plot_area <- ggplot(data = subset(plot.dat.2, !is.na(EUNIS.level)), aes(x = EUNIS.level, y = area))+
  geom_col(aes(fill = ifelse(plot.dat.2$EUNIS.level[!is.na(plot.dat.2$EUNIS.level)] < 4,"salmon","lightblue"), aplha = 0.7)) +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(color="black", 
                                   size=12,
                                   margin = margin(t = 0, r = 20, b = 0, l = 1)),
        axis.text.y = element_text(color = "black",
                                   size = 12,
                                   margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  labs(x = "EUNIS level", y = "Area in square degrees") +
  theme(legend.title = element_blank(),
        legend.position = "none")


plot_polys <- ggplot(data = subset(plot.dat.2, !is.na(EUNIS.level)), aes(x = EUNIS.level, alpha = 0.7))+
  geom_histogram(binwidth=1, position="identity", colour = c("#00BFC4", "#00BFC4", "#00BFC4","salmon", "salmon", "salmon"), fill = c("#00BFC4", "#00BFC4", "#00BFC4","salmon", "salmon", "salmon")) +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(color="black", 
                                   size=12,
                                   margin = margin(t = 0, r = 20, b = 0, l = 1)),
        axis.text.y = element_text(color = "black",
                                   size = 12,
                                   margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  labs(x = "EUNIS level", y = "Number of polygons") +
  theme(legend.title = element_blank(),
        legend.position = "none")

require(cowplot)
theme_set(theme_cowplot(font_size=12)) 
plot_output <- cowplot::plot_grid(plot_area, plot_polys, labels = c('A)', 'B)'), align = 'h')
#png::writePNG(plot_output,target = "F:/projects/marine_biotope_sensitivity/report/figures/eunis_area_polys_plot.png", dpi = 150)  
save_plot("F:/projects/marine_biotope_sensitivity/report/figures/eunis_area_polys_plot.png", plot_output,base_aspect_ratio = 1.2)  
