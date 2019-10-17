#libraries
library(tidyverse)
library(cowplot)

# plot
#read data
english_waters <- read_rds("./outputs/english_waters_area_calcs.RDS")

#plotting variable
english_waters_subset <- subset(english_waters, !is.na(level)) %>%
        dplyr::filter(level > 0) %>% 
        # Create the variable you need for the plot
        mutate(level,
               fill_col = case_when(
                       level < 4 ~ "grey",
                       level >= 4 ~ "red"
               ))


#plot_area 1
#plot_area <- ggplot(data = english_waters_subset, aes(x = level, y = area, fill = level, alpha = 0.5))+
#       geom_col() +
#        theme(axis.title = element_text(size = 28),
#              axis.text.x = element_text(color="black", 
#                                         size=20,
#                                         margin = margin(t = 0, r = 20, b = 0, l = 1)),
#              axis.text.y = element_text(color = "black",
#                                         size = 20,
#                                         margin = margin(t = 0, r = 20, b = 0, l = 10)))+
#        scale_x_continuous() +      
#        scale_fill_manual(
#                values = fill_col#,
#                #limits = names(fill_col)
#        ) +
#        labs(x = "EUNIS level", y = "Area in square kilometers") +
#        theme(legend.title = element_blank(),
#              legend.position = "none")

#plot_area 2
plot_area <- ggplot(data = english_waters_subset, aes(x = as.factor(level), y = area))+
        geom_col(aes(fill = ifelse(english_waters_subset$level[!is.na(english_waters_subset$level)] < 4,"salmon","lightblue"), 
                     alpha = 0.7)) +
        theme(axis.title = element_text(size = 14),
              axis.text.x = element_text(color="black", 
                                         size=12,
                                         margin = margin(t = 0, r = 20, b = 0, l = 1)),
              axis.text.y = element_text(color = "black",
                                         size = 12,
                                         margin = margin(t = 0, r = 20, b = 0, l = 10)))+
        xlab("EUNIS level") +#, y = "Area in square kilometers") + #xlab(bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))
        ylab(bquote('Area ('*km^2*')')) +
        theme(legend.title = element_blank(),
              legend.position = "none")
plot_area


plot_polys <- ggplot(data = english_waters_subset, aes(x = level, alpha = 0.7))+
        geom_histogram(binwidth=1, position="identity", 
                       colour = c("#00BFC4", "#00BFC4", "#00BFC4","salmon", "salmon", "salmon"), 
                       fill = c("#00BFC4", "#00BFC4", "#00BFC4","salmon", "salmon", "salmon")) +
        scale_x_continuous(breaks=c(1:6)) +
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

#plot_polys
require(cowplot)
theme_set(theme_cowplot(font_size=12)) 
plot_output <- cowplot::plot_grid(plot_area, plot_polys, labels = c('A)', 'B)'), align = 'h')
#png::writePNG(plot_output,target = "F:/projects/marine_biotope_sensitivity/report/figures/eunis_area_polys_plot.png", dpi = 150)  
save_plot("F:/projects/marine_biotope_sensitivity/report/figures/eunis_area_polys_plot.png", plot_output,base_aspect_ratio = 1.618)  
