library(ggplot2)
library(RColorBrewer)

# http://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
# Create a custom color scale for region
# display.brewer.all()
# myColors <- brewer.pal(7, "Dark2")
# names(myColors) <- levels(vdat$region)
# colScale <- scale_colour_manual(name = "grp",values = myColors)

# I need 4 colors, one for each scenario

# color blind palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


##### SCENARIO #####


cb_pal_scenario <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
names(cb_pal_scenario) <- c(
col_scale_nation <- scale_colour_manual(name = "Nation", values = pal_nation)



get_region_palette <- function(vdat){
  
  cb_pal_region <- c("#999999", "#E69F00", "#56B4E9", 
                     "#009E73", "#0072B2", "#D55E00", "#CC79A7")
  names(cb_pal_region) <- levels(vdat$region)
  col_scale_region <- scale_colour_manual(name = "Region",values = cb_pal_region)
  
  return(col_scale_region)
  
}



##### NATION #####


