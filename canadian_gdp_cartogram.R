
# If Canadian Regions were Sized relative to their GDP Contribution
# Author: ZygoMattic 
# Github: https://github.com/Fehiroh


#  Libraries ###############################################################
# Setup Pacman 
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# install/load libraries 
p_load(tidyverse, raster, sf, cartogram, broom, maptools, mapproj,
       viridis, tmap, magick)


# Input Setup ##############################################################
home <- "file_path_to_the_folder_you've_stored_the_shape_file_and_csv_in/Cartogram"

# point at your shp
shp <- st_read(paste0(home, "/shp_file/gpr_000b11a_e.shp")) %>%
  mutate(PRUID = as.numeric(as.character(PRUID)))

# point at the csv you've made
gdp_csv <- read_csv(paste0(home, "/canadian_gdp.csv")) %>% 
  rename(PRENAME = province_or_territory) 


# Merging csv information into the spatial data  #########################
merged <- merge(shp, gdp_csv, by = "PRUID")


merged2 <- st_transform(merged, crs = crs("+init=epsg:4326", asText = TRUE))
merged2 <- st_simplify(merged2, dTolerance = 0.29) 
merged2$id <- as.character(row_number(merged2$PRUID))
merged2$share_of_nat_gdp <- as.numeric(merged2$share_of_nat_gdp)

# turn the sfc into a spatialpolygonDataFrame and give it a projection to play
# nice with cartogram
merged3 <-as(merged2, "Spatial")
merged3 <- spTransform(merged3, CRS("+init=epsg:26978"))

# setting up tmap options ####################################################################
tmap_mode("plot")

tmap_options(bg.color = "darkslategray", legend.text.color = "white", legend.title.color = "white", 
             title.color = "white", title.position = c(0.65, 0.925), legend.stack = "horizontal",
             legend.position = c(0.7, 0.675), title.fontfamily = "serif", 
             legend.title.fontfamily = "serif", legend.text.fontfamily = "serif", title.size = .6,
             title.fontface = 2, outer.bg.color = "darkslategray", frame = FALSE, legend.text.size = .5, 
             legend.title.size = 0.7)

# Creating the Gif ##########################################################################
# starting an image 
img <- image_graph(600, 340, res = 96)

# making 30 iteractions at various stages of completion
for(i in 1:30){
  gdp_cartogram <- cartogram(merged3, "share_of_nat_gdp", itermax = i)
  
  a <- tm_shape(gdp_cartogram) +
    tm_polygons("share_of_nat_gdp",  palette = "viridis", legend.title = "Share of National GDP") + 
    tm_shape(gdp_cartogram) + 
    tm_borders(col = "white", lwd = 1.5)
  
  print(a)
}

dev.off()

animation <- image_animate(img, fps = 10)
print(animation)


# Saving the gif ########################################################################
image_write(animation, paste0(home, "/cartogram.gif"))


