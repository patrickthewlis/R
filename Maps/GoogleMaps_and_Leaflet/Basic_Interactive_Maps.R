# Specify Path

.libPaths("")

# Set Working Directory

setwd("~/R")

lapply(c("RCurl", "XML", "plyr", "dplyr", "tidyr",
         "tmap", "ggmap", "ggplot2",
         "readxl", "sp", "foreign", "RgoogleMaps",
         "rgdal", "rgeos", "maps", "maptools"),
       library, character.only = TRUE)




###################################################################################
#                        FIRST DATASET: PRIVATE UNIVERSITIES
###################################################################################


############################
#   SETTING UP THE DATA
############################


# Read in the Excel File

universities <- read_excel("Universities_Addresses.xlsx", 1) # Select Sheet 1

# universities <- universities[-c(1), ] --> Delete Row 1 if this is null

View(universities)
head(universities) # Preview 1st 6 observations/rows

# Subset to the Columns of Interest

df <- universities[,c(1, 5:6)]
head(df)

# Change Data into SpatialPointsDataFrame

coords <- cbind(Longitude = as.numeric(as.character(universities$Longitude)),
                Latitude = as.numeric(as.character(universities$Latitude)))

# Make sure we are working with rows that do NOT have missing values

coords <- coords[complete.cases(coords),]


# Define Coordinates Reference System (CRS = Coordinates Reference System)

map_crs <- CRS("+proj=longlat +ellps=WGS84")

# Put into Spatial Data Frame

univ_points <- SpatialPointsDataFrame(coords, universities[, -(2:3)], proj4string = map_crs)






########################################
#       SETTING UP THE BASEMAP
#               LAYER 1
########################################

pakistan <- get_map(location = c(lon = 69.3, lat = 30.0),
                    zoom = 6, crop = T, scale = "auto",
                    color = "color", maptype = "hybrid")

# For countries, smaller zoom is good. For cities, a zoom
# of 8,9,10 will be more suitable.

# Call the basemap

basemap <- ggmap(pakistan, extent = "panel", padding = 0)




########################################
#       ADDING POINTS TO THE MAP
#               LAYER 2
########################################

# Points not in the lat/lon range of the basemap layer will be excluded

# Add points to the basemap
# Define x-axis as Longitude / y-axis as Latitude

newmap <- basemap +
  geom_point(data = df, aes(x = long, y = lat), size = 4,
             pch = 21, fill = "red") +
  ggtitle("Universities in Pakistan")

print(newmap) # Print Map




##########################################
#       USING GOOGLE MAPS
##########################################


with(df, PlotOnStaticMap(lat = lat, lon = long, zoom = 7,
                         size = c(640,640), pch = 21,
                         bg = "orange", col = "black",
                         FUN = points, add = F))








###################################################################################
#                     SECOND DATASET: PUBLIC UNIVERSITIES
###################################################################################


############################
#   SETTING UP THE DATA
############################


# Public Universities

public_universities <- read_excel("Universities_Addresses.xlsx", 2)
View(public_universities)
head(public_universities)

# Subset to the Columns of Interest

pub_df <- public_universities[,c(1, 5:6)]
head(pub_df)






###################################################################################
#                 MAIN DATASET:  PRIVATE AND PUBLIC UNIVERSITIES
###################################################################################



#####################################
# Setting up the Data
#####################################


# Add "Type of School" variable to both datasets

# Make new variables (Public)

public_universities$typeschool <- "public" # Size of markers

# Make new variables (Private)

universities$typeschool <- "private" # Size of markers



# Merge the Datasets
# Given we have same columns/variables and the same number of
# columns, we just need to merge by row

pub_priv <- rbind(universities, public_universities) 
View(pub_priv)
head(pub_priv)

# If we had given them ID numbers of cluster numbers, we can update them
# Using an "ID" variable
# This will set the first row at 1,...,n

pub_priv$id <- 1:nrow(pub_priv)

# Subset to the Columns of Interest

main_df <- pub_priv[,c(1, 5:8)]
head(main_df)





#####################################

#             GOOGLE MAPS

#####################################

########################################
#       SETTING UP THE BASEMAP
#               LAYER 1
########################################


# Setting up map

lon <- c(66,75) # define the map's y-axis limits i.e. longitude
lat <- c(26,34) # define the map's x-axis limits i.e. latitude

center = c(mean(lat), mean(lon)) # Tell which point to center on

zoom <- 7 # Suitable zoom for a country (range: 1-20)

pub_pak_map <- GetMap(center = center, zoom = zoom, maptype = 'hybrid',
                      destfile = "public_pakistan.png")


########################################
#       ADDING POINTS TO THE MAP
#               LAYER 2
########################################

# Make new variables

main_df$size <- "small" # Size of markers
main_df$col <- "red" # Colour of markers
main_df$char <- ""

# Create dataframe which contains points

df_markers <- cbind.data.frame(main_df$long,
                               main_df$lat,
                               main_df$size,
                               main_df$col,
                               main_df$char)

# Name the markers

names(df_markers) <- c("lon", "lat", "size", "col", "char")
View(df_markers)

# Draw subset of the dataframe

df_markers <- df_markers[c(1:18,50:64),] # Take Rows 1-18, and 50-64


#####################################
#        Using Get_GoogleMap 
#####################################


# Read Map from Google Maps 

ImageData <- get_googlemap(c(lon = 69.3, lat = 30.0),
                           zoom = 6, maptype = 'hybrid')

# Save Data to File

save(ImageData, file="pakistan.rda")

ggmap(ImageData) + 
  geom_point(aes(x = long, y = lat), data = main_df,
             colour = "orange", size = 2)



#####################################
#        Using GetMap.bbox
#####################################

bb <- qbbox(lat = df_markers[,"lat"], lon = df_markers[, "lon"]) # Get columns for latitude and longitude

# Download the Map Using GetMap.bbox
# Queries the Google Server for a Static Map

# Option 1
public_pakistan_map <- GetMap.bbox(bb$lonR, bb$latR,
                                   destfile = "public_pakistan.png",
                                   markers = df_markers,
                                   zoom = 6,
                                   maptype = 'hybrid')

# Option 2
maps <- GetMap.bbox(lonR = range(lon), # Define range/limits of longitude
                    latR = range(lat),
                    center = center,
                    destfile = "public_pakistan.png",
                    markers = df_markers,
                    zoom = 6,
                    maptype = 'hybrid')





###########################################

#              SHAPEFILE

###########################################



###########################################
#           Using a SHAPEFILE

# Step 1: Importing a Map from a Shapefile

###########################################


# Importing Shapefile

pub_shp <- file.choose() # Find folder where shapefile is stored
# OR pub_shp <- rgdal::readOGR(file.choose())

# Read-in the Shapefile

pub_pak_shp <- readOGR(pub_shp)

# Identify the object type

class(pub_pak_shp) # a SpatialPolygonsDataFrame


# Construct shapefile as a dataframe
# 1) ggplot does NOT work with spatial objects like shape files
# 2) Convert Shapefile into Dataframe that ggmap will know what to do with
# 3) Command: "fortify"

pub_shp_df <- fortify(pub_pak_shp)



# Explore structure of dataframe

dim(pub_shp_df) # How many variables and observations

class(pub_shp_df)

head(pub_shp_df)

names(pub_shp_df) # Variable names are different




###########################################
#           Mapping Points 

#       Step 2: Creating the Map

###########################################


# Creating the map
# 1) Use Geom_Point to Map the Borders  
# 2) Use Geom_Path to Join the Borders
# 3) Use "mercator" for a neater output


pub_pak_map <- ggplot() +
  geom_point(data = pub_shp_df, aes(x = long, y = lat, group=group)) +
  geom_path() + coord_map("mercator")

print(pub_pak_map) 


# Alternative: Using Plot Command

# Plot the Shapefile on the Map
# Use Non-Dataframe Shapefile

# Clear old plot, and setting up new plot

plot.new() # Also use if plot graphics device does not automatically open and we get a plot.new error
# This will open up a new device screen


# Plot the Base Map

plot(pub_pak_shp) 

# Add Features
# Fill colour = Sky Blue

plot(pub_pak_shp, add = TRUE, xlim = lon, ylim = lat,
     col = alpha("sky blue", 0.6), border = FALSE)

# Add Features
# Fill colour = Gray

plot(pub_pak_shp, add = TRUE, xlim = lon, ylim = lat,
     col = "gray90", border = TRUE) # Retain the Borders

# Map Cities onto the Shapefile

map.cities(x = world.cities, country = "Pakistan",
           capitals = 2,
           label = TRUE,
           cex = 0.6, xlim = lon, ylim = lat,
           pch = 13)


###########################################
#           Mapping Points 

#   Step 3: Adding Markers to the Map

###########################################

# pch refers to the "plotting character" i.e. plot symbol --> pch = 2 is a triangle
# cex refers to the size of the markers

points(main_df$long, main_df$lat, cex = 1.5, col = "yellow", pch = 18)

# Add Features to the Points











###########################################

#                LEAFLET

###########################################



###########################################
#            INTERACTIVE MAP
#             Using Leaflet
###########################################

library(leaflet)
library(viridis)

# Browse Options of Open Street Map Tiles
# Link: http://leaflet-extras.github.io/leaflet-providers/preview/index.html

# Map 1

leaflet(main_df) %>%
  addTiles() %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~as.character(university),
             label = ~as.character(university))
addLegend("bottomright", colors= "red", labels="Community ",
          title="Public and Private Universities in Pakistan")

# Map 2

leaflet(main_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~as.character(university),
             label = ~as.character(university)) %>%
  addLegend("bottomright", colors= "red", labels="University",
            title="Public and Private Universities in Pakistan")

# Map 3

leaflet(main_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(lng = ~long, 
                   lat = ~lat, 
                   radius = 1.5, 
                   color = "yellow",
                   stroke=FALSE,
                   fillOpacity = 0.8) %>%
  addLegend("bottomright", colors= "yellow", labels="University", title="Public and Private Universities in Pakistan")

# Map 4

leaflet(main_df) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~as.character(university),
             label = ~as.character(university)) %>%
  addLegend("bottomright", colors= "red", labels="University",
            title="Public and Private Universities in Pakistan")



###########################################
#           Modifying Markers

#       Step 2: Markers by Type

###########################################


###############
#   OPTION 1  #
###############

# Create "if" argument for public and private universities
# Use "lapply" instead of "sapply"
# "lapply" returns a list of schoolids, each individual one as independent
# "sapply" returns a vector of schoolids, so just groups them as one
# Therefore, when using "sapply" we only get ONE type of schooltype,
and thus it will NOT distinguish between different types

type_color <- function(main_df) {
  lapply(main_df$typeschool, function(typeschool) {
    
    if(typeschool == "private") {    # If university is private, then marker is red
      # MUST use "==", not "="
      "green"
      
    } else {                        # If anything else, then marker is blue
      
      "blue"
      
    } })
  
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = type_color(main_df)
)

leaflet(main_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addAwesomeMarkers(~long,
                    ~lat,
                    icon = icons,
                    label = ~as.character(university))




# Concatenate Strings
# i.e. Name of University + City it is in

~paste(university, city, sep = "_")




###############
#   OPTION 2  #
############### 

# Marker Colour by Type of School (Public/Private)

type_colour <- colorFactor(viridis(7), main_df$typeschool)

leaflet(main_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~long,
                   lat = ~lat,
                   fillColor = ~type_colour(typeschool),
                   fillOpacity = 0.8,
                   label = ~as.character(university),
                   popup = ~as.character(university))
