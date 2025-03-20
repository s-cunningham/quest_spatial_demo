library(ggplot2)
library(terra)
library(tidyterra)

theme_set(theme_bw())

# # Modified
# elc <- vect("efb650_data/elc.shp")
# elc <- elc[,c(1:17, 24, 36)]
# names(elc)[18] <- "MOOSE"
# elc <- project(elc, "EPSG:4326")
# writeVector(elc, "efb650_data/elc.shp", overwrite=TRUE)

# Set up a color palette so that we don't have to copy it into our code so many times
pal <- c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","gray","#4EB3D3","#2B8CBE","#0868AC","#084081")

#### Section 1: Vector Data ####

## Read in data
# Ecological Land Classification (i.e., ecoregions) 
elc <- vect("efb650_data/elc.shp")
# Trails in the Clearwater River Watershed 
trails <- vect("efb650_data/trails.shp")
# Camp sites in the Clearwater River Watershed 
camp <- vect("efb650_data/camp.shp")

## Check the spatial reference
elc
crs(elc)

camp
crs(camp)

trails
crs(trails)

## Does the CRS match up between layers?
crs(elc)==crs(camp)
crs(elc)==crs(trails) 
crs(camp)==crs(trails) # Looks like elc is the problem. Let's reproject it

# Reproject elc to camp CRS
elc <- project(elc, crs(camp))
# Check it
crs(elc)==crs(camp)

## Let's plot
ggplot() +
  geom_spatvector(data=elc) +
  geom_spatvector(data=trails) +
  geom_spatvector(data=camp) 
# doesn't look like much at this point

# Try again to make it look better
ggplot() +
  geom_spatvector(data=elc, aes(fill=DOMVEG)) +
  scale_fill_manual(values=pal) +
  geom_spatvector(data=trails, color="darkred", linewidth=1) +
  geom_spatvector(data=camp, shape=21, size=3, fill="white") +
  theme_void() 

## Attribute tables
values(camp)
head(values(elc))

## Indentify which polygons have campsites in them
# First, check if there are unique identifiers for elc polygons
nrow(elc) # how many rows (i.e., unique polygons)
length(unique(elc$ID)) # doesn't seem like its a unique ID

# Doesn't seem like it, so lets add one
elc$poly_id <- 1:nrow(elc)

# check column names...confirm we've added a unique column
names(elc)

# Determine which polygon IDs have camps
w_camps <- intersect(elc, camp)$poly_id 

# Filter elc to just the polygons that have camps using square brackets
w_camps <- elc[elc$poly_id %in% w_camps,]

# Let's add these outlines to the plot
# Note that the order of geoms defines drawing order, camps are on top to be able to see
ggplot() +
  geom_spatvector(data=elc, aes(fill=DOMVEG), color=NA) +
  scale_fill_manual(values=pal) +
  geom_spatvector(data=trails, color="darkred", linewidth=1) +
  geom_spatvector(data=w_camps, fill=NA, color="hotpink", linewidth=1) +
  geom_spatvector(data=camp, shape=21, size=3, fill="white") +
  theme_void() 

### Spatial analysis

## Create a 50 m buffer around the trails
tr_buff <- buffer(trails, width=50, capstyle="round", joinstyle="round")

# Note that there is not an argument in the buffer() function to dissolve, so we have to do that separately
plot(tr_buff) # quick look
tr_buff <- aggregate(tr_buff, dissolve=TRUE)

# Visualize on full map
ggplot() +
  geom_spatvector(data=elc, aes(fill=DOMVEG)) +
  scale_fill_manual(values=pal) +
  geom_spatvector(data=trails, color="black", linewidth=1) +  # Note the change in line color for better visualization
  geom_spatvector(data=camp, shape=21, size=3, fill="white") +
  geom_spatvector(data=tr_buff, color="red", fill=NA, linewidth=0.5) +
  theme_void() +
  theme(legend.position="none") # Dropping legend for this plot so that the map is bigger

## Create a buffer around the campgrounds, but let campground capacity determine how big the buffer will be

# Add a column to the camp point vector
camp$buff_size <- ifelse(camp$CAPACITY <= 10, 100, 200)
values(camp) # Check it worked

# create buffers
ca_buff <- buffer(camp, width=camp$buff_size)
plot(ca_buff) # check it worked

## Combine all of the buffers into a single layer
buffunion <- union(tr_buff, ca_buff)

# Again we need to aggregate and dissolve our buffers
buffunion <- aggregate(buffunion, dissolve=TRUE)

## How much area is potentially disturbed by the trails and campsites?
disturbed_m <- expanse(buffunion) # Note that this returns values in m^2

# convert to km2
disturbed_km <- disturbed_m / 1000 / 1000
disturbed_km # print area to console

# Visualize on full map
ggplot() +
  geom_spatvector(data=elc, aes(fill=DOMVEG)) +
  scale_fill_manual(values=pal) +
  geom_spatvector(data=trails, color="black", linewidth=1) +  # Note the change in line color for better visualization
  geom_spatvector(data=camp, shape=21, size=3, fill="white") +
  geom_spatvector(data=buffunion, color="red", fill=NA, linewidth=0.5) +
  theme_void() +
  theme(legend.position="none")

### Now let's see how this disturbance impacts wolverine habitat
# IF we look at just the wolverine column, it has categorical 
elc$WOLVERINE

# Plot
ggplot() +
  geom_spatvector(data=elc, aes(fill=WOLVERINE))

# Filter using square bracket indexing (these are the most suitable categories)
wolv <- elc[elc$WOLVERINE==4 | elc$WOLVERINE==5,]
ggplot() +
  geom_spatvector(data=wolv, aes(fill=factor(WOLVERINE)))

# How much area is there?
sum(expanse(wolv)) / 1000 / 1000 # convert to km2

## Let's clip it by the disturbance area
wolv_dist <- crop(wolv, buffunion)
plot(wolv_dist)

# how much area is being disturbed?
sum(expanse(wolv_dist)) / 1000 / 1000 # convert to km2

# about 5%

#### Section 2: Raster Data ####

## Import data. Note that some of these are vectors that we may have used already.
# Ecological Land Classification (i.e., ecoregions) 
elc <- vect("efb650_data/elc.shp")
# Digital elevation model 
dem <- rast("efb650_data/dem.tif")
# Rivers
rivers <- vect("efb650_data/rivers.shp")

# Check CRS
crs(elc)==crs(dem)
# Reproject 
elc <- project(elc, crs(rivers))
crs(elc)==crs(rivers)

## Let's plot dem#
# take a look at the raster by itself
ggplot() +
  geom_spatraster(data=dem) +
  scale_fill_cross_blended_c(palette="cold_humid") # adjust the color scale using tidyterra
  
# Now all three layers
ggplot() +
  geom_spatraster(data=dem) +
  scale_fill_cross_blended_c(palette="cold_humid") + # adjust the color scale using tidyterra
  geom_spatvector(data=elc, fill=NA, color="gray40") +
  geom_spatvector(data=rivers, color="blue", linewidth=1)

# Now let's take a look at the ELC layer as viewed by moose
ggplot() +
  geom_spatvector(data=elc, aes(fill=factor(MOOSE)))

# Convert polygon to raster grid
moose <- rasterize(elc, dem, field="MOOSE")
plot(moose) # Plot to check

# Look at the raster info
moose
dem

# We want to relabel the moose raster to reflect suitability. 

# Set up raster values and classes
moose_values <- c(1, 3, 4, 5, 6, 7)
moose_class <- c("Poor", "Low", "Fair", "Good", "High", "Water")

# Add class names and numbers to the raster
levels(moose) <- list(data.frame(ID = moose_values,
                                 suitability = moose_class))

# Now we want to specify specify colors by level
coltb <- data.frame(value=moose_values, col=c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26", "#0570b0"))
# add color table
coltab(moose) <- coltb

# Plot
ggplot() +
  geom_spatraster(data=moose) +
  # Note that we have to specify data for the color table again
  scale_fill_coltab(data=moose, na.translate = FALSE, name="Suitability")


### Reclassifying a raster
# First, we want to create a raster based on dominant vegetation
domveg <- rasterize(elc, dem, field="DOMVEG")

# Plot, and note that because the column in the vector was categorical, we don't have to change the raster from numeric values to categorical
plot(domveg)

# Print out a list of the vegetation types
unique(domveg)

hist(domveg)

# Build 'from-to' matrix - this doesn't work
# rcl <- as.matrix(data.frame(from=unique(domveg)$DOMVEG, 
#                             to=c("Meadow","Shrub","Forest","Forest","Meadow",
#                                  "Nonvegetated","Forest", "Forest","Forest", "Shrub")))
# 
# domveg <- classify(domveg, rcl)

###

## River kernel density
library(spatstat)
riv_sf <- sf::st_as_sf(rivers)
density(riv_sf)

## Distance to trails / rivers?
test <- gridDist(trails)

# focal stats 
terra::focal(r_spat, w = 3, fun = "sum")


spatstat.geom::as.psp(riv_sf)
spatstat.geom::pixellate(rivers)




### Create hillshade from DEM raster

## Use terrain function to create new rasters with slope and aspect (in radians!)
# Slope
dem_s <- terrain(dem, v="slope", neighbors=8, unit="radians") 

# Quick plot to visualize
plot(dem_s)

# Aspect
dem_a <- terrain(dem, v="aspect", neighbors=8, unit="radians") 

# Quick plot to visualize
plot(dem_a)

## Use slope and aspect to create hillshade
hill <- shade(slope=dem_s, aspect=dem_a, angle=40, direction=270)
plot(hill)

## Plot
# but first, make a palette
pal_greys <- hcl.colors(1000, "Grays")

ggplot() +
  geom_spatraster(data=hill) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA)

## plot hillshade with VEGCOVER semi-transparent 
ggplot() +
  # geom_spatraster(data=hill) +
  # scale_fill_gradientn(colors=pal_greys, na.value = NA) +
  geom_spatraster(data=moose, alpha=0.5, use_coltab=TRUE, na.rm=TRUE)

#### Distance to rivers ####
temp_rast <- rast(rivers, resolution=30) # create template raster 30 x 30 m
temp_rast

# add this argument to visualize cells
# , vals=rnorm(749700)
crs(rivers)==crs(temp_rast)

# Calculate the distance raster from the polyline
# The function will calculate distance between each cell and closest part of line
distance_raster <- distance(temp_rast, rivers)










