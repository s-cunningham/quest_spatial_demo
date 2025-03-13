library(ggplot2)
library(terra)
library(tidyterra)

theme_set(theme_bw())

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
  geom_spatvector(data=elc, aes(fill=DOMVEG)) +
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
disturbed_km

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








#### Section 2: Raster Data ####

## Import data. Note that some of these are vectors that we may have used already.
# Ecological Land Classification (i.e., ecoregions) 
elc <- vect("efb650_data/elc.shp")
# Digital elevation model 
dem <- rast("efb650_data/dem.tif")
# Rivers
rivers <- vect("efb650_data/rivers.shp")







