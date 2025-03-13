library(ggplot2)
library(terra)
library(tidyterra)

theme_set(theme_bw())
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
  scale_fill_manual(values=c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","gray","#4EB3D3","#2B8CBE","#0868AC","#084081")) +
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
  scale_fill_manual(values=c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","gray","#4EB3D3","#2B8CBE","#0868AC","#084081")) +
  geom_spatvector(data=trails, color="darkred", linewidth=1) +
  geom_spatvector(data=w_camps, fill=NA, color="hotpink", linewidth=1) +
  geom_spatvector(data=camp, shape=21, size=3, fill="white") +
  theme_void() 

### Spatial analysis

# Create a 50 m buffer around the trails
tr_buff <- buffer(trails, width=50, capstyle="round", joinstyle="round")

# Note that there is not an argument in the buffer() function to dissolve, so we have to do that separately
plot(tr_buff) # quick look
tr_buff <- aggregate(tr_buff, dissolve=TRUE)

# another quick look to see the difference
plot(tr_buff)


#### Section 2: Raster Data ####


