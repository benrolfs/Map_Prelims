library("maptools")
library("ggplot2")
library("gpclib")
library("ggmap")
#setwd("~/Dropbox/Twitter/prelims/map_data/")


# ggmap demo. Examples taken from http://stat405.had.co.nz/ggmap.pdf ###

sport <- readShapePoly("london_sport.shp")
p<-ggplot(sport@data, aes(Partic_Per,Pop_2001))

gpclibPermit()

# London Sports
sport_geom <- fortify(sport, region="ons_label")
sport_geom <- merge(sport_geom, sport@data, by.x="id", by.y="ons_label")

Map <- ggplot(sport_geom, aes(long, lat, group=group, fill=Partic_Per)) +
  geom_polygon() + coord_equal() + labs(x="Easting(m)", y = "Northing (m)",
                                        fill="% Sport Partic.") + 
  ggtitle("London Sports Participation") + scale_fill_gradient(low="white", high="black")

ggsave("london_sports.pdf")

# Ambulance Stats

input <- read.csv("ambulance_assault.csv")
p_ass <- ggplot(input, aes(x=assault_09_11)) + geom_histogram()

# qmap Examples ##############
# Notes: 
#   - Function "geocode" converts tagged locations (cities, landmarks) to lat/long
#   - There are a bunch of different types of maps available. 
#     One of these (cloudmade) requires a license, which is below. (api_key_str)
#   - cloudmade has a number of different styles which can be found at the url:
#     http://maps.cloudmade.com/editor
#   - The cloudmade/stamen map databases are at times inaccessible 
#   - I think the cloudmade server just times out while downloading a map type 
#   - sometimes. Not sure how to increase the timeout .

geocode("Stanford University")
stanford <- "Stanford University"
qmap(stanford, zoom=14)
geocode("426 Oak St. San Francisco CA")
home <- "426 Oak St. San Francisco CA"
qmap(home, zoom = 16, source="osm")
geocode("Twitter Headquarters, San Francisco CA")
twitter <- "Twitter Headquarters, San Francisco CA"
qmap(twitter, zoom = 18, maptype="satellite")
qmap(home, zoom="auto", source="stamen", maptype="watercolor")
api_key_str = "bc2bd53b533742d481ccf909e6ffe295"
qmap("houston", zoom = 10, maptype = 58916, api_key=api_key_str, source="cloudmade")
qmap("East Pike Lake, MN", zoom = 13, maptype = 58916, api_key=api_key_str, source="cloudmade")
qmap(twitter, zoom=14, maptype=99474, api_key=api_key_str, source="cloudmade")
qmap("Boston, MA", zoom=14, maptype = 53428, api_key = api_key_str, source = "cloudmade")
qmap("Boston, MA", zoom=13, maptype = 997, api_key = api_key_str, source = "cloudmade")

# ggmap example ###########
# Notes: 
#   - First get a raster with get_cloudemademap, e.g., then plot with ggmap
#   - We then do an example with plotting data directly on the map. 
#     This would (not shown below) allow more layering of the plot using ggplot2. 

berlin <- get_map(location="Berlin") # raster ojbect
berlinBB <- attr(berlin, "bb")
berlinCorners <- expand.grid(lat = c(berlinBB$ll.lat, berlinBB$ur.lat), lon = c(berlinBB$ll.lon, berlinBB$ur.lon))
berlinMap <- ggmap(berlin, extent="normal")

# Crime example (taken directly from PDF). Example is for Houston.
# Notes:
#   - gglocator is real neat. You specify a number of points, run the function,
#     then click on the map a number of times. It returns the corresponding lat/long pairs.
qmap("Houston, TX", zoom = 13, extent="normal")
gglocator(2) 
violent_crimes <- subset(crime, offense != "auto theft" &
                           offense != "theft" & offense != "burglary")
violent_crimes$offense <- factor(violent_crimes$offense, levels =
    c("robbery", "aggravated assault", "rape", "murder"))
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188 &
                           29.73631 <= lat & lat <= 29.78400)
theme_set(theme_bw(16))
houston <- get_map("Houston, TX", zoom = 14)
HoustonMap <- qmap("houston", zoom = 14,
                   color = "bw", legend = "topleft")
CrimeMap <- HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense, size = offense), data = violent_crimes)
CrimeMap
CrimeMap2 <- HoustonMap +
  stat_bin2d(aes(x = lon, y = lat, colour = offense, fill = offense),
    size = .5, bins = 30, alpha = 1/2, data = violent_crimes)
CrimeMap2

# Important: Code from the .pdf doesn't actually work. Let's try it 
# and see the difference. First, let's do the code from the .pdf. 
houston <- get_map("Houston", zoom=14)
HoustonMap <- ggmap(houston, extent="device", legend="topleft")
DensityMap <- HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 2, bins = 4, data = violent_crimes, geom = 'polygon') 
DensityMap

# Note that ..level.. is an aesthetic variable (other examples include ..density.. and
# ..count) which get passed as a statistic to stat_density2d. The function stat_density2d
# creates a variable called ..level.., while other stat_xxx functions may produce different
# explanatory statistics. 

# Now, we need to understand the fill and alpha aesthetics. Fill controls the fill color
# while alpha controls the transparency. Above, we are having both fill and alpha
# scale based on the ..level.. varaible produced by stat_density2d. The problem is that
# for whatever reason the fill and level are defaulting to different colors. To verify this
# run the following: 
DensityMap <- DensityMap + scale_fill_gradient('Violent Crime Density') + 
  scale_alpha('Alpha density')
DensityMap
# which shows how each alpha and fill have a different color. Let's fix this. 
DensityMap <- HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 2, bins = 4, data = violent_crimes, geom = 'polygon') 
DensityMap <- DensityMap + scale_alpha(range = c(.4, .75), guide = FALSE)
DensityMap
# What did we do? First we scaled the alpha range to be more visible. Then we
# simply removed the legend for alpha with guide=FALSE. In general, we have access 
# to legends for both variables. We can now add the correct legend title.
DensityMap <- DensityMap + scale_fill_gradient('Violent Crime Density') 
DensityMap
# The bin variable controls the number of bins the color scale is divided into.
DensityMap <- HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 2, bins = 4, data = violent_crimes, geom = 'polygon') 
DensityMap <- DensityMap + scale_alpha(range = c(.4, .75), guide = FALSE)
DensityMap <- DensityMap + scale_fill_gradient('Violent\n Crime\n Density') 
DensityMap

# Now we show how to do an inlay
overlay <- stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon", data = violent_crimes) + scale_alpha(range = c(.4, .75))
InsetMap <- DensityMap + inset(grob = ggplotGrob(ggplot() + overlay + theme_inset()),
                              xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062)
InsetMap

# This is also cool: mapdist
mapdist("Twitter Headquarters, San Francisco, CA", home)
# Importantly...The Distance Matrix API limits users to 100 requests per query, 
# 100 requests per 10 seconds, and 2500 requests per 24 hours. To the extent to which
# these can be easily monitored, the exported function distQueryCheck
# helps the user keep track of their remaining balance of queries. It relies 
# on the hidden global variable .GoogleDistQueryCount

# One more useful example from the PDF....
download.file('http://www.census.gov/geo/cob/bdy/tr/tr00shp/tr48_d00_shp.zip', 
              destfile = 'census.zip')
unzip('census.zip'); 
library(maptools); library(gpclib); library(sp); gpclibPermit()

calShape <- readShapeSpatial('tr48_d00.shp', proj4string = CRS("+proj=longlat +datum=WGS84"))