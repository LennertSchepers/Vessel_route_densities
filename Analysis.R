## load rasters
library(raster)
library(sf)
library(mapview)
library(ggplot2)
library(data.table)
#
base_url <- 'https://ows.emodnet-humanactivities.eu/wcs?SERVICE=WCS&VERSION=1.0.0&request=GetCapabilities'

# BACKGROUND INFO ON EMODNet Human Activities' WCS Service, and the 'route density maps'
    # The Open Geospatial Consortium (OGC) Web Coverage Service Interface Standard (WCS) defines Web-based retrieval of coverages – that is, digital geospatial information representing space/time-varying phenomena.
    #
    # Get Capabilities:
    #   https://ows.emodnet-humanactivities.eu/wcs?SERVICE=WCS&VERSION=1.0.0&request=GetCapabilities
    #
    # Get Coverage Example:
    #   https://ows.emodnet-humanactivities.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=emodnet:2019_01_rd_All&crs=EPSG:4326&BBOX=15,20.5,30,32.5&format=image/tiff&interpolation=nearest&resx=0.00833333&resy=0.00833333
    #
    # The names of the web service layers are -
    #   '<year>_<month>_rd_<vessel code>' Eg. '2019_01_rd_All' for all vessels during January 2019
    # '<year>_<season>_rd_<vessel code>' Eg. '2019_spring_rd_All' for all vessels during Spring 2019
    # '<year>_yearly_rd_<vessel code>' Eg. '2019_yearly_rd_All' for the total of all vessels during 2019.
    # The vessel codes are as follows:
    #
    # All - All types
    # 01 - Cargo
    # 02 - Fishing
    # 03 - Passenger
    # 04 - Tanker
    # 05 - Other
    #
    #Available data: January 2019 - March 2020


BCP <- st_read("http://geo.vliz.be/geoserver/MarineRegions/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=eez&cql_filter=mrgid=%273293%27&outputFormat=application/json")
BCP <- st_cast(BCP, "POLYGON")
BCP_bbox <- st_bbox(BCP$geometry)
mapview(BCP)

# function to get belgian raster data, based on year, month, vessel_code
getBelgianCSW <- function(year, month, vessel_code){
  url <- paste0("https://ows.emodnet-humanactivities.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=emodnet:",
                year, "_",
                formatC(month, width = 2, flag = 0), "_rd_",
                vessel_code, "&",
                "crs=EPSG:4326&BBOX=", paste(BCP_bbox, collapse = ","), "&",
                "format=image/tiff&",
                "interpolation=nearest&resx=0.00833333&resy=0.00833333"
  )
  raster(url)
}



Cargo <- brick(x = c(getBelgianCSW(2019, 1, "01"),
                     getBelgianCSW(2019, 2, "01"),
                     getBelgianCSW(2019, 3, "01"),
                     getBelgianCSW(2019, 4, "01"),
                     getBelgianCSW(2019, 5, "01"),
                     getBelgianCSW(2019, 6, "01"),
                     getBelgianCSW(2019, 7, "01"),
                     getBelgianCSW(2019, 8, "01"),
                     getBelgianCSW(2019, 9, "01"),
                     getBelgianCSW(2019, 10, "01"),
                     getBelgianCSW(2019, 11, "01"),
                     getBelgianCSW(2019, 12, "01"),
                     getBelgianCSW(2020, 1, "01"),
                     getBelgianCSW(2020, 2, "01"),
                     getBelgianCSW(2020, 3, "01")
                                   )
)

mapview(Fishing[[1]]) + mapview(BCP, alpha.regions = 0.1)

Fishing <- brick(x = c(getBelgianCSW(2019, 1, "02"),
                     getBelgianCSW(2019, 2, "02"),
                     getBelgianCSW(2019, 3, "02"),
                     getBelgianCSW(2019, 4, "02"),
                     getBelgianCSW(2019, 5, "02"),
                     getBelgianCSW(2019, 6, "02"),
                     getBelgianCSW(2019, 7, "02"),
                     getBelgianCSW(2019, 8, "02"),
                     getBelgianCSW(2019, 9, "02"),
                     getBelgianCSW(2019, 10, "02"),
                     getBelgianCSW(2019, 11, "02"),
                     getBelgianCSW(2019, 12, "02"),
                     getBelgianCSW(2020, 1, "02"),
                     getBelgianCSW(2020, 2, "02"),
                     getBelgianCSW(2020, 3, "02")
                     )
)

Passenger <- brick(x = c(getBelgianCSW(2019, 1, "03"),
                       getBelgianCSW(2019, 2, "03"),
                       getBelgianCSW(2019, 3, "03"),
                       getBelgianCSW(2019, 4, "03"),
                       getBelgianCSW(2019, 5, "03"),
                       getBelgianCSW(2019, 6, "03"),
                       getBelgianCSW(2019, 7, "03"),
                       getBelgianCSW(2019, 8, "03"),
                       getBelgianCSW(2019, 9, "03"),
                       getBelgianCSW(2019, 10, "03"),
                       getBelgianCSW(2019, 11, "03"),
                       getBelgianCSW(2019, 12, "03"),
                       getBelgianCSW(2020, 1, "03"),
                       getBelgianCSW(2020, 2, "03"),
                       getBelgianCSW(2020, 3, "03")
)
)

Tanker <- brick(x = c(getBelgianCSW(2019, 1, "04"),
                         getBelgianCSW(2019, 2, "04"),
                         getBelgianCSW(2019, 3, "04"),
                         getBelgianCSW(2019, 4, "04"),
                         getBelgianCSW(2019, 5, "04"),
                         getBelgianCSW(2019, 6, "04"),
                         getBelgianCSW(2019, 7, "04"),
                         getBelgianCSW(2019, 8, "04"),
                         getBelgianCSW(2019, 9, "04"),
                         getBelgianCSW(2019, 10, "04"),
                         getBelgianCSW(2019, 11, "04"),
                         getBelgianCSW(2019, 12, "04"),
                         getBelgianCSW(2020, 1, "04"),
                         getBelgianCSW(2020, 2, "04"),
                         getBelgianCSW(2020, 3, "04")
)
)

Other <- brick(x = c(getBelgianCSW(2019, 1, "05"),
                      getBelgianCSW(2019, 2, "05"),
                      getBelgianCSW(2019, 3, "05"),
                      getBelgianCSW(2019, 4, "05"),
                      getBelgianCSW(2019, 5, "05"),
                      getBelgianCSW(2019, 6, "05"),
                      getBelgianCSW(2019, 7, "05"),
                      getBelgianCSW(2019, 8, "05"),
                      getBelgianCSW(2019, 9, "05"),
                      getBelgianCSW(2019, 10, "05"),
                      getBelgianCSW(2019, 11, "05"),
                      getBelgianCSW(2019, 12, "05"),
                      getBelgianCSW(2020, 1, "05"),
                      getBelgianCSW(2020, 2, "05"),
                      getBelgianCSW(2020, 3, "05")
)
)

All <- brick(x = c('All-2019-01' = getBelgianCSW(2019, 1, "All"),
                   'All-2019-02' = getBelgianCSW(2019, 2, "All"),
                   'All-2019-03' = getBelgianCSW(2019, 3, "All"),
                   'All-2019-04' = getBelgianCSW(2019, 4, "All"),
                   'All-2019-05' = getBelgianCSW(2019, 5, "All"),
                   'All-2019-06' = getBelgianCSW(2019, 6, "All"),
                   'All-2019-07' = getBelgianCSW(2019, 7, "All"),
                   'All-2019-08' = getBelgianCSW(2019, 8, "All"),
                   'All-2019-09' = getBelgianCSW(2019, 9, "All"),
                   'All-2019-10' = getBelgianCSW(2019, 10, "All"),
                   'All-2019-11' = getBelgianCSW(2019, 11, "All"),
                   'All-2019-12' = getBelgianCSW(2019, 12, "All"),
                   'All-2020-01' = getBelgianCSW(2020, 1, "All"),
                   'All-2020-02' = getBelgianCSW(2020, 2, "All"),
                   'All-2020-03' = getBelgianCSW(2020, 3, "All")
)
)

# create data frame, with 1st column 'time'
  df.sr <- data.frame(Date = seq(as.Date("2019/1/1"), by = "month", length.out = 15))

  # extract all values from the rasters, by the BCP polygon, and average the values.
  df.sr$Cargo     <- as.vector(extract(Cargo, BCP, fun = mean))
  df.sr$Fishing   <- as.vector(extract(Fishing, BCP, fun = mean))
  df.sr$Passenger <- as.vector(extract(Passenger, BCP, fun = mean))
  df.sr$Tanker    <- as.vector(extract(Tanker, BCP, fun = mean))
  df.sr$Other     <- as.vector(extract(Other, BCP, fun = mean))
  df.sr$All       <- as.vector(extract(All, BCP, fun = mean))

# transform to long format:
  df.sr.long <- melt.data.table(as.data.table(df.sr),
                                id.vars = "Date")

ggplot(df.sr.long, aes(x = Date, y = value, group = variable)) +
  geom_point(aes(color = variable)) +
  geom_line(aes(color = variable))
