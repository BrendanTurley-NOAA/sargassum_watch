library(ncdf4)
library(rgdal)

lonbox_e <- -79 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 27 ### northern coast
latbox_s <- 21 ### southern edge of Key West

### load map
# setwd("C:/Users/brendan.turley/Desktop/FL_habs/ne_10m_admin_0_countries")
setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

url <- 'https://cwcgom.aoml.noaa.gov/thredds/dodsC/AFAI/USFAFAI7D.nc'

sarg <- nc_open(url)
time <- ncvar_get(sarg, 'time')
time2 <- as.POSIXct(time,origin=ISOdate(1970,01,01,00,00), tz='UTC')
lat <- ncvar_get(sarg, 'lat')
lon <- ncvar_get(sarg, 'lon')

lon_i <- which(lonbox_e>=lon & lonbox_w<=lon)
lat_i <- which(latbox_n>=lat & latbox_s<=lat)

n <- 3
afai <- ncvar_get(sarg, 'AFAI',
                  start=c(lon_i[1],lat_i[1],length(time)-(n-1)),
                  count=c(length(lon_i),length(lat_i),n))
afai1 <- apply(afai,c(1,2),mean,na.rm=T)

range(afai,na.rm=T)
breaks <- seq(-.002, .004, .001)
cols <- colorRampPalette(c(1,'forestgreen','darkorange','gold'))
cols1 <- cols(length(breaks)-1)

image(lon[lon_i],
      lat[lat_i],
      afai1,
      asp=1, las=1, xlab='', ylab='',
      col=cols1, breaks=breaks)
plot(world, col='gray70',add=T)


url <- 'https://podaac-opendap.jpl.nasa.gov/opendap/allData/oscar/preview/L4/oscar_third_deg/oscar_vel10182.nc.gz'
oscar <- nc_open(url)
time <- ncvar_get(oscar, 'time')
time2 <- as.POSIXct(time*3600*24,origin=ISOdate(1992,10,05,00,00), tz='UTC')
lon2 <- ncvar_get(oscar, 'longitude')
lon2 <- lon2-360
lat2 <- ncvar_get(oscar, 'latitude')
lon2_i <- which(lonbox_e>=lon2 & lonbox_w<=lon2)
lat2_1 <- which(latbox_n>=lat2 & latbox_s<=lat2)
u <- ncvar_get(oscar, 'u',
               start=c(lon2_i[1],lat2_1[1],1,1),
               count=c(length(lon2_i),length(lat2_1),1,1))
u <- as.vector(u)
v <- ncvar_get(oscar, 'v',
               start=c(lon2_i[1],lat2_1[1],1,1),
               count=c(length(lon2_i),length(lat2_1),1,1))
v <- as.vector(v)

lon_lat <- expand.grid(lon2[lon2_i],lat2[lat2_1])

plot(lon_lat,col='white',asp=1)
arrows(lon_lat$Var1,lon_lat$Var2,
       (lon_lat$Var1+u),(lon_lat$Var2+v),
       length = .05)



image(lon[lon_i],
      lat[lat_i],
      afai1,
      asp=1, las=1, xlab='', ylab='',
      col=cols1, breaks=breaks)
arrows(lon_lat$Var1,lon_lat$Var2,
       (lon_lat$Var1+u/2),(lon_lat$Var2+v/2),
       length = .05, col='magenta')
plot(world, col='gray70',add=T)
