library(tidync)
library(RNetCDF)
library(ncdf4)
library(here)
library(lubridate)
library(raster)

# Read nc grids from ERDDAP and write to TIF with appropriate CRS
for (i in seq(1:3)) {
  fday = i - 1
  
  dt = today() + fday
  dt_file = here("data", "current_forecast", paste0("forecast_day_",fday,".nc"))
  download.file(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/wvcharmV3_3day.nc?particulate_domoic[(",dt,"T12:00:00Z)][(31.3):(43.0)][(232.5):(243.0)]"), dt_file)
  
  src = tidync(dt_file)
  
  # read raster from .nc and transform long to -180/180 from 0/360
  pda_raster = raster::raster(nc_file, varname = "particulate_domoic") %>% rotate() %>% projectRaster(crs  = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs")
  
  # write raster as .tif
  raster::writeRaster(pda_raster, here("data", "current_forecast", paste0("forecast_day_",fday,".tif")), format="GTiff", overwrite = T)
}

# # read land boundary shapefile
# CA_land = read_sf(here("data","stanford-NA","ns372xw1938.shp")) %>% dplyr::select(name) %>% filter(name =="California")
# write_sf(CA_land, here("data", "CA_land", "CA_land_boundaries.shp"))
# 
# # inverse mask to remove raster overflowing land
# pda_raster = pda_raster %>% raster::mask(CA_land, inverse = TRUE)