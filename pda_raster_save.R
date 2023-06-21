library(tidync)
library(RNetCDF)
library(ncdf4)
library(here)
library(lubridate)
library(raster)

# ncf = ncdf4::nc_open("https://coastwatch.pfeg.noaa.gov/erddap/griddap/wvcharmV3_3day.nc?particulate_domoic[(2023-06-23T12:00:00Z)][(31.3):(43.0)][(232.5):(243.0)]")
# ncf = RNetCDF::open.nc("https://coastwatch.pfeg.noaa.gov/erddap/griddap/wvcharmV3_3day.nc?particulate_domoic[(2023-06-23T12:00:00Z)][(31.3):(43.0)][(232.5):(243.0)]")
# siteInfo = info("wvcharmV3_3day", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
# grid = rerddap::griddap(siteInfo, time = '2023-06-23T12:00:00Z')

# TODO download programmatically then manipulate and resave
nc_file = here("data", "wvcharmV3_3day_c953_6f44_0287_U1687374714880.nc")
src = tidync(nc_file)

# read raster from .nc and transform long to -180/180 from 0/360
pda_raster = raster::raster(nc_file, varname = "particulate_domoic") %>% rotate()

# read land boundary shapefile
CA_land = read_sf(here("data","stanford-NA","ns372xw1938.shp")) %>% dplyr::select(name) %>% filter(name =="California")

# inverse mask to remove raster overflowing land
pda_raster = pda_raster %>% raster::mask(CA_land, inverse = TRUE)

# write raster as .tif
raster::writeRaster(pda_raster, here("data", "test.tif"), format="GTiff", overwrite = T)
