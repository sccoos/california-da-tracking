# Stranding Regions
# Breaking up coastal zone by county/stranding center coverage

library(tidyverse)
library(here)
library(sf)

wgs84 <- st_crs(4326)

counties = read_sf(here("data", "CA_counties", "CA_Counties.shp"))
cz = read_sf(here("data", "CA_coastal_zone", "CA_coastal_zone.shp")) %>% select(OBJECTID)
czm = st_multipolygon(as.list(cz$geometry)) %>% st_geometry() %>% st_set_crs(wgs84)


get_county_bbox = function(county_name) {
  counties %>% filter(NAME == county_name) %>% st_geometry() %>% st_set_crs(wgs84) %>% st_bbox()
}

combine_counties = function(county_names) {
  cs = counties %>% filter(NAME %in% county_names)
  st_union(cs)
}

crop_coastal_zone = function(county_poly) {
  st_intersection(czm, county_poly) %>% st_geometry() %>% st_set_crs(wgs84)
}

# zone definitions
zones = list(
  z1 = c("Del Norte","Humboldt"),
  z2 = c("Mendocino", "Sonoma", "Marin", "San Francisco", "San Mateo"),
  z3 = c("Santa Cruz"),
  z4 = c("Monterey"),
  z5 = c("San Luis Obispo"),
  z6 = c("Santa Barbara", "Ventura"),
  z7 = c("Los Angeles"),
  z8 = c("Orange"),
  z9 = c("San Diego")
)

zone_df = data.frame(OBJECTID = c(1:length(zones)), county = "")
zone_polys = list()
for (i in c(1:nrow(zone_df))) {
  zone = zone_df[i,]
  zone_polys[[i]] = zone %>% mutate(
    geometry = crop_coastal_zone(combine_counties(zones[[i]]))
  ) %>%
  st_as_sf(sf_column_name = "geometry") %>% st_set_crs(wgs84) %>% select(-county)
}

shp = bind_rows(zone_polys)
#st_write(shp, "data/stranding_regions/stranding_regions.shp", delete_layer=T)
