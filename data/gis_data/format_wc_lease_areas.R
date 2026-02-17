
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/grants/boem/data/gis_data/raw/BOEM_Renewable_Energy_Shapefiles_1/"
outdir <- "/Users/cfree/Dropbox/Chris/UCSB/grants/boem/data/gis_data/processed"

# Read data
lease_orig <- sf::st_read(file.path(indir, "Wind_Lease_Outlines_2_2023.shp"))
plan_orig <- sf::st_read(file.path(indir, "BOEM_Wind_Planning_Areas_Outlines_04_2023.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format data
################################################################################

# Format data
leases <- lease_orig %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lease_id=lease_numb,
         lease_id_long=lease_nu_1,
         protraction_id=protractio,
         protraction_loc=protract_1,
         lease_doc1=lease_docu,
         lease_doc2=lease_do_1,
         area_acres=acres)

# Inspect
str(leases)

# Format data
plans <- plan_orig %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(protraction_id=protractio)

# Export data
################################################################################

# Export data
saveRDS(leases, file=file.path(outdir, "US_wind_lease_sites.Rds"))
saveRDS(plans, file=file.path(outdir, "US_wind_call_areas.Rds"))







