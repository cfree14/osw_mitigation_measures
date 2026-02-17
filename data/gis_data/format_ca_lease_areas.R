
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "~/Dropbox/Chris/UCSB/projects/mpa_projects/boem/data/gis_data/raw/CA_FinalLeaseAreas_Shapefiles"
outdir <- "~/Dropbox/Chris/UCSB/projects/mpa_projects/boem/data/gis_data/processed"

# Read data
data_orig <- sf::st_read(file.path(indir, "LeaseAreas_CA_Outlines_2022_09_08.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lease_id=lease_numb,
         area_sqmi=sq_miles) %>% 
  # Simplify
  select(lease_id, area_sqmi, geometry)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CA_wind_lease_sites.Rds"))







