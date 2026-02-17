
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
gisdir <- "/Users/cfree/Dropbox/Chris/UCSB/grants/boem/data/gis_data/processed"
plotdir <- "/Users/cfree/Dropbox/Chris/UCSB/grants/boem/figures"

# Read data
leases <- readRDS(file=file.path(gisdir, "US_wind_lease_sites.Rds")) %>% mutate(type="Lease area (current)")
call_areas <- readRDS(file=file.path(gisdir, "US_wind_call_areas.Rds")) %>%  mutate(type="Call area (proposed)")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build data
################################################################################

# Build data
data <- rbind(leases %>% select(type, geometry), 
              call_areas %>% select(type, geometry))

# Ports
ports <- matrix(data=c("Morro Bay", 35.367222, -120.846667,
                        "Eureka", 40.801944, -124.163611,
                        "Brookings", 42.0575, -124.286389,
                        "La Push", 47.905278, -124.626111),
                 ncol=3, byrow=T) %>% 
  as_tibble() %>% 
  setNames(c("port", "lat_dd", "long_dd")) %>% 
  mutate_at(.vars=c("lat_dd", "long_dd"), .funs = as.numeric)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title = element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.3, 0.1),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lease areas
  geom_sf(data=data, mapping=aes(fill=type), alpha=0.5, color="grey30") +
  # Plot ports
  geom_point(data=ports, aes(x=long_dd, y=lat_dd)) +
  geom_text(data=ports, mapping=aes(x=long_dd, y=lat_dd, label=port), hjust=-0.15, size=2.2) +
  # Legend
  scale_fill_manual(name="OSW area", values=c("darkblue", "darkred")) +
  # Axes
  scale_y_continuous(breaks=seq(32,48,2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 48)) +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "Fig1_lease_sites_map_wc.png"), 
       width=2.5, height=5, units="in", dpi=600)

