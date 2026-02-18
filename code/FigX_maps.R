



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
tabledir <- "tables"
plotdir <- "figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")

# Read USA WEAs
weas <- sf::st_read("data/gis_data/raw/boem-renewable-energy-shapefiles_0/Offshore_Wind_Leases.shp")

# Focus group ports
ports <- readxl::read_excel("data/focus_groups/focus_group_ports.xlsx")

# CBAs
cbas_sea <- readxl::read_excel("data/cbas/cba_metadata.xlsx", sheet="Data") %>% mutate(type="Offshore")
cbas_land <- readxl::read_excel("data/cbas/cba_metadata.xlsx", sheet="Land") %>% mutate(type="Land-based")
cbas <- bind_rows(cbas_sea, cbas_land) %>% 
  mutate(type=factor(type, levels=c("Offshore", "Land-based")))
  
# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key = element_rect(fill = NA, color=NA),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot full US
g2 <- ggplot() +
  # Land
  geom_sf(data=world, fill="grey85", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey85", color="white", lwd=0.2, inherit.aes = F) +
  # WEAs
  geom_sf(data=weas, fill="skyblue", color=NA) +
  # Focus group ports
  geom_point(data=ports, mapping=aes(x=long_dd, y=lat_dd), size=0.9) +
  # CBAs
  geom_point(data=cbas, mapping=aes(x=long_dd, y=lat_dd, color=type), size=0.6) +
  # Focus group port labels
  geom_text(data=ports, mapping=aes(x=long_dd+0.4, y=lat_dd, label=port), hjust=0, size=2.2) +
  # Legend
  scale_color_manual(name="CBA type", values=c("navy", "darkgreen")) +
  # Crop
  coord_sf(ylim=c(26, 49), xlim=c(-123, -68)) +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.4, "cm"))
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "FigX_maps.png"), 
       width=6.5, height=3.75, units="in", dpi=600, bg="white")


