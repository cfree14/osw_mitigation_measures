



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(patchwork)

# Directories
tabledir <- "tables"
plotdir <- "figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")

# Read USA WEAs
weas <- sf::st_read("data/gis_data/raw/boem-renewable-energy-shapefiles_0/Offshore_Wind_Leases.shp")

# Read OR WEAs
weas_or <- sf::st_read("data/gis_data/raw/Oregon_Lease_Areas_0/Oregon_Lease_Areas_2024_08_21.shp") %>% 
  sf::st_transform(sf::st_crs(weas))

# Focus group ports
ports <- readxl::read_excel("data/focus_groups/focus_group_ports.xlsx")

# CBAs
cbas_sea <- readxl::read_excel("data/cbas/cba_metadata.xlsx", sheet="Data", na="N/A") %>% 
  mutate(type="Offshore") %>% 
  # Simplify to reduce double Barnstable
  select(lat_dd, long_dd, community) %>% 
  unique()

# WEA labels
wea_labels <- matrix(c("Morro Bay", 36.0, -121.7,
                 "Humboldt", 41.2, -124.1), byrow=T, ncol=3) %>% 
  as_tibble() %>% 
  setNames(c("wea", "lat_dd", "long_dd")) %>% 
  mutate_at(.vars=c("lat_dd", "long_dd"), as.numeric)
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  plot.title = element_text(size=9),
                  plot.tag=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key = element_rect(fill = NA, color=NA),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# West Coast
g1 <- ggplot() +
  # Land
  geom_sf(data=world, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  # WEAs
  geom_sf(data=weas, fill="darkred", color=NA) +
  geom_sf(data=weas_or, fill=NA, color="darkred", lwd=0.2) +
  # Focus group ports
  geom_point(data=ports, mapping=aes(x=long_dd, y=lat_dd), size=1.2) +
  # CBAs
  geom_point(data=cbas_sea, mapping=aes(x=long_dd, y=lat_dd), size=1, color="blue") +
  ggrepel::geom_text_repel(data=cbas_sea %>% filter(long_dd < -90), 
                           mapping=aes(x=long_dd, y=lat_dd, label=community), size=2, color="blue") +
  # Focus group port labels
  geom_text(data=ports, mapping=aes(x=long_dd+0.3, y=lat_dd, label=port), 
            hjust=0, vjust=-0.2, size=2.2) +
  # WEA labels
  geom_text(data=wea_labels, mapping=aes(x=long_dd+0.3, y=lat_dd, label=wea), 
            hjust=0.5, vjust=0.5, size=2.2, fontface="italic", color="darkred") +
  # Labels 
  labs(title="A. West Coast") +
  # Crop
  coord_sf(ylim=c(33, 49), xlim=c(-125, -116)) +
  # Theme 
  theme_bw() + my_theme
g1


# East Coast
g2 <- ggplot() +
  # Land
  geom_sf(data=world, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  # WEAs
  geom_sf(data=weas, fill="darkred", color=NA) +
  # CBAs
  geom_point(data=cbas_sea, mapping=aes(x=long_dd, y=lat_dd), size=1, color="blue") +
  ggrepel::geom_text_repel(data=cbas_sea %>% filter(long_dd > -90), 
                           mapping=aes(x=long_dd, y=lat_dd, label=community), 
                           size=2, color="blue",
                           max.overlaps = Inf) +
  # Labels 
  labs(title="B. East Coast") +
  # Crop
  coord_sf(ylim=c(33, 49), xlim=c(-81, -67)) +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.1),
        legend.key.size = unit(0.4, "cm"))
g2

# Whole country
g3 <- ggplot() +
  # Land
  geom_sf(data=world, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey88", color="white", lwd=0.2, inherit.aes = F) +
  # WEAs
  geom_sf(data=weas, fill="darkred", color=NA) +
  geom_sf(data=weas_or, fill=NA, color="darkred", lwd=0.2) +
  # CBAs
  # geom_point(data=cbas_sea, mapping=aes(x=long_dd, y=lat_dd), size=1, color="blue") +
  # East Coast box
  annotate("rect", 
           ymin=33, ymax=49, 
           xmin=-81, xmax=-67, 
           color="black", fill=NA, lwd=0.3) +
  # West Coast box
  annotate("rect", 
           ymin=33, ymax=49, 
           xmin=-125, xmax=-116, 
           color="black", fill=NA, lwd=0.3) +
  # Crop
  coord_sf(ylim=c(26, 49), xlim=c(-124, -69)) +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.1),
        legend.key.size = unit(0.4, "cm"), 
        # Erase white
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(linewidth = 0.2),
        panel.border = element_rect(color="black", fill=NA, linewidth = 0.2),
        panel.spacing = unit(0, "pt"))
g3

g2_final <- g2 +
  inset_element(
    g3,
    left = 0.03,  # Distance from left
    top = 0.98,  # Distance from bottom
    bottom = 0.75,
    right = 0.6
  )
g2_final


# Merge
g <- g1 + g2_final +
  plot_layout(
    nrow = 1,
    widths = c(0.39, 0.61)
  )
g
# g <- gridExtra::grid.arrange(g1, g2_final, nrow=1, widths=c(0.4, 0.6))


# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_maps.png"), 
       width=6.5, height=6.25, units="in", dpi=600, bg="white")
     

