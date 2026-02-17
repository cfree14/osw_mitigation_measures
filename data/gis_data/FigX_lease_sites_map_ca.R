
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
gisdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/cc_offshore_wind/data/gis_data/processed"
plotdir <- "~/Dropbox/Chris/UCSB/projects/mpa_projects/boem/figures"

# Read data
sites <- readRDS(file=file.path(gisdir, "CA_wind_lease_sites.Rds"))

# Get data
blocks <- wcfish::blocks %>% 
  sf::st_as_sf() %>% 
  filter(block_state=="California" & block_type!="Offshore")
blocks_df <- blocks %>% 
  sf::st_drop_geometry()

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Which blocks intersect lease areas?
################################################################################

# Perform interesection
lease_blocks1 <- sf::st_intersection(sites, blocks)

# Format intersection
lease_blocks2 <- lease_blocks1 %>% 
  # Simplify
  select(lease_id, block_id, block_type) %>% 
  sf::st_drop_geometry() %>% 
  # Label region
  mutate(lease=ifelse(block_id < 300, "Northern", "Southern")) %>% 
  # Remove one mid-shore block in south
  filter(block_type=="Inshore") %>% 
  select(-block_type)

# Even simpler key
lease_blocks3 <- lease_blocks2 %>% 
  select(lease, block_id) %>% 
  unique()
  
  

# Fish tickets
################################################################################

# Directory
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/"

# Read data
data_orig <- readRDS(file=file.path(datadir, "1980_2022_landings_receipts.Rds"))

# Summarize data
data <- data_orig %>% 
  # Annual stats by block
  group_by(year, block_id) %>% 
  summarize(landings_lb=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T),
            ntickets=n_distinct(receipt_id, na.rm=T)) %>% 
  ungroup() %>% 
  # Reduce to years of interest
  filter(year %in% c(2011:2022)) %>% 
  # Average by block
  group_by(block_id) %>% 
  summarize(landings_lb=mean(landings_lb, na.rm=T),
            value_usd=mean(value_usd, na.rm=T),
            ntickets=mean(ntickets, na.rm=T)) %>% 
  ungroup() %>% 
  # Format
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"),
         landings_mt=landings_kg / 1000) %>% 
  # Add block meta-data
  left_join(blocks_df %>% select(block_id, block_type, block_state), by="block_id") %>% 
  # Remove misreported blocks
  filter(!is.na(block_state))

# Spatialize
data_sf <- blocks %>% 
  select(block_id) %>% 
  left_join(data, by="block_id")


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey30", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lease areas
  geom_sf(data=sites, fill="darkred", alpha=0.5, color=NA) +
  # Plot state waters
  # geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-125.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "Fig1_lease_sites_map.png"), 
       width=3.75, height=5, units="in", dpi=600)



# Plot data
################################################################################

# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=data_sf , mapping=aes(fill=landings_mt), color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lease areas
  geom_sf(data=sites, fill="grey30", alpha=0.7, color=NA) +
  # Plot fill
  scale_fill_gradientn(name="Landings (mt)", 
                       trans="log10", breaks=c(1, 10, 100, 1000, 10000),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-125.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.15),
        legend.key.size = unit(0.2, "cm"))
g1

# Plot data
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=data_sf , mapping=aes(fill=value_usd), color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lease areas
  geom_sf(data=sites, fill="grey30", alpha=0.7, color=NA) +
  # Plot fill
  scale_fill_gradientn(name="Value (USD)", 
                       trans="log10", 
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6)),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(tag="B") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-125.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.15),
        legend.key.size = unit(0.2, "cm"))
g2

# Plot data
g3 <- ggplot() +
  # Plot blocks
  geom_sf(data=data_sf , mapping=aes(fill=ntickets), color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lease areas
  geom_sf(data=sites, fill="grey30", alpha=0.7, color=NA) +
  # Plot fill
  scale_fill_gradientn(name="Number of trips", 
                       trans="log10", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(tag="C") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-125.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.15),
        legend.key.size = unit(0.2, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_lease_sites_map.png"), 
       width=6.5, height=3, units="in", dpi=600)



# Annual proportion within lease blocks
################################################################################

# Calculate stats
stats <- data %>% 
  # Add lease
  left_join(lease_blocks3, by="block_id") %>% 
  mutate(lease=ifelse(is.na(lease), "Non-lease area", lease)) %>% 
  # Summarize
  group_by(lease) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
         value_usd=sum(value_usd, na.rm=T),
         ntickets=sum(ntickets, na.rm=T)) %>% 
  ungroup() %>% 
  # Gtaher
  gather(key="metric", value="value", 2:ncol(.)) %>% 
  # Calculate prop
  group_by(metric) %>% 
  mutate(prop=value/sum(value)) %>% 
  ungroup() %>% 
  # Format
  mutate(metric=recode_factor(metric, 
                       "landings_mt"="Landings",
                       "value_usd"="Revenues",
                       "ntickets"="Number of trips"))
  

# Plot
g <- ggplot(stats %>% filter(lease!="Non-lease area"), aes(y=metric, x=prop, fill=lease)) +
  geom_bar(stat="identity") +
  # Scales
  labs(x="Percent of statewide amount", y="") +
  scale_x_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Lease area") +
  # Theme
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_lease_area_importance.png"), 
       width=6.5, height=2, units="in", dpi=600)


# Species impacted by each area
################################################################################

# Calculate species
data_spp <- data_orig %>% 
  # Summarize
  group_by(year, block_id, comm_name) %>% 
  summarize(landings_lb=sum(landings_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  # Annual average by block and species
  group_by(block_id, comm_name) %>% 
  summarize(landings_lb=mean(landings_lb)) %>% 
  ungroup() %>% 
  # Reduce to lease blocks
  left_join(lease_blocks3, by="block_id") %>% 
  filter(!is.na(lease)) %>% 
  # Sum by lease area
  group_by(lease, comm_name) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm = T)) %>% 
  ungroup() %>% 
  # Reduce to top 20 species
  arrange(lease, desc(landings_lb)) %>% 
  group_by(lease) %>% 
  slice(1:20) %>% 
  ungroup() %>% 
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"),
         landings_mt=landings_kg / 1000)


# Plot data
g <- ggplot(data_spp, aes(x=landings_mt, y=reorder(comm_name, landings_mt))) +
  facet_wrap(~lease, ncol=1, scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Landings (mt)", y="") +
  # Theme
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_lease_area_important_species.png"), 
       width=6.5, height=5, units="in", dpi=600)

