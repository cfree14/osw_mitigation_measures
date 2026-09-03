

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(ggtext)

# Directories
tabledir <- "tables"
plotdir <- "figures"

# Read data
actions_orig <- readxl::read_excel("data/actions/action_database.xlsx", sheet=1)
measures_orig <- readxl::read_excel("data/actions/action_database.xlsx", sheet=2)
att_key <- readxl::read_excel("data/actions/action_database.xlsx", sheet=3)


# Build data
################################################################################

# Action count
actions_tot <- actions_orig %>% 
  # Ignore
  filter(is.na(ignore_yn)) %>% 
  # Count
  group_by(strategy) %>% 
  summarize(n=n(),
            nsources=n_distinct(source)) %>% 
  arrange(desc(n)) %>% 
  mutate(nsources=as.character(nsources)) %>% 
  # Order
  mutate(strategy=factor(strategy, levels=strategy))



# Build data
################################################################################

# Build measures data
measures <- measures_orig %>% 
  # Simplify
  select(strategy, attributes) %>% 
  # Split
  separate(attributes, sep=", ", into=paste0("attribute", 1:5)) %>% 
  # Gather
  gather(key="num", value="attribute", 2:ncol(.)) %>% 
  # Clean up
  select(-num) %>% 
  filter(!is.na(attribute)) %>% 
  # Add
  left_join(att_key) %>% 
  # Mark 
  mutate(mark="yes") %>% 
  # Order
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological")))


# Build data
template <- purrr::map_df(measures$strategy, function(x){
  df <- att_key %>% 
    mutate(strategy=x)
}) %>% left_join(measures) %>% 
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological"))) %>% 
  # Shorten some
  mutate(domain=case_when(dimension %in% c("Ecological", "Socioeconomic") & domain=="Organization" ~ "Org",
                          dimension %in% c("Governance", "Socioeconomic") & domain=="Agency" ~ "Ag",
                          dimension %in% c("Governance") & domain=="Learning" ~ "Le",
                          dimension %in% c("Governance") & domain=="Flexibility" ~ "Fl",
                          T ~ domain)) %>% 
  # Add domain id
  mutate(domain_id=paste(dimension, domain, sep="_"))

# Build domain id order
dom_order <- template %>% 
  group_by(dimension, domain, domain_id, attribute) %>% 
  summarize(supported_yn=sum(!is.na(mark))>0) %>% 
  ungroup() %>% 
  group_by(dimension, domain, domain_id) %>% 
  summarize(nattributes=sum(supported_yn)) %>% 
  arrange(dimension, desc(nattributes))

# Build attribute order
att_order <- template %>% 
  # Order domains
  mutate(domain_id=factor(domain_id, levels=dom_order$domain_id)) %>% 
  # Count strategies per attribute
  group_by(dimension, domain_id, attribute) %>% 
  summarize(nstrategies=sum(mark=="yes", na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(dimension, domain_id, desc(nstrategies))

# Identify attributes to bold
att_bold <- att_order %>% 
  filter(nstrategies>0) %>% pull(attribute)

# Order data
data <- template %>% 
  mutate(domain_id=factor(domain_id, levels=dom_order$domain_id),
         attribute=factor(attribute, levels=att_order$attribute))

# Full
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=9),
                     strip.text = element_text(size=8),
                     # Gridlines
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.2, "cm"),
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot all
g <- ggplot(data, aes(x=attribute, 
                          y=strategy, 
                          fill=mark)) +
  # Setup empty facet
  ggh4x::facet_nested(~ dimension + domain_id, 
                      scales = "free_x", 
                      space = "free_x",
                      labeller = labeller(
                        domain_id = function(x) sub("^[^_]+_", "", x)
                      )) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x="Resilience attribute", y="") +
  # X-axis labels
  scale_x_discrete(
    labels = function(x) {
      label <- sub("^[^_]+_", "", x)
      
      ifelse(
        x %in% att_bold,
        paste0("<span style='color:grey30'><b>", label, "</b></span>"),
        paste0("<span style='color:grey60'>", label, "</span>")
      )
    }
  ) +
  # Legend
  scale_fill_manual(values="black", na.value = "white", guide="none") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = ggtext::element_markdown(angle = 45, vjust = 1, hjust=1),
        panel.spacing = unit(0, "lines"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_strategy_resilience_attributes_full.png"), 
       width=8.5, height=5.0, units="in", dpi=600, bg="white")


# Reduced
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=9),
                     plot.tag=element_text(size=10),
                     strip.text = element_text(size=9),
                     # Gridlines
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.2, "cm"),
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot socioeconomic
g <- ggplot(data %>% filter(dimension!="Ecological"), 
            aes(x=attribute, 
                y=strategy, 
                fill=mark)) +
  # Setup empty facet
  ggh4x::facet_nested(~ dimension + domain_id, 
                      scales = "free_x", 
                      space = "free_x",
                      labeller = labeller(
                        domain_id = function(x) sub("^[^_]+_", "", x)
                      )) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x="Resilience attribute", y="") +
  # X-axis labels
  scale_x_discrete(
    labels = function(x) {
      label <- sub("^[^_]+_", "", x)
      
      ifelse(
        x %in% att_bold,
        paste0("<span style='color:grey30'><b>", label, "</b></span>"),
        paste0("<span style='color:grey60'>", label, "</span>")
      )
    }
  ) +
  # Legend
  scale_fill_manual(values="black", na.value = "white", guide="none") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = ggtext::element_markdown(angle = 45, vjust = 1, hjust=1),
        panel.spacing = unit(0, "lines"))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_strategy_resilience_attributes_no_eco.png"), 
       width=8.5, height=5.0, units="in", dpi=600, bg="white")




