

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

# Read data
actions_orig <- readxl::read_excel("data/actions/action_database.xlsx", sheet=1)
measures_orig <- readxl::read_excel("data/actions/action_database.xlsx", sheet=2)
att_key <- readxl::read_excel("data/actions/action_database.xlsx", sheet=3)


# Build data
################################################################################

# Sources
freeR::uniq(actions_orig$source)
sources <- c("Focus groups", "Castle Wind CBA", "CA CFO CBA", "7c Working Group", "BOEM guidelines")

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


# Action stats
actions_by_source <- actions_orig %>% 
  # Ignore
  filter(is.na(ignore_yn)) %>% 
  # Count
  count(source, strategy) %>% 
  # Order sources
  mutate(source=factor(source, levels=sources),
         strategy=factor(strategy, levels=actions_tot$strategy))




# Plot data
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


# Plot data
g1 <- ggplot(actions_by_source, mapping=aes(x=source, y=strategy, size=n)) +
  geom_point() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_size_continuous(name="# of actions") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g1

g2 <- ggplot(actions_tot, mapping=aes(x=n, y=strategy, fill=nsources)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="# of actions\n\n\n\n", 
       y="", 
       tag="B") +
  # Legend
  scale_fill_manual(name="# of sources", values=RColorBrewer::brewer.pal(5, "Blues")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        legend.position = "top")
g2

# Merges
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.7, 0.3))

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_measures_by_source.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")


# Build and plot data
################################################################################

# Build measures data
measures <- measures_orig %>% 
  # Split
  separate(attributes, sep=", ", into=paste0("attribute", 1:5)) %>% 
  # Gather
  gather(key="num", value="attribute", 2:ncol(.)) %>% 
  # Clean up
  select(-num) %>% 
  filter(!is.na(attribute)) %>% 
  # Add
  left_join(att_key) %>% 
  # Order
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological")))

# Plot
g <- ggplot(measures, aes(x=domain, y=strategy)) +
  facet_wrap(~dimension) +
  geom_point() +
  # Labels
  labs(x="Resilience domain", y="") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_measures_by_res_domain.png"), 
       width=6.5, height=3.5, units="in", dpi=600, bg="white")


# Build data
################################################################################

# Build measures data
measures <- measures_orig %>% 
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


template <- purrr::map_df(measures$strategy, function(x){
  df <- att_key %>% 
    mutate(strategy=x)
}) %>% left_join(measures) %>% 
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological"))) %>% 
  # Shorten some
  mutate(domain=case_when(dimension %in% c("Ecological", "Socioeconomic") & domain=="Organization" ~ "Org.",
                          dimension %in% c("Governance", "Socioeconomic") & domain=="Agency" ~ "Ag.",
                          dimension %in% c("Governance") & domain=="Learning" ~ "Le.",
                          dimension %in% c("Governance") & domain=="Flexibility" ~ "Fl.",
                          T ~ domain))

# Plot
g <- ggplot(measures, aes(x=attribute, y=strategy)) +
  # facet_grid(.~dimension+domain, space="free_x", scales="free_x") +
  ggh4x::facet_nested(~ dimension + domain, 
               scales = "free_x", 
               space = "free_x") +
  geom_point() +
  # Labels
  labs(x="Resilience attribute", y="") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.spacing = unit(0, "lines"))
g

ggsave(g, filename=file.path(plotdir, "FigX_measures_by_res_attribute.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")


# Plot
g <- ggplot(template, aes(x=attribute, y=strategy, fill=mark)) +
  # Setup empty facet
  ggh4x::facet_nested(~ dimension + domain, 
                      scales = "free_x", 
                      space = "free_x") +
  # Plot data
  geom_tile() +
  # Labels
  labs(x="Resilience attribute", y="") +
  # Legend
  scale_fill_discrete(na.value = "white", guide="none") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.spacing = unit(0, "lines"))
g

ggsave(g, filename=file.path(plotdir, "FigX_measures_by_res_attribute2.png"), 
       width=8.5, height=4.0, units="in", dpi=600, bg="white")




# Build data
################################################################################

atts_by_source <- actions_by_source %>% 
  right_join(measures, by=c("strategy"="strategy"), relationship = "many-to-many")

g <- ggplot(atts_by_source, mapping=aes(x=domain, y=source)) +
  facet_wrap(~dimension) +
  geom_tile() +
  # Labels
  labs(x="Resilience domain", y="") + 
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g

ggsave(g, filename=file.path(plotdir, "FigX_domains_by_source.png"), 
       width=6.5, height=2.5, units="in", dpi=600, bg="white")



