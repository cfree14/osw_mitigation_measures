

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
data <- readxl::read_excel("data/actions/action_database.xlsx")

# Source grid
sources <- c("Focus groups", "Fleming", 
             "Castle Wind CBA", "Offshore CBAs", "Land-based CBAs",
             "BOEM guidance", "Selected reviews")
source_grid <- expand.grid(measure=data$measure,
                           source=sources) %>% 
  mutate(source=factor(source, levels=sources)) %>% 
  mutate(reported_yn=sample(c("yes", "no"), n(), replace=T))


# Wellbeing grid
constituents <- c("Conditions", "Connections", "Capabilities", "    Cross-cutting")
constituent_colors <- c("#8a777d", "#b96040", "#cfb474", "grey80")
constituent_grid <- expand.grid(measure=data$measure,
                                constituent=constituents) %>% 
  mutate(constituent=factor(constituent, levels=constituents)) %>% 
  mutate(reported_yn=sample(c("yes", "no"), n(), replace=T),
         reported_yn_use=ifelse(reported_yn=="yes", as.character(constituent), NA) %>% factor(., constituents))

# Climate resilience grid
dimensions <- c("Governance", "Ecological", "Socio-economic")
domains <- c("Assets", "Flexibility", "     Organization", "Learning", "Agency")
domain_colors <- c("#8a777d", "#667d45", "#b96040", "#5b7d8c", "#cfb474")
resilience_grid <- expand.grid(measure=data$measure,
                               dimension=dimensions,
                               domain=domains) %>% 
  # Eliminate domains that don't exist
  filter( !(dimension=="Governance" & domain=="Assets") ) %>% 
  filter( !(dimension=="Ecological" & domain %in% c("Agency", "Learning")) ) %>% 
  # Factor dimensions/domains
  mutate(dimension=factor(dimension, levels=dimensions),
         domain=factor(domain, levels=domains)) %>% 
  # Simulate data
  mutate(reported_yn=sample(c("yes", "no"), n(), replace=T),
         reported_yn_use=ifelse(reported_yn=="yes", as.character(domain), NA) %>% factor(., domains))

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.tag=element_text(size=8),
                    strip.background = element_rect(fill=NA, color=NA),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Source
g1 <- ggplot(source_grid, aes(y=measure, x=source, fill=reported_yn)) +
  geom_tile() +
  # Labels
  labs(x="Source", y="Mitigation measure", tag="A", subtitle="") +
  # Legend
  scale_fill_manual(name="", values=c("white", "grey30")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Source
g2 <- ggplot(constituent_grid, aes(y=measure, x=constituent, fill=reported_yn_use)) +
  geom_tile() +
  # Labels
  labs(x="Wellbeing\nconstituent", y="Mitigation measure", tag="B", subtitle="") +
  # Legend
  # scale_fill_manual(name="", values=c("white", "grey30")) +
  scale_fill_manual(name="", values=constituent_colors, na.value = "white") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.title.y = element_blank())
g2

# Resilience
g3 <- ggplot(resilience_grid, aes(y=measure, x=domain, fill=reported_yn_use)) +
  facet_grid(.~dimension, scales="free_x", space="free_x") +
  geom_raster() +
  # Labels
  labs(x="Resilience\ndomain", y="Mitigation measure", tag="C") +
  # Legend
  # scale_fill_manual(name="", values=c("white", "grey30")) +
  scale_fill_manual(name="", values=domain_colors, na.value = "white") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.title.y = element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.45, 0.15, 0.4))

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_measures.png"), 
       width=6.5, height=3.0, units="in", dpi=600, bg="white")


