

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
g1 <- ggplot(actions_by_source, mapping=aes(x=source, y=strategy, size=n)) + # fill=n
  geom_point() +
  # geom_tile(color="black", lwd=0.2) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_size_continuous(name="# of actions", range=c(0.5, 3)) +
  # scale_fill_gradientn(name="# of actions", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.title.position = "top",
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
        legend.position = "top",
        legend.title.position = "top")
g2

# Merges
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.7, 0.3))

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig3_measures_by_source.png"), 
       width=6.5, height=4.75, units="in", dpi=600, bg="white")

