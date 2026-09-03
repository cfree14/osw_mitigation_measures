

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
actions_orig <- readxl::read_excel("data/actions/action_database_other_cbas.xlsx", sheet=1)
measures_orig <- readxl::read_excel("data/actions/action_database_other_cbas.xlsx", sheet=2)
# att_key <- readxl::read_excel("data/actions/action_database.xlsx", sheet=3)


# Build data
################################################################################

# Action count
actions_tot <- actions_orig %>% 
  # Count
  group_by(strategy) %>% 
  summarize(n=n(),
            ntowns=n_distinct(town)) %>% 
  arrange(desc(n)) %>% 
  # Order
  mutate(strategy=factor(strategy, levels=strategy))

# Town name
towns <- actions_orig %>% 
  # Summarize
  group_by(town) %>% 
  summarize(nstrategies=n_distinct(strategy)) %>% 
  ungroup() %>% 
  arrange(desc(nstrategies))

# Action stats
actions_by_town <- actions_orig %>% 
  # Count
  count(town, strategy) %>% 
  # Order sources
  mutate(town=factor(town, levels=towns$town),
         strategy=factor(strategy, levels=actions_tot$strategy))


# Plot data
################################################################################

# Custom fonts
font_faces <- ifelse(grepl("fish", levels(actions_by_town$strategy)), "bold", "plain")
font_colors <- ifelse(grepl("fish", levels(actions_by_town$strategy)), "red", "black")


# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   plot.tag.position = c(0.05, 1),
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
g1 <- ggplot(actions_by_town, mapping=aes(x=town, y=strategy, size=n)) + # fill=n
  geom_point() +
  # geom_tile(color="black", lwd=0.2) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_size_continuous(name="# of actions", range=c(0.5, 3), breaks=seq(2,12, 2)) +
  # scale_fill_gradientn(name="# of actions", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.7, 0.897),
        # legend.position="top",
        # legend.title.position = "top",
        axis.text.y=element_text(face=font_faces, color=font_colors),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) # , size=6
g1

g2 <- ggplot(actions_tot, mapping=aes(x=n, y=strategy, fill=as.character(ntowns))) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="# of actions\n\n\n\n\n\n\n\n\n\n", 
       y="", 
       tag="B") +
  # Legend
  scale_fill_manual(name="# of sources",
                    values=RColorBrewer::brewer.pal(n_distinct(actions_tot$ntowns), "Blues")) +
  # scale_fill_gradientn(name="# of sources", 
  #                      colors=RColorBrewer::brewer.pal(9, "Blues")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        # legend.position = "top",
        # legend.title.position = "top",
        legend.position = c(0.6, 0.9))
g2

# Merges
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.82, 0.18))

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig2_measures_by_town.png"), 
       width=6.5, height=5.75, units="in", dpi=600, bg="white")


