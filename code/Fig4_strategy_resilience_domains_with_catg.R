

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

# Domains per dimension
se_domains <- c("Assets", "Flexibility", "Learning", "Agency", "Organization")
gov_domains <- c("Organization", "Flexibility", "Agency","Learning")
eco_domains <- c("Assets", "Flexibility", "Organization")

# Domain ids
domain_ids <- c(paste("Socioeconomic", se_domains, sep="_"),
                paste("Governance", gov_domains, sep="_"),
                paste("Ecological", eco_domains, sep="_"))

# Bold ids
bold_ids <- c(
  "Socioeconomic_Agency",
  "Socioeconomic_Assets",
  "Socioeconomic_Flexibility",
  "Socioeconomic_Learning",
  "Socioeconomic_Organization",
  "Governance_Flexibility",
  "Governance_Organization",
  "Ecological_Assets"
)

# Build measures data
measures <- measures_orig %>% 
  # Simplify
  select(category, strategy, attributes) %>% 
  # Split
  separate(attributes, sep=", ", into=paste0("attribute", 1:5)) %>% 
  # Gather
  gather(key="num", value="attribute", 3:ncol(.)) %>% 
  # Clean up
  select(-num) %>% 
  filter(!is.na(attribute)) %>% 
  # Add
  left_join(att_key) %>% 
  # Reduce to category, strategy, domain, dimension
  select(category, strategy, domain, dimension) %>% 
  unique() %>% 
  # Order
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological"))) %>% 
  # Add domain id
  mutate(domain_id=paste(dimension, domain, sep="_") %>% factor(., levels=domain_ids)) %>% 
  # Present?
  mutate(supported_yn="yes")

# Build template
measures_full <- expand.grid(strategy=unique(measures$strategy),
                             domain_id=domain_ids) %>% 
  # Add whether domain id is supported
  left_join(measures %>% select(strategy, domain_id, supported_yn)) %>% 
  # Seperate domain id into dimension/domain
  separate(domain_id, into=c("dimension", "domain"), sep="_", remove=F) %>% 
  # Order dimension
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological"))) %>% 
  # Add category
  left_join(measures_orig %>% select(category, strategy)) %>% 
  mutate(category=recode(category, 
                         "Learning"="Learning and\ninnovation",
                         "Infrastructure"="Equipment and\ninfrastructure")) %>% 
  mutate(category=factor(category, levels=c("Biology",
                                             "Marketing",
                                             "Subsidies",
                                             "Equipment and\ninfrastructure",
                                             "Learning and\ninnovation",
                                             "Compensation",
                                             "Governance")))

# Most commonly support domains
measures %>% count(dimension, domain) %>% 
  arrange(dimension, desc(n))


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   strip.text = element_text(size=7),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(measures_full, aes(x = domain_id, y = strategy, fill = supported_yn)) +
  # Facet
  facet_grid(category~dimension, scales = "free", space = "free") +
  geom_tile() +
  # Lables
  labs(x = "Resilience domain", y = "") +
  # X-axis labels
  scale_x_discrete(
    labels = function(x) {
      label <- sub("^[^_]+_", "", x)
      
      ifelse(
        x %in% bold_ids,
        paste0("<span style='color:grey30'><b>", label, "</b></span>"),
        paste0("<span style='color:grey60'>", label, "</span>")
      )
    }
  ) +
  # Legend
  scale_fill_manual(values = "black", na.value = NA) +
  # Theme
  theme_bw() +
  base_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        strip.text.y.right = element_text(angle = 0), # Horizontal right-side labels
        panel.spacing.y = grid::unit(0, "lines"),     # Remove vertical facet spacing
        axis.text.x = ggtext::element_markdown(angle = 45, vjust = 1, hjust = 1))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig4_measures_by_res_domain_w_catg.png"), 
       width=6.5, height=4, units="in", dpi=600, bg="white")


