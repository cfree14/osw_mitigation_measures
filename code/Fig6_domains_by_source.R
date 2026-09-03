

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
sources <- c("Focus groups", "Castle Wind CBA", "CA CFO CBA", "7c Working Group", "BOEM guidelines")

# Domains per dimension
se_domains <- c("Assets", "Flexibility", "Learning", "Organization", "Agency")
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
  # Reduce to strategy, domain, dimension
  select(strategy, domain, dimension) %>% 
  unique() %>% 
  # Order
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological"))) %>% 
  # Add domain id
  mutate(domain_id=paste(dimension, domain, sep="_") %>% factor(., levels=domain_ids)) %>% 
  # Present?
  mutate(supported_yn="yes")

# Strategies by source
strategy_by_source <- actions_orig %>% 
  # Ignore
  filter(is.na(ignore_yn)) %>% 
  # Count
  count(source, strategy) %>% 
  # Order sources
  mutate(source=factor(source, levels=sources))

# Domains by source
domains_by_source <- strategy_by_source %>% 
  select(-n) %>% 
  right_join(measures, by="strategy", relationship = "many-to-many") %>% 
  # Simplify
  select(source, dimension, domain_id, domain) %>% 
  unique() %>% 
  # Supported
  mutate(support_yn="yes")
 
# Build out 
domains_by_source_full <- expand.grid(source=sources,
                                      domain_id=domain_ids) %>% 
  # Add 
  left_join(domains_by_source %>% select(source, domain_id, support_yn)) %>% 
  # Seperate domain id into dimension/domain
  separate(domain_id, into=c("dimension", "domain"), sep="_", remove=F) %>% 
  # Order dimension
  mutate(dimension=factor(dimension, levels=c("Socioeconomic", "Governance", "Ecological")))



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
g <- ggplot(domains_by_source_full, mapping=aes(x=domain_id, y=source, fill=support_yn)) +
  facet_wrap(~dimension, space="free_x", scales="free_x") +
  geom_tile() +
  # Labels
  labs(x="Resilience domain", y="") + 
  # Legend
  scale_fill_manual(values = "black", na.value = NA) +
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
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none", 
        axis.text.x = ggtext::element_markdown(angle = 45, vjust = 1, hjust = 1))
g

ggsave(g, filename=file.path(plotdir, "Fig6_domains_by_source.png"), 
       width=5.5, height=2.5, units="in", dpi=600, bg="white")



