library(ggplot2)
library(rnaturalearth)
library(data.table)
library(dplyr)
library(sf)

## read these all in and save as single CSV
rich <- fread("data/Palaearctic_PA_AsCSV3_wgs84.csv")

# reduce to lentic only species
traits <- read.csv("data/traits/Palaearctic_Species.csv") %>% 
  dplyr::select(binomial, LL) 

rich_t <- left_join(rich, traits) %>% 
  na.omit()%>% 
  filter(LL != "")  

rich_t <- rich_t %>% 
  mutate(LL = case_when(LL == "Lentic" | LL == "lentic" ~ "Lentic",
                               LL == "Lotic" ~ "Lotic"))

gb <- rich_t %>% 
  group_by(binomial, LL) %>% 
  summarise(rangeSize = sum(PA, na.rm = T))

ggplot(gb) +
  geom_violin(mapping = aes(x = LL, y = rangeSize, fill = LL)) +
  geom_boxplot(mapping = aes(x = LL, y = rangeSize), fill = NA) +
  scale_fill_manual(values = c("#73AE80", "#6C83B5")) +
  scale_y_log10() +
  labs(y = "Range size", x = "",
       fill = "") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none")

  

ggsave(filename = "figures/LenticLotic_RangeSize_Fig4_logScale.png")
