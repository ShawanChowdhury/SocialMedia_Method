library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)

# Total groups
com <- read.csv("data/fb_groups.csv")
head(com)

# Plotting data
ggplot(com) +
  geom_bar(aes(group, n_mem, fill = taxa_up), stat = "identity") +
  theme_classic() + xlab("") + ylab("Number of members") + 
  scale_fill_viridis_d() + 
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        legend.position = "top")  +
  scale_x_continuous(breaks = c(1, 5, 6, 8, 14, 15, 30, 33))

ggsave("Figures/tot_group_fb.png")

###################
com <- read.csv("data/cleanedRecords_FB.csv")

# Year-wise records
year <- com %>% 
  dplyr::select(group, year, source)

# Summarised data
year_sum <- year %>% 
  group_by(group, year) %>% 
  summarise(n = n())

birds <- year_sum %>% 
  filter(group == "Birds")
butterflies <- year_sum %>% 
  filter(group == "Butterflies")
ov <- com %>% 
  group_by(year) %>% 
  summarise(n = NROW(species)) %>% 
  mutate(group = "All")

year_rec <- rbind(birds, butterflies, ov)

rest <- year_sum %>% 
  filter(group %in% c("Amphibians", "Mammals", "Reptiles"))

ggplot(year_rec, aes(year, n)) +
  geom_line(lwd = 1, color = "royalblue") +
  xlab("") + ylab("Number of records") +
  theme_classic() + xlim(c(1978, 2020)) +
  theme(legend.position = "none") + facet_wrap(~group)

ggsave("Figures/all_bird_butterfly_year_growth_method.png")

ggplot(rest, aes(year, n)) +
  geom_line(lwd = 1, color = "royalblue") +
  xlab("") + ylab("") +
  theme_classic() + facet_wrap(~group) + xlim(c(1978, 2020)) +
  theme(legend.position = "none")

ggsave("Figures/Amp_Mam_Rep_year_growth_method.png")

###############################
# Figure 3A [% of records]
com <- read_csv("data/cleanedRecords_FB.csv")

com <- com %>% 
  filter(!iucn %in% c("RE"))

# Overall threatened and non-threatened %
sum_grp <- com %>% group_by(thrt_status) %>% 
  summarise(n = n())

# % of threatened species
sum_grp <- sum_grp %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()

# Group-wise number
head(com)

# Summarising data
com_sum_grp <- com %>% group_by(group, thrt_status) %>% 
  summarise(n_rec = n())

com_sum_sp <- com %>% 
  select(group, species, thrt_status) %>% 
  unique() %>% 
  group_by(group, thrt_status) %>% 
  summarise(n_sp = NROW(species))

com_sum <- dplyr::left_join(com_sum_grp, com_sum_sp, by = c("group", "thrt_status"))

head(com_sum)

# % of threatened species
com_sum_grp_up <- com_sum %>% 
  group_by(group) %>% 
  mutate(rec_per = n_rec/sum(n_rec)*100, sp_per = n_sp/sum(n_sp)*100) %>% 
  ungroup()

head(com_sum_grp_up)

write_csv(com_sum_grp_up, "data/sum_rec_taxa_threat.csv")

library(RColorBrewer)

com_sum_grp_up$group <- factor(com_sum_grp_up$group, 
                               levels=c("Butterflies", "Amphibians", "Reptiles",
                                        "Birds", "Mammals"))

# Plotting data
ggplot(com_sum_grp_up) +
  geom_bar(aes(group, rec_per, fill = thrt_status), stat = "identity") +
  theme_bw() + xlab("") + ylab("") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept=seq(1.5, length(com_sum_grp_up$group)-0.5, 1), 
             lwd=1, colour="grey94")
ggsave("Figures/tot_rec_group_thrt_method.png")

########################################
# Figure 3B [% of species]
com <- read_csv("data/cleanedRecords_FB.csv")
com <- com %>% 
  filter(!iucn %in% c("RE", "DD"))

sp_sum <- com %>% 
  group_by(group, species, thrt_status, iucn) %>% 
  summarise(n = NROW(species))

# Thrt and non-thrt by groups
sp_sum_thrt <- sp_sum %>% 
  group_by(group, thrt_status) %>% 
  summarise(n = NROW(species))

sp_sum_thrt <- sp_sum_thrt %>% 
  group_by(group) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()

head(sp_sum_thrt)

sp_sum_thrt$group <- factor(sp_sum_thrt$group, 
                               levels=c("Butterflies", "Amphibians", "Reptiles",
                                        "Birds", "Mammals"))

# Plotting data
ggplot(sp_sum_thrt, aes(group, percent, fill = thrt_status)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("") + ylab("") + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept=seq(1.5, length(sp_sum_thrt$group)-0.5, 1), 
             lwd=1, colour="grey94")
ggsave("Figures/tot_spp_group_thrt_method.png")

###############
head(com)

# Species summary by taxa & thrt_status
sp <- com %>% 
  select(group, species, thrt_status) %>% 
  unique()

sp_sum <- sp %>% 
  group_by(group, thrt_status) %>% 
  summarise(n = NROW(species))

################
# Range of records
sp_sum <- com %>% 
  group_by(group, species, thrt_status, iucn) %>% 
  summarise(n = NROW(species))

# Total median: 27

sp_sum_80 <- sp_sum %>% 
  filter(n > 79)

#################################
# Thrt and non-thrt by groups
sp_sum_thrt <- sp_sum %>% 
  group_by(group, thrt_status) %>% 
  summarise(n = NROW(species))

#################################
# Data distribution by taxa
# Amphibians
amp <- sp_sum %>% 
  filter(group == "Amphibians")
median_amp <- median(amp$n)

# Birds
birds <- sp_sum %>% 
  filter(group == "Birds")
median_birds <- median(birds$n)

# Butterflies
but <- sp_sum %>% 
  filter(group == "Butterflies")
median_but <- median(but$n)

# Mammals
mam <- sp_sum %>% 
  filter(group == "Mammals")
median_mam <- median(mam$n)

# Reptiles
rep <- sp_sum %>% 
  filter(group == "Reptiles")
median_rep <- median(rep$n)

################################
# Year-wise growth in number of species
com <- read_csv("data/cleanedRecords_FB.csv")
head(com)

# Summarising results by year [all]
year_rec <- com %>% 
  group_by(year) %>% 
  summarise(n = NROW(species))

# Figure
ggplot(year_rec, aes(year, n)) +
  geom_line(lwd = 1, color = "royalblue") +
  xlab("") + ylab("") +
  theme_classic() + xlim(c(1978, 2020)) +
  theme(legend.position = "none")

# Summarising results by year [by Taxa]
year_rec <- com %>% 
  group_by(group, year) %>% 
  summarise(n = NROW(species))

################################
# Records for DD species
com <- read_csv("data/cleanedRecords_FB.csv")
com <- com %>% 
  filter(iucn %in% c("DD"))

################################
# Records by species
com <- read_csv("data/cleanedRecords_FB.csv")
head(com)

sp_n_rec <- com %>% 
  group_by(group, species) %>% 
  summarise(n = NROW(species))
