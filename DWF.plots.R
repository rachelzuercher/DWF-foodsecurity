# Plots for "Distant water fishing and its effects on domestic nutritional security"
# Rachel Zuercher, rzuercher@mbayaq.org
# Last edited: 28 June 2024

# Before creating plots, run all code in 'DWF.case.analysis.R' script

##### MAIN PAPER ####

# Figure 3.
temp <- SAU_landings[SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Neritic tunas",]
temp$fishing_sector <- ifelse(temp$Fleet=="DWF", "DWF", temp$fishing_sector)

temp$fishing_sector <- factor(temp$fishing_sector, levels = c("Subsistence", "Artisanal", 
                                                              "Industrial", "DWF"))
temp <- aggregate(tonnes~fishing_sector+year, data=temp, FUN=sum)

ggplot(temp, 
       aes(fill=fishing_sector, y=tonnes, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  ylab("Tonnes landed") +
  scale_fill_manual(labels = c("Subsistence\n(domestic)", "Artisanal\n(domestic)", "Industrial\n(domestic)", "DWF"),
                    values=c("orange1","orange3","brown3","#275663")) +
  theme_bw() +
  theme(legend.spacing.y = unit(0.5, 'cm'), legend.position="top") +
  labs(fill=NULL)

ggsave(here::here(output, "Fig3a.pdf"), width = 4.5, height = 4.5)

neritic.nut.content <- nutrient_bar_child[nutrient_bar_child$stock=="Neritic tunas",]
neritic.nut.content$demo <- "child"
neritic.nut.content1 <- nutrient_bar_F[nutrient_bar_F$stock=="Neritic tunas",]
neritic.nut.content1$demo <- "F"
neritic.nut.content <- rbind(neritic.nut.content, neritic.nut.content1)
rm(neritic.nut.content1)

ggplot(neritic.nut.content, 
       aes(x=nutrient, y=perc_of_daily, fill=demo)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width=0.6) +
  coord_flip() +
  xlab("") +
  ylab("% of RNI") +
  #ggtitle("Percentage of RNI fulfilled by an 100-g serving") +
  scale_fill_manual(labels = c("Female (19-50 yrs)", "Child (1-3 yrs)"),
                    values=c("grey20", "grey60"),
                    breaks=c("F","child")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  labs(fill = NULL) +
  theme(legend.position = c(0.75, 0.9))

ggsave(here::here(output, "Fig3b.pdf"), width = 5.5, height = 5.5)

# all nutrients except Selenium (Child, Madagascar)
ggplot(results.children[!results.children$nutrient=="Selenium" & results.children$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  labs(title = "") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Fig3c-1.pdf"), width = 5, height = 5)

# Selenium (Child, Madagascar)
ggplot(results.children[results.children$nutrient=="Selenium" & results.children$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of children ages 1-3 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() + 
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Fig3c-2.pdf"), width = 5, height = 1.3)



# Figure 5. 

roundscad_landings <- read.csv(here::here(directory, "roundscad_landings_BFAR.csv"))

ggplot(roundscad_landings, 
       aes(color=Fleet, y=Landings, x=Year)) + 
  geom_line() +
  geom_point() +
  xlab("") +
  ylab("Metric tonnes") +
  #ggtitle("Round scad landings (Philippines)") +
  scale_color_manual(values=c("orange3", "orange1")) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.85),legend.title=element_blank())

ggsave(here::here(output, "Fig5a.pdf"), width = 4, height = 2.666666)

galunggong_prices <- read.csv(here::here(directory, "scad_price_PSAdata.csv"), sep=",", quote="", na.strings=c("NA", "", " "), header=TRUE)

galunggong_prices <- ddply(galunggong_prices, "Year", summarise,
                           mean = mean(Price_galunggong_medium_1kg),
                           sd   = sd(Price_galunggong_medium_1kg))

ggplot(galunggong_prices, aes(x=Year, y=mean)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
  xlab("") +
  ylab("Price (PhP) per kg") +
  ylim(0,200) +
  #ggtitle("Round scad retail price 2012-2021") +
  theme_bw() +
  theme(text = element_text(size=14))

ggsave(here::here(output, "Fig5b.pdf"), width = 6, height = 5)

# CHILDREN (AGES 1-3)
# all nutrients except Selenium (Philippines)
ggplot(phil.hypothetical.child[!phil.hypothetical.child$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  #labs(title = "Round scads / Philippines") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Fig5c-1.pdf"), width = 5, height = 5)

# Selenium (Philippines)
ggplot(phil.hypothetical.child[phil.hypothetical.child$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of children (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() + 
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Fig5c-2.pdf"), width = 5, height = 1.3)

#### SUPPLEMENTARY ####

# Figure S1 (a and b)

ggplot(nutrient_bar_child[nutrient_bar_child$stock=="Jumbo flying squid" |
                            nutrient_bar_child$stock=="Neritic tunas" |
                            nutrient_bar_child$stock=="Round scads (Decapterus)",], aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (child, 1-3 yrs) fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_grid(cols = vars(nutrient_bar_child$stock[nutrient_bar_child$stock=="Jumbo flying squid" |
                                                    nutrient_bar_child$stock=="Neritic tunas" |
                                                    nutrient_bar_child$stock=="Round scads (Decapterus)"]), scales = "free", space="free_y") 

ggsave(here::here(output, "SFig1A.pdf"), width = 7, height = 5)

ggplot(nutrient_bar_F[nutrient_bar_F$stock=="Jumbo flying squid" |
                        nutrient_bar_F$stock=="Neritic tunas" |
                        nutrient_bar_F$stock=="Round scads (Decapterus)",], aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (female, 18-50 yrs) fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_grid(cols = vars(nutrient_bar_F$stock[nutrient_bar_F$stock=="Jumbo flying squid" |
                                                nutrient_bar_F$stock=="Neritic tunas" |
                                                nutrient_bar_F$stock=="Round scads (Decapterus)"]), scales = "free", space="free_y") 

ggsave(here::here(output, "SFig1B.pdf"), width = 7, height = 5)


# Figure S2 (a and b)
ggplot(nutrient_bar_child[nutrient_bar_child$stock=="Kawakawa" |
                            nutrient_bar_child$stock=="Longtail tuna" |
                            nutrient_bar_child$stock=="Bullet tuna" |
                            nutrient_bar_child$stock=="Frigate tuna" |
                            nutrient_bar_child$stock=="Narrow-barred Spanish mackerel",], aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (child, 1-3 yrs) fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_grid(cols = vars(nutrient_bar_child$stock[nutrient_bar_child$stock=="Kawakawa" |
                                                    nutrient_bar_child$stock=="Longtail tuna" |
                                                    nutrient_bar_child$stock=="Bullet tuna" |
                                                    nutrient_bar_child$stock=="Frigate tuna" |
                                                    nutrient_bar_child$stock=="Narrow-barred Spanish mackerel"]), scales = "free", space="free_y") 

ggsave(here::here(output, "SFig2A.pdf"), width = 11, height = 6)

ggplot(nutrient_bar_F[nutrient_bar_F$stock=="Kawakawa" |
                        nutrient_bar_F$stock=="Longtail tuna" |
                        nutrient_bar_F$stock=="Bullet tuna" |
                        nutrient_bar_F$stock=="Frigate tuna" |
                        nutrient_bar_F$stock=="Narrow-barred Spanish mackerel",], aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (female, 18-50 yrs) fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_grid(cols = vars(nutrient_bar_F$stock[nutrient_bar_F$stock=="Kawakawa" |
                                                nutrient_bar_F$stock=="Longtail tuna" |
                                                nutrient_bar_F$stock=="Bullet tuna" |
                                                nutrient_bar_F$stock=="Frigate tuna" |
                                                nutrient_bar_F$stock=="Narrow-barred Spanish mackerel"]), scales = "free", space="free_y") 

ggsave(here::here(output, "SFig2B.pdf"), width = 11, height = 6)

# Madagascar full-length case narrative ####

# Figure 1.
country <- "Madagascar" 
  
d <- SAU_landings_summary[SAU_landings_summary$country==country, ]
d <- d[,1:5]
d <- d[!is.na(d$DWF),] # Only interested in stocks that have DWF landings
d <- d[d$DWF > 1,]
d <- d[!is.na(d$Domestic),] # Only interested in stocks that also have Domestic landings
d <- d[d$Domestic > 1,]
d <- gather(d[,1:5], fleet, tonnes, Domestic:Regional, factor_key=TRUE)
  
d$country <- NULL
names(d) <- c("Species", "Fleet", "value")
  
df <- d %>%
  make_long(Species, Fleet, value=value)
  
ggplot(as.data.frame(d),
       aes(y = value, axis1 = Species, axis2 = Fleet)) +
  geom_alluvium(aes(fill = Fleet), 
                width = 1/12, reverse = FALSE,
                curve_type="cubic") +
  scale_fill_manual(values = c(Domestic = "#C67D39", DWF = "#275663", Regional = "#7C9885")) +
  guides(fill = "none") +
  geom_stratum(colour="black", fill="white", width = 1/8, reverse = FALSE) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, reverse=FALSE, direction = "y", nudge_x = -.4) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, reverse=FALSE, direction = "y", nudge_x = 0) +
  scale_x_continuous(breaks = 1:2, labels = c("Species", "Fleet")) +
  ylab("Tonnes landed") +
  theme_bw() +
  ggtitle(country) +
  labs(subtitle="Species/species groups with DWF & Domestic landings of >1 tonne") +
  labs(caption="Data source: SAU catch reconstructions (2015-2019)")

ggsave(here::here(output, "Mada_Fig1.pdf"), width = 12, height = 10)


# Figure 2. 
madagascar_pre <- madagascar_pre[madagascar_pre$common_name=="Kawakawa" |
                                   madagascar_pre$common_name=="Longtail tuna" |
                                   madagascar_pre$common_name=="Bullet tuna" |
                                   madagascar_pre$common_name=="Frigate tuna" |
                                   madagascar_pre$common_name=="Bullet and frigate tunas" |
                                   madagascar_pre$common_name=="Narrow-barred Spanish mackerel",]

# there is one Regional entry for Reunion (France), 0.02 tonnes in Industrial landings of Narrow-barred Spanish mackerel
madagascar_pre$Fleet <- ifelse(madagascar_pre$Fleet=="Regional", "DWF", madagascar_pre$Fleet)

madagascar_pre <- aggregate(tonnes~common_name+Fleet+year, data=madagascar_pre, FUN=sum)

ggplot(madagascar_pre, aes(x = Fleet, y = tonnes, fill = common_name)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap( ~ year) +
  scale_fill_viridis(discrete=TRUE, option="viridis", direction=-1) +
  theme_bw() +
  theme(legend.title=element_blank())


ggsave(here::here(output, "Mada_Fig2.pdf"), width = 7, height = 5)

# Figure 3.
timeseries.plot <- SAU_landings[SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Neritic tunas",]

# there is one Regional entry for Reunion (France), 0.02 tonnes in Industrial landings of Narrow-barred Spanish mackerel
timeseries.plot$Fleet[timeseries.plot$area_name=="Madagascar" &
                        timeseries.plot$common_name=="Neritic tunas"] <- 
  ifelse(timeseries.plot$Fleet[timeseries.plot$area_name=="Madagascar" & timeseries.plot$common_name=="Neritic tunas"]=="Regional", "DWF", 
         timeseries.plot$Fleet[timeseries.plot$area_name=="Madagascar" & timeseries.plot$common_name=="Neritic tunas"])

timeseries.plot <- aggregate(tonnes~year+Fleet, data=timeseries.plot, FUN=sum)

ggplot(timeseries.plot, 
       aes(fill=Fleet, y=tonnes, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  ylab("Tonnes landed") +
  ggtitle("Neritic tuna landings (Madagascar)") +
  scale_fill_manual(values=c( "#C67D39", "#275663")) +
  theme_bw()

ggsave(here::here(output, "Mada_Fig3.pdf"), width = 7, height = 6)


# Figure 4. 
ggplot(aggregate(tonnes~year+fishing_entity, data=SAU_landings[SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Neritic tunas" &
                      !SAU_landings$Fleet=="Domestic",], FUN=sum), 
       aes(fill=fishing_entity, y=tonnes, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  ylab("Tonnes landed") +
  ggtitle("Landings by DWF and Regional fleets by flag country") +
  scale_fill_manual(values=c("skyblue4", "skyblue3", "skyblue2", "skyblue1", "skyblue", "lightblue1")) +
  theme_bw()

ggsave(here::here(output, "Mada_Fig4.pdf"), width = 7, height = 5)


# Figure 5. 
ggplot(aggregate(tonnes~fishing_sector+year, data=SAU_landings[SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Neritic tunas" &
                     SAU_landings$Fleet=="Domestic",], FUN=sum), 
       aes(fill=fishing_sector, y=tonnes, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  ylab("Tonnes landed") +
  ggtitle("Domestic neritic tuna landings (Madagascar)") +
  scale_fill_manual(values=c( "orange3", "brown3", "orange1")) +
  theme_bw()

ggsave(here::here(output, "Mada_Fig5.pdf"), width = 6, height = 6)


# Figure 6. 
ggplot(nutrient_bar_child[nutrient_bar_child$stock=="Neritic tunas",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (child, 1-3 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Mada_Fig6A.pdf"), width = 5.5, height = 6)

ggplot(nutrient_bar_F[nutrient_bar_F$stock=="Neritic tunas",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (female, 19-50 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Mada_Fig6B.pdf"), width = 5.5, height = 6)


# Figure 7. 
# CHILDREN (AGES 1-3)
# all nutrients except Selenium (Madagascar)
ggplot(results.children[!results.children$nutrient=="Selenium" & results.children$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  labs(title = "") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Mada_Fig7A-1.pdf"), width = 5, height = 5)


# Selenium (Madagascar)
ggplot(results.children[results.children$nutrient=="Selenium" & results.children$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of children ages 1-3 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() +
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Mada_Fig7A-2.pdf"), width = 5, height = 1.3)


# FEMALES OF REPRODUCTIVE AGE (19-50)
# all nutrients except Selenium (Madagascar)
ggplot(results.Freproductive[!results.Freproductive$nutrient=="Selenium" & results.Freproductive$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  labs(title = "") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Mada_Fig7B-1.pdf"), width = 5, height = 5)

# Selenium (Madagascar)
ggplot(results.Freproductive[results.Freproductive$nutrient=="Selenium" & results.Freproductive$stock=="Neritic tunas",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of females ages 19-50 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() +
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Mada_Fig7B-2.pdf"), width = 5, height = 1.3)



# Peru full-length case narrative ####

# Figure 1.
ggplot(aggregate(tonnes~reporting_status+year, data=SAU_landings[SAU_landings$area_name=="Peru" & SAU_landings$common_name=="Jumbo flying squid" &
                      SAU_landings$Fleet=="Domestic",], FUN=sum), 
       aes(fill=reporting_status, y=tonnes, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  ylab("Landings (tonnes)") +
  scale_fill_manual(values=c("orange3", "orange1")) +
  labs(fill='Reporting status') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=12))

ggsave(here::here(output, "Peru_Fig1.pdf"), width = 6, height = 6)

# Figure 2. 

# Data: https://comtradeplus.un.org/
peru_trade <- read.csv(here::here(directory, "comtrade_Peru_squid.csv"), sep=",", quote="", na.strings=c("NA", "", " "), header=TRUE)
peru_trade <- peru_trade[peru_trade$Trade.Flow=="Export",]
topexport <- aggregate(Netweight..kg.~Partner, data=peru_trade, FUN=sum)

# gives me the top 6 countries that Peruvian squid is exported to
ggplot(topexport[!topexport$Partner=="World" & topexport$Netweight..kg.>10000000,], aes(y=Netweight..kg., x=Partner)) + 
  geom_bar(position="stack", stat="identity", fill="#76B7B2") +
  xlab("") +
  ylab("Netweight (kg)") +
  labs(title = "Cephalopod exports: Peru (2018)",
       caption = "Data: UN Comtrade Database") +  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=16))

ggsave(here::here(output, "Peru_Fig2.pdf"), width = 7, height = 6)

# Figure 3.
ggplot(nutrient_bar_child[nutrient_bar_child$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (child, 1-3 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Peru_Fig3A.pdf"), width = 5.5, height = 6)

ggplot(nutrient_bar_F[nutrient_bar_F$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (female, 19-50 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Peru_Fig3B.pdf"), width = 5.5, height = 6)


# Figure 4. 
# CHILDREN (AGES 1-3)
# all nutrients except Selenium (Peru)
ggplot(results.children[!results.children$nutrient=="Selenium" & results.children$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  labs(title = "") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Peru_Fig4A-1.pdf"), width = 5, height = 5)


# Selenium (Peru)
ggplot(results.children[results.children$nutrient=="Selenium" & results.children$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of children ages 1-3 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() +
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Peru_Fig4A-2.pdf"), width = 5, height = 1.3)


# FEMALES OF REPRODUCTIVE AGE (19-50)
# all nutrients except Selenium (Peru)
ggplot(results.Freproductive[!results.Freproductive$nutrient=="Selenium" & results.Freproductive$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  labs(title = "") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Peru_Fig4B-1.pdf"), width = 5, height = 5)

# Selenium (Peru)
ggplot(results.Freproductive[results.Freproductive$nutrient=="Selenium" & results.Freproductive$stock=="Jumbo flying squid",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range=c(0.5, 1)) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of females ages 19-50 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() +
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Peru_Fig4B-2.pdf"), width = 5, height = 1.3)


# Philippines full-length case narrative ####

# Figure 1.
ggplot(roundscad_landings, 
       aes(color=Fleet, y=Landings, x=Year)) + 
  geom_line() +
  geom_point() +
  xlab("") +
  ylab("Metric tonnes") +
  ggtitle("Round scad landings (Philippines)") +
  scale_color_manual(values=c("orange3", "orange1")) +
  theme_bw()

ggsave(here::here(output, "Phil_Fig1.pdf"), width = 7, height = 4)


# Figure 2. 
taxa_roundscads <- read.csv(here::here(directory, "SAU_Taxa_galunggong_v50-1.csv"), sep=",", quote="", na.strings=c("NA", "", " "), header=TRUE)
taxa_roundscads <- taxa_roundscads[taxa_roundscads$year>2009,]
taxa_roundscads <- taxa_roundscads[taxa_roundscads$area_name=="Brunei Darussalam" |
                                     taxa_roundscads$area_name=="Taiwan" |
                                     taxa_roundscads$area_name=="Malaysia (Sabah)" |
                                     taxa_roundscads$area_name=="Malaysia (Sarawak)" |
                                     taxa_roundscads$area_name=="Philippines" |
                                     taxa_roundscads$area_name=="China" |
                                     taxa_roundscads$area_name=="Indonesia (Central)" |
                                     taxa_roundscads$area_name=="Viet Nam",]

taxa_roundscads$year <- as.integer(taxa_roundscads$year)
taxa_roundscads_agg <- aggregate(tonnes~year+fishing_entity, data=taxa_roundscads, FUN=sum)
taxa_roundscads_agg[nrow(taxa_roundscads_agg) + 1,] <- list(2018, "Brunei Darussalam", 0)

ggplot(taxa_roundscads_agg, 
       aes(fill=fishing_entity, y=tonnes, x=year)) + 
  geom_area(stat="identity", alpha=0.6 , linewidth=0.2, colour="black", aes(fill=fishing_entity)) + 
  scale_fill_viridis(discrete = T, name="Fishing country") +
  xlab("") +
  ylab("Tonnes landed") +
  ggtitle("Roundscad landings in South China Sea area") +
  scale_x_continuous(breaks = 2010:2019) +
  theme_bw()

ggsave(here::here(output, "Phil_Fig2.pdf"), width = 6, height = 5)


# Figure 3.
ggplot(galunggong_prices, aes(x=Year, y=mean)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
  xlab("") +
  ylab("Price (PhP) per kg") +
  ylim(0,200) +
  ggtitle("Round scad retail price 2012-2021") +
  theme_bw() +
  theme(text = element_text(size=14))

ggsave(here::here(output, "Phil_Fig3.pdf"), width = 6, height = 6)


# Figure 4. 
ggplot(nutrient_bar_child[nutrient_bar_child$stock=="Round scads (Decapterus)",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (child, 1-3 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Phil_Fig4A.pdf"), width = 5.5, height = 6)

ggplot(nutrient_bar_F[nutrient_bar_F$stock=="Round scads (Decapterus)",], 
       aes(x=nutrient, y=perc_of_daily, fill=nutrient)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Percentage of RNI (female, 19-50 yrs) \n fulfilled by an 100-g serving") +
  scale_fill_manual(values=c("#440154FF", "#414487FF", "#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  theme(legend.position="none")

ggsave(here::here(output, "Phil_Fig4B.pdf"), width = 5.5, height = 6)


# Figure 5.
# CHILDREN (AGES 1-3)
# all nutrients except Selenium (Philippines)
ggplot(phil.hypothetical.child[!phil.hypothetical.child$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  #labs(title = "Round scads / Philippines") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Phil_Fig5a-1.pdf"), width = 5, height = 5)

# Selenium (Philippines)
ggplot(phil.hypothetical.child[phil.hypothetical.child$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of children ages 1-3 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() + 
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Phil_Fig5a-2.pdf"), width = 5, height = 1.3)

# FEMALES, REPRODUCTIVE AGE
# all nutrients except Selenium (Philippines)
ggplot(phil.hypothetical.Freproductive[!phil.hypothetical.Freproductive$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("") +
  #labs(title = "Round scads / Philippines") +
  scale_fill_manual(values=c("#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF", "goldenrod1")) +
  theme_bw() +
  guides(fill=FALSE) +
  labs(alpha=NULL) +
  theme(strip.text.x = element_text(size = 16), legend.position = c(0.85, 0.1))

ggsave(here::here(output, "Phil_Fig5b-1.pdf"), width = 5, height = 5)

# Selenium (Philippines)
ggplot(phil.hypothetical.Freproductive[phil.hypothetical.Freproductive$nutrient=="Selenium",], 
       aes(x=nutrient, y=people20, fill=nutrient, alpha=fleet)) + 
  geom_bar(stat = "identity", color="grey40", size=0.3) +
  scale_alpha_discrete(#range=c(0.1, 0.5, 1.0),
    labels=c("Domestic","DWF (10%)", "DWF (30%)"),
    breaks=c("Domestic","DWF (10%)", "DWF (30%)")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3)) + # thousands
  coord_flip() + 
  xlab("") +
  ylab("Number of females ages 19-50 (thousands)") +
  scale_fill_manual(values="#440154FF") +
  theme_bw() + 
  guides(fill=FALSE, alpha=FALSE) +
  theme(strip.text.x = element_text(size = 16))

ggsave(here::here(output, "Phil_Fig5b-2.pdf"), width = 5, height = 1.3)

