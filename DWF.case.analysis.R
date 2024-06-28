# Analysis code for "Distant water fishing and its effects on domestic nutritional security"
# Rachel Zuercher, rzuercher@mbayaq.org
# Last edited: 27 June 2024

# load packages ####
library(data.table)
library(stringr) 
library(plyr) 
library(dplyr)
library(forcats)
library(ggpattern)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)
library(reshape2)
library(ggrepel)
library(ggalluvial)
#library(devtools)
#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

options(scipen=999) #prevent scientific notation  

# Load Sea Around Us catch reconstructions ####
directory <- here::here("data","raw")
output <- here::here("data","output")

SAUfiles = list.files(path=directory, pattern="*v50-1.csv", full.names=TRUE)
SAU_landings = ldply(SAUfiles, read.csv)
rm(SAUfiles)

# Sea Around Us data processing ####
# limit analysis to the years 2015-2019
SAU_landings <- SAU_landings[SAU_landings$year>2014,]

# re-replace '_' with ','
SAU_landings$common_name <- gsub('_', ',', SAU_landings$common_name)

# standardize country names (as fishing entity) with other project datasets
SAU_landings <- SAU_landings %>%
  mutate(., fishing_entity = ifelse(fishing_entity=="Korea (South)", "South Korea",
                                    fishing_entity))

# Exclude landings by fishing_entity="FISHING COUNTRY UNKNOWN"
SAU_landings <- SAU_landings[!SAU_landings$fishing_entity=="Unknown Fishing Country",]

# Create a 'Fleet' column to categorize landings as 'Domestic', 'Regional' or 'DWF'
SAU_landings$Fleet <- NA

peru.regional <- c("Brazil", "Colombia", "Ecuador", "Guyana", "Suriname", "Guyana and Suriname", "Trinidad and Tobago", "Chile")
madagascar.regional <- c("Comoros", "Kenya", "Mauritius", "Mozambique", "Réunion (France)", "Seychelles", "Somalia", "South Africa", "Tanzania")
philippines.regional <- c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Singapore", "Thailand", "Viet Nam")

SAU_landings$Fleet[SAU_landings$area_name=="Peru"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Peru"]=="Peru", "Domestic", "DWF")
SAU_landings$Fleet[SAU_landings$area_name=="Peru"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Peru"] %in% peru.regional, "Regional", SAU_landings$Fleet[SAU_landings$area_name=="Peru"])

SAU_landings$Fleet[SAU_landings$area_name=="Madagascar"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Madagascar"]=="Madagascar", "Domestic", "DWF")
SAU_landings$Fleet[SAU_landings$area_name=="Madagascar"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Madagascar"] %in% madagascar.regional, "Regional", SAU_landings$Fleet[SAU_landings$area_name=="Madagascar"])

SAU_landings$Fleet[SAU_landings$area_name=="Philippines"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Philippines"]=="Philippines", "Domestic", "DWF")
SAU_landings$Fleet[SAU_landings$area_name=="Philippines"] <- ifelse(SAU_landings$fishing_entity[SAU_landings$area_name=="Philippines"] %in% philippines.regional, "Regional", SAU_landings$Fleet[SAU_landings$area_name=="Philippines"])

# Dataset where neritic tuna species are not combined to 'neritic tunas'
madagascar_pre <- SAU_landings[SAU_landings$area_name=="Madagascar",]

# Combine neritic tunas for Madagascar
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Kawakawa", "Neritic tunas", SAU_landings$common_name)
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Longtail tuna", "Neritic tunas", SAU_landings$common_name)
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Bullet tuna", "Neritic tunas", SAU_landings$common_name)
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Frigate tuna", "Neritic tunas", SAU_landings$common_name)
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Bullet and frigate tunas", "Neritic tunas", SAU_landings$common_name)
SAU_landings$common_name <- ifelse(SAU_landings$area_name=="Madagascar" & SAU_landings$common_name=="Narrow-barred Spanish mackerel", "Neritic tunas", SAU_landings$common_name)

total_landings_by_country_by_spp <- aggregate(tonnes~area_name+common_name+year, data=SAU_landings, FUN=sum)
total_landings_by_country <- aggregate(tonnes~area_name+year, data=SAU_landings, FUN=sum)

# Average by species, fleet and year. Then calculate an average for 2015-2019
SAU_landings_summary <- aggregate(tonnes~area_name+common_name+Fleet+year, data=SAU_landings, FUN=sum)
SAU_landings_summary <- aggregate(tonnes~area_name+Fleet+common_name, data=SAU_landings_summary, FUN=mean)
SAU_landings_summary <- spread(SAU_landings_summary, key="Fleet", value="tonnes")

# Add a row for Philippines Roundscad (since this species isn't in the SAU database)
# Source: https://psa.gov.ph/sites/default/files/Fisheries%20Statistics%20of%20the%20Philippines%2C%202018-2020.pdf
# I apply the 'Municipal fishing' (fishing with vessel of 3 gross tons of less) landings to Domestic
# Commercial roundscad landings:   (Philippine Statistics Authority, 2021) #https://www.bfar.da.gov.ph/wp-content/uploads/2022/11/2021-Fisheries-Profile-FINAL-FILE.pdf
# Municipal roundscad landings: 43735.98 (Philippine Statistics Authority, 2021) #https://www.bfar.da.gov.ph/wp-content/uploads/2022/11/2021-Fisheries-Profile-FINAL-FILE.pdf
# DWF landings: Unknown
SAU_landings_summary[nrow(SAU_landings_summary) + 1,] = list(area_name="Philippines", common_name="Round scads (Decapterus)", Domestic=181516.46, DWF=NA, Regional=NA)

# edit column names
colnames(SAU_landings_summary)[1] <- "country"
colnames(SAU_landings_summary)[2] <- "stock"

# Make NAs --> zeros
SAU_landings_summary[is.na(SAU_landings_summary)] <- 0

countries <- unique(SAU_landings_summary$country)
stocks <- unique(SAU_landings_summary$stock)

# Conversions of total biomass landed to edible weight ####
edible_conversions <- read.csv(here::here(directory, "edible_conversions.csv"))
SAU_landings_summary <- join(SAU_landings_summary, edible_conversions, by="stock")

SAU_landings_summary$edible_conversion[is.na(SAU_landings_summary$edible_conversion)] <- 1

SAU_landings_summary$Domestic_edible <- SAU_landings_summary$Domestic*SAU_landings_summary$edible_conversion
SAU_landings_summary$DWF_edible <- SAU_landings_summary$DWF*SAU_landings_summary$edible_conversion
SAU_landings_summary$Regional_edible <- SAU_landings_summary$Regional*SAU_landings_summary$edible_conversion

rm(edible_conversions)

# Load nutritional content and recommended nutrient intake data ####
# Run the 'compile_fish_nutrition_df.R' script to create the following file:
nutritional_content <- readRDS(here::here(directory, "nutritional_content.rds"))

# World Health Organization Recommended Nutrient Intakes (RNI) #
# Source: https://www.who.int/activities/establishing-global-nutrient-requirements

# Data decisions made when putting together the below spreadsheet:
# 1. Using moderate bioavailability values for Zinc and 10% bioavailability (mid) for Iron (this is what Nash et al. 2022 used; also what Thilsted et al. 2016 used)
# 2. All other nutrients have RNIs for a 10-18 year bracket, but iron breaks down from 11-14 and 15-17 (and for females, 11-14 pre-menarche and 11-14 pre-menarche)
#    I average for M and F to keep the same 10-18 year bracket
# 3. For unspecified values for pregnant females (denoted as 'm' in the WHO book cited above or 'n' in the case of iron to denote that pregnant females are recommended to take iron supplements), 
#    values for females 19-50 are used

RNI <- read.csv(here::here(directory, "WHO_RNI.csv"))

# NASEM Recommended Dietary Allowances (RDAs)
# Institute of Medicine (2005) Dietary reference intakes for energy, carbohydrate, fiber, fat,
# fatty acids, cholesterol, protein, and amino acids (National Academy Press: Washington, DC, USA).
# URL: https://nap.nationalacademies.org/read/10490/chapter/32#1323
# Protein table on page 1324

RNI$Protein_g_day <- c(9.1,9.1,11.0,11.0,13,13,19,19,19,19,40,43,46,56,46,56,46,56,71,71,71,71,71,71) 
# age brackets in the tables did not match up perfectly, so some slight adjustments were made:
# protein values for ages 4-8 were used for both the RNI.RDA ages 3-6 and 7-9 age brackets
# protein values for age brackets 9-13 and 14-18 were averaged for the RNI.RDA ages 10-18 bracket
# protein values for age brackets 19-30 and 31-50 were averaged for the RNI.RDA ages 19-50 bracket

RNI$Omega3_PUFA_g_day <- 1.1 # this is the adequate daily intake value used in Nash et al. 2022 citing:
# Institute of Medicine (2005) Dietary reference intakes for energy, carbohydrate, fiber, fat,
# fatty acids, cholesterol, protein, and amino acids (National Academy Press: Washington, DC, USA).

# Create a dataframe for RNI for females of reproductive age
RNI_F_reproductive <- RNI[RNI$Sex=="F" & RNI$Age=="19-50 years",]
RNI_F_reproductive <- gather(RNI_F_reproductive, key=nutrient, value=value, Calcium_mg_day:Omega3_PUFA_g_day, na.rm=FALSE)
RNI_F_reproductive <- RNI_F_reproductive[,c(3:4)]

# Create a dataframe for RDI for children 
RNI_child <- RNI[RNI$Sex=="F" & RNI$Age=="1-3 years",] # M and F are same RNIs at this age
RNI_child <- gather(RNI_child, key=nutrient, value=value, Calcium_mg_day:Omega3_PUFA_g_day, na.rm=FALSE)
RNI_child <- RNI_child[,c(3:4)]

nutrient_bar <- nutritional_content[nutritional_content$stock=="Round scads (Decapterus)" |
                                      nutritional_content$stock=="Pacific chub mackerel" |
                                      nutritional_content$stock=="Largehead hairtail" |
                                      nutritional_content$stock=="Skipjack tuna" |
                                      nutritional_content$stock=="Jumbo flying squid" |
                                      nutritional_content$stock=="Longtail tuna" |
                                      nutritional_content$stock=="Bullet tuna" |
                                      nutritional_content$stock=="Frigate tuna" |
                                      nutritional_content$stock=="Kawakawa" |
                                      nutritional_content$stock=="Narrow-barred Spanish mackerel" |
                                      nutritional_content$stock=="Neritic tunas",]
nutrient_bar <- nutrient_bar[,c(6,9,12,15,18,21,24,27)]
nutrient_bar <- gather(nutrient_bar, key=nutrient, value=value, Calcium_mg_100g:Zinc_mg_100g, na.rm=FALSE)

# Modify nutrient names to match across data frames
nutrient_bar$nutrient <- fct_recode(nutrient_bar$nutrient,
                                    "Calcium" = "Calcium_mg_100g",
                                    "Iron" = "Iron_mg_100g",
                                    "Omega-3" = "Omega3_PUFA_g_100g",
                                    "Protein" = "Protein_g_100g",
                                    "Selenium" = "Selenium_mcg_100g",
                                    "Vitamin A" = "VitaminA_mcg_100g",
                                    "Zinc" = "Zinc_mg_100g")

RNI_F_reproductive$nutrient <- fct_recode(RNI_F_reproductive$nutrient,
                                          "Calcium" = "Calcium_mg_day",
                                          "Iron" = "Iron_10bioavailability_mg_day",
                                          "Omega-3" = "Omega3_PUFA_g_day",
                                          "Protein" = "Protein_g_day",
                                          "Selenium" = "Selenium_microg_day",
                                          "Vitamin A" = "VitaminA_microg_RE_day",
                                          "Zinc" = "Zinc_moderate_bioavailability_mg_day")


nutrient_bar_F <- merge(nutrient_bar, RNI_F_reproductive, by="nutrient") #nutrient bar value is in g/mg/mcg per 100g
nutrient_bar_F$perc_of_daily <- (nutrient_bar_F$value.x / nutrient_bar_F$value.y)*100 # this tells me the % of RDI that is fulfilled by an 100g serving
nutrient_bar_F$perc_of_daily <- ifelse(nutrient_bar_F$perc_of_daily>100, 100, nutrient_bar_F$perc_of_daily)

RNI_child$nutrient <- fct_recode(RNI_child$nutrient,
                                 "Calcium" = "Calcium_mg_day",
                                 "Iron" = "Iron_10bioavailability_mg_day",
                                 "Omega-3" = "Omega3_PUFA_g_day",
                                 "Protein" = "Protein_g_day",
                                 "Selenium" = "Selenium_microg_day",
                                 "Vitamin A" = "VitaminA_microg_RE_day",
                                 "Zinc" = "Zinc_moderate_bioavailability_mg_day")

nutrient_bar_child <- merge(nutrient_bar, RNI_child, by="nutrient") #nutrient bar value is in g/mg/mcg per 100g
nutrient_bar_child$perc_of_daily <- (nutrient_bar_child$value.x / nutrient_bar_child$value.y)*100 # this tells me the % of RDI that is fulfilled by an 100g serving
nutrient_bar_child$perc_of_daily <- ifelse(nutrient_bar_child$perc_of_daily>100, 100, nutrient_bar_child$perc_of_daily)

nutrient_bar_child$nutrient <- factor(nutrient_bar_child$nutrient, levels = c("Selenium", "Calcium", "Zinc", "Iron", "Vitamin A", "Protein", "Omega-3")) # reorder factor levels so that I can have selenium on the top to match the 'people's needs met' plot
nutrient_bar_F$nutrient <- factor(nutrient_bar_F$nutrient, levels = c("Selenium", "Calcium", "Zinc", "Iron", "Vitamin A", "Protein", "Omega-3")) # reorder factor levels so that I can have selenium on the top to match the 'people's needs met' plot

# calculate DWF potential contributions to nutrition ####

max.nutrition <- SAU_landings_summary[,c(1:6, 8:10)]
max.nutrition <- max.nutrition %>%
  filter((max.nutrition$country=="Philippines" & max.nutrition$stock=="Round scads (Decapterus)") |
           (max.nutrition$country=="Peru" & max.nutrition$stock=="Jumbo flying squid") |
           (max.nutrition$country=="Madagascar" & max.nutrition$stock=="Neritic tunas"))

nutrient.list <- c("Calcium_mg_100g", "Iron_mg_100g", "Omega3_PUFA_g_100g",
                   "Protein_g_100g", "Selenium_mcg_100g", "VitaminA_mcg_100g", "Zinc_mg_100g")
nutrient.shortnames <- sub("_.*", "", nutrient.list)
RNI.list <- c("Calcium_mg_day", "Iron_10bioavailability_mg_day", "Omega3_PUFA_g_day", "Protein_g_day", 
              "Selenium_microg_day", "VitaminA_microg_RE_day", "Zinc_moderate_bioavailability_mg_day")

column.names <- c("ppl.domestic", "ppl.DWF")
matrix.names <- nutrient.shortnames

max.nutrition$case <- paste(max.nutrition$country, max.nutrition$stock, sep="_")
case.list <- max.nutrition$case

column.names <- c("ppl.domestic", "ppl.DWF")
matrix.names <- nutrient.shortnames
results.children <- array(dim=c(nrow(max.nutrition),length(column.names),
                                length(nutrient.list)), dimnames=list(case.list, 
                                                                      column.names, matrix.names))
results.Freproductive <- array(dim=c(nrow(max.nutrition),length(column.names),
                                     length(nutrient.list)), dimnames=list(case.list, 
                                                                           column.names, matrix.names))

# For this analysis, combine regional landings with DWF landings
max.nutrition$DWF_edible <- max.nutrition$DWF_edible+max.nutrition$Regional_edible

for (i in 1:length(case.list)){
  
  country <- sub("_.*", "", rownames(results.Freproductive)[i])
  stock <- sub(".*_", "", rownames(results.Freproductive)[i])
  
  for (j in 1:length(nutrient.list)) {
    
    temp <- nutritional_content[nutritional_content$stock==stock, nutrient.list[j]] #mg or mcg/100g for nutrient j
    temp <- temp*10000 # converts to mg/ or mcg/tonne
    domestic.temp <- temp*max.nutrition$Domestic_edible[max.nutrition$country==country & 
                                                     max.nutrition$stock==stock]
    DWF.temp <- temp*max.nutrition$DWF_edible[max.nutrition$country==country & 
                                           max.nutrition$stock==stock]
    
    # calculate the necessary mg or mcg nutrient annually (rather than daily; *365)
    # for females 19-50
    results.Freproductive[case.list[i], 1, nutrient.shortnames[j]] <- domestic.temp/(RNI[RNI$Sex=="F" & RNI$Age=="19-50 years", RNI.list[j]] * 365)
    results.Freproductive[case.list[i], 2, nutrient.shortnames[j]] <- DWF.temp/(RNI[RNI$Sex=="F" & RNI$Age=="19-50 years", RNI.list[j]] * 365)
  }
} 

for (i in 1:length(case.list)){
  
  country <- sub("_.*", "", rownames(results.children)[i])
  stock <- sub(".*_", "", rownames(results.children)[i])
  
  for (j in 1:length(nutrient.list)) {
    
    temp <- nutritional_content[nutritional_content$stock==stock, nutrient.list[j]] #mg or mcg/100g for nutrient j
    temp <- temp*10000 # converts to mg/ or mcg/tonne
    domestic.temp <- temp*max.nutrition$Domestic_edible[max.nutrition$country==country & 
                                                     max.nutrition$stock==stock]
    DWF.temp <- temp*max.nutrition$DWF_edible[max.nutrition$country==country & 
                                           max.nutrition$stock==stock]
    
    # calculate the necessary mg or mcg nutrient annually (rather than daily; *365)
    # for children ages 1-3
    results.children[case.list[i], 1, nutrient.shortnames[j]] <- domestic.temp/(RNI[RNI$Sex=="F" & RNI$Age=="1-3 years", RNI.list[j]] * 365)
    results.children[case.list[i], 2, nutrient.shortnames[j]] <- DWF.temp/(RNI[RNI$Sex=="F" & RNI$Age=="1-3 years", RNI.list[j]] * 365)
  }
} 

# results matrix gives the number of people who's annual nutrient needs could be met
# by annual DWF or domestic fleet landings

# Create final tables
results.Freproductive <- data.frame(results.Freproductive[,,1:7])
results.Freproductive$country <- sub("_.*", "", rownames(results.Freproductive)) 
results.Freproductive$stock <- sub(".*_", "", rownames(results.Freproductive)) 
results.Freproductive <- reshape2::melt(results.Freproductive, id.vars=c("country", "stock"), variable.name="nutrient", value.name="people100")
results.Freproductive$nutrient <- str_replace(results.Freproductive$nutrient, "ppl.", "")
results.Freproductive$fleet <- NA
results.Freproductive$fleet <- ifelse(grepl("domestic", results.Freproductive$nutrient), "domestic", "DWF")
results.Freproductive$nutrient <- unlist(lapply(strsplit(results.Freproductive$nutrient, '.', fixed = TRUE), '[', 2))
results.Freproductive <- results.Freproductive[,c(1,2,5,3,4)]
results.Freproductive$people100 <- round(results.Freproductive$people100)
results.Freproductive$people20 <- results.Freproductive$people100*5 # the number of people for whom 20% of their nutrient needs could be met

results.children <- data.frame(results.children[,,1:7])
results.children$country <- sub("_.*", "", rownames(results.children)) 
results.children$stock <- sub(".*_", "", rownames(results.children)) 
results.children <- reshape2::melt(results.children, id.vars=c("country", "stock"), variable.name="nutrient", value.name="people100")
results.children$nutrient <- str_replace(results.children$nutrient, "ppl.", "")
results.children$fleet <- NA
results.children$fleet <- ifelse(grepl("domestic", results.children$nutrient), "domestic", "DWF")
results.children$nutrient <- unlist(lapply(strsplit(results.children$nutrient, '.', fixed = TRUE), '[', 2))
results.children <- results.children[,c(1,2,5,3,4)]
results.children$people100 <- round(results.children$people100)
results.children$people20 <- results.children$people100*5 # the number of people for whom 20% of their nutrient needs could be met

rm(i,j, RNI.list, stock, temp, country, domestic.temp, DWF.temp, column.names, matrix.names)

results.children$fleet <- factor(results.children$fleet, levels = c("DWF", "domestic")) # reorder factor levels so that domestic comes before DWF on barplot
results.children$stock <- factor(results.children$stock, levels = c("Jumbo flying squid", "Neritic tunas", "Round scads (Decapterus)", "Skipjack tuna")) # reorder factor levels so that domestic comes before DWF on barplot
results.children$nutrient <- factor(results.children$nutrient, levels = c("Selenium", "Calcium", "Zinc", "Iron", "VitaminA", "Protein", "Omega3")) # reorder factor levels so macronutrients come first, followed by micronutrients in order of importance

results.Freproductive$fleet <- factor(results.Freproductive$fleet, levels = c("DWF", "domestic")) # reorder factor levels so that domestic comes before DWF on barplot
results.Freproductive$stock <- factor(results.Freproductive$stock, levels = c("Jumbo flying squid", "Neritic tunas", "Round scads (Decapterus)", "Skipjack tuna")) # reorder factor levels so that domestic comes before DWF on barplot
results.Freproductive$nutrient <- factor(results.Freproductive$nutrient, levels = c("Selenium", "Calcium", "Zinc", "Iron", "VitaminA", "Protein", "Omega3")) # reorder factor levels so macronutrients come first, followed by micronutrients in order of importance

# Calculate hypothetical DWF round scad landings in Philiippines EEZ (10%, 30%)
phil.hypothetical.child <- results.children[results.children$country=="Philippines",]
phil.hypothetical.child$fleet <- ifelse(phil.hypothetical.child$fleet=="DWF", "DWF (10%)", "Domestic")

new.rows <- phil.hypothetical.child[phil.hypothetical.child$fleet=="DWF (10%)",]
new.rows$fleet <- "DWF (30%)"

phil.hypothetical.child <- rbind(phil.hypothetical.child, new.rows)

phil.hypothetical.child <- phil.hypothetical.child %>%
  group_by(nutrient) %>%
  mutate(people100=replace(people100, fleet=="DWF (10%)", people100[fleet=="Domestic"]*0.1)) %>%
  mutate(people100=replace(people100, fleet=="DWF (30%)", people100[fleet=="Domestic"]*0.3)) %>%
  mutate(people20=replace(people20, fleet=="DWF (10%)", people20[fleet=="Domestic"]*0.1)) %>%
  mutate(people20=replace(people20, fleet=="DWF (30%)", people20[fleet=="Domestic"]*0.3))

phil.hypothetical.child$fleet <- factor(phil.hypothetical.child$fleet, levels = c("DWF (30%)", "DWF (10%)", "Domestic"))

phil.hypothetical.Freproductive <- results.Freproductive[results.Freproductive$country=="Philippines",]
phil.hypothetical.Freproductive$fleet <- ifelse(phil.hypothetical.Freproductive$fleet=="DWF", "DWF (10%)", "Domestic")

phil.hypothetical.Freproductive <- rbind(phil.hypothetical.Freproductive, new.rows)
rm(new.rows)

phil.hypothetical.Freproductive <- phil.hypothetical.Freproductive %>%
  group_by(nutrient) %>%
  mutate(people100=replace(people100, fleet=="DWF (10%)", people100[fleet=="Domestic"]*0.1)) %>%
  mutate(people100=replace(people100, fleet=="DWF (30%)", people100[fleet=="Domestic"]*0.3)) %>%
  mutate(people20=replace(people20, fleet=="DWF (10%)", people20[fleet=="Domestic"]*0.1)) %>%
  mutate(people20=replace(people20, fleet=="DWF (30%)", people20[fleet=="Domestic"]*0.3))

phil.hypothetical.Freproductive$fleet <- factor(phil.hypothetical.Freproductive$fleet, levels = c("DWF (30%)", "DWF (10%)", "Domestic"))





