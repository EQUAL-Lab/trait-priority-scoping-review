# Literature Review on trait prioritization
# Script to replicate the figures contained in the paper
# For inquiries on the code, contact Martina Occelli (mo386@cornell.edu)

# Load the package
library(readxl)
library(ggplot2)
library(treemapify)
library(dplyr)
library(RColorBrewer)
library(sf)
library(raster)
library(spData)
library(tmap) 
library(webr)
library(tidyr)
library(writexl)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(stats)


# Load the working directory [Remember to change directory according to your local path]
h <- ("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL")
setwd(h)


# Load the dataset
lit_data <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!!Data_round1_round2_merged.xlsx")

# General information on the studies 
table(lit_data$peer_reviewed_journal) # how many are peer - reviewed?
median(lit_data$year, na.rm = T) # median year of publication
lit_data$lag_pub <- as.numeric(lit_data$lag_pub) 
mean(lit_data$lag_pub, na.rm=T) # average number of years lagged between data collection and publication

# ------------------------------------------------------------------------------
# Map the studies geographically 

u_1 <- lit_data %>%
  group_by(crop_1_cat, country_1) %>%
  count()

colnames(u_1)[1] ="crop_cat"
colnames(u_1)[2] ="country"

u_2 <- lit_data %>%
  group_by(crop_2_cat, country_1) %>%
  count()

colnames(u_2)[1] ="crop_cat"
colnames(u_2)[2] ="country"


u_3 <- lit_data %>%
  group_by(crop_1_cat, country_2) %>%
  count()

colnames(u_3)[1] ="crop_cat"
colnames(u_3)[2] ="country"

u <- merge(u_1, u_2, by = c("crop_cat", "country"), all.x = TRUE)
u_final <- merge(u, u_3, by = c("crop_cat", "country"), all.x = TRUE)

u_final$sum <- rowSums(u_final[ , c("n.x", "n.y", "n")], na.rm = T)

# Geographical distribution of the studies done using Google Sheet
# Link available here: https://docs.google.com/spreadsheets/d/17Ruolp2OvU4sktRKyTJYiydF--FjHtfZnaKlgubljDU/edit?usp=sharing
# To create the map, highlight the data of interest, then click insert and select type of chart = map

# ------------------------------------------------------------------------------
# Anlysis on tools and methods

# Summary of tools 

i <- lit_data %>%
  group_by(tool_1) %>%
  count

colnames(i)[1] ="tool"

i_2 <- lit_data %>%
  group_by(tool_2) %>%
  count

colnames(i_2)[1] ="tool"

i_3 <- lit_data %>%
  group_by(tool_3) %>%
  count

colnames(i_3)[1] ="tool"

i_4 <- lit_data %>%
  group_by(tool_4) %>%
  count

colnames(i_4)[1] ="tool"

i_5 <- lit_data %>%
  group_by(tool_5) %>%
  count

colnames(i_5)[1] ="tool"

i_6 <- lit_data %>%
  group_by(tool_6) %>%
  count

colnames(i_6)[1] ="tool"

i_7 <- lit_data %>%
  group_by(tool_7) %>%
  count

colnames(i_7)[1] ="tool"

i_8 <- lit_data %>%
  group_by(tool_8) %>%
  count

colnames(i_8)[1] ="tool"

t <- merge(i, i_2, by = c("tool"), all.x = TRUE, all.y = TRUE)
to <- merge(t, i_3, by = c("tool"), all.x = TRUE, all.y = TRUE)
too <- merge(to, i_4, by = c("tool"), all.x = TRUE, all.y = TRUE)
tool <- merge(too, i_5, by = c("tool"), all.x = TRUE, all.y = TRUE)
tool_n <- merge(tool, i_6, by = c("tool"), all.x = TRUE, all.y = TRUE)
tool_ne <- merge(tool_n, i_7, by = c("tool"), all.x = TRUE, all.y = TRUE)
tool_new <- merge(tool_ne, i_8, by = c("tool"), all.x = TRUE, all.y = TRUE)

tool_new$sum <- rowSums(tool_new[ , 2:9], na.rm = T)

write_xlsx(tool_new,"tool_new_second_round.xlsx") # export the file

# The file exported is included in the main dataset, sheet = "list tools"

# Summary of methods

m <- lit_data %>%
  group_by(method_1) %>%
  count

colnames(m)[1] ="method"

m_2 <- lit_data %>%
  group_by(method_2) %>%
  count

colnames(m_2)[1] ="method"

m_3 <- lit_data %>%
  group_by(method_3) %>%
  count

colnames(m_3)[1] ="method"

m_4 <- lit_data %>%
  group_by(method_4) %>%
  count

colnames(m_4)[1] ="method"

m_5 <- lit_data %>%
  group_by(method_5) %>%
  count

colnames(m_5)[1] ="method"

m_6 <- lit_data %>%
  group_by(method_6) %>%
  count

colnames(m_6)[1] ="method"

m_7 <- lit_data %>%
  group_by(method_7) %>%
  count

colnames(m_7)[1] ="method"

me <- merge(m, m_2, by = c("method"), all.x = TRUE, all.y = TRUE)
met <- merge(me, m_3, by = c("method"), all.x = TRUE, all.y = TRUE)
meth <- merge(met, m_4, by = c("method"), all.x = TRUE, all.y = TRUE)
method <- merge(meth, m_5, by = c("method"), all.x = TRUE, all.y = TRUE)
method_n <- merge(method, m_6, by = c("method"), all.x = TRUE, all.y = TRUE)
method_new <- merge(method_n, m_7, by = c("method"), all.x = TRUE, all.y = TRUE)

method_new$sum <- rowSums(method_new[ , 2:8], na.rm = T)

write_xlsx(method_new,"method_new_second_round.xlsx") # export the file

# The file exported is included in the main dataset, sheet = "list tools"

# Tools - PieDonut chart

tool <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!!Data_round1_round2_merged.xlsx", sheet = "list_tools")

tool <- tool[, c(1,2,4)]

tool <- tool %>% 
  rename("narrow_type" = `Narrow Tool Type`,
         "broad_type" = `Broad Tool Type`,
         "n" = `Number of studies_R2`)

PieDonut(tool, aes(broad_type, narrow_type, count = n), ratioByGroup = FALSE, r1 = 5, r2 = 6, 
         showRatioThreshold = F, addPieLabel = F,addDonutLabel = F,labelposition = 1, 
         start = pi/3, showRatioDonut = F, showRatioPie = T, showPieName = F, donutLabelSize = 7, pieLabelSize = 7) 

# Methods - PieDonut chart

method <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!!Data_round1_round2_merged.xlsx", sheet = "list_methods")

method <- method[, c(1, 2, 4)]

method <- method %>% 
  rename("narrow_type" = `Narrow Method Type`,
         "broad_type" = `Broad Method Type`,
         "n" = `Number of studies_R2`)

PieDonut(method, aes(broad_type, narrow_type, count = n), ratioByGroup = FALSE, r1 = 5, r2 = 6, 
         showRatioThreshold = F, addPieLabel = F,addDonutLabel = F,labelposition = 1, 
         start = pi/3, showRatioDonut = F, showRatioPie = T, showPieName = F, donutLabelSize = 9, pieLabelSize = 9)

# ------------------------------------------------------------------------------
# Anlysis on participatory and gender

table(lit_data$participatory_author, lit_data$crop_1_cat)

# Percentage of studies declaring participatory methods, by crop

lit_data %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "Other", "RTB"),
                               labels = c("Cereals", "Legumes", "Other", "Root, Tubers, Bananas") 
  )) %>%
  mutate(participatory_author = factor(participatory_author,
                                       levels = c("0", "1"),
                                       labels = c("No", "Yes")
  )) %>%
  filter(participatory_author != "NA") %>%
  filter(crop_1_cat != "NA") %>%
  ggplot(aes(x=participatory_author, y = (..count..)/sum(..count..), fill = participatory_author)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ crop_1_cat) +
  scale_fill_manual(values = c("#99d8c9", "#2ca25f")) +
  theme_classic() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Authors declare of using a participatory research design", y = "Count (%)") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14))

# Respondents (of type farmers) by year of study publication  and crop - time series chart

lit_data$resp_farmer <- as.numeric(lit_data$resp_farmer)
mean(lit_data$resp_farmer, na.rm = T)

str(lit_data$crop_1_cat)

lit_data %>%
  filter(crop_1_cat != "Other") %>%
  filter(crop_1_cat != "NA") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "RTB", "Vegetable"),
                               labels = c("Cereals", "Legumes", "Root, Tubers, Bananas", "Vegetables and Fruits") 
  )) %>%
  ggplot(aes(x=as.numeric(year))) + 
  geom_point(aes(y=resp_farmer), size = 2) +
  geom_hline(yintercept = mean(lit_data$resp_farmer, na.rm = T), color="blue") +
  #scale_color_manual(values = c("#00AFBB","#FC4E07", "#52854C", "#E7B800"), name = "Crop groups") +
  facet_wrap(~ crop_1_cat) +
  theme_classic() +
  labs(x = "Study publication year", y = "Farmers involved in the studies") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  theme(legend.position = "bottom") 

# Percentage of studies collecting sex-disaggregated data - pie chart / bar chart

u <- lit_data %>%
  group_by(crop_1_cat, sex_data) %>%
  count()

lit_data %>%
  group_by(crop_1_cat, sex_data) %>%
  summarise(mean = mean(as.numeric(resp_women_perc), na.rm = T))

str(lit_data$crop_1_cat)

lit_data %>%
  filter(sex_data ==1 ) %>%
  filter(crop_1_cat!= "NA") %>%
  filter(crop_1_cat!= "Other") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "RTB", "Vegetable"),
                               labels = c("Cereals", "Legumes", "RTB crops", "Vegetables and Fruits") 
  )) %>%
  ggplot(aes(x=crop_1_cat, y=as.numeric(resp_women_perc), fill = crop_1_cat)) +
  geom_boxplot()+
  theme_classic() +
  scale_fill_manual(values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe")) +
  labs(x = "Crop category", y = "Women participants (%)") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  theme(legend.position = "") 

f1

#Bar chart for sex-disaggregated studies

str(lit_data$sex_data)

lit_data %>%
  filter(crop_1_cat!= "NA") %>%
  filter(crop_1_cat!= "Other") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "RTB", "Vegetable"),
                               labels = c("Cereals", "Legumes", "RTB crops", "Vegetables and Fruits") 
  )) %>%
  mutate (sex_data = factor (sex_data,
                               levels = c(0,1),
                               labels = c("No", "Yes") 
  )) %>%
  ggplot(aes(x=as.factor(sex_data), y = (..count..)/sum(..count..)*100, fill = as.factor(crop_1_cat))) +
  geom_bar(position = "stack") +
  theme_classic() +
  #facet_wrap(~ crop_1_cat) +
  scale_fill_manual(values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe"), name = "Crop groups") +
  labs(x = "Sex-disaggregated sample", y = "Studies (%)") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  theme(legend.position = "bottom") 

lit_data %>%
  filter(crop_1_cat!= "NA") %>%
  filter(crop_1_cat!= "Other") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "RTB", "Vegetable"),
                               labels = c("Cereals", "Legumes", "RTB crops", "Vegetables and Fruits") 
  )) %>%
  mutate (sex_data = factor (sex_data,
                             levels = c(0,1),
                             labels = c("No", "Yes") 
  )) %>%
  ggplot(aes(x=crop_1_cat, y = (..count..)/sum(..count..)*100, fill = as.factor(sex_data))) +
  geom_bar(position = "stack") +
  theme_classic() +
  #facet_wrap(~ crop_1_cat) +
  scale_fill_manual(values = c("#7bccc4", "#2b8cbe"), name = "Sex-disaggregated sample") +
  labs(x = "", y = "Studies (%)") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 12)) +
  theme(legend.position = "bottom") 


#Bar chart for sex-disaggregated studies
pie_df <- data_frame(
  crop_category = c("Cereal", "Cereal", "Legumes", "Legumes", "RTB", "RTB", "Vegetables and Fruits", "Vegetables and Fruits"),
  sex = c(0,1,0,1,0,1,0,1),
  percentage = c(64, 36, 60, 40, 59, 41, 59, 41)
) 

pie_df %>%
  mutate (sex_disaggregated = factor (sex,
                                      levels = c("0", "1"),
                                      labels = c("No", "Yes")
  )) %>%
  ggplot(aes(x = crop_category, y = percentage, fill = sex_disaggregated)) +
  geom_col() +
  #coord_polar ("y") +
  scale_fill_manual(values = c("#7bccc4", "#2b8cbe"), name = "Sex-disaggregated sample") +
  theme_classic() +
  labs(y = "Percentage (%)", x = "Crops category") +
  theme(axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=12), axis.text.x = element_text(size=12),
        axis.text.y= element_text(size = 12)) +
  theme(legend.position = "bottom") 



# Percentage of respondents who are women by crop (boxplot)

lit_data$resp_women_perc <- as.numeric(lit_data$resp_women_perc)

box <- lit_data %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                          levels = c("cereal", "legumes", "Vegetable", "RTB"),
                          labels = c("Cereals", "Legumes", "Vegetable", "Root, Tubers, Bananas") 
  )) %>%
  filter(crop_1_cat != "NA") %>%
  #filter(geo_area_1 == "SSA" | geo_area_1 == "CSA") %>%
  ggplot(aes(x=crop_1_cat, y=resp_women_perc, fill = crop_1_cat)) +
  geom_boxplot() +
  #facet_grid(~ geo_area_1) +
  scale_fill_manual(values=c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe")) +
  theme_classic() +
  theme(legend.position="") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  labs(x = "Crop category", y = "Respondents involved in the studies who are women (%)") 

boxplot(lit_data$resp_women_perc)$stats

table(lit_data$sex_data)
summary(lit_data$resp_women_perc)

# ------------------------------------------------------------------------------
# Longitudinal analysis on traits

# Trait rank (for the subset of data where rank of trait is available ~ 25%)
rank_1 <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!!Data_round1_round2_merged.xlsx", sheet = "rank_trait_crop_Round2")

rank_1$p_id <- factor(rank_1$p_id)
rank_long <- gather(rank_1, rank, trait, crop1_overall_trait_1:crop1_overall_trait_19, factor_key=T) # wide to long format

str(rank_long$rank)

# Re-factor all the levels to make them appears as ranks 

levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_1"] <- "1"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_2"] <- "2"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_3"] <- "3"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_4"] <- "4"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_5"] <- "5"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_6"] <- "6"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_7"] <- "7"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_8"] <- "8"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_9"] <- "9"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_10"] <- "10"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_11"] <- "11"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_12"] <- "12"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_13"] <- "13"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_14"] <- "14"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_15"] <- "15"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_16"] <- "16"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_17"] <- "17"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_18"] <- "18"
levels(rank_long$rank)[levels(rank_long$rank)=="crop1_overall_trait_19"] <- "19"

rank_long <- rank_long[order(rank_long$p_id),] # order based on p_id

write_xlsx(rank_long,"rank_long.xlsx") # save and merge in the master excel file

rank_long <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!!Data_round1_round2_merged.xlsx", sheet = "rank_trait_R_Round2")

rank_long$rank <- as.numeric(rank_long$rank)

rank_filtered <- rank_long %>%
  filter(trait == "High yield" | trait == "Drought tolerance" |
           trait == "Market demand" | trait == "Pest disease resistance" | trait == "Color" | trait == "Taste" | trait == "Early maturity")

str(rank_filtered$trait)

rank_filtered %>%
  mutate(Trait = factor(trait)) %>%
  filter(crop_1_cat != "NA") %>%
  filter(crop_1_cat != "Other") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "Vegetable", "RTB"),
                               labels = c("Cereals", "Legumes", "Vegetable", "RTB") 
  )) %>%
  ggplot(aes(x = year, y = rank, group = Trait)) +
  #geom_line(aes(color = trait), size = 1) +
  geom_point(aes(color = Trait), size = 4) +
  scale_y_reverse(breaks = 1:nrow(rank_filtered)) +
  scale_color_manual(values = c("#8c510a", "#4d9221", "#af8dc3", "#d73027","#2166ac", "#01665e", "#c7eae5")) +
  facet_wrap(~ crop_1_cat) +
  theme_classic()+
  labs(x = "Year", y = "Trait rank") +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  theme(legend.position = "bottom")


lit_data %>%
  group_by(crop_1_cat) %>%
  summarise(mean = mean(as.numeric(resp_women_perc), na.rm=T)) 


# ------------------------------------------------------------------------------
# Network analysis

library(readr)

# Authors - cleaning of the data done in R, network representation done in VOSviewer
network_authors <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!Network_data_extracted.xlsx", sheet = "network_authors")
network_authors <- network_authors[, c(1, 3, 4, 5)]

colnames(network_authors)[4] = "Authors"
colnames(network_authors)[3] = "Year"

write.csv(network_authors, "network_semi.csv", row.names = F) # overall 

Final_Authors_Round2 <- read_excel("Priority Setting/PS Literature Review/Rscript/Network files_Round2/Final_Authors_Round2.xlsx")
colnames(Final_Authors_Round2)[2] = "Authors"
colnames(Fina)[1] = "Year"

write.csv(Final_Authors_Round2, "network_authors_2.csv", row.names = F) # overall 


# Across time

k <- Final_Authors_Round2 %>%
  filter(Year <= 2000) 

j <- Final_Authors_Round2 %>%
  filter(Year > 2000 & Year <= 2010)  

w <- Final_Authors_Round2 %>%
  filter(Year > 2010) 

write.csv(k, "network_authors_2000.csv", row.names = F) # before 2000
write.csv(j, "network_authors_2010.csv", row.names = F) # before 2010
write.csv(w, "network_authors_2020.csv", row.names = F) # before 2020

# By crops

k <- network_authors %>%
  filter(crop == "cereal") 

j <- network_authors %>%
  filter(crop == "RTB")  

w <- network_authors %>%
  filter(crop == "legumes") 

z <- network_authors %>%
  filter(crop == "Other")

write.csv(k, "network_authors_cereal.csv", row.names = F) # cereal
write.csv(j, "network_authors_RTB.csv", row.names = F) # RTB
write.csv(w, "network_authors_legumes.csv", row.names = F) # legumes
write.csv(z, "netwrok_authors_other.csv", row.names = F) # other

# donors

network_donor <- read_excel("Priority Setting/PS Literature Review/Rscript/Donors_round2-CLEAN.xlsx")

colnames(network_donor)[3] = "Authors"
colnames(network_donor)[2] = "Year"

write.csv(network_donor, "network_donor_r2.csv", row.names = F) # overall 

donors <- read_csv("~/Desktop/donors.csv")

colnames(donors)[4] = "Authors"
colnames(donors)[3] = "Year"

write.csv(donors, "network_donor_gio.csv", row.names = F) # overall 

# Across time

k <- network_donor %>%
  filter(Year <= 2000) 

j <- network_donor %>%
  filter(Year > 2000 & Year <= 2010)  

w <- network_donor %>%
  filter(Year > 2010) 

write.csv(k, "network_donor_2000.csv", row.names = F) # before 2000
write.csv(j, "network_donor_2010.csv", row.names = F) # before 2010
write.csv(w, "network_donor_2020.csv", row.names = F) # before 2020

# By crops

k <- network_donor %>%
  filter(crop == "cereal") 

j <- network_donor %>%
  filter(crop == "RTB")  

w <- network_donor %>%
  filter(crop == "legumes") 

z <- network_donor %>%
  filter(crop == "Other")

write.csv(k, "network_donor_cereal.csv", row.names = F) # cereal
write.csv(j, "network_donor_RTB.csv", row.names = F) # RTB
write.csv(w, "network_donor_legumes.csv", row.names = F) # legumes
write.csv(z, "netwrok_donor_other.csv", row.names = F) # other


# Institutions

network_institutions <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!Network_data_extracted.xlsx", sheet = "network_affiliations")
network_institutions <- network_institutions[, c(1, 3, 4, 5)]

colnames(network_institutions)[4] = "Authors"
colnames(network_institutions)[3] = "Year"

write.csv(network_institutions, "network_institutions_semi.csv", row.names = F) # overall 

# ------------------------------------------------------------------------------
# Extra analysis done while reviewing the data, but not published in the manuscript

#  Percentage of respondents who are women and participatory nature of the study by crop (tree map)

m <- lit_data %>%
  group_by(crop_1_cat, geo_area_1) %>%
  count(participatory_author)


b <- lit_data %>%
  group_by(crop_1_cat, geo_area_1) %>%
  summarise(mean = mean(resp_women_perc, na.rm=T))

b$mean <- round(b$mean, digits = 1)

m %>%
  filter(crop_1_cat != "NA") %>%
  filter(participatory_author == 1) %>%
  filter(geo_area_1 == "SSA" | geo_area_1 == "SEA" | geo_area_1 == "CSA") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "Other", "RTB"),
                               labels = c("Cereals", "Legumes", "Other", "Root, Tubers, Bananas") 
  )) %>%
  ggplot(aes(area = n, fill = crop_1_cat, label = paste(crop_1_cat, n, sep = "\n"))) +
  geom_treemap() +
  scale_fill_manual(values = c("#00AFBB","#FC4E07","#E7B800", "#52854C", "#9ebcda")) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = F, size = 16) +
  facet_grid(~ geo_area_1) +
  theme(legend.position = "none")

b %>%
  filter(crop_1_cat != "NA") %>%
  filter(geo_area_1 == "SSA" | geo_area_1 == "CSA") %>%
  mutate (crop_1_cat = factor (crop_1_cat,
                               levels = c("cereal", "legumes", "Other", "RTB"),
                               labels = c("Cereals", "Legumes", "Other", "Root, Tubers, Bananas") 
  )) %>%
  ggplot(aes(area = mean, fill = crop_1_cat, label = paste(crop_1_cat, mean, sep = "\n"))) +
  geom_treemap() +
  scale_fill_manual(values = c("#00AFBB","#FC4E07","#E7B800", "#52854C", "#9ebcda")) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = F, size = 16) +
  facet_grid(~ geo_area_1) +
  theme(legend.position = "none")

# WorldCloud - objectives

quote <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!Data_extracted.xlsx", sheet = "quotes_objective")
text <- quote$objective #Create a vector containing only the text
docs <- Corpus(VectorSource(text)) # Create a corpus  

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c("objective", "paper", "objectives", "determine", "study", "new", "research", "using", "present")) 
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud(words = df$word, freq = df$freq, min.freq = 10,          
          max.words=200, random.order=FALSE, rot.per=0.35,           
          colors=brewer.pal(8, "Paired"))

# WorldCloud - knowledge

quote <- read_excel("Priority Setting/PS Literature Review/Final repo - papers for data extraction /!Data_extracted.xlsx", sheet = "quotes_knowledge")
text <- quote$knowledge_quote #Create a vector containing only the text
docs <- Corpus(VectorSource(text)) # Create a corpus  

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c("knowledge", "research", "study", "new", "can", "also")) 
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud(words = df$word, freq = df$freq, min.freq = 10,          
          max.words=200, random.order=FALSE, rot.per=0.35,           
          colors=brewer.pal(8, "Paired"))


