
# loading necessary packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(maps)

# loading CSVs

terrorism_data <- read.csv(file = "globalterrorismdb_0919dist.csv") 

arms_import <- read.csv(file = "TIV-Import-All-1950-2018.csv") 

arms_export <- read.csv(file="TIV-Export-All-1950-2018.csv")

#View(terrorism_data)

# summary analysis for Import/Export
#Wrangeling Import

arms_import_wr <- arms_import %>% .[-(1:10),-(2:50)] 

colnames(arms_import_wr) <- c("Country","2000","2001","2002","2003","2004","2005","2006","2007",
                              "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","Sum","NA")

arms_summary_table <- arms_import_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% group_by(Country) %>% 
                 summarise("Total_Arms_Imports_2000_2018_Millions"=sum(value,na.rm = TRUE),"Mean_Arms_Imports"=mean(value)) %>% 
                 arrange(-Total_Arms_Imports_2000_2018_Millions) %>%head(11)
                 
arms_import_years <-arms_import_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% group_by(Year) %>% 
                    summarise("Total_Arms_Imports_2000_2018_Millions"=sum(value,na.rm = TRUE),"Median_Arms_Imports"=median(value,na.rm = TRUE))

non_state_import <- arms_import_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% filter(endsWith(as.character(Country),suffix = "*")) %>%
                    group_by(Country) %>% 
                    summarise("Total_arms_imports_2000_2018_Millions"= sum(value,na.rm = TRUE),"Mean_arms_imports_Millions"=mean(value,na.rm = TRUE)) %>%
                    arrange(-Total_arms_imports_2000_2018_Millions) %>% head(11)
                    

#Wrangeling Export

arms_export_wr <- arms_export %>% .[-(1:10),-(2:50)] 

colnames(arms_export_wr) <- c("Country","2000","2001","2002","2003","2004","2005","2006","2007",
                              "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","Sum","NA")

arms_summary_table_export <- arms_export_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% group_by(Country) %>% 
  summarise("Total_Arms_Exports_2000_2018_Millions"=sum(value,na.rm = TRUE),"Mean_Arms_Exports"=mean(value)) %>% 
  arrange(-Total_Arms_Exports_2000_2018_Millions) %>%head(11)

arms_export_years <-arms_export_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% group_by(Year) %>% 
  summarise("Total_Arms_Exports_2000_2018_Millions"=sum(value,na.rm = TRUE),"Median_Arms_Imports"=median(value,na.rm = TRUE))

# Terrorism summary

terrorism_data %>%
  select(iyear, country_txt, region_txt, success, attacktype1_txt, targtype1_txt, targtype1_txt, gname, weaptype1_txt, nkill, nwound) -> terrorism_df

terrorism_df %>%
  mutate(iso3c = iso.alpha(country_txt, n = 3)) -> terrorism_df

terrorism_df[terrorism_df$country_txt == "United States", "iso3c"] <- c("USA")

terrorism_df %>%
  group_by(country_txt, region_txt, iso3c) %>%
  select(country_txt, region_txt, iso3c) %>%
  count(country_txt) %>%
  arrange(-n) -> counts_country

counts_country %>%
  head(1) -> top_country_attacks

counts_country %>%
  tail(1) -> bottom_country_attacks

top_country_attacks$n

terrorism_df %>%
  group_by(iyear) %>%
  select(iyear, success, nkill, nwound) %>%
  summarise(mean_success = mean(success, na.rm = TRUE), 
    mean_nkill = mean(nkill, na.rm = TRUE), 
    mean_nwound = mean(nwound, na.rm = TRUE), 
    max_nkill = max(nkill, na.rm = TRUE), 
    max_nwound = max(nwound, na.rm = TRUE)) -> summary_stats_year

terrorism_df %>%
  group_by(iyear) %>%
  select(iyear) %>%
  count(iyear) -> counts_iyear

counts_iyear

iyear_summary <- left_join(counts_iyear, summary_stats_year, by = "iyear") %>%
  head(30)

iyear_summary

terrorism_df %>%
  select(attacktype1_txt) %>%
  count(attacktype1_txt) %>%
  arrange(-n) %>%
  head(10) -> counts_attacktype

counts_attacktype  

terrorism_df %>%
  select(targtype1_txt) %>%
  count(targtype1_txt) %>%
  arrange(-n) %>%
  head(10) -> counts_targtype
 
counts_targtype

terrorism_df %>%
  select(weaptype1_txt) %>%
  count(weaptype1_txt) %>%
  arrange(-n) %>%
  head(10) -> counts_weaptype

counts_weaptype

terrorism_df %>%
  group_by(gname) %>%
  select(gname, success, nkill, nwound) %>%
  summarise(mean_success = mean(success, na.rm = TRUE), 
            mean_nkill = mean(nkill, na.rm = TRUE), 
            mean_nwound = mean(nwound, na.rm = TRUE), 
            max_nkill = max(nkill, na.rm = TRUE), 
            max_nwound = max(nwound, na.rm = TRUE)) -> summary_stats_gname

summary_stats_gname

terrorism_df %>%
  group_by(gname) %>%
  select(gname) %>%
  count(gname) %>%
  arrange(-n) -> counts_gname

counts_gname

gname_summary <- left_join(counts_gname, summary_stats_gname, by = "gname") %>%
  head(30)

gname_summary

# Summary analysis: GTDB trends

success_rate_time_plot <- ggplot(data = iyear_summary, aes(x = iyear, y = n)) +
  geom_bar(stat = "identity", alpha = 0.75, fill = "red", color = "white") +
  geom_point(data = iyear_summary, aes(x = iyear, y = mean_success*max(iyear_summary$n)), color = "red") +
  geom_line(data = iyear_summary, aes(x = iyear, y = mean_success*max(iyear_summary$n))) +
  scale_y_continuous(sec.axis = sec_axis(~./max(iyear_summary$n))) +
  labs(title = "Number of Terrorist Attacks and Average Success Rate of Attacks Over Time", x = "Year", y = "Number of Attacks") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90))
  
success_rate_time_plot

# Number of imports over time
arms_import_years_updated <-arms_import_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% group_by(Year) %>% 
  summarise("Total_Arms_Imports_Millions"=sum(value,na.rm = TRUE),"Median_Arms_Imports"=median(value,na.rm = TRUE))

arms_import_to_2018 <- arms_import_years_updated[-c(20), ]

arms_import_and_attacks <- arms_import_to_2018 %>% 
  mutate(num_attacks = iyear_summary$n)

imports_over_time_plot <- ggplot(data = arms_import_and_attacks) +
  geom_bar(mapping = aes(x = Year, y = Total_Arms_Imports_Millions), stat = "identity", alpha = 0.75, fill = "darkblue", color = "white") +
  labs(
    title = "Total Arms Imports Over Time",
    subtitle = "From 2000 - 2018",
    x = "Year",
    y = "Total Arms Imports (Millions)"
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 90))

# Number of attacks over time

attacks_over_time_plot <- ggplot(data = arms_import_and_attacks) +
  geom_bar(mapping = aes(x = Year, y = num_attacks), stat = "identity", alpha = 0.75, fill = "red", color = "white") +
  labs(
    title = "Number of Attacks Over Time",
    subtitle = "From 2000 - 2018",
    x = "Year",
    y = "Number of Attacks"
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 90))

# Specific Analysis question 1: Matthew 

arms_data_analysis <- arms_import_wr %>% select(1:21) %>% gather(key = Year, value=value,-Country) %>% filter(endsWith(as.character(Country),suffix = "*")) %>%
                      filter(endsWith(as.character(Country),")*")) %>% group_by(Year) %>% summarize("sum_of_arms_imports_to_insurgents"=sum(value,na.rm = TRUE)) 

arms_data_analysis$Year <- as.character(arms_data_analysis$Year)

terrorism_data_analysis <- terrorism_df %>% group_by(iyear) %>% summarise("Num_Casualties_Terrorism"=sum(nkill,na.rm = TRUE)) %>% 
                           rename(Year=iyear) 
terrorism_data_analysis$Year <- as.character(terrorism_data_analysis$Year)

terrorism_arms_data <- left_join(arms_data_analysis,terrorism_data_analysis,"Year") %>% filter(Year != "2019")


terrorism_arms_plot <- ggplot(data = terrorism_arms_data) +
                      geom_bar(mapping = aes(x=as.numeric(Year),y=Num_Casualties_Terrorism),stat = "identity",fill="red") +
                       geom_point(data = terrorism_arms_data,aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(terrorism_arms_data$sum_of_arms_imports_to_insurgents))) +
                       geom_line(data =terrorism_arms_data, aes(x=as.numeric(Year),y=sum_of_arms_imports_to_insurgents*max(terrorism_arms_data$sum_of_arms_imports_to_insurgents))) +
                        scale_y_continuous(sec.axis = sec_axis(~./max(terrorism_arms_data$sum_of_arms_imports_to_insurgents),name="Value of arms Sales($Millions)")) + 
                     labs(title = "Arms Sales to non-State forces and Casualties from Terrorsim",x="Year",y="Number of Casualties")
                        
                    

# Specific Analysis question 2: Matthew 

attack_data <- terrorism_df %>% select(gname,weaptype1_txt,targtype1_txt) %>% group_by(gname) %>%
               summarise("Biological"= sum(as.character(weaptype1_txt)=="Biological",na.rm = TRUE),"Chemical"= sum(as.character(weaptype1_txt)=="Chemical",na.rm = TRUE),
                "Explosives"= sum(as.character(weaptype1_txt)=="Explosives",na.rm = TRUE),"Fake Weapons"= sum(as.character(weaptype1_txt)=="Fake Weapons",na.rm = TRUE),
                "Firearms"= sum(as.character(weaptype1_txt)=="Firearms",na.rm = TRUE),"Incendiary"= sum(as.character(weaptype1_txt)=="Incendiary",na.rm = TRUE),
                 "Melee"= sum(as.character(weaptype1_txt)=="Melee",na.rm = TRUE),"Other"= sum(as.character(weaptype1_txt)=="Other",na.rm = TRUE),
                "Radiological"= sum(as.character(weaptype1_txt)=="Radiological",na.rm = TRUE),"Sabotage Equipment"= sum(as.character(weaptype1_txt)=="Sabotage Equipment",na.rm = TRUE),
                "Unknown"= sum(as.character(weaptype1_txt)=="Unknown",na.rm = TRUE),"Vehicle-borne explosive"= sum(as.character(weaptype1_txt)=="Biological",na.rm = TRUE)) %>% 
                 mutate("sum_attacks"= rowSums(.[,c(2:13)])) %>% arrange(-sum_attacks) %>% head(11) %>% filter(gname != "Unknown")

gathered_attack_data <- attack_data %>% select(-sum_attacks) %>% gather(key = attack_type,value=value,-gname) 


attack_plot <- ggplot(data = gathered_attack_data) + 
               geom_bar(mapping = aes(x=reorder(gname,value),y=value,fill=attack_type),position = "stack",stat = "identity") +
               labs(title = "Do Terror Groups Differ in Their Strategies?",y="Number of Attacks",x="Group Name",fill="attack type") +
                scale_fill_brewer(palette ="Set3") + 
                theme(axis.text.x = element_text(size = 7,angle = 25))

# Specific Analysis question 1: Arnon

arms_import_to_2018_mean_success <- arms_import_to_2018 %>% 
  mutate(mean_success = iyear_summary$mean_success)

# Total Number of Imports and Average Success Rate of Attacks Over Time
success_rate_number_of_imports_plot <- ggplot(data = arms_import_to_2018_mean_success , aes(x = Year, y = Total_Arms_Imports_Millions)) +
  geom_bar(stat = "identity", alpha = 0.75, fill = "orange", color = "white") +
  geom_point(data = arms_import_to_2018_mean_success, aes(x = Year, y = mean_success*max(arms_import_to_2018_mean_success$Total_Arms_Imports_Millions))) +
  geom_line(data = arms_import_to_2018_mean_success, aes(x = Year, y = mean_success*max(arms_import_to_2018_mean_success$Total_Arms_Imports_Millions), group = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~./max(arms_import_to_2018_mean_success$Total_Arms_Imports_Millions))) +
  labs(title = "Total Number of Imports and Average Success Rate of Attacks Over Time from 2000 - 2018: ",
       subtitle = "Key: Black = Success Rate, Orange = Number of Imports",
       x = "Year",
       y = "Number of Imports (Millions)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(size = 10, angle = 90))

# Specific Analysis question 4: Jonathan

terrorism_df %>%
  group_by(country_txt, region_txt, iso3c) %>%
  select(country_txt, region_txt, iso3c) %>%
  count(country_txt) %>%
  mutate(labels = cut(n, breaks = c(-Inf, 10, 50, 100, 300, 500, 1000, 5000, 10000, Inf), labels = c("< 10", "10 to 50", "50 to 100", "100 to 300", "300 to 500", "500 to 1000", "1000 to 5000", "5000 to 10000", "> 10000"))) %>%
  arrange(-n) -> num_attacks_map_data

map_data("world") %>%
  mutate(iso3c = iso.alpha(region, n = 3)) -> map_data

map_terrorist_attacks_data <- left_join(map_data, num_attacks_map_data, by = "iso3c") 

world_terrorist_attacks_plot <- ggplot(map_terrorist_attacks_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = labels)) +
  coord_quickmap() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Number of Terrorist Attacks", fill = "Attacks") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

world_terrorist_attacks_plot



