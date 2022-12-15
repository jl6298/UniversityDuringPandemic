library('cowplot')
library("choroplethr")
library("dplyr")
library("forcats")
library("gridExtra")
library("ggplot2")
library("ggpubr")
library("ggridges")
library("ggmap")
library("openintro")
#remotes::install_github("jtr13/redav")
library("redav")
library("readr")
library("readxl")
library("tidyr")
library("tidyverse")
library("viridis")
library("HH")
library("jsonlite")
library("colorspace")

University_info <- data.frame(read_csv("./resources/data_folder/data_Yudu/University/University_info.csv"))

Student_aid<- data.frame(read_xlsx("./resources/data_folder/data_Yudu/Student_Aid/Student_Aid_2017-2021.xlsx"))
Student_aid <- Student_aid %>% rename(
  "UNITID" = "Unit.Id",
  "Aid_type" = "...3",
  "Data_representation" = "...4",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
Student_aid$UNITID <- as.character(Student_aid$UNITID)
Student_aid_inst <- Student_aid[Student_aid$Aid_type == "Institutional grants", ]
Student_aid_inst_grant_percent <- Student_aid_inst[Student_aid_inst$Data_representation == "Percent", ]
Student_aid_inst_grant_Amount <- Student_aid_inst[Student_aid_inst$Data_representation == "Average amount", ]

Student_aid_inst_grant_percent <- Student_aid_inst_grant_percent %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "Year", 
    names_transform = list(year = as.integer),
    values_to = "Percent",
    values_transform = list(num_nests = as.integer)
  )
University_info$UNITID <- as.character(University_info$UNITID)
Student_aid_inst_grant_percent <- inner_join(Student_aid_inst_grant_percent, University_info , by = c("UNITID"))
Student_aid_inst_grant_percent$Percent <- as.numeric(Student_aid_inst_grant_percent$Percent)
Student_aid_inst_grant_percent <- Student_aid_inst_grant_percent %>%
  mutate(Percent_grant = case_when(
    Student_aid_inst_grant_percent$Percent >= 0 & Student_aid_inst_grant_percent$Percent < 0.25 ~ "0% ~ 25%",
    Student_aid_inst_grant_percent$Percent >= 0.25 & Student_aid_inst_grant_percent$Percent < 0.5 ~ "25% ~ 50%",
    Student_aid_inst_grant_percent$Percent >= 0.5 & Student_aid_inst_grant_percent$Percent < 0.75 ~ "50% ~ 75%",
    Student_aid_inst_grant_percent$Percent >= 0.75 & Student_aid_inst_grant_percent$Percent <= 100 ~ "75% ~ 100%"
  ))


Student_aid_inst_grant_Amount <- Student_aid_inst_grant_Amount %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "Year", 
    names_transform = list(year = as.integer),
    values_to = "Amount",
    values_transform = list(num_nests = as.integer)
  )

Student_aid_inst_grant_Amount <- inner_join(Student_aid_inst_grant_Amount, University_info , by = c("UNITID"))
Student_aid_inst_grant_na <- Student_aid_inst_grant_Amount[Student_aid_inst_grant_Amount$Amount == "-", ]
Student_aid_inst_grant_Amount <- Student_aid_inst_grant_Amount[Student_aid_inst_grant_Amount$Amount != "-", ]
Student_aid_inst_grant_Amount$Amount <-as.numeric(Student_aid_inst_grant_Amount$Amount)


inst_grant_percent_count <- 
  Student_aid_inst_grant_percent %>% count(Year, Institution_type, Percent_grant)
inst_grant_percent_count$Percent_grant <- inst_grant_percent_count$Percent_grant %>% replace_na("Not provided")

inst_grant_percent_count_2017 <- inst_grant_percent_count[inst_grant_percent_count$Year == 2017, ]
bar_inst_grant_percentile_2017 <- 
  ggplot(inst_grant_percent_count_2017,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Barplot of percent grant to Institutions in 2017") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2018 <- inst_grant_percent_count[inst_grant_percent_count$Year == 2018, ]
bar_inst_grant_percentile_2018 <- 
  ggplot(inst_grant_percent_count_2018,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Barplot of percent grant to Institutions in 2018") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2019 <- inst_grant_percent_count[inst_grant_percent_count$Year == 2019, ]
bar_inst_grant_percentile_2019 <- 
  ggplot(inst_grant_percent_count_2019,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Barplot of percent grant to Institutions in 2018") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2020 <- inst_grant_percent_count[inst_grant_percent_count$Year == 2020, ]
bar_inst_grant_percentile_2020 <- 
  ggplot(inst_grant_percent_count_2020,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Barplot of percent grant to Institutions in 2020") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2021 <- inst_grant_percent_count[inst_grant_percent_count$Year == 2021, ]
bar_inst_grant_percentile_2021 <- 
  ggplot(inst_grant_percent_count_2021,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Barplot of percent grant to Institutions in 2021") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

total_plot_percent_grant <- ggarrange(bar_inst_grant_percentile_2017,
                                      bar_inst_grant_percentile_2018,
                                      bar_inst_grant_percentile_2019,
                                      bar_inst_grant_percentile_2020,
                                      bar_inst_grant_percentile_2021,
                                      nrow = 2, ncol = 3)


Student_aid_avg_state <- Student_aid_inst_grant_Amount[, c("Year", "State", "Amount")] %>%
  group_by(as.factor(Year), as.factor(State)) %>%
  summarise_at(vars(Amount), list (name = mean))

Student_aid_avg_state  <- Student_aid_avg_state   %>% rename(
  "Year" = "as.factor(Year)",
  "region" = "as.factor(State)",
  "value" = "name")
Student_aid_avg_state$region <- tolower(abbr2state(Student_aid_avg_state$region))

Student_aid_avg_state <- Student_aid_avg_state                           

Student_aid_avg_state_2017_2019 <- Student_aid_avg_state[Student_aid_avg_state$Year == 2017|Student_aid_avg_state$Year == 2019,  ]

Student_aid_avg_state_2019_2021 <- Student_aid_avg_state[Student_aid_avg_state$Year == 2019|Student_aid_avg_state$Year == 2021,  ]

Student_aid_avg_state_2017_2019 <- Student_aid_avg_state_2017_2019 %>%
  pivot_wider(names_from = Year, 
              values_from = value, 
              values_fill = 0)
Student_aid_avg_state_2017_2019 <- Student_aid_avg_state_2017_2019 %>%
  mutate(Amount_Change_USD = Student_aid_avg_state_2017_2019$`2019` - Student_aid_avg_state_2017_2019$`2017`)

Student_aid_avg_state_2019_2021 <- Student_aid_avg_state_2019_2021 %>%
  pivot_wider(names_from = Year, 
              values_from = value, 
              values_fill = 0)
Student_aid_avg_state_2019_2021 <- Student_aid_avg_state_2019_2021 %>%
  mutate(Amount_Change_USD = Student_aid_avg_state_2019_2021$`2021` - Student_aid_avg_state_2017_2019$`2019`)


statesMap <- map_data("state")

Student_aid_avg_state_2017_2019_Map <- merge(statesMap, Student_aid_avg_state_2017_2019, by = "region")

Grant_Map_2017_2019 <- ggplot(Student_aid_avg_state_2017_2019_Map,aes(x=long, y=lat, group=group,fill=Amount_Change_USD))+
  geom_polygon(color="black")+
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0) +
  ggtitle("Change of Insti grant 2017-2019")

Student_aid_avg_state_2019_2021_Map <- merge(statesMap, Student_aid_avg_state_2019_2021, by = "region")

Grant_Map_2019_2021 <-ggplot(Student_aid_avg_state_2019_2021_Map,aes(x=long, y=lat, group=group,fill=Amount_Change_USD))+
  geom_polygon(color="black")+
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0) +
  ggtitle("Change of Insti grant 2019-2021")

