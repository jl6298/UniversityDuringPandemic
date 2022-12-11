library("readxl")
library("dplyr")
library("tidyr")
library("readr")
#library("ggplot2")
library("ggridges")
#install.packages("viridis")
library("viridis")
library("forcats")
#install.packages("ggpubr")
library("ggpubr")
#install.packages('cowplot')
library('cowplot')
library(gridExtra)
library(ggplot2)
library(ggpubr)
#install.packages('choroplethr')
library("choroplethr")
library("openintro")

#install.packages('choroplethrMaps')
library(choroplethrMaps)
University_type <- data.frame(read_csv("./data_folder/CollegeScorecard_Raw_Data/Most-Recent-Cohorts-Field-of-Study.csv"))
University_type <- unique(University_type[c("UNITID","CONTROL")])
str(University_type)

University_loc <- data.frame(read_csv("./data_folder/CollegeScorecard_Raw_Data/Most-Recent-Cohorts-Institution.csv"))
University_loc <- unique(University_loc[c("UNITID","STABBR")])
str(University_loc)
University_loc$UNITID <- as.character(University_loc$UNITID)

University_info <- inner_join(University_type, University_loc, by = c("UNITID"))

University_info <- University_info %>% rename(
  "Institution_type" = "CONTROL", 
  "State" = "STABBR")

in_state_tuition<- data.frame(read_xlsx("./data_folder/Tuition_Data/Pricetrend_in_state.xlsx"))
in_state_tuition <- in_state_tuition %>% rename(
   "UNITID" = "Unit.Id",
   "Type_of_Pay" = "Type.of.cost...residency..and.student.housing",
                                        "2021" = "X2021",
                                        "2020" = "X2020",
                                        "2019" = "X2019", 
                                        "2018" = "X2018",
                                        "2017" = "X2017")
in_state_tuition <- in_state_tuition %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "amount",
    values_transform = list(num_nests = as.integer)
  )
in_state_tuition <- in_state_tuition[in_state_tuition$Type_of_Pay == "On-campus in-state", ]
in_state_na <- in_state_tuition[is.na(in_state_tuition$amount), ]
in_state_tuition <- in_state_tuition %>% drop_na(amount)
in_state_tuition <- inner_join(in_state_tuition, University_info , by = c("UNITID"))



out_state_tuition<- data.frame(read_xlsx("./data_folder/Tuition_Data/Pricetrend_out_state.xlsx"))
out_state_tuition <- out_state_tuition %>% rename(
  "UNITID" = "Unit.Id",
  "Type_of_Pay" = "Type.of.cost...residency..and.student.housing",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
out_state_tuition <- out_state_tuition %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "amount",
    values_transform = list(num_nests = as.integer)
  )
out_state_tuition <- out_state_tuition[out_state_tuition$	Type_of_Pay == "On-campus out-of-state", ]
out_state_na <- out_state_tuition[is.na(out_state_tuition$amount), ]
out_state_tuition <- out_state_tuition %>% drop_na(amount)
out_state_tuition <- inner_join(out_state_tuition, University_info , by = c("UNITID"))



Student_aid<- data.frame(read_xlsx("./data_folder/Student_Aid/Student_Aid_2017-2021.xlsx"))
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
Student_aid_inst_grant_amount <- Student_aid_inst[Student_aid_inst$Data_representation == "Average amount", ]

Student_aid_inst_grant_percent <- Student_aid_inst_grant_percent %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Percent",
    values_transform = list(num_nests = as.integer)
  )
Student_aid_inst_grant_percent <- inner_join(Student_aid_inst_grant_percent, University_info , by = c("UNITID"))
Student_aid_inst_grant_percent$Percent <- as.numeric(Student_aid_inst_grant_percent$Percent)
Student_aid_inst_grant_percent <- Student_aid_inst_grant_percent %>%
  mutate(Percent_grant = case_when(
    Student_aid_inst_grant_percent$Percent >= 0 & Student_aid_inst_grant_percent$Percent < 0.25 ~ "0% ~ 25%",
    Student_aid_inst_grant_percent$Percent >= 0.25 & Student_aid_inst_grant_percent$Percent < 0.5 ~ "25% ~ 50%",
    Student_aid_inst_grant_percent$Percent >= 0.5 & Student_aid_inst_grant_percent$Percent < 0.75 ~ "50% ~ 75%",
    Student_aid_inst_grant_percent$Percent >= 0.75 & Student_aid_inst_grant_percent$Percent <= 100 ~ "75% ~ 100%"
  ))


Student_aid_inst_grant_amount <- Student_aid_inst_grant_amount %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Amount",
    values_transform = list(num_nests = as.integer)
  )

Student_aid_inst_grant_amount <- inner_join(Student_aid_inst_grant_amount, University_info , by = c("UNITID"))
Student_aid_inst_grant_na <- Student_aid_inst_grant_amount[Student_aid_inst_grant_amount$Amount == "-", ]
Student_aid_inst_grant_amount <- Student_aid_inst_grant_amount[Student_aid_inst_grant_amount$Amount != "-", ]
Student_aid_inst_grant_amount$Amount <-as.numeric(Student_aid_inst_grant_amount$Amount)


mean_aid_inst_grant <- data.frame(read_xlsx("./data_folder/Student_Aid/Student_Aid_Aggregate_2017-2021.xlsx"))

mean_aid_inst_grant <- mean_aid_inst_grant  %>% rename(
  "Document_Coverage" = "...1",
  "Aid_type" = "...2",
  "Data_representation" = "...3",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
mean_aid_inst_grant$Document_Coverage <- replace_na(mean_aid_inst_grant$Document_Coverage, "Aggregate Result - Title IV degree-granting institutions in the U.S.")
mean_aid_inst_grant <- mean_aid_inst_grant[mean_aid_inst_grant$Aid_type == "Institutional grants", ]

mean_aid_inst_grant <- mean_aid_inst_grant %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Amount",
    values_transform = list(num_nests = as.integer)
  )




ridgeline_out_state_tuition <- ggplot(out_state_tuition , aes(x = amount, y = as.factor(year), fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75),quantile_line = TRUE, geom="density_ridges_gradient")  + 
  xlab("Out state tuition amount($") +
  ylab("Year") +
  ggtitle("Plot of ridgeline density distribution of out state tuition 2017 -2021") +
  scale_y_discrete(expand = expansion(add = c(0.2, 1.5))) +
  facet_wrap(~Institution_type, scales = 'free_x') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete=TRUE
                     , name = "Quantile"
                     , alpha = 0.3
                     , option = "cividis")
ridgeline_out_state_tuition

ridgeline_in_state_tuition <- ggplot(in_state_tuition , aes(x = amount, y = as.factor(year), fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75),quantile_line = TRUE, geom="density_ridges_gradient") + 
  xlab("In state tuition amount($)") +
  ylab("Year") +
  ggtitle("Plot of ridgeline density distribution of out state tuition 2017 -2021") +
  scale_y_discrete(expand = expansion(add = c(0.2, 1.5))) +
  facet_wrap(~Institution_type, scales = 'free_x') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete=TRUE
                     , name = "Quantile"
                     , alpha = 0.3
                     , option = "cividis")
ridgeline_in_state_tuition


mean_outState_tuition_year_type <- out_state_tuition[, c("year", 'amount', 'Institution_type')] %>%
  group_by(as.factor(year), as.factor(Institution_type)) %>%
  summarise_at(vars(amount), list(amount = mean)) 

mean_outState_tuition_year_type  <- mean_outState_tuition_year_type   %>% rename(
  "Year" = "as.factor(year)",
  "Institution_type" = "as.factor(Institution_type)")

line_mean_outState_tuition <- ggplot(mean_outState_tuition_year_type, aes(Year, amount, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  facet_grid(~Institution_type) +
  labs(x = "Year", y = "Tuition amount ($)", 
       title = "Mean out state tuition 2017-2021") +
  theme(plot.title = element_text(hjust = 0.5))

line_mean_outState_tuition


mean_inState_tuition_year_type <- in_state_tuition[, c("year", 'amount', 'Institution_type')] %>%
  group_by(as.factor(year), as.factor(Institution_type)) %>%
  summarise_at(vars(amount), list(amount = mean)) 

mean_inState_tuition_year_type  <- mean_inState_tuition_year_type   %>% rename(
  "Year" = "as.factor(year)",
  "Institution_type" = "as.factor(Institution_type)")

line_mean_inState_tuition <- ggplot(mean_inState_tuition_year_type, aes(Year, amount, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  facet_grid(~Institution_type) +
  labs(x = "Year", y = "Tuition amount ($)", 
       title = "Mean in state tuition 2017-2021") +
  theme(plot.title = element_text(hjust = 0.5))

line_mean_inState_tuition

box_inst_grant <- ggplot(Student_aid_inst_grant_amount , aes(x = Amount, y = as.factor(year))) + 
  geom_boxplot() +
  facet_grid(~Institution_type, scale = "free") +
  labs(x = "Year", y = "Grant amount ($)", 
       title = "Boxplot of institution grant 2017-2021") +
  theme(plot.title = element_text(hjust = 0.5))

box_inst_grant

inst_grant_percent_count <- 
  Student_aid_inst_grant_percent %>% count(year, Institution_type, Percent_grant)
inst_grant_percent_count$Percent_grant <- inst_grant_percent_count$Percent_grant %>% replace_na("Not provided")

inst_grant_percent_count_2017 <- inst_grant_percent_count[inst_grant_percent_count$year == 2017, ]
bar_inst_grant_percentile_2017 <- 
  ggplot(inst_grant_percent_count_2017,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Boxplot of percent grant to Institutions in 2017") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2018 <- inst_grant_percent_count[inst_grant_percent_count$year == 2018, ]
bar_inst_grant_percentile_2018 <- 
  ggplot(inst_grant_percent_count_2018,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Boxplot of percent grant to Institutions in 2018") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2019 <- inst_grant_percent_count[inst_grant_percent_count$year == 2019, ]
bar_inst_grant_percentile_2019 <- 
  ggplot(inst_grant_percent_count_2019,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Boxplot of percent grant to Institutions in 2018") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2020 <- inst_grant_percent_count[inst_grant_percent_count$year == 2020, ]
bar_inst_grant_percentile_2020 <- 
  ggplot(inst_grant_percent_count_2020,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Boxplot of percent grant to Institutions in 2020") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

inst_grant_percent_count_2021 <- inst_grant_percent_count[inst_grant_percent_count$year == 2021, ]
bar_inst_grant_percentile_2021 <- 
  ggplot(inst_grant_percent_count_2021,  aes(x = Percent_grant, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Institution_type) +
  labs(x = "Percent Grant", y = "Number of institution", 
       title = "Boxplot of percent grant to Institutions in 2021") +
  theme(axis.text.x = element_text(angle = -90), plot.title = element_text(hjust = 0.5),
        text=element_text(size=5)) +
  ylim(0, 1000)

total_plot_percent_grant <- ggarrange(bar_inst_grant_percentile_2017,
                        bar_inst_grant_percentile_2018,
                        bar_inst_grant_percentile_2019,
                        bar_inst_grant_percentile_2020,
                        bar_inst_grant_percentile_2021,
          nrow = 2, ncol = 3)
total_plot_percent_grant

               

# Student_aid_inst_grant_na
# 
# Student_aid_inst_grant_na_aggregate <- Student_aid_inst_grant_na[, c("year", 'Institution_type')] %>%
#   group_by(as.factor(year), as.factor(Institution_type)) %>%
#   summarise(count = n()) 


# Student_aid_inst_grant_na_alluvial <- Student_aid_inst_grant_percent %>%
#   mutate(status = if_else(is.na(Percent_grant), "not provided", "provided"))
# Student_aid_inst_grant_na_alluvial <- 
#   Student_aid_inst_grant_na_alluvial[c("year", "Institution_type", "status")]
# 
# Student_aid_inst_grant_na_alluvial_public <- 
#   Student_aid_inst_grant_na_alluvial[Student_aid_inst_grant_na_alluvial$Institution_type == "Public",]
# 
# Student_aid_inst_grant_na_alluvial_private_np <- 
#   Student_aid_inst_grant_na_alluvial[Student_aid_inst_grant_na_alluvial$Institution_type == "Private, nonprofit",]
# 
# Student_aid_inst_grant_na_alluvial_private_fp <- 
#   Student_aid_inst_grant_na_alluvial[Student_aid_inst_grant_na_alluvial$Institution_type == "Private, for-profit",]
# 
# 
# Student_aid_inst_grant_na_alluvial_public <- Student_aid_inst_grant_na_alluvial_public[, c("year", "status")] %>%
#   group_by(as.factor(year), as.factor(status)) %>%
#   summarize(count = n())


Student_aid_avg_state <- Student_aid_inst_grant_amount[, c("year", "State", "Amount")] %>%
  group_by(as.factor(year), as.factor(State)) %>%
  summarise_at(vars(Amount), list (name = mean))

Student_aid_avg_state  <- Student_aid_avg_state   %>% rename(
  "year" = "as.factor(year)",
  "region" = "as.factor(State)",
  "value" = "name")
Student_aid_avg_state$region <- tolower(abbr2state(Student_aid_avg_state$region))

category <- cut(Student_aid_avg_state$value, 
                breaks = c(2000,4000,6000,  8000, 10000,12000, 14000, 16000,25000),
                labels = c("[, 4000)",
                           "[4000, 6000)",
                           "[6000, 8000)",
                           "[8000, 10000)",
                           "[10000, 12000)",
                           "[12000, 14000)",
                           "[14000, 16000)",
                           "[16000, ]"))
category

Student_aid_avg_state$value <- category

Student_aid_avg_state_2017 <- 
  Student_aid_avg_state[Student_aid_avg_state$year == 2017, c("region", "value")]

Student_aid_avg_state_2018 <- 
  Student_aid_avg_state[Student_aid_avg_state$year == 2018, c("region", "value")]

Student_aid_avg_state_2019 <- 
  Student_aid_avg_state[Student_aid_avg_state$year == 2019, c("region", "value")]

Student_aid_avg_state_2020 <- 
  Student_aid_avg_state[Student_aid_avg_state$year == 2020, c("region", "value")]

Student_aid_avg_state_2021 <- 
  Student_aid_avg_state[Student_aid_avg_state$year == 2021, c("region", "value")]

Student_aid_2017_map <- state_choropleth(Student_aid_avg_state_2017,
                                         title = "Average institutional grant of Schools at each state in 2017",
                                         legend = "Grant Amount($)") 
 

Student_aid_2018_map <- state_choropleth(Student_aid_avg_state_2018,
                                         title = "Average institutional grant of Schools at each state in 2018",
                                         legend = "Grant Amount($)")

Student_aid_2019_map <- state_choropleth(Student_aid_avg_state_2019,
                                         title = "Average institutional grant of Schools at each state in 2019",
                                         legend = "Grant Amount($)")

Student_aid_2020_map <- state_choropleth(Student_aid_avg_state_2020,
                                         title = "Average institutional grant of Schools at each state in 2020",
                                         legend = "Grant Amount($)")

Student_aid_2021_map <- state_choropleth(Student_aid_avg_state_2021,
                                         title = "Average institutional grant of Schools at each state in 2021",
                                         legend = "Grant Amount($)")

total_plot_avg_aid_map <- ggarrange(Student_aid_2017_map,
                                    Student_aid_2018_map,
                                    Student_aid_2019_map,
                                    Student_aid_2020_map,
                                    Student_aid_2021_map,
                                    nrow = 2, ncol = 3)
Student_aid_2017_map
Student_aid_2018_map
Student_aid_2019_map
Student_aid_2020_map
Student_aid_2021_map





video_game <- data.frame(read_csv("./video_games.csv"))


video_game_ridge <- ggplot(video_game , aes(x = Metrics.Used.Price , y = as.factor(Release.Console), fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75),quantile_line = TRUE, geom="density_ridges_gradient") + 
  xlab("Price($)") +
  ylab("Comsole type") +
  ggtitle("Plot of ridgeline density distribution of out state tuition 2017 -2021") +
  scale_y_discrete(expand = expansion(add = c(0.2, 1.5))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete=TRUE
                     , name = "Quantile"
                     , alpha = 0.3
                     , option = "cividis")
video_game_ridge



