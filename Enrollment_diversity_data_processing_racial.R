library("HH")
library("dplyr")
library("readxl")
library("tidyr")
library("ggplot2")
library("forcats")
library("choroplethr")
library("openintro")
Enrollment_Race_Data <- data.frame(read_xlsx("./data_folder/Enrollment_Data/Enrollment_Data_Race_Gender.xlsx"))
Enrollment_Race_Data <- Enrollment_Race_Data %>% rename(
  "UNITID" = "Unit.Id",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")

Enrollment_Race_Data <- Enrollment_Race_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Enrollment Amount",
    values_transform = list(num_nests = as.integer)
  )
Enrollment_Race_Data <- Enrollment_Race_Data[Enrollment_Race_Data$Gender == "Total",]
Enrollment_Race_Data <- subset(Enrollment_Race_Data, select = -c(Level.of.student))
Enrollment_Race_Data$`Enrollment Amount` <- as.numeric(Enrollment_Race_Data$`Enrollment Amount`)
Enrollment_Race_Data <- drop_na(Enrollment_Race_Data)


Enrollment_Race_Data <- Enrollment_Race_Data %>%
  pivot_wider(names_from = Race.ethnicity, 
              values_from = `Enrollment Amount`, 
              values_fill = 0)

Enrollment_Race_Data$UNITID <- as.character(Enrollment_Race_Data$UNITID)
Enrollment_Race_Data <- inner_join(University_loc, Enrollment_Race_Data, by = c("UNITID"))

Enrollment_Race_Data <- data.frame(Enrollment_Race_Data)
diversity_idx = function(x, output){
  
  # accessing elements from first column
  
  vec <- as.vector(as.numeric(x[7:15]))

  total <- as.numeric(x[6])
  toReturn <- 0;

  for (values in vec) {
   
    if(values != 0){
      prob <- values / total
     
      toReturn <- toReturn + prob * (1 - prob)
    }
  }
    
  return (toReturn)
}




vec <- base::apply(X = Enrollment_Race_Data, MARGIN = 1, FUN = diversity_idx)
Enrollment_Race_Data <- cbind(Enrollment_Race_Data,diversity_index = vec)

Enrollment_Race_Data <- Enrollment_Race_Data %>%
  mutate(Diversity_rating = case_when(
    Enrollment_Race_Data$diversity_index >= 0.5 ~"High Diversity", 
    Enrollment_Race_Data$diversity_index < 0.5 ~"Low Diversity", 
    
  ))
Diversity_Data_aggregated <-Enrollment_Race_Data %>% group_by(year, STABBR, Diversity_rating)  %>%
  summarise(Diversity_count = n())

Diversity_Data_aggregated <- Diversity_Data_aggregated %>%
  pivot_wider(names_from = Diversity_rating, 
              values_from = Diversity_count, 
              values_fill = 0)

diversity_percentage= function(x, output){
  
  # accessing elements from first column
  
  High_count <- as.numeric(x[3])
  Low_count <- as.numeric(x[4])
  

  return (High_count / (High_count + Low_count))
}
High_Div_percentage <- base::apply(X = Diversity_Data_aggregated, MARGIN = 1, FUN = diversity_percentage)
Diversity_Data_aggregated <- cbind(Diversity_Data_aggregated,High_Diversity_Percentage = High_Div_percentage)

Diversity_Data_aggregated$High_Diversity_Percentage <- as.numeric(Diversity_Data_aggregated$High_Diversity_Percentage)*100


cleveland_Diversity_plot_2017 <-
  ggplot(Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2017, ] , aes(x = High_Diversity_Percentage, y=reorder(STABBR, High_Diversity_Percentage))) +
  geom_point(size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme_linedraw() +
  ylab("Stated") +
  ggtitle("Percentage of High Diversity School in 2017") +
  theme(plot.title = element_text(hjust = 0.5))
  

cleveland_Diversity_plot_2017

cleveland_Diversity_plot_2018 <-
  ggplot(Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2018, ] , aes(x = High_Diversity_Percentage, y=reorder(STABBR, High_Diversity_Percentage))) +
  geom_point(size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme_linedraw() +
  ylab("Stated") +
  ggtitle("Percentage of High Diversity School in 2018") +
  theme(plot.title = element_text(hjust = 0.5))


cleveland_Diversity_plot_2018

cleveland_Diversity_plot_2019 <-
  ggplot(Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2019, ] , aes(x = High_Diversity_Percentage, y=reorder(STABBR, High_Diversity_Percentage))) +
  geom_point(size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme_linedraw() +
  ylab("Stated") +
  ggtitle("Percentage of High Diversity School in 2019") +
  theme(plot.title = element_text(hjust = 0.5))


cleveland_Diversity_plot_2019


cleveland_Diversity_plot_2020 <-
  ggplot(Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2020, ] , aes(x = High_Diversity_Percentage, y=reorder(STABBR, High_Diversity_Percentage))) +
  geom_point(size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme_linedraw() +
  ylab("Stated") +
  ggtitle("Percentage of High Diversity School in 2020") +
  theme(plot.title = element_text(hjust = 0.5))


cleveland_Diversity_plot_2020

cleveland_Diversity_plot_2021 <-
  ggplot(Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2021, ] , aes(x = High_Diversity_Percentage, y=reorder(STABBR, High_Diversity_Percentage))) +
  geom_point(size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme_linedraw() +
  ylab("Stated") +
  ggtitle("Percentage of High Diversity School in 2021") +
  theme(plot.title = element_text(hjust = 0.5))


cleveland_Diversity_plot_2021



Enrollment_Race_Data <- Enrollment_Race_Data %>%
  mutate(Percent_Diversity = case_when(
    Enrollment_Race_Data$diversity_index >= 0 & Enrollment_Race_Data$diversity_index  < 0.25 ~ "0% ~ 25%",
    Enrollment_Race_Data$diversity_index  >= 0.25 & Enrollment_Race_Data$diversity_index  < 0.5 ~ "25% ~ 50%",
    Enrollment_Race_Data$diversity_index  >= 0.5 & Enrollment_Race_Data$diversity_index < 0.75 ~ "50% ~ 75%",
    Enrollment_Race_Data$diversity_index  >= 0.75 & Enrollment_Race_Data$diversity_index  <= 100 ~ "75% ~ 100%"
    
  ))
Diversity_mean_index <- Enrollment_Race_Data %>% group_by(year, STABBR)  %>%
  summarise(mean_index = mean(diversity_index))
Diversity_Data_aggregated
Diversity_Data_aggregated <- Diversity_Data_aggregated %>%
  mutate( Diversity_Bin = dplyr::case_when(
    High_Diversity_Percentage >= 0 & High_Diversity_Percentage < 25 ~ "0% ~ 25%",
    High_Diversity_Percentage  >= 25 & High_Diversity_Percentage  < 50 ~ "25% ~ 50%",
    High_Diversity_Percentage  >= 50 & High_Diversity_Percentage < 75 ~ "50% ~ 75%",
    High_Diversity_Percentage >= 75.0 & High_Diversity_Percentage <= 100.0 ~ "75% ~ 100%"
  ))



Diversity_Data_aggregated$STABBR <- tolower(abbr2state(Diversity_Data_aggregated$STABBR))
Diversity_Data_aggregated  <- Diversity_Data_aggregated  %>% rename(
  "region" = "STABBR",
  "value" = "Diversity_Bin")

category <- cut(Diversity_Data_aggregated$High_Diversity_Percentage, 
                breaks = c(0,25,50,75, 100),
                labels = c("[, 25)",
                           "[25, 50)",
                           "[50, 75)",
                           "[75, 100)"
                          ))

Diversity_Data_aggregated$Diversity_Bin <- category
Diversity_Data_aggregated_2017 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2017, c("region", "value")]

Diversity_Data_aggregated_2018 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2018, c("region", "value")]

Diversity_Data_aggregated_2019 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2019, c("region", "value")]

Diversity_Data_aggregated_2020 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2020, c("region", "value")]

Diversity_Data_aggregated_2021 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$year == 2021, c("region", "value")]

High_Diversity_Percentage_2017_map <- state_choropleth(Diversity_Data_aggregated_2017,
                                         title = "Percent of High diversity school at each state in 2017",
                                         legend = "Percentage") 

High_Diversity_Percentage_2018_map <- state_choropleth(Diversity_Data_aggregated_2018,
                                                       title = "Percent of High diversity school at each state in 2018",
                                                       legend = "Percentage") 

High_Diversity_Percentage_2019_map <- state_choropleth(Diversity_Data_aggregated_2019,
                                                       title = "Percent of High diversity school at each state in 2019",
                                                       legend = "Percentage") 

High_Diversity_Percentage_2020_map <- state_choropleth(Diversity_Data_aggregated_2020,
                                                       title = "Percent of High diversity school at each state in 2020",
                                                       legend = "Percentage") 

High_Diversity_Percentage_2021_map <- state_choropleth(Diversity_Data_aggregated_2021,
                                                       title = "Percent of High diversity school at each state in 2021",
                                                       legend = "Percentage") 
High_Diversity_Percentage_2017_map
High_Diversity_Percentage_2018_map
High_Diversity_Percentage_2019_map
High_Diversity_Percentage_2020_map
High_Diversity_Percentage_2021_map







