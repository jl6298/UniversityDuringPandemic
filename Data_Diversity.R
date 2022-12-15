library('cowplot')
library("choroplethr")
library("dplyr")
library("forcats")
library("gridExtra")
library("ggplot2")
library("ggpubr")
library("ggridges")
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

University_info <- data.frame(read_csv("./resources/data_folder/data_Yudu/University/University_info.csv"))

Enrollment_Race_Data <- data.frame(read_xlsx("./resources/data_folder/data_Yudu/Enrollment_Data/Enrollment_Data_Race_Gender.xlsx"))

Enrollment_Gender_Data <- data.frame(read_xlsx("./resources/data_folder/data_Yudu/Enrollment_Data/Enrollment_Data_Race_Gender.xlsx"))

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
    names_to = "Year", 
    names_transform = list(Year = as.integer),
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
Diversity_Data_aggregated <-Enrollment_Race_Data %>% group_by(Year, STABBR, Diversity_rating)  %>%
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
  Diversity_Data_aggregated[Diversity_Data_aggregated$Year == 2017, c("region", "value")]

Diversity_Data_aggregated_2018 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$Year == 2018, c("region", "value")]

Diversity_Data_aggregated_2019 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$Year == 2019, c("region", "value")]

Diversity_Data_aggregated_2020 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$Year == 2020, c("region", "value")]

Diversity_Data_aggregated_2021 <- 
  Diversity_Data_aggregated[Diversity_Data_aggregated$Year == 2021, c("region", "value")]

High_Diversity_Percentage_2017_map <- state_choropleth(Diversity_Data_aggregated_2017,
                                                       title = "High_Div Uni Pct 2017",
                                                       legend = "Percentage") 
layer_no <- grep("GeomText", sapply(High_Diversity_Percentage_2017_map $layers, function(x) class(x$geom)[1]))
High_Diversity_Percentage_2017_map $layers[[layer_no]] <- NULL

High_Diversity_Percentage_2018_map <- state_choropleth(Diversity_Data_aggregated_2018,
                                                       title = "High_Div Uni Pct 2018",
                                                       legend = "Percentage") 
layer_no <- grep("GeomText", sapply(High_Diversity_Percentage_2018_map $layers, function(x) class(x$geom)[1]))
High_Diversity_Percentage_2018_map $layers[[layer_no]] <- NULL

High_Diversity_Percentage_2019_map <- state_choropleth(Diversity_Data_aggregated_2019,
                                                       title = "High_Div Uni Pct 2019") 
layer_no <- grep("GeomText", sapply(High_Diversity_Percentage_2019_map $layers, function(x) class(x$geom)[1]))
High_Diversity_Percentage_2019_map $layers[[layer_no]] <- NULL

High_Diversity_Percentage_2020_map <- state_choropleth(Diversity_Data_aggregated_2020,
                                                       title = "High_Div Uni Pct 2020") 
layer_no <- grep("GeomText", sapply(High_Diversity_Percentage_2020_map $layers, function(x) class(x$geom)[1]))
High_Diversity_Percentage_2020_map $layers[[layer_no]] <- NULL

High_Diversity_Percentage_2021_map <- state_choropleth(Diversity_Data_aggregated_2021,
                                                       title = "High_Div Uni Pct 2021") 
layer_no <- grep("GeomText", sapply(High_Diversity_Percentage_2021_map $layers, function(x) class(x$geom)[1]))
High_Diversity_Percentage_2021_map $layers[[layer_no]] <- NULL

# This is the code we use to generate diversity histogram in interactive 
# component. We leave it here for your reference.

#Enrollment_Race_Data_2021 <- Enrollment_Race_Data[, c('UNITID', #'STABBR',"Year", "Institution.name", 'diversity_index')]

#Enrollment_Race_Data_2021$STABBR <- abbr2state(Enrollment_Race_Data_2021$STABBR)
#Enrollment_Race_Data_2021<- #Enrollment_Race_Data_2021[Enrollment_Race_Data_2021$Year == 2021, ]



#StateVec <- unique(Enrollment_Race_Data_2021$STABBR)
#for (values in StateVec){
# toSave <- ggplot(Enrollment_Race_Data_2021[Enrollment_Race_Data_2021$STABBR == values, ], aes(x=diversity_index)) + 
#    geom_histogram( binwidth=0.05, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#   xlab("Diversity index for schools") +
#    ylab("School count") +
#   xlim(0, 1) +
#  ggtitle(paste(values)) +
# theme(plot.title = element_text(hjust = 0.5), text = element_text(size = #20))

# ggsave(paste0(values, ".png"), plot = toSave)
#}



# This is the code we use to generate  Java script Key-valuye pair in #interactive  component. We leave here for your reference.

#dictionary <- '{'
#for (row in 1:nrow(Enrollment_Race_Data_2021_Mean)) {
# if (row == 52) {
#  dictionary <- paste0(dictionary, '}')
# break;
#}
#if (row == 51){
# dictionary <- paste0(dictionary,"\'", Enrollment_Race_Data_2021_Mean[row, "STABBR"],"\'", ':', Enrollment_Race_Data_2021_Mean[row, "mean_diversity",])

#  } else {
#   dictionary <- paste0(dictionary,"\'", Enrollment_Race_Data_2021_Mean[row, "STABBR"],"\'",':', Enrollment_Race_Data_2021_Mean[row, "mean_diversity",], ',')
#  }

#}


Enrollment_Gender_Data <- Enrollment_Gender_Data %>% rename(
  "UNITID" = "Unit.Id",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")

Enrollment_Gender_Data <- Enrollment_Gender_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "Year", 
    names_transform = list(Year = as.integer),
    values_to = "Enrollment Amount",
    values_transform = list(num_nests = as.integer)
  )
Enrollment_Gender_Data <- Enrollment_Gender_Data[Enrollment_Gender_Data$Race.ethnicity == "Total", ]
Enrollment_Gender_Data_Men <- Enrollment_Gender_Data[Enrollment_Gender_Data$Gender == "Men", ]
Enrollment_Gender_Data_Men <- Enrollment_Gender_Data_Men %>% rename(
  "Enrolled_Men" = "Enrollment Amount")
Enrollment_Gender_Data_Women <- Enrollment_Gender_Data[Enrollment_Gender_Data$Gender == "Women", ]
Enrollment_Gender_Data_Women <- Enrollment_Gender_Data_Women %>% rename(
  "Enrolled_Women" = "Enrollment Amount")
Enrollment_Gender_Data_cleaned <- inner_join(Enrollment_Gender_Data_Men, Enrollment_Gender_Data_Women , by = c("UNITID", "Year"))
Enrollment_Gender_Data_cleaned <- Enrollment_Gender_Data_cleaned[!Enrollment_Gender_Data_cleaned$Enrolled_Men == "-",]
Enrollment_Gender_Data_cleaned <- Enrollment_Gender_Data_cleaned %>%
  mutate(Male_to_Female_Ratio = as.integer(Enrolled_Men) / as.integer(Enrolled_Women)
  )
Enrollment_Gender_Data_cleaned <- Enrollment_Gender_Data_cleaned %>%
  mutate(Gender_Demographics = case_when(
    Enrollment_Gender_Data_cleaned$Male_to_Female_Ratio >= 1.1 ~ "Male Majority",
    Enrollment_Gender_Data_cleaned$Male_to_Female_Ratio < 1.1 & 
      Enrollment_Gender_Data_cleaned$Male_to_Female_Ratio >= 0.9~ "Neutral",
    Enrollment_Gender_Data_cleaned$Male_to_Female_Ratio  < 0.9 ~ "Female Majority"
  ))

University_info$UNITID <- as.integer(University_info$UNITID)
Enrollment_Gender_Data_cleaned <- inner_join(Enrollment_Gender_Data_cleaned, University_info, by = c("UNITID"))

Enrollment_Gender_Data_cleaned_Aggregate <-Enrollment_Gender_Data_cleaned %>% group_by(Year, Gender_Demographics)  %>%
  summarise(total_number = n())

Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- Enrollment_Gender_Data_cleaned_Aggregate 

Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- Enrollment_Gender_Data_cleaned_Aggregate_rightorder %>%
  pivot_wider(names_from = Gender_Demographics, 
              values_from = total_number, 
              values_fill = 0)
Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder[c("Year", "Female Majority",
                                                        "Neutral",
                                                        "Male Majority")]
diverging_Enrollment_Gender_All_School <- HH::likert(Year ~ ., Enrollment_Gender_Data_cleaned_Aggregate_rightorder, main="School demographics")


