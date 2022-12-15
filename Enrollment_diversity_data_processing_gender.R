library("HH")
library("dplyr")
library("readxl")
library("tidyr")
library("ggplot2")
Enrollment_Gender_Data <- data.frame(read_xlsx("./data_folder/Enrollment_Data/Enrollment_Data_Race_Gender.xlsx"))
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
    names_to = "year", 
    names_transform = list(year = as.integer),
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
Enrollment_Gender_Data_cleaned <- inner_join(Enrollment_Gender_Data_Men, Enrollment_Gender_Data_Women , by = c("UNITID", "year"))
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

Enrollment_Gender_Data_cleaned_Aggregate <-Enrollment_Gender_Data_cleaned %>% group_by(year, Gender_Demographics)  %>%
  summarise(total_number = n())

#stack_bar_gender_demo <- ggplot(Enrollment_Gender_Data_cleaned_Aggregate, aes(fill=Gender_Demographics, y=total_number, x=year)) + 
 # geom_bar(position="stack", stat="identity") +
#  xlab("Year") +
 # ylab("Number of Schools") +
  #ggtitle("Number of schools with different Gender demographics 2017 -2021") 
#stack_bar_gender_demo 








Enrollment_Gender_Data_cleaned_Insti_Type <-Enrollment_Gender_Data_cleaned %>% group_by(year, Gender_Demographics, Institution_type)  %>%
  summarise(total_number = n())

#stack_bar_gender_demo_insti_Type <- ggplot(Enrollment_Gender_Data_cleaned_Insti_Type, aes(fill=Gender_Demographics, y=total_number, x=year)) + 
 # geom_bar(position="stack", stat="identity") +
#  xlab("Year") +
 # ylab("Number of Schools") +
  #facet_grid(~Institution_type)+
  #ggtitle("Number of schools with different Gender demographics 2017 -2021") 

#stack_bar_gender_demo_insti_Type


Enrollment_Gender_Data_Missing <- Enrollment_Gender_Data[Enrollment_Gender_Data$`Enrollment Amount` == "-", ]

Enrollment_Gender_Data_Missing <- Enrollment_Gender_Data_Missing[Enrollment_Gender_Data_Missing$Gender == "Total", c('UNITID', 'Institution.name', 'year')]

Enrollment_Gender_Yearly_population <- Enrollment_Gender_Data_cleaned %>% group_by(year, Institution_type) %>%
  summarize(total_men = sum(as.integer(Enrolled_Men)), total_women = sum(as.integer(Enrolled_Women)))

Enrollment_Gender_Yearly_population <- Enrollment_Gender_Yearly_population %>% 
  pivot_longer(
    cols = c('total_men', 'total_women'), 
    names_to = "gender_category", 
    values_to = "Enrollment_Amount",
    values_transform = list(num_nests = as.integer)
  )


stack_bar_gender_population <- ggplot(Enrollment_Gender_Yearly_population, aes(fill=gender_category, y= Enrollment_Amount, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of Enrolled Students") +
  facet_grid(~Institution_type)+
  ggtitle("Number of Enrolled Men and Women 2017-2021") 
stack_bar_gender_population








Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- Enrollment_Gender_Data_cleaned_Aggregate 

Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- Enrollment_Gender_Data_cleaned_Aggregate_rightorder %>%
  pivot_wider(names_from = Gender_Demographics, 
              values_from = total_number, 
              values_fill = 0)
Enrollment_Gender_Data_cleaned_Aggregate_rightorder <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder[c("year", "Female Majority",
                                                        "Neutral",
                                                        "Male Majority")]
diverging_Enrollment_Gender_All_School <- HH::likert(year ~ ., Enrollment_Gender_Data_cleaned_Aggregate_rightorder, main="School demographics")
diverging_Enrollment_Gender_All_School







Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst <-Enrollment_Gender_Data_cleaned %>% group_by(year, Gender_Demographics, Institution_type)  %>%
  summarise(total_number = n())
Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst


Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst[
    Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst$Institution_type == "Public",c("year", "Gender_Demographics", "total_number")]

Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub <- Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub %>%
  pivot_wider(names_from = Gender_Demographics, 
              values_from = total_number, 
              values_fill = 0)
Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub  <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub[c("year", "Female Majority",
                                                        "Neutral",
                                                        "Male Majority")]
diverging_Enrollment_Gender_Public_School <- HH::likert(year ~ ., Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_pub , main="Gender demographics in Public School")
diverging_Enrollment_Gender_Public_School



Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst[
    Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst$Institution_type == "Private, for-profit",c("year", "Gender_Demographics", "total_number")]

Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate <- Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate %>%
  pivot_wider(names_from = Gender_Demographics, 
              values_from = total_number, 
              values_fill = 0)
Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate  <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate[c("year", "Female Majority",
                                                                 "Neutral",
                                                                 "Male Majority")]
diverging_Enrollment_Gender_fpPrivate_School <- HH::likert(year ~ ., Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_fpprivate , main="Gender demographics in For-Profit Private School")
diverging_Enrollment_Gender_fpPrivate_School






Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst[
    Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst$Institution_type == "Private, nonprofit",c("year", "Gender_Demographics", "total_number")]

Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate <- Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate %>%
  pivot_wider(names_from = Gender_Demographics, 
              values_from = total_number, 
              values_fill = 0)
Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate  <- 
  Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate[c("year", "Female Majority",
                                                                       "Neutral",
                                                                       "Male Majority")]
diverging_Enrollment_Gender_npPrivate_School <- HH::likert(year ~ ., Enrollment_Gender_Data_cleaned_Aggregate_rightorder_inst_npprivate , main="Gender demographics in non-Profit Private School")
diverging_Enrollment_Gender_npPrivate_School













  
  
  
  