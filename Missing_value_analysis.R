library(ggplot2)
remotes::install_github("jtr13/redav")

library("redav")
SAT_Data_2017 <- data.frame(read_xlsx("./data_folder/SAT_Score_Data/SAT_Score_2017.xlsx"))
SAT_Data_2017 <- SAT_Data_2017[, c("Unit.Id", "SAT.Math")]
SAT_Data_2017 <- SAT_Data_2017 %>% mutate(year = 2017)
SAT_Data_2017$SAT.Math <- as.numeric(SAT_Data_2017$SAT.Math)
SAT_Data_2017  <- SAT_Data_2017  %>% rename(
  "SAT" = "SAT.Math")


SAT_Data_2018 <- data.frame(read_xlsx("./data_folder/SAT_Score_Data/SAT_Score_2018.xlsx"))
SAT_Data_2018 <- SAT_Data_2018[, c("Unit.Id", "SAT.Math")]
SAT_Data_2018 <- SAT_Data_2018 %>% mutate(year = 2018)
SAT_Data_2018$SAT.Math <- as.numeric(SAT_Data_2018$SAT.Math)
SAT_Data_2018  <- SAT_Data_2018  %>% rename(
  "SAT" = "SAT.Math")

SAT_Data_2019 <- data.frame(read_xlsx("./data_folder/SAT_Score_Data/SAT_Score_2019.xlsx"))
SAT_Data_2019 <- SAT_Data_2019[, c("Unit.Id", "SAT.Math")]
SAT_Data_2019 <- SAT_Data_2019 %>% mutate(year = 2019)
SAT_Data_2019$SAT.Math <- as.numeric(SAT_Data_2019$SAT.Math)
SAT_Data_2019  <- SAT_Data_2019  %>% rename(
  "SAT" = "SAT.Math")


SAT_Data_2020 <- data.frame(read_xlsx("./data_folder/SAT_Score_Data/SAT_Score_2020.xlsx"))
SAT_Data_2020 <- SAT_Data_2020[, c("Unit.Id", "SAT.Math")]
SAT_Data_2020 <- SAT_Data_2020 %>% mutate(year = 2020)
SAT_Data_2020$SAT.Math <- as.numeric(SAT_Data_2020$SAT.Math)
SAT_Data_2020  <- SAT_Data_2020  %>% rename(
  "SAT" = "SAT.Math")

SAT_Data_2021 <- data.frame(read_xlsx("./data_folder/SAT_Score_Data/SAT_Score_2021.xlsx"))
SAT_Data_2021 <- SAT_Data_2021[, c("Unit.Id", "SAT.Math")]
SAT_Data_2021 <- SAT_Data_2021 %>% mutate(year = 2021)
SAT_Data_2021$SAT.Math <- as.numeric(SAT_Data_2021$SAT.Math)
SAT_Data_2021  <- SAT_Data_2021  %>% rename(
  "SAT" = "SAT.Math")

ACT_Data_2017 <- data.frame(read_xlsx("./data_folder/ACT_Score_Data/ACT_Score_2017.xlsx"))
ACT_Data_2017 <- ACT_Data_2017[, c("Unit.Id", "ACT.Math")]
ACT_Data_2017 <- ACT_Data_2017 %>% mutate(year = 2017)
ACT_Data_2017$ACT.Math <- as.numeric(ACT_Data_2017$ACT.Math)
ACT_Data_2017  <- ACT_Data_2017  %>% rename(
  "ACT" = "ACT.Math")

ACT_Data_2018 <- data.frame(read_xlsx("./data_folder/ACT_Score_Data/ACT_Score_2018.xlsx"))
ACT_Data_2018 <- ACT_Data_2018[, c("Unit.Id", "ACT.Math")]
ACT_Data_2018 <- ACT_Data_2018 %>% mutate(year = 2018)
ACT_Data_2018$ACT.Math <- as.numeric(ACT_Data_2018$ACT.Math)
ACT_Data_2018  <- ACT_Data_2018  %>% rename(
  "ACT" = "ACT.Math")

ACT_Data_2019 <- data.frame(read_xlsx("./data_folder/ACT_Score_Data/ACT_Score_2019.xlsx"))
ACT_Data_2019 <- ACT_Data_2019[, c("Unit.Id", "ACT.Math")]
ACT_Data_2019 <- ACT_Data_2019 %>% mutate(year = 2019)
ACT_Data_2019$ACT.Math <- as.numeric(ACT_Data_2019$ACT.Math)
ACT_Data_2019  <- ACT_Data_2019  %>% rename(
  "ACT" = "ACT.Math")

ACT_Data_2020 <- data.frame(read_xlsx("./data_folder/ACT_Score_Data/ACT_Score_2020.xlsx"))
ACT_Data_2020 <- ACT_Data_2020[, c("Unit.Id", "ACT.Math")]
ACT_Data_2020 <- ACT_Data_2020 %>% mutate(year = 2020)
ACT_Data_2020$ACT.Math <- as.numeric(ACT_Data_2020$ACT.Math)
ACT_Data_2020  <- ACT_Data_2020  %>% rename(
  "ACT" = "ACT.Math")

ACT_Data_2021 <- data.frame(read_xlsx("./data_folder/ACT_Score_Data/ACT_Score_2021.xlsx"))
ACT_Data_2021 <- ACT_Data_2021[, c("Unit.Id", "ACT.Math")]
ACT_Data_2021 <- ACT_Data_2021 %>% mutate(year = 2021)
ACT_Data_2021$ACT.Math <- as.numeric(ACT_Data_2021$ACT.Math)
ACT_Data_2021  <- ACT_Data_2021  %>% rename(
  "ACT" = "ACT.Math")


Enrollment_Data<- data.frame(read_xlsx("./data_folder/Enrollment_Data/Enrollment_Data_Race_Gender.xlsx"))
Enrollment_Data <- Enrollment_Data %>% rename(
  "UNITID" = "Unit.Id",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")

Enrollment_Data <- Enrollment_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Enrollment",
    values_transform = list(num_nests = as.integer)
  )
Enrollment_Data <- Enrollment_Data[Enrollment_Data$Gender == "Total",]

Enrollment_Data$Enrollment <- as.numeric(Enrollment_Data$Enrollment)
Enrollment_Data <- Enrollment_Data[Enrollment_Data$Race.ethnicity == "Total",]

Enrollment_Data<- subset(Enrollment_Data, select = -c(Gender, Race.ethnicity))
Enrollment_Data$Enrollment <-as.numeric(Enrollment_Data$Enrollment)

Admission_Data_2017 <- data.frame(read_xlsx("./data_folder/Admission_Data/Admission_2017.xlsx"))
Admission_Data_2017 <- Admission_Data_2017[Admission_Data_2017$Gender == "Total",]
Admission_Data_2017 <- Admission_Data_2017[, c("Unit.Id", "Number.admitted")]
Admission_Data_2017 <- Admission_Data_2017 %>% mutate(year = 2017)
Admission_Data_2017$Number.admitted <- as.numeric(Admission_Data_2017$Number.admitted)
Admission_Data_2017 <- Admission_Data_2017  %>% rename(
  "Total_Admission" = "Number.admitted")


Admission_Data_2018 <- data.frame(read_xlsx("./data_folder/Admission_Data/Admission_2018.xlsx"))
Admission_Data_2018 <- Admission_Data_2018[Admission_Data_2018$Gender == "Total",]
Admission_Data_2018 <- Admission_Data_2018[, c("Unit.Id", "Number.admitted")]
Admission_Data_2018 <- Admission_Data_2018 %>% mutate(year = 2018)
Admission_Data_2018$Number.admitted <- as.numeric(Admission_Data_2018$Number.admitted)
Admission_Data_2018 <- Admission_Data_2018  %>% rename(
  "Total_Admission" = "Number.admitted")

Admission_Data_2019 <- data.frame(read_xlsx("./data_folder/Admission_Data/Admission_2019.xlsx"))
Admission_Data_2019 <- Admission_Data_2019[Admission_Data_2019$Gender == "Total",]
Admission_Data_2019 <- Admission_Data_2019[, c("Unit.Id", "Number.admitted")]
Admission_Data_2019 <- Admission_Data_2019 %>% mutate(year = 2019)
Admission_Data_2019$Number.admitted <- as.numeric(Admission_Data_2019$Number.admitted)
Admission_Data_2019 <- Admission_Data_2019  %>% rename(
  "Total_Admission" = "Number.admitted")


Admission_Data_2020 <- data.frame(read_xlsx("./data_folder/Admission_Data/Admission_2020.xlsx"))
Admission_Data_2020 <- Admission_Data_2020[Admission_Data_2020$Gender == "Total",]
Admission_Data_2020 <- Admission_Data_2020[, c("Unit.Id", "Number.admitted")]
Admission_Data_2020 <- Admission_Data_2020 %>% mutate(year = 2020)
Admission_Data_2020$Number.admitted <- as.numeric(Admission_Data_2020$Number.admitted)
Admission_Data_2020 <- Admission_Data_2020  %>% rename(
  "Total_Admission" = "Number.admitted")

Admission_Data_2021 <- data.frame(read_xlsx("./data_folder/Admission_Data/Admission_2021.xlsx"))
Admission_Data_2021 <- Admission_Data_2021[Admission_Data_2021$Gender == "Total",]
Admission_Data_2021 <- Admission_Data_2021[, c("Unit.Id", "Number.admitted")]
Admission_Data_2021 <- Admission_Data_2021 %>% mutate(year = 2021)
Admission_Data_2021$Number.admitted <- as.numeric(Admission_Data_2021$Number.admitted)
Admission_Data_2021 <- Admission_Data_2021  %>% rename(
  "Total_Admission" = "Number.admitted")

out_state_tuition_Data<- data.frame(read_xlsx("./data_folder/Tuition_Data/Pricetrend_out_state.xlsx"))
out_state_tuition_Data <- out_state_tuition_Data%>% rename(
  "UNITID" = "Unit.Id",
  "Type_of_Pay" = "Type.of.cost...residency..and.student.housing",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
out_state_tuition_Data <- out_state_tuition_Data[out_state_tuition_Data$Type_of_Pay == "On-campus out-of-state",]
out_state_tuition_Data <- out_state_tuition_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Out_state_tuition",
    values_transform = list(num_nests = as.integer)
  )
out_state_tuition_Data <- out_state_tuition_Data[, c("UNITID", "year", "Out_state_tuition")]

in_state_tuition_Data<- data.frame(read_xlsx("./data_folder/Tuition_Data/Pricetrend_in_state.xlsx"))
in_state_tuition_Data <- in_state_tuition_Data%>% rename(
  "UNITID" = "Unit.Id",
  "Type_of_Pay" = "Type.of.cost...residency..and.student.housing",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
in_state_tuition_Data <- in_state_tuition_Data[in_state_tuition_Data$Type_of_Pay == "On-campus in-state",]
in_state_tuition_Data <- in_state_tuition_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "In_state_tuition",
    values_transform = list(num_nests = as.integer)
  )
in_state_tuition_Data <- in_state_tuition_Data[, c("UNITID", "year", "In_state_tuition")]

Student_aid_Data<- data.frame(read_xlsx("./data_folder/Student_Aid/Student_Aid_2017-2021.xlsx"))
Student_aid_Data <- Student_aid_Data %>% rename(
  "UNITID" = "Unit.Id",
  "Aid_type" = "...3",
  "Data_representation" = "...4",
  "2021" = "X2021",
  "2020" = "X2020",
  "2019" = "X2019", 
  "2018" = "X2018",
  "2017" = "X2017")
Student_aid_Data <- Student_aid_Data[Student_aid_Data$Aid_type == "Institutional grants", ]
Student_aid_Data <- Student_aid_Data[Student_aid_Data$Data_representation == "Average amount", ]
Student_aid_Data <- Student_aid_Data %>% 
  pivot_longer(
    cols = c('2021', '2020', '2019', '2018', '2017'), 
    names_to = "year", 
    names_transform = list(year = as.integer),
    values_to = "Student Aid",
    values_transform = list(num_nests = as.integer)
  )
Student_aid_Data$`Student Aid` <- as.numeric(Student_aid_Data$`Student Aid`)
Student_aid_Data<- Student_aid_Data[, c("UNITID", "year", "Student Aid")]

SAT_Data_Bind <- rbind(SAT_Data_2017, SAT_Data_2018, SAT_Data_2019, SAT_Data_2020, SAT_Data_2021)
SAT_Data_Bind <- SAT_Data_Bind %>% rename(
  "UNITID" = "Unit.Id")
ACT_Data_Bind <- rbind(ACT_Data_2017, ACT_Data_2018, ACT_Data_2019, ACT_Data_2020, ACT_Data_2021)  
ACT_Data_Bind <- ACT_Data_Bind %>% rename(
  "UNITID" = "Unit.Id")
Admission_Data <- rbind(Admission_Data_2017, Admission_Data_2018, Admission_Data_2019, Admission_Data_2020, Admission_Data_2021)
Admission_Data <- Admission_Data %>% rename(
  "UNITID" = "Unit.Id")
SAT_Data_Bind$UNITID <- as.character(SAT_Data_Bind$UNITID)
ACT_Data_Bind$UNITID <- as.character(ACT_Data_Bind$UNITID)
Admission_Data$UNITID <- as.character(Admission_Data$UNITID)
Student_aid_Data$UNITID <- as.character(Student_aid_Data$UNITID)
#in_state_tuition_Data
#out_state_tuition_Data
Enrollment_Data$UNITID <- as.character(Enrollment_Data$UNITID)

Missing_Val_Analysis_table <- left_join(Enrollment_Data, Student_aid_Data, by = c("UNITID", "year"))
Missing_Val_Analysis_table <- left_join(Missing_Val_Analysis_table, Admission_Data, by = c("UNITID", "year"))
Missing_Val_Analysis_table <- left_join(Missing_Val_Analysis_table, in_state_tuition_Data, by = c("UNITID", "year"))
Missing_Val_Analysis_table <- left_join(Missing_Val_Analysis_table, out_state_tuition_Data, by = c("UNITID", "year"))
Missing_Val_Analysis_table <- left_join(Missing_Val_Analysis_table, SAT_Data_Bind, by = c("UNITID", "year"))
Missing_Val_Analysis_table <- left_join(Missing_Val_Analysis_table, ACT_Data_Bind, by = c("UNITID", "year"))



missing_Val_2017 <- plot_missing(Missing_Val_Analysis_table[Missing_Val_Analysis_table$year == 2017, c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
missing_Val_2017 

missing_Val_2018 <- plot_missing(Missing_Val_Analysis_table[Missing_Val_Analysis_table$year == 2018, c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
missing_Val_2018

missing_Val_2019 <- plot_missing(Missing_Val_Analysis_table[Missing_Val_Analysis_table$year == 2019, c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
missing_Val_2019 

missing_Val_2020 <- plot_missing(Missing_Val_Analysis_table[Missing_Val_Analysis_table$year == 2020, c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
missing_Val_2020 

missing_Val_2021 <- plot_missing(Missing_Val_Analysis_table[Missing_Val_Analysis_table$year == 2021, c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
missing_Val_2021


University_info$UNITID <- as.character(University_info$UNITID)
Missing_Val_school_Type <- left_join(Missing_Val_Analysis_table, University_info, by = c("UNITID"))
Missing_Val_fpPrivate <- plot_missing(Missing_Val_school_Type [Missing_Val_school_Type$Institution_type == "Private, for-profit", c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
Missing_Val_fpPrivate 

Missing_Val_npPrivate <- plot_missing(Missing_Val_school_Type [Missing_Val_school_Type$Institution_type == "Private, nonprofit", c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
Missing_Val_npPrivate 

Missing_Val_public <- plot_missing(Missing_Val_school_Type [Missing_Val_school_Type$Institution_type == "Public", c('ACT', 'SAT', 'Total_Admission', 'Student Aid', 'In_state_tuition', 'Out_state_tuition', 'Enrollment')], percent = TRUE)
Missing_Val_public
