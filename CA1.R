#CA1 Data Exploration & Preparation // Dr.Muhammad lqbal 
# Heloisa Eugenio- 2023264

#===============================================================================
# 1. DATA EXPLORATION AND PREPARATION
#==============================================================================+
  # 1.1 Data Loading and Preparation 

students <- read.csv("data.csv", sep = ";")

# Displaying the first six rows 
head(students)

# Numbers of rows and columns
dim(students)

# All column names
colnames(students)

# Checking the structure and data types of each variable
str(students)

# Show a summary of the data
summary(students)

#-------------------------------------------------------------------------------
  # 1.2 Data Cleaning // Checking for

# Missing values
sum(is.na(students))

# Missing values in each column
colSums(is.na(students))

# Duplicate rows
sum(duplicated(students))

#-------------------------------------------------------------------------------
  # 1.3 Outlier Detection 

# Check outliers in students' ages
boxplot(students$Age.at.enrollment,
        main = "Age at Enrollment",
        ylab = "Age (Years)",
        col = "#A8DCC8")

# Age: Some students are much older than the majority, with ages reaching 70.


# Check outliers in admission grades
boxplot(students$Admission.grade,
        main = "Admission Grade",
        ylab = "Admission Grade",
        col ="#A8DCC8")

# Admission grade: Some students have unusually high admission grades, reaching 190.

# No missing values or duplicate were found.
# Outliers were identified in age and admission grades.
# These values were kept because they may represent real students.
# Removing them could exclude useful information from the analysis.

#===============================================================================
# 2. EXPLORATORY DATA ANALYSIS (EDA)
#===============================================================================
  # 2.1 Student Academic Outcomes 

library(ggplot2)

# Comparing the number of students by academic outcome
ggplot(students, aes(x = Target, fill = Target)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("lightpink", "lightyellow", "lightblue")) +
  labs(title = "Student Academic Outcomes",
       x = "Academic Outcome",
       y = "Number of Students") +
  theme_minimal() +
  theme(legend.position = "none")

# Research Question: What are the students' academic outcomes?
# Findings: 49.93% graduated, 32.12% dropped out, and 17.95% remained enrolled.
# Approximately one-third of students dropped out.

#-------------------------------------------------------------------------------
  # 2.2 Admission Grade Distribution

# Show the distribution of admission grades
ggplot(students, aes(x = Admission.grade)) +
  geom_histogram(bins = 25,
                 fill = "#B8A9E8",
                 color = "black") +
  labs(title = "Distribution of Admission Grades",
       x = "Admission Grade",
       y = "Number of Students") +
  theme_minimal()

# Research Question: How are admission grades distributed?
# Findings: Most admission grades are concentrated between 115 and 140.
# Very high admission grades are less common.

#-------------------------------------------------------------------------------
  # 2.3 Age vs Academic Outcome

# Compare students' ages by academic outcome
ggplot(students, aes(x = Target,
                     y = Age.at.enrollment,
                     fill = Target)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c(
    "Dropout" = "lightpink",
    "Enrolled" = "lightyellow",
    "Graduate" = "lightblue"
  )) +
  labs(title = "Age by Academic Outcome",
       x = "Academic Outcome",
       y = "Age at Enrollment") +
  theme_minimal() +
  theme(legend.position = "none")

# Research Question: Does age vary across academic outcomes?
# Findings: Students who dropped out were usually older.
# The drop out group also showed a wider variation in age.

#-------------------------------------------------------------------------------
  # 2.4 Scholarship Status vs Academic Outcome

# Compare academic outcomes by scholarship status
ggplot(students, aes(x = factor(Scholarship.holder),
                     fill = Target)) +
  geom_bar(position = "fill", color = "black") +
  scale_x_discrete(labels = c("No Scholarship", "Scholarship Holder")) +
  scale_fill_manual(values = c(
    "Dropout" = "lightpink",
    "Enrolled" = "lightyellow",
    "Graduate" = "lightblue"
  )) +
  labs(title = "Academic Outcomes by Scholarship Status",
       x = "Scholarship Status",
       y = "Proportion of Students",
       fill = "Academic Outcome") +
  theme_minimal()

# Research Question: Does having a scholarship affect student results?
# Findings: Students with scholarship were more likely to graduates.
# Students without scholarships had a higher chance to drop out.

#-------------------------------------------------------------------------------
  # 2.5 Tuition Fees vs Academic Outcome

# Compare academic outcomes by tuition fee status
ggplot(students, aes(x = factor(Tuition.fees.up.to.date),
                     fill = Target)) +
  geom_bar(position = "fill", color = "black") +
  scale_x_discrete(labels = c("Not Up to Date", "Up to Date")) +
  scale_fill_manual(values = c(
    "Dropout" = "lightpink",
    "Enrolled" = "lightyellow",
    "Graduate" = "lightblue"
  )) +
  labs(title = "Academic Outcomes by Tuition Fee Status",
       x = "Tuition Fee Status",
       y = "Proportion of Students",
       fill = "Academic Outcome") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Research Question: Is tuition fee status associated with academic outcomes?
# Findings: Students with unpaid fees had a much higher drop out proportion.
# Students with fees up to date had a higher graduation proportion.

#-------------------------------------------------------------------------------
  # 2.6 Admission Grades by Academic Outcome

ggplot(students, aes(x = Target, y = Admission.grade,
                     fill = Target)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Dropout" = "lightpink",
                               "Enrolled" = "lightyellow",
                               "Graduate" = "lightblue")) +
  labs(title = "Admission Grades by Academic Outcome",
       x = "Academic Outcome",
       y = "Admission Grade") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Research Question: Do admission grades vary across academic outcomes?
# Findings: Graduates had slightly higher admission grades.
# However, admission grades were similar across all three groups.

#------------------------------------------------------------------------------- 
  # 2.7 First vs Second Semester Grades

ggplot(students, aes(x = Curricular.units.1st.sem..grade.,
                     y = Curricular.units.2nd.sem..grade.)) +
  geom_point(color = "#B8A9E8", alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE, color = "#7B68B5") +
  labs(title = "First vs Second Semester Grades",
       x = "First Semester Grade",
       y = "Second Semester Grade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Research Question: Are first and second semester grades related?
# Findings: Higher first-semester grades were associated with higher
# second-semester grades, showing a positive relationship.
# Some zero grades may influence the trend line.

#-------------------------------------------------------------------------------
  # 2.8 EDA Summary

# Student dropout was associated with several factors.
# Dropouts were generally older than graduates.
# Students with scholarships were more likely to graduate.
# Students with unpaid fees were more likely to drop out. 
#Graduates had slightly higher admission grades. 
# First and second semester grades had a positive relationship.
# These results show relationships, not direct causes.


