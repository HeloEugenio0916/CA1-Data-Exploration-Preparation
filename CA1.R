#CA1 Data Exploration & Preparation // Dr.Muhammad lqbal 
# Heloisa Eugenio- 2023264

#==============================================================================
  # 1. Data Loading and Preparation 

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

#==============================================================================
  # 2. Data Cleaning // Checking for

# Missing values
sum(is.na(students))

# Missing values in each column
colSums(is.na(students))

# Duplicate rows
sum(duplicated(students))

#===============================================================================
  # 3. Outliers   

# Check outliers in students' ages
boxplot(students$Age.at.enrollment,
        main = "Age at Enrollment",
        ylab = "Age (Years)",
        col = "lightpink")

# Age: Some students are much older than the majority, with ages reaching 70.


# Check outliers in admission grades
boxplot(students$Admission.grade,
        main = "Admission Grade",
        ylab = "Admission Grade",
        col = "lightpink")

# Admission grade: Some students have unusually high admission grades, reaching 190.

#===============================================================================
  # 4. Exploratory Data Analysis (EDA)

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

#===============================================================================


