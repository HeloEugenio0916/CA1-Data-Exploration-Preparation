#CA1 Data Exploration & Preparation // Dr.Muhammad lqbal 
# Heloisa Eugenio- 2023264

#-------------------------------------------------------------------------------
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
