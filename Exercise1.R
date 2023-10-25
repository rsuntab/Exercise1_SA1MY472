#EXERCISE 1 - SUMMATIVE ASSIGNMENT 1 MY472
#Raquel Lorenzo Mesa
#2 November 2023


#As indicated in the exercise, we construct an R script defining a function with a single
#argument 'data', that takes a dataset and performs some input transformation on it. 

#For this, we will use a built-in dataset. First, we select it: 
data()
?UCBAdmissions

#We choose the dataset with which we will work: 'UCBAdmissions'. This dataset contains
#aggregate data on applicants to graduate school at Berkeley for the six largest 
#departments in 1973 classified by admission and sex. It contains 4526 observations on 
#3 different variables. 
adminberk <- as.data.frame(UCBAdmissions)
View(adminberk)

#We defined the function as requested in the exercise.
#First, we load the needed library 
library(dplyr)

#In this function, we try to group the information by gender and department. 
#We want to see the proportion of admitted among the applicants. 
#For this, we calculate the percentage. 
admission_percentages <- function(data, option = "admis_percentages") {
  transformed_data <- data %>%
      group_by(Gender, Dept) %>%
      summarize(
        Applicants = sum(Freq),
        Admitted = sum(Freq[Admit == "Admitted"]),
        Admission_Percentage = (Admitted / Applicants) * 100
      )
  return(transformed_data)
}

result <- admission_percentages(adminberk)
print(result)
