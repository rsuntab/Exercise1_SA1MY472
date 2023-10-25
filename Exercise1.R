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
  transformed_data <- data
  if (option == "admis_percentages") {
    transformed_data <- data %>%
      group_by(Gender, Dept) %>%
      summarize(
        Applicants = sum(Freq),
        Admitted = sum(Freq[Admit == "Admitted"]),
        Admission_Percentage = (Admitted / Applicants) * 100
      )
  } else if (option == "totals_and_percentages") {
    transformed_data <- data %>%
      group_by(Gender) %>%
      summarize(
        Applicants = sum(Freq),
        Admitted = sum(Freq[Admit == "Admitted"]),
        Admission_Percentage = (Admitted / Applicants) * 100
      )
  }
  return(transformed_data)
}


#We use the function in our database
result <- admission_percentages(adminberk)
print(result)

#Then, we will print the solutions with and without the second argument 
#First, we use the option = "admis_percentages". This result is equal to the default one.
result1 <- admission_percentages(adminberk, option = "admis_percentages")
print(result1)

#Second, we use the option = "totals_and_percentages".
#This will show the total number of applicants and admitted people in the university,
#only by gender 
result2 <- admission_percentages(adminberk, option = "totals_and_percentages")
print(result2)

