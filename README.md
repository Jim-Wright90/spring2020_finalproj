spring2020_finalproj

###Background:
This project is a shiny app that provides a quick look into data from three different years of the School Survey on Crime and Safety (05-06, 07-08, 17-18). The app provides users with all the variables from these datasets that are matched between the different years. We hope this shiny app will be able to provide general associations between continuous variables while being able to choose which year of the dataset the user is interested in as well as looking at differences between categories for any variable of interest. 

### Description of the Data 
The data for our project comes from the 2005-2006 School Survey on Crime and Safety. The data was obtained via [data.gov](https://catalog.data.gov/dataset/2006-school-survey-on-crime-and-safety) and is a large SPSS file with 2,724 rows by 525 columns. The goal of our project will be to explore the relation between various variables within the data set. Specifically, four categorical variables we intend to use in models include: _school enrollment_, _minority percentage school enrollment_, _school urbanicity_, _school level_. These variables will be modeled to predict the following quantitative outcome variable: _out of school suspensions_. 

Further, we will explore the relation between the provision of teacher training on predicting out of school suspensions as well as the relation between the aforementioned categorical variables on predicting the provision of teacher training. The teacher training variables are logical with a binary yes/no coded response. We intend for our final product to identify trends that would be helpful for school administrations to identify what types of teacher trainings may prevent out of school suspensions. 