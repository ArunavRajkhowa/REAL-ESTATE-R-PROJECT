# In this problem we will build a model to predict the "Prices" 
#### importing libraries--------------------------------
library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC) # for AOC curve
library(ggplot2)
library(tidyr)
library(ROCit)
library(dplyr)


### reading the files-----------------------------------
setwd("D:\\IITK Data Analytics\\R\\REAL-ESTATE-R-PROJECT")
filetrain='housing_train.csv'
filetest='housing_test.csv'
train=read.csv(filetrain,stringsAsFactors = F)
test=read.csv(filetest,stringsAsFactors = F)

### studying the dataset-----------------------------
str(train)
glimpse(train)
vis_dat(train)
# 
# Price : numeric :: This is the "target variable", price of the property 

# Suburb : categorical :: Which subsurb the property is located in 
# Address : categorical :: short address
# Type : categorical :: type of the property
# Method : categorical :: method for selling 
# SellerG : categorical :: Name of the seller 
# Postcode : categorical :: postcode of the property
# CouncilArea : categorical :: council area to which the propery belongs

# Rooms : numeric :: Number of Rooms
# Distance : numeric :: distance from the city center
# Bedroom2 : Numeric :: numbers of secondary bedrooms (this is different from rooms)
# Bathroom : numeric :: number of bathrooms
# Car : numeric :: number of parking spaces
# Landsize : numeric :: landsize
# BuildingArea : numeric :: buildup area
# YearBuilt : numeric :: year of building 






# Part 1 : Quiz --------------------------------