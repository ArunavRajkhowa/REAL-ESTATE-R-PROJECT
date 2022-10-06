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

# 1) Should we use Address as is in the modeling process? 
# Ans: No table(train$Address)

# 2) Find the variance of the target variable 'Price'.
# Ans : 432958829215 var(train$Price)

# 3) Find out how many observations have missing values for variable 'YearBuilt'?
# Ans: 3717  sum(is.na(train$YearBuilt))

# 4) What is the difference in average pTrice between house type h and t?
# Ans: 0.5151274 
if (train$Type=="h"){
  a1=mean(train$Price,na.rm=T)
}
aa= 1069723
else (train$Type =='t'){
  a2=mean(train$Price,na.rm=T)
}

ifelse(train$Type=="h",mean(train$Price,na.rm=T),1)
ifelse(train$Type=="t",mean(train$Price,na.rm=T),1)


a1=ifelse(train$Type =='h',mean(train$Price,na.rm=T),ifelse())
 a2=mean(train$Type =='t')
 a1-a2

# 5) How many unique values variable postcode takes?
# ANs: 94 length(unique(train$Postcode))

# 6) how should you treat post code . As a categorical variable or numeric variable 
# ( write "categorical" or "numeric" as your answer)
# Ans : categorical


# 7) Does distance follow a normal distribution?
# Ans: no ggplot(train,aes(x=Distance)) + geom_density()

# 8) Which seller has maximum value transactions? ( Sum of Price)
# ANs: Jellis
# temp=train %>% 
#       select(SellerG,Price) %>% 
#       group_by(SellerG) %>% 
#       
#   summarise(max_price_sum=sum(Price)) %>% 
#   arrange(max_price_sum,desc(max_price_sum))

# 9) Which CouncilArea has maximum average price?
# ANs: Bayside
  temp=train %>%select(Price,CouncilArea) %>%  
 group_by(CouncilArea) %>% mutate(meanprice= mean(Price,na.rm=T))%>%
  arrange(CouncilArea,desc(meanprice))


# 10) which CouncilArea has maximum variance in the price?
# Ans: Stonnington
  
  
  
#### Part 2 starts -------------------------------------
  test$Price=NA 
  train$data='train' #creating placeholders
  test$data='test'   #creating placeholders
  
  df=rbind(train,test)
  glimpse(df)
  vis_dat(df)

  
# Data Preparation
  df = df %>% select(-Address) #dropping address
  df$Price=as.numeric(df$Price) #treating response  column separately

    dp_pipe=recipe(Price~.,data=df) %>%
 
    update_role(Suburb,Type,Method,SellerG,
                CouncilArea,new_role="to_dummies") %>% 

    step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
    step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
    step_dummy(has_role("to_dummies")) %>%
    step_impute_median(all_numeric(),-all_outcomes())
  
  dp_pipe=prep(dp_pipe)
  
  prep_df=bake(dp_pipe,new_data=NULL)
  #test=bake(dp_pipe,new_data=test)
  
vis_dat(prep_df) #all looks good!

#dropping NA response rows in training set
prep_df=prep_df[!((is.na(prep_df$Price)) & prep_df$data=='train'), ]


#separating train and test dataset
train=prep_df %>% filter(data=='train') %>% select(-data)
test=prep_df %>% filter(data=='test') %>% select(-data,-Price)
vis_dat(test)
# Data Prep ends here



