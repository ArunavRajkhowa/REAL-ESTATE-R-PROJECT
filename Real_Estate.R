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

  
# Data Preparation--------------------------------------
  #df = df %>% select(-Address) #dropping address
  df$Price=as.numeric(df$Price) #treating response  column separately
  df$Postcode=as.factor(df$Postcode) #treating postcode as category
    dp_pipe=recipe(Price~.,data=df) %>%
      
    update_role(Address,Postcode,new_role = "drop_vars") %>%
    update_role(Suburb,Type,Method,SellerG,
                CouncilArea ,new_role="to_dummies") %>% 
    
    step_rm(has_role("drop_vars")) %>%
    step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
    step_other(has_role("to_dummies"),threshold =0.03,other="__other__") %>% 
    step_dummy(has_role("to_dummies")) %>%
    step_impute_median(all_numeric(),-all_outcomes())
  
  dp_pipe=prep(dp_pipe)
  
  prep_df=bake(dp_pipe,new_data=NULL)
  #test=bake(dp_pipe,new_data=test)
  
vis_dat(prep_df) #all looks good!

#dropping NA response rows in training set
prep_df=prep_df[!((is.na(prep_df$Price)) & prep_df$data=='train'), ]


#separating train and test dataset---------------------------
train=prep_df %>% filter(data=='train') %>% select(-data,-Type_X__other__)
test=prep_df %>% filter(data=='test') %>% select(-data,-Price,-Type_X__other__)
vis_dat(train)
vis_dat(test)
# Data Prep ends here , subtracting typeXother because it contained only 0s


#MODEL  building starts here-----------------------------------------

#### 1st model : simple linear regression------------------------


for_vif=lm(Price~.,data=train)


sort(vif(for_vif),decreasing = T)[1:3]

rm(for_vif)
fit=lm(Price~.,data=train)
fit=stats::step(fit)
summary(fit)

fit=lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
         Car + Landsize + BuildingArea + YearBuilt + Suburb_X__other__ + 
         Type_t + Type_u + Method_S + 
         SellerG_Buxton + SellerG_Jellis + SellerG_Marshall + SellerG_X__other__ + 
         CouncilArea_Banyule + CouncilArea_Bayside + CouncilArea_Boroondara + 
         CouncilArea_Darebin + CouncilArea_Hobsons.Bay + CouncilArea_Maribyrnong + 
         CouncilArea_Melbourne + CouncilArea_Moonee.Valley + CouncilArea_Moreland + 
         CouncilArea_Port.Phillip + CouncilArea_Yarra + 
         CouncilArea_X__other__, data = train)


#PREDICTION
test.predictions=predict(fit,newdata=test)

write.csv(test.predictions,'SimpleLinearRegression.csv',row.names = F)
plot(fit,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit,2) # errors are normal or not

plot(fit,3) # variance is constant or not

plot(fit,4) # outliers in the data if cook's distance >1




### Decision Tree Model------------------------------

tree_model=decision_tree( 
  cost_complexity = tune(), 
  tree_depth = tune(),
  min_n = tune() #min number of obs in a node
) %>%
  set_engine("rpart") %>% #package name rpart , show_engines("decision_tree")
  set_mode("regression") #regression/classification


folds = vfold_cv(train, v = 10)


tree_grid = grid_regular(cost_complexity(), tree_depth(),   # run each ot these indvidually to get idea of range
                         min_n(), levels = 5) #select 3 best values for each of these

# below code runs your code on parallel cores of your cpu
# makes the process faster
# doParallel::registerDoParallel() 


my_res=tune_grid(
  tree_model,
  Price~.,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(rmse,mae), #rmse ,mae for regression
  control = control_grid(verbose = TRUE)
)

autoplot(my_res)+theme_light() #roc higher the better

fold_metrics=collect_metrics(my_res) 

my_res %>% show_best()

final_tree_fit=tree_model %>% 
  finalize_model(select_best(my_res)) %>% 
  fit(Price~.,data=train)

# feature importance
library(vip)
final_tree_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# plot the tree

rpart.plot(final_tree_fit$fit)

# predictions

train_pred=predict(final_tree_fit,new_data = train) 
test_pred=predict(final_tree_fit,new_data = test) 
write.csv(test_pred,'Decision Tree.csv',row.names = F)

#ggplot(train,aes(x=energy))+geom_density()
#ggplot(test_pred,aes(x=.pred))+geom_density()

plot(density(test_pred$.pred))
lines(density(train$Price))




### Random Forest ------------------------------------


rf_model = rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

folds = vfold_cv(train, v = 10)

rf_grid = grid_regular(mtry(c(5,25)), trees(c(100,500)),
                       min_n(c(2,10)),levels = 5)


my_res=tune_grid(
  rf_model,
  Price~.,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(rmse,mae),
  control = control_grid(verbose = TRUE)
)

autoplot(my_res)+theme_light()

fold_metrics=collect_metrics(my_res)

my_res %>% show_best()

final_rf_fit=rf_model %>% 
  set_engine("ranger",importance='permutation') %>% 
  finalize_model(select_best(my_res,"rmse")) %>% 
  fit(Price~.,data=train)

# variable importance 

final_rf_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# predicitons

train_pred=predict(final_rf_fit,new_data = train) 
test_pred=predict(final_rf_fit,new_data = test) 
write.csv(test_pred,'RandomForest.csv',row.names = F)

plot(density(test_pred$.pred))
lines(density(train$Price))








