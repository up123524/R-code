install.packages("moments")
library(tidymodels);library(caret);library(lubridate);library(tidyverse)
library(moments); library(corrr); library(randomForest); library(dplyr)
bike<-read_csv("/Users/umar/Documents/practise datasets/Bike-Sharing-Dataset/hour.csv")
bike %>% dim()
View(bike)

bike <- bike %>% mutate(instant=NULL, yr=2011+yr) %>%
  dplyr::rename(
    date=dteday,
    year=yr,
    month=mnth,
    hour=hr,
    weather=weathersit,
    humidity=hum,
    total=cnt)
head(bike)

bike %>%
  pivot_longer(
    cols = c(casual,registered,total),
    names_to = "usertype",
    values_to = "count"
  )%>%
  ggplot(aes(count, colour=usertype))+
  geom_density()+
  labs(title="distribution of the number of rental bikes",
  x="number per hour", y = "density")+
  scale_color_discrete(
    name="User Type",
    breaks=c("casual", "registered", "total"),
    labels=c("non-registered", "registered", "total")
  )


bike_cor <- bike %>% select(where(is.numeric)) %>%
  correlate() %>%
  rearrange(absolute=FALSE)%>%
  shave()
rplot(bike_cor, print_cor = TRUE)

bike_all<- bike %>% select(-registered, -casual)

skewness(bike_all$total)
skewness(log10(bike_all$total))
skewness(log1p(bike_all$total))
skewness(sqrt(bike_all$total))
skewness(bike_all$total^(1/3))
bike_all$total<-bike_all$total^(1/3)

str(bike_all)

bike_all$season<- factor(
  bike_all$season,
  level=c(1,2,3,4),
  labels=c("spring","summer", "autumn", "winter")
)

bike_all$holiday<-factor(
  bike_all$holiday,
  levels=c(0,1), labels=c("FALSE", "TRUE")
)

bike_all$workingday <- factor(
  bike_all$workingday,
  levels = c(0, 1), labels = c(FALSE, TRUE)
)

bike_all$weather <- factor(
  bike_all$weather,
  levels = c(1, 2, 3, 4),
  labels = c("clear", "cloudy", "rainy", "heavy rain"),
  ordered = TRUE
)

head(bike_all)

set.seed(25)
split<-initial_split(bike_all,prop=0.8)
train_data<-training(split)
train_data%>% dim()

train_cv<-vfold_cv(train_data, v=10)

prep_recipe<-
  recipe(total ~., data=train_data)%>%
  step_rm(year,month,weekday)%>%
  step_date(date)%>%
  step_corr(all_numeric(), threshold=0.8)%>%
  step_dummy(all_nominal())

tree_cp<-seq(0.01,0.1,0.01)

set.seed(25)
tree_tidy_time1<-Sys.time()

##set the model
tree_engine<-
  decision_tree(mode="regression", cost_complexity=tune())%>%
  set_engine("rpart")
###set workflow
tree_workflow<-
  workflow()%>%
  add_recipe(prep_recipe)%>%
  add_model(tree_engine)
##tune parameters with cross-validation
tree_tune<-tune_grid(
  tree_workflow,
  resamples=train_cv,
  grid=data.frame(cost_complexity=tree_cp),
  metrics = metric_set(rmse)
)
##Fit again with the best parameter
tree_best<-finalize_workflow(tree_workflow, select_best(tree_tune))%>%
  fit(train_data)

tree_tidy_time2<-Sys.time()
print(tree_tidy_time2-tree_tidy_time1)

predict_table<-function(model,data, tidy_flag){
  if(tidy_flag==TRUE){
    result<-model%>%
      predict(data)%>%
      rename(pred=.pred)%>%
      mutate(
        actual=data$total,
        pred_real=pred^3,
        actual_real=actual^3
      )
  } else {
    result<-model%>%
      predict(data)%>%
      as_tibble_col(column_name="pred")%>%
      mutate(
        actual=data$total,
        pred_real=pred^3,
        actual_real=actual^3
      )
  }
  result
}

pull_rmse<-function(result_table){
  rmse_result<-rmse(result_table, pred, actual) %>%
    pull(.estimate)
  rmse_result_real<-rmse(result_table, pred_real, actual_real)%>%
    result<-c(rmse=rmse_result, real_rmse=rmse_result+real)
}

tree_tidy_train_pred<-predict_table(tree_best, train_data, TRUE)
tree_tidy_train_rmse<-pull_rmse(tree_tidy_train_pred)






