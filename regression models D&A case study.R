####### some regression models, easily modified for many more metric/dimension combinations
gadata <- google_analytics(ga_id, 
                           date_range = c("2022-03-05", "2022-07-05"), 
                           metrics = c("ga:pageviewsPerSession", "ga:avgTimeOnPage", "ga:goalCompletionsAll"),
                           dimensions = c("date", "ga:userGender") 
)
head(gadata)
view(gadata)
str(gadata)

#all numeric predictors
gadata$date<-as.numeric(gadata$date)

#####dummy variable coding, convert characters into factors(see from str())
gadata$userGender<-as.factor(gadata$userGender)

######for glm if using default family,however ive used poisson
meangca<-mean(gadata$goalCompletionsAll)
gadata$goalCompletionsAll <- as.numeric(gadata$goalCompletionsAll>=meangca)
head(gadata)

####generalised linear model
a <-glm(gadata$goalCompletionsAll ~ ., gadata, family = "poisson")
summary(a)
plot(a)


###linear regression
b<-lm(gadata$goalCompletionsAll ~ ., gadata)
summary(b)
plot(b)

######levels of multi-collinearity
ggpairs(gadata)
##test levels of multi-collinearity in regression model
vif_values <- vif(b)
barplot(vif_values, main = "VIF Values", horiz=TRUE, col = "steel blue")
abline(v=5, lwd=3, lty=2)
####if levels are significant try lasso regression model(at the end of script)
##use MASS package
data_x<-gadata[,3:5]
var<-cor(data_x)
varinv<-ginv(var)
colnames(varinv) <- colnames(data_x)                
rownames(varinv) <- colnames(data_x)
corrplot(varinv,method='number',is.corr = F) 
remove.packages("MASS")
###########################################

###further QQ plot analysis
QQ2<-density(b$residuals)
plot(QQ2,main='Residual pdf shape Plot',xlab='Residuals')
####should roughly resemble bell curve if lm model can be used

#############################################exits
gadata <- google_analytics(ga_id, 
                           date_range = c("2022-03-05", "2022-07-05"), 
                           metrics = c("ga:pageviewsPerSession", "ga:avgTimeOnPage", "ga:exits"),
                           dimensions = c("date", "ga:userGender") 
)
head(gadata)
view(gadata)

#all numeric predictors
gadata$date<-as.numeric(gadata$date)


#############dummy variable coding(use to test if a logistic regression model is better)
gadata$userGender<-as.factor(gadata$userGender)

##########for glm
meangca<-mean(gadata$exits)
gadata$exits <- as.numeric(gadata$exits>=meangca)
head(gadata)

a <-glm(gadata$exits ~ ., gadata, family = "poisson")
summary(a)
plot(a)
##if binary response
a <-glm(gadata$exits ~ ., gadata)
summary(a)
plot(a)

#########linear regression model
b<-lm(gadata$exits ~ ., gadata)
summary(b)
plot(b)

############### Age bracket dummy coding
gadata <- google_analytics(ga_id, 
                           date_range = c("2022-03-05", "2022-07-05"), 
                           metrics = c("ga:pageviewsPerSession", "ga:avgTimeOnPage", "ga:goalCompletionsAll"),
                           dimensions = c("date", "ga:userAgeBracket") 
)
head(gadata)
view(gadata)

######levels of multi-collinearity
ggpairs(gadata)

#all numeric predictors
gadata$date<-as.numeric(gadata$date)

##dummy variable coding
gadata$userAgeBracket<-as.factor(gadata$userAgeBracket)
view(gadata)

####goal completions regression model
meangca<-mean(gadata$goalCompletionsAll)
gadata$goalCompletionsAll <- as.numeric(gadata$goalCompletionsAll>=meangca)
head(gadata)

a <-glm(gadata$goalCompletionsAll ~ ., gadata, family = "poisson")
summary(a)
plot(a)

#########linear regression model
b<-lm(gadata$goalCompletionsAll ~ ., gadata)
summary(b)
plot(b)

##############################exits age bracket
gadata <- google_analytics(ga_id, 
                           date_range = c("2022-03-05", "2022-07-05"), 
                           metrics = c("ga:pageviewsPerSession", "ga:avgTimeOnPage", "ga:exits"),
                           dimensions = c("date", "ga:userAgeBracket") 
)
head(gadata)
view(gadata)



######levels of multi-collinearity
ggpairs(gadata)

#all numeric predictors
gadata$date<-as.numeric(gadata$date)

##dummy variable coding
gadata$userAgeBracket<-as.factor(gadata$userAgeBracket)
view(gadata)

####goal completions regression model
meangca<-mean(gadata$exits)
gadata$exits<- as.numeric(gadata$exits>=meangca)
head(gadata)

a <-glm(gadata$exits ~ ., gadata, family = "poisson")
summary(a)
plot(a)

#########linear regression model
b<-lm(gadata$exits ~ ., gadata)
summary(b)
plot(b)

############csv dna customer appointments regression model(csv file can be loaded
#from the master script)
dna_data<- page_data %>%
  select("Gender", "Total.Appointments")

dna_data <- dna_data [c(1:249),]
view(dna_data)

dna_data1 <- dna_data %>%
  filter(!is.na(Total.Appointments))
head(dna_data1)
view(dna_data1)

dna_data1$Gender <-as.factor(dna_data1$Gender)

testlm <- lm(Total.Appointments ~ ., dna_data1)
summary(testlm)

#####lasso and ridge regression, better for prediction models over inference
#####use when data has high multi-collinearity, lots of not useful variables
gadata <- google_analytics(ga_id, 
                           date_range = c("2022-03-05", "2022-07-05"), 
                           metrics = c("ga:pageviewsPerSession", "ga:avgTimeOnPage", "ga:goalCompletionsAll"),
                           dimensions = c("date", "ga:userGender") 
)
head(gadata)
view(gadata)
str(gadata)

#define response variable
y <- gadata$goalCompletionsAll

###predictor variables
x <- data.matrix(gadata[, c('pageviewsPerSession', 'avgTimeOnPage', 'userGender')])

#perform k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
####dot implies variable is not usefull as coefficient has shrunk to 0
#to make predictions from this model
new_data<-matrix(c(2.5,56.2,2), nrow=1,ncol=3)
new<-predict(best_model, s=best_lambda, newx=new_data)

###evaluation
y_pred<-predict(best_model, s=best_lambda, newx=x)
sst<-sum((y-mean(y))^2)
sse<-sum((y_pred-y)^2)
#r-squared
rsq<-1-sse/sst
rsq
