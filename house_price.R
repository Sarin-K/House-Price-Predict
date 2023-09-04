library(tidyverse)
library(caret)

df <- read_csv("house_price_2016.csv")

# Check for row with missing values
df %>% 
  complete.cases() %>%
  mean()

# Plot distribution of Price
hist(df$Price)

df$Price <- log(df$Price)
hist(df$Price)

split_df <- function(df,train_size=0.8){
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n,size= n*train_size)
  train_df <- df[id,]
  test_df <- df[-id,]
  list(train=train_df,test=test_df)
}

# Select variable
df <- df %>% 
  select(`number of bedrooms`,
         `living area`,
         `number of floors`,
         `grade of the house`,
         `Area of the house(excluding basement)`,
         Price)

prep_df <- split_df(df)  

train_df <- prep_df[[1]]
test_df <- prep_df[[2]]

## train model
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)


## ridge/ lasso regression

my_grid <- expand.grid(alpha = 0:1,
                       lambda = seq(0.0005,0.05,length = 20))

set.seed(42)
glmnet_model <- train(Price ~ .,
                      data = train_df,
                      method = "glmnet",
                      preProcess =  c("center","scale"),
                      tuneGrid = my_grid,
                      trControl = ctrl)

test_p <- predict(glmnet_model,newdata = test_df)

error <- test_df$Price - test_p

mae <- mean(abs(error))
mse <- mean(error **2)
rmse <- sqrt(mean(error **2))

train_rmse <- min(glmnet_model$results[,'RMSE'])

best_para <- glmnet_model$results %>% 
  filter(RMSE == train_rmse)

train_mae <- best_para['MAE'][[1]]

test_df$predict_price <- test_p
glimpse(test_df)
View(test_df)

true_price_err <- exp(test_df$Price) - exp(test_df$predict_price)
mean(abs(true_price_err))
