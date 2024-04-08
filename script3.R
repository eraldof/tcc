library(randomMachines)
set.seed(24)
test1 <- testing(splitted)
treinamento1 <- training(splitted)


treinamento <- recipe(formula = `Y` ~ . , treinamento1) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% prep() %>% juice() 
  

test <- recipe(formula = `Y` ~ . , test1) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% prep() %>% juice() 



set.seed(24)
x <- randomMachines(`Y`~. , train = treinamento, validation = test, B = 100, automatic_tuning = TRUE)
y_hat <- predict(x, test)
observado <- test$Y
RMSE(y_hat, observado)
