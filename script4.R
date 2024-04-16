doParallel::registerDoParallel(parallel::detectCores(logical = FALSE))


library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)
library(patchwork)
library(ggcorrplot)
library(baguette)
library(poissonreg)
tidymodels_prefer()


rm(list = ls())
dados <- read.csv('https://raw.githubusercontent.com/eraldof/tcc/main/base_tratada_v2_kmeans.csv')

colnames(dados) <- c("codcli", "dcadastro", "d1", "d2", "d3", "dmedia", 
                     "ddesvio", "r", "f", "v", "Y", "VL_DEV", "QT_DEV", 
                     "COMPROU", "clr", "clf", "clv", "rfv", "clusters")

## Imputar dias para os clientes que não voltaram a comprar, acaba implicando em um aumento
## no RMSE. 
# 
# dados <- dados %>%
#   mutate(Y = ifelse(is.na(Y),0, Y))

dados <- dados %>%
  filter_all(all_vars(!is.na(.)))


# dados_censurados <- base_tratada %>%
#   filter_all(any_vars(is.na(.)))
#



dados1 <- dados %>% select("r", "f", "v", "d1", "d2", "d3", "VL_DEV", 
                           "QT_DEV", "dcadastro", "dmedia", "ddesvio" , "Y")

dados2 <- dados %>% select("clr", "clf", "clv", "dmedia", "ddesvio",
                           "VL_DEV", "QT_DEV", "dcadastro","d1", "d2", "d3", "Y")

dados3 <- dados %>% select("d1", "d2", "d3", "dcadastro", "f", "v", "clusters", "Y")

dados4 <- dados %>% select("dmedia", "ddesvio", "dcadastro", "v", "f", "r", "clusters", "Y")

dados5 <- dados %>% select("rfv", "d1", "d2", "d3", "VL_DEV", 
                           "QT_DEV", "dcadastro", "dmedia", "ddesvio", "Y")

dados6 <- dados %>% select("rfv", "dmedia", "ddesvio", "dcadastro", 
                           "d1", "d2", "d3", "VL_DEV", "QT_DEV", "Y")




set.seed(2024)
splitted1 <- initial_split(dados1, prop = 0.7, strata = "Y")
treinamento1 <- training(splitted1)

splitted2 <- initial_split(dados2, prop = 0.7, strata = "Y")
treinamento2 <- training(splitted2)

splitted3 <- initial_split(dados3, prop = 0.7, strata = "Y")
treinamento3 <- training(splitted3)

splitted4 <- initial_split(dados4, prop = 0.7, strata = "Y")
treinamento4 <- training(splitted4)

splitted5 <- initial_split(dados5, prop = 0.7, strata = "Y")
treinamento5 <- training(splitted5)

splitted6 <- initial_split(dados6, prop = 0.7, strata = "Y")
treinamento6 <- training(splitted6)


#Banco 1
receita1 <- recipe(formula = `Y` ~ . , dados1) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#Banco 2
receita2 <- recipe(formula = `Y` ~ . , dados2) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#Banco 3
receita3 <- recipe(formula = `Y` ~ . , dados3) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#Banco 4
receita4 <- recipe(formula = `Y` ~ . , dados4) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#Banco 5
receita5 <- recipe(formula = `Y` ~ . , dados5) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#Banco 6
receita6 <- recipe(formula = `Y` ~ . , dados6) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#vfold
vv = 10L
#grid
gg = 30L

randomforest <- 
  rand_forest(
    mtry = tune(), 
    trees = tune(), 
    min_n = tune()
  ) %>%
  set_mode("regression") %>% 
  set_engine("ranger")

mlp <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("nnet")

bag_mlp <-
  bag_mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine("nnet")

xgb <-
  boost_tree(
    tree_depth = tune(),
    trees = tune(),
    learn_rate = tune(),
    mtry = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

svm_linear <- 
  svm_linear(
    cost = tune(),
    margin = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("kernlab")

svm_rbf <- 
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune(),
    margin = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("kernlab")

pois <-
  poisson_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

grid_control <- control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "resamples"
)

metrica <- metric_set(rmse)


vfold1 <- 
  treinamento1 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf1 <-
  workflow_set(
    preproc = list(receita1),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning1 <- 
  all_wf1 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold1,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )


vfold2 <- 
  treinamento2 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf2 <-
  workflow_set(
    preproc = list(receita2),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning2 <- 
  all_wf2 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold2,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )


vfold3 <- 
  treinamento3 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf3 <-
  workflow_set(
    preproc = list(receita3),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning3 <- 
  all_wf3 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold3,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )


vfold4 <- 
  treinamento4 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf4 <-
  workflow_set(
    preproc = list(receita4),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning4 <- 
  all_wf4 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold4,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )


vfold5 <- 
  treinamento5 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf5 <-
  workflow_set(
    preproc = list(receita5),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning5 <- 
  all_wf5 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold5,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )


vfold6 <- 
  treinamento6 %>% 
  vfold_cv(v = vv, strata = Y)

all_wf6 <-
  workflow_set(
    preproc = list(receita6),
    models = list(
      modelo_rf = randomforest,
      modelo_mlp = mlp,
      modelo_bagmlp = bag_mlp,
      modelo_poi = pois,
      modelo_svmrbf = svm_rbf,
      modelo_svmlinear = svm_linear,
      modelo_xgb = xgb
    ),
    cross = TRUE
  ) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

tunning6 <- 
  all_wf6 %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold6,
    control = grid_control,
    grid = gg,
    metrics = metrica,
  )

###################

autoplot(
  tunning1,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")


autoplot(
  tunning2,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")


autoplot(
  tunning3,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")

autoplot(
  tunning4,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")

autoplot(
  tunning5,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")


autoplot(
  tunning6,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")


################## 



set.seed(2024)
best1 <- tunning1 %>%
  extract_workflow_set_result("modelo_svmlinear") %>%
  select_best(metric = "rmse")

wf_final1 <-
  tunning1 %>% extract_workflow("modelo_svmlinear") %>%
  finalize_workflow(best1)

teste1 <-
  wf_final1 %>%
  last_fit(split = splitted1)


set.seed(2024)
best2 <- tunning2 %>%
  extract_workflow_set_result("modelo_rf") %>%
  select_best(metric = "rmse")

wf_final2 <-
  tunning2 %>% extract_workflow("modelo_rf") %>%
  finalize_workflow(best2)

teste2 <-
  wf_final2 %>%
  last_fit(split = splitted2)


set.seed(2024)
best3 <- tunning3 %>%
  extract_workflow_set_result("modelo_xgb") %>%
  select_best(metric = "rmse")

wf_final3 <-
  tunning3 %>% extract_workflow("modelo_xgb") %>%
  finalize_workflow(best3)

teste3 <-
  wf_final3 %>%
  last_fit(split = splitted3)


set.seed(2024)
best4 <- tunning4 %>%
  extract_workflow_set_result("modelo_svmlinear") %>%
  select_best(metric = "rmse")

wf_final4 <-
  tunning4 %>% extract_workflow("modelo_svmlinear") %>%
  finalize_workflow(best4)

teste4 <-
  wf_final4 %>%
  last_fit(split = splitted4)

set.seed(2024)
best5 <- tunning5 %>%
  extract_workflow_set_result("modelo_rf") %>%
  select_best(metric = "rmse")

wf_final5 <-
  tunning5 %>% extract_workflow("modelo_rf") %>%
  finalize_workflow(best5)

teste5 <-
  wf_final5 %>%
  last_fit(split = splitted5)


set.seed(2024)
best6 <- tunning6 %>%
  extract_workflow_set_result("modelo_rf") %>%
  select_best(metric = "rmse")

wf_final6 <-
  tunning6 %>% extract_workflow("modelo_rf") %>%
  finalize_workflow(best6)

teste6 <-
  wf_final6 %>%
  last_fit(split = splitted6)


teste1$.metrics
teste2$.metrics
teste3$.metrics
teste4$.metrics
teste5$.metrics
teste6$.metrics
