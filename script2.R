# x <- parallel::detectCores(logical = FALSE)
# doParallel::registerDoParallel(x)

rm(list = ls())
library(tictoc)
library(dplyr)
library(readr)
# library(ggplot2)
library(tidymodels)
# library(patchwork)
# library(ggcorrplot)
library(baguette)
library(poissonreg)
tidymodels_prefer()

tic()
url = 'https://raw.githubusercontent.com/eraldof/tcc/main/base_tratada.csv'
dados <- read.csv(url, sep = ";")
colnames(dados) <- c("codcli", "dcadastro", "d1", "d2", "d3", "dmedia", 
                     "ddesvio", "freq", "tgasto", "Y", "VL_DEV", "QT_DEV", "COMPROU", 
                     "r", "f", "v", "rfv", "clusters")

# 
# dados_censurados <- base_tratada %>%
#   filter_all(any_vars(is.na(.)))
#
dados <- dados %>%
  mutate(Y = ifelse(is.na(Y), 999, Y))


#dados1 <- dados %>% select("r", "f", "v", "d1", "d2", "d3", "VL_DEV", "QT_DEV", "dcadastro", "tgasto", "freq", "Y")
# dados2 <- dados %>% select("r", "f", "v", "dmedia", "ddesvio","VL_DEV", "QT_DEV", "dcadastro", "tgasto", "freq", "Y")
# dados3 <- dados %>% select("d1", "d2", "d3", "dcadastro", "tgasto", "freq", "clusters", "Y")
# dados4 <- dados %>% select("dmedia", "ddesvio", "dcadastro", "tgasto", "freq", "clusters", "Y")
# dados5 <- dados %>% select("rfv", "d1", "d2", "d3","VL_DEV", "QT_DEV", "dcadastro", "tgasto", "freq", "Y")
dados1 <- dados %>% select(c("rfv", "dmedia", "ddesvio", "dcadastro", "tgasto", "freq", "VL_DEV", "QT_DEV", "Y"))


set.seed(2024)
splitted <- initial_split(dados1, prop = 0.7, strata = "Y")
treinamento <- training(splitted)


## Com o banco1
receita <- recipe(formula = `Y` ~ . , dados1) %>% 
  step_poly(all_numeric_predictors(), degree = 2) %>%
  #step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

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

vfold <- 
  treinamento %>% 
  vfold_cv(v = 10L, strata = Y)

metrica <- metric_set(rmse)


all_wf <-
  workflow_set(
    preproc = list(receita),
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

tunning <- 
  all_wf %>% 
  workflow_map(
    verbose = TRUE,
    seed = 2024,
    resamples = vfold,
    control = grid_control,
    grid = 50L,
    metrics = metrica,
    
  )

autoplot(
  tunning,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
) + 
  labs(title = "Melhor resultado dos modelos")+ ylab("rmse")+ xlab("Ranking")

toc()

final <- tunning %>% extract_workflow("modelo_rf") %>% 
            finalize_workflow(tunning %>% 
                                extract_workflow_set_result("modelo_rf") %>%
                                select_best(metric = "rmse")
                              ) %>% 
            last_fit(splitted)

final$.metrics

