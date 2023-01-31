library(targets)
source("funs/train-data.R")
source("funs/test-data.R")
source("funs/def-models.R")
source("funs/def-recipes.R")
source("funs/utility.R")


tar_option_set(packages = c("tidyverse", "tidymodels", "tidytext", "SnowballC", "easystats", "textrecipes", "remoji", "fastrtext"))

# Pipeline
list(
  tar_target(data_train, read_train_data()),
  tar_target(data_test, read_test_data()),
  
  tar_target(recipe1, def_recipe1(data_train)),
  tar_target(rec1_prepped, prep(recipe1)),
  tar_target(rec1_baked, bake(rec1_prepped, new_data = NULL)),
  
  tar_target(recipe2, def_recipe2(data_train)),
  tar_target(rec2_prepped, prep(recipe2)),
  tar_target(rec2_baked, bake(rec2_prepped, new_data = NULL)),
  
  tar_target(recipe3, def_recipe3(data_train)),
  tar_target(rec3_prepped, prep(recipe3)),
  tar_target(rec3_baked, bake(rec3_prepped, new_data = NULL)),
  
  tar_target(model_glm, def_model_glm()),
  tar_target(model_boost, def_model_boost()),
  
  tar_target(wf_set, workflow_set(preproc = list(recipe1 = recipe1, recipe2 = recipe2, recipe3 = recipe3),
                                  models = list(model_glm = model_glm, model_boost = model_boost),
                                  cross = TRUE)),

  tar_target(wf_set_adjusted, wf_set %>%
               option_add(grid = 10, id = str_match(wf_set$wflow_id, ".*model_boost$")) %>%
               option_add(grid = lambda_grid, id = str_match(wf_set$wflow_id, ".*model_glm$"))),

  tar_target(wf_set_fit,
             workflow_map(wf_set_adjusted, fn = "tune_grid", resamples = vfold_cv(data_train, v = 10, strata = c1), verbose = TRUE)),
  
  tar_target(do_autoplot, autoplot(wf_set_fit)),
  tar_target(train_metrics, wf_set_fit %>%
               collect_metrics() %>%
               filter(.metric == "roc_auc") %>%
               arrange(-mean)),
  tar_target(best_wf_id, train_metrics %>% 
               slice_head(n = 1) %>% 
               pull(wflow_id)),
  tar_target(best_wf, wf_set_fit %>%
               extract_workflow(best_wf_id)),
  tar_target(best_wf_fit, wf_set_fit %>% 
               extract_workflow_set_result(best_wf_id)),
  tar_target(best_wf_finalized, best_wf %>% 
               finalize_workflow(select_best(best_wf_fit))),
  tar_target(last_fit,
             fit(best_wf_finalized, data_train)),
  tar_target(test_predicted,
             bind_cols(data_test, predict(last_fit, new_data = data_test)) %>% 
               mutate(c1 = factor(c1))),
  tar_target(test_metrics, test_predicted %>% 
               metrics(c1, .pred_class))
)
