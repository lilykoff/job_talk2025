library(tidyverse)

get_summarized_predictions = function(predictions, rank = FALSE, exp = FALSE) {
  # predictions is tibble
  # column j is the predictions from fitting the model where the "true subject" is subject j
  # each row is prediction for a given second
  # true subject is the final column

  if (rank) {
    # will return data frame with columns true subject, model, mean prediction, rank of correct prediction,
    # and rank1, rank5 which indicate whether predicted subject was in top 1 or top 5
    cn_new = unique(predictions$true_subject)
    predictions %>%
      magrittr::set_colnames(c(cn_new, "true_subject")) %>%
      janitor::clean_names() %>%
      mutate(true_subject = as.numeric(true_subject)) %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      # get mean probability across seconds for each true subject / model combo
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      mutate(
        rank = rank(-mean_pred)
      ) %>% # get the rank for each prediction
      ungroup() %>%
      filter(model == true_subject) %>% # only keep the correct combos and get ranks
      mutate(
        rank1 = if_else(rank == 1, 1, 0),
        rank5 = if_else(rank <= 5, 1, 0)
      )
  } else {
    cn_new = unique(predictions$true_subject)
    predictions %>%
      magrittr::set_colnames(c(cn_new, "true_subject")) %>%
      janitor::clean_names() %>%
      mutate(true_subject = as.numeric(true_subject))  %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      select(-name) %>%
      # now we have the prediction for each second for each model / true subject combo
      mutate(pred = case_when(exp ~ exp(pred),
                              .default = pred)) %>% # exponentiate based on exp argument
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop") %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = if_else(as.numeric(predicted_sub) == true_subject, 1, 0)) %>%
      ungroup()
  }
}



predictions = read_rds(here::here("docs", "data", "pred_df_xgb_med.rds"))

pred_res = get_summarized_predictions(predictions, rank = TRUE)

cn_new = unique(predictions$true_subject)
mean_preds =
  predictions %>%
  magrittr::set_colnames(c(cn_new, "true_subject")) %>%
  janitor::clean_names() %>%
  mutate(true_subject = as.numeric(true_subject)) %>%
  group_by(true_subject) %>%
  mutate(sec = row_number()) %>%
  pivot_longer(cols = -c("true_subject", "sec"), names_to = "name", values_to = "pred") %>%
  mutate(model = as.numeric(sub(".*x", "", name))) %>%
  select(-name) %>%
  # now we have the prediction for each second for each model / true subject combo
  # exponentiate based on exp argument
  ungroup() %>%
  group_by(true_subject, model) %>%
  # get mean probability across seconds for each true subject / model combo
  summarize(mean_pred = mean(pred, na.rm = TRUE), .groups = "drop")




write_rds(pred_res, here::here("docs", "data", "pred_res_df_xgb.rds"))
write_rds(mean_preds, here::here("docs", "data", "mean_preds_xgb.rds"))

#### accelerometry
library(SummarizedActigraphy)

lily = read_actigraphy(here::here("docs", "data", "lily.gt3x"))

lily_acc =
  lily$data %>%
  fix_zeros()


walking =
  lily_acc %>%
  filter(time>= as.POSIXct("2023-08-14 16:31:42", tz = "GMT")  & time <= as.POSIXct("2023-08-14 16:31:52", tz = "GMT")) %>%
  mutate(
    vm = sqrt(X^2 + Y^2 + Z^2),
    type = "walking",
    t = difftime(time, min(time), units = "secs") %>% as.numeric)
write_rds(walking, here::here("docs", "data", "walking.rds"))

running =
  lily_acc %>%
  filter(
    time >= as.POSIXct("2023-08-15 07:40:00", tz = "GMT")  &
      time <= as.POSIXct("2023-08-15 07:40:10", tz = "GMT")
  ) %>%
  mutate(vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2),
         type = "running",
         t = difftime(time, min(time), units = "secs") %>% as.numeric)

write_rds(running, here::here("docs", "data", "running.rds"))

cooking_small =
  lily_acc %>%
  filter(time>= as.POSIXct("2023-08-14 17:02:10", tz = "GMT")  & time <= as.POSIXct("2023-08-14 17:02:20", tz = "GMT")) %>%
  mutate(
    vm = sqrt(X^2 + Y^2 + Z^2),
    type = "cooking",
    t = difftime(time, min(time), units = "secs") %>% as.numeric)

write_rds(cooking_small, here::here("docs", "data", "cooking.rds"))
driving =
  lily_acc %>%
  filter(time>= as.POSIXct("2023-08-15 20:08:29", tz = "GMT")  & time <= as.POSIXct("2023-08-15 20:08:39", tz = "GMT")) %>%
  mutate(
    vm = sqrt(X^2 + Y^2 + Z^2),
    type = "driving",
    t = difftime(time, min(time), units = "secs") %>% as.numeric)

write_rds(driving, here::here("docs", "data", "driving.rds"))


## process steps data

steps = read_rds(here::here("docs", "data", "steps_covariates.rds"))
steps_long =
 steps %>%
  select(gender, SEQN, age = age_in_years_at_screening, starts_with("min")) %>%
  pivot_longer(cols = starts_with("min"), names_to = "ind", names_transform = ~as.integer(sub(".*min\\_", "", .x))) %>%
     mutate(age_cat = cut(age, breaks = c(0, 18, 30, 40, 50, 60, 70, Inf), include.lowest = TRUE),
             age_cat = factor(age_cat, labels = c("<19", "19-29", "30-39", "40-49", "50-59", "60-69", "70+")))

steps_summ =
  steps_long %>%
  group_by(ind, gender, age, age_cat) %>%
  summarize(mean = mean(value, na.rm = TRUE), .groups = "drop")

write_rds(steps_summ, here::here("docs", "data", "step_means.rds"))

### run svyfosr model
# pak::pak("jhuwit/svyfosr")
library(svyfosr)
model_fit = svyfosr::svyfui()

y_mat = steps %>%
  select(starts_with("min")) %>%
  as.matrix()

steps_df =
  steps %>%
  select(gender,
         age = age_in_years_at_screening,
         strata = masked_variance_pseudo_stratum,
         psu = masked_variance_pseudo_psu,
         weight = full_sample_2_year_mec_exam_weight) %>%
  mutate(weight = weight / 2) %>%
  mutate(steps_mat = y_mat)

model_fit = svyfosr::svyfui(steps_mat ~ age + gender,
                            weights = weight,
                            data = steps_df,
                            family = gaussian(),
                            boot_type = "BRR",
                            num_boots = 500,
                            nknots_min = 15,
                            parallel = FALSE,
                            seed = 2213)

### fprint

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


sample_dat2 = read_rds(here::here("docs", "data", "fingerprint_data_sample_temporal.rds"))
ids = unique(sample_dat2$id)

dens_df =
  sample_dat2 %>%
  group_by(id, second) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80)))


write_rds(dens_df, here::here("docs", "data", "dens_temp_df.rds"))
