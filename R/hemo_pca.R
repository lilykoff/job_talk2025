### fPCA with waveforms
library(tidyverse)
library(refund)
library(viridis)
## functions ##
plot_fpca = function(fpca_res, num_eigen = 4, n_plt = 10, seed = 213, pve = FALSE) {
  ncol = dim(fpca_res$Yhat)[2]
  # loop over eigenfunctions

  if (pve){
    total_pve = fpca_res$pve
    pve_vec = 100 * ((fpca_res$evalues / sum(fpca_res$evalues)) * total_pve)
  }
  get_mean_curves = function(k){
    q_k = quantile(fpca_res$scores[,k],c(0.1,0.9))
    # get indices associated with low/high scores
    inx_low_k = which(fpca_res$scores[,k] <= q_k[1])
    inx_high_k = which(fpca_res$scores[,k] > q_k[2])
    # get the average curves in each quantile: 0-.1, .9-1
    mu_low_k = colMeans(fpca_res$Yhat[inx_low_k,]) %>%
      as_tibble() %>%
      mutate(sind = row_number(),
             quantile = "low")
    mu_high_k = colMeans(fpca_res$Yhat[inx_high_k,]) %>%
      as_tibble() %>%
      mutate(sind = row_number(),
             quantile = "high")
    if (pve) {
      return(bind_rows(mu_low_k, mu_high_k) %>%
               mutate(pc = glue::glue("PC ", k, ": ", {round(pve_vec[k], 0)}, "% variance expl.")))

    } else{
      return(
        bind_rows(mu_low_k, mu_high_k) %>%
            mutate(pc = paste0("PC: ", k)))
    }
  }

  get_indiv_curves = function(k, n_plt) {
    q_k = quantile(fpca_res$scores[,k],c(0.1,0.9))
    # get indices associated with low/high scores
    inx_low_k = which(fpca_res$scores[,k] <= q_k[1])
    inx_high_k = which(fpca_res$scores[,k] > q_k[2])
    mu_low_df =
      fpca_res$Yhat[sample(inx_low_k, size=n_plt),] %>%
      t() %>%
      as_tibble() %>%
      mutate(sind = row_number()) %>%
      pivot_longer(cols = -sind, names_to = "id", values_to = "value", cols_vary  = "slowest", names_transform = ~sub(".*V", "", .x)) %>%
      mutate(quantile = "low")
    mu_high_df =
      fpca_res$Yhat[sample(inx_high_k, size=n_plt),] %>%
      t() %>%
      as_tibble() %>%
      mutate(sind = row_number()) %>%
      pivot_longer(cols = -sind, names_to = "id", values_to = "value", cols_vary  = "slowest", names_transform = ~sub(".*V", "", .x)) %>%
      mutate(quantile = "high")

    if (pve) {
      return(bind_rows(mu_low_df, mu_high_df) %>%
               mutate(pc = glue::glue("PC ", k, ": ", {round(pve_vec[k], 0)}, "% variance expl.")))
    } else {
      return(bind_rows(mu_low_df, mu_high_df) %>%
               mutate(pc = paste0("PC: ", k)))
    }

  }
  df_mu = map_dfr(.x = 1:num_eigen, .f = get_mean_curves)
  set.seed(seed)
  df_ind = map_dfr(.x = 1:num_eigen, .f = get_indiv_curves, n_plt = n_plt)
  list(df_mu = df_mu, df_ind = df_ind)

  p = df_ind %>%
    mutate(id = factor(id),
           id_q = paste0(id, "_", quantile)) %>%
    ggplot() +
    geom_line(aes(x = sind, y = value, group = id_q, color = quantile), alpha=0.25) +
    facet_wrap(~ pc, ncol = 3) +
    labs(x = "Time (sec)", y = "Arterial Blood Pressure (mmHg)") +
    geom_line(data = df_mu,
              aes(x = sind, y = value, group = quantile, color = quantile),
              linewidth = 1.1)

  return(p)
}

### data
x = read_rds(here::here("docs", "data", "aligned_beats_small.rds"))

ids = unique(x$id)

df_small =
  x %>%
  dplyr::filter(id %in% ids[1:5]) %>%
  group_by(id, beat_id) %>%
  dplyr::filter(beat_ind <= 120) %>%
  ungroup()

df_small %>%
  mutate(id = paste0("id: ", id)) %>%
  ggplot(aes(x = beat_ind, y = abp_smooth, group = beat_id)) +
  geom_line(alpha = 0.01) +
  facet_grid(.~id) +
  scale_x_continuous(breaks = seq(0, 120, 30),
                     labels = seq(0, 1, 0.25)) +
  labs(x = "Time (seconds)", y = "Arterial Blood Pressure (mmHg)",
       title = "Aligned heart beats")

df_small =
  x %>%
  dplyr::filter(id %in% ids[1:20]) %>%
  dplyr::filter(beat_ind <= 120) %>%
  mutate(idx = paste0(beat_id,"_", id))

# write_rds(df_small, here::here("docs", "data", "aligned_beats_sample.rds"))
x %>%
  dplyr::filter(id %in% ids[1:20]) %>%
  dplyr::filter(beat_ind <= 120) %>%
  mutate(idx = paste0(beat_id,"_", id)) %>%
  # mutate(id = paste0("id: ", id)) %>%
  ggplot(aes(x = beat_ind, y = abp_smooth, group = idx, color = id)) +
  geom_line(alpha = 0.01) +
  scale_color_viridis_d(option = "C") +
  scale_x_continuous(breaks = seq(0, 120, 30),
                     labels = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(20, 200), breaks = seq(0, 200, 20)) +
  labs(x = "Time (seconds)", y = "Arterial Blood Pressure (mmHg)",
       title = "Aligned heart beats from 20 patients") +
  theme_light() +
  theme(legend.position = "none")

df_avgs =
  df_small %>%
  group_by(id, beat_ind) %>%
  summarize(abp_smooth = mean(abp_smooth, na.rm = TRUE),
            .groups = "drop")

df_avgs %>%
  mutate(id = paste0("id: ", id)) %>%
  ggplot(aes(x = beat_ind, y = abp_smooth)) +
  geom_line() +
  facet_grid(.~id) +
  scale_x_continuous(breaks = seq(0, 120, 30),
                     labels = seq(0, 1, 0.25)) +
  labs(x = "Time (seconds)", y = "Arterial Blood Pressure (mmHg)",
       title = "Aligned heart beats")


df_mat = x %>%
  dplyr::filter(id == id[1]) %>%
  dplyr::filter(elapsed <= 1 & beat_ind <= 120) %>%
  select(beat_ind, abp_smooth, beat_id) %>%
  pivot_wider(names_from = beat_ind, values_from = abp_smooth)

df_mat = x %>%
  dplyr::filter(id == last(id)) %>%
  dplyr::filter(elapsed <= 1 & beat_ind <= 120) %>%
  select(beat_ind, abp_smooth, beat_id) %>%
  pivot_wider(names_from = beat_ind, values_from = abp_smooth)

pca_mat =
  df_mat %>%
  select(-beat_id) %>%
  as.matrix()

fpca_res$efunctions %>%
  as_tibble() %>%
  mutate(ind = row_number()) %>%
  pivot_longer(cols = -ind) %>%
  ggplot(aes(x = ind, y = value, color = name)) +
  geom_line()



fpca_res = fpca.face(pca_mat)

write_rds(fpca_res, here::here("docs", "data", "fpca_res_1.rds"))

df_mat = x %>%
  dplyr::filter(id == first(id)) %>%
  dplyr::filter(elapsed <= 1 & beat_ind <= 120) %>%
  select(beat_ind, abp_smooth, beat_id) %>%
  pivot_wider(names_from = beat_ind, values_from = abp_smooth)

pca_mat =
  df_mat %>%
  select(-beat_id) %>%
  as.matrix()




fpca_res = fpca.face(pca_mat)

write_rds(fpca_res, here::here("docs", "data", "fpca_res_2.rds"))
plot_fpca(fpca_res, num_eigen = 3, n_plt = 12) +
  scale_x_continuous(limits=c(0, 120), breaks=seq(0,120,30), labels = seq(0, 1, 0.25)) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), labels = c("High", "Low"), name = "") +
  theme_light() +
  theme(legend.position = c(.9, .8),
        legend.title = element_blank()) +
  labs(title = "First 3 PCs from 1 Patient")


df_mat_all =
  x %>%
  dplyr::filter(elapsed <= 1 & beat_ind <= 120) %>%
  select(beat_ind, abp_smooth, beat_id, id) %>%
  pivot_wider(names_from = beat_ind, values_from = abp_smooth)


pca_mat_all =
  df_mat_all %>%
  select(-beat_id, -id) %>%
  as.matrix()


if(!file.exists(here::here("docs", "data", "fpca_res_allbeats.rds"))) {
  fpca_res_all = fpca.face(pca_mat_all, center = TRUE, pve = 0.99)
  write_rds(fpca_res_all, here::here("docs", "data", "fpca_res_allbeats.rds"))
} else fpca_res_all = read_rds(here::here("docs", "data", "fpca_res_allbeats.rds"))

plot_fpca(fpca_res_all, num_eigen = 3, n_plt = 25) +
  scale_x_continuous(limits=c(0, 120), breaks=seq(0,120,30), labels = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(20, 200), breaks = seq(0, 200, 20)) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), labels = c("High", "Low"), name = "") +
  theme_light() +
  theme(legend.position = c(.9, .8),
        legend.title = element_blank()) +
  labs(title = "First 3 PCs from All Patients")

## add in pct variance explained
# calculate percent variance explained by each PC
total_pve = fpca_res_all$pve

# Distribute it proportionally based on eigenvalues
pve_vec = (fpca_res_all$evalues / sum(fpca_res_all$evalues)) * total_pve; pve_vec * 100


plot_fpca(fpca_res_all, num_eigexn = 3, n_plt = 12, pve = TRUE) +
  scale_x_continuous(limits=c(0, 120), breaks=seq(0,120,30), labels = seq(0, 1, 0.25)) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), labels = c("High", "Low"), name = "") +
  theme_light() +
  theme(legend.position = c(.9, .8),
        legend.title = element_blank())

