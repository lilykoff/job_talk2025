## make gifs

### generate figs
library(tidyverse)
options(digits.secs = 3)
library(tidymodels)
library(viridis)
library(gt)
library(SummarizedActigraphy)
library(gtsummary)
library(paletteer)
library(patchwork)
library(adept)
library(gganimate)
library(gifski)
library(geomtextpath)
library(viridis)
library(adeptdata)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

library(showtext)
showtext_auto()
font_add_google("Source Sans Pro", "Source Sans Pro")
theme_set(
  theme_minimal(base_family = "Source Sans Pro") +
    theme(
      plot.background = element_rect(fill = "#f6f5f3", color = NA),
      panel.background = element_rect(fill = "#f6f5f3", color = NA),
      panel.grid.major = element_line(color = "#e1dfdb"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#1a1a1a", size = 12),
      axis.title = element_text(color = "#1a1a1a", size = 14),
      plot.title = element_text(face = "bold", size = 18, color = "#000000"),
      plot.subtitle = element_text(size = 14, color = "#444444"),
      strip.background = element_rect(fill = "#56B4E9", color = NA),
      strip.text = element_text(color = "white", size = 12)
    )
)




data = read_csv(here::here("docs", "data", "df_all_IU.csv"))

iu_dat =
  data %>%
  rename(vm = signal_lw)

df =
  iu_dat %>%
  filter(between(time, 200, 201), ID2 == 4) %>%
  mutate(lag_vm = lag(vm, n = 15)) %>%
  mutate(time = time - min(time))


df_anim = df %>%
  mutate(frame = time)  # Creating frame index

p2 = df %>%
  filter(!is.na(lag_vm)) %>%
  mutate(col = case_when(row_number() == 1 ~ "1",
                         row_number() == 2 ~ "2",
                         .default = "3")) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point(aes(group = time, color = col), size = 1.5) +
  scale_color_manual(values = c("#56B4E9", "#D55E00", "#383838")) +
  scale_x_continuous(limits = c(0.4, 2.6)) +
  scale_y_continuous(limits = c(0.4, 2.6)) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lagged acceleration (g)") +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(40, 20, 40, 40),
        axis.title.x = element_text(vjust = 1.5),
        axis.title.y = element_text(vjust = -1.2),
        legend.title = element_text(vjust = -1))
p_anim = p2 + transition_reveal(time)
#
# suppressWarnings(animate(p_anim, fps = 5, nframes = 101, width = 600, height = 250, renderer = gifski_renderer()))
# anim_save("p2.gif", renderer = gifski_renderer())
suppressWarnings(animate(p_anim, fps = 15, nframes = 101, width = 1200, height = 600, res = 150, renderer = gifski_renderer()))
anim_save(here::here("docs/figs/p2a.gif"), renderer = gifski_renderer())


df_anim = df %>%
  filter(!is.na(lag_vm)) %>%
  pivot_longer(cols = c(vm, lag_vm), names_to = "variable", values_to = "value") %>%
  rename(frame = time)

p = ggplot() +
  geom_line(data = df %>% pivot_longer(cols = c(vm, lag_vm), names_to = "variable", values_to = "value") %>%
              mutate(variable= factor(variable, levels = c("Acceleration (g)", "lag_vm"))), aes(x = time, y = value, color = variable, linetype = variable),
            linewidth = .9) +
  geom_point(data = df_anim, aes(x = frame, y = value, group = variable), color = "black") +
  geom_rect(data = df_anim, aes(xmin = 0.15, xmax = frame, ymin = min(df_anim$value, na.rm = TRUE), ymax = max(df_anim$value, na.rm = TRUE)),
            fill = "yellow", alpha = 0.3) +
  scale_color_manual(values = c("black", "darkgrey"), labels = c("Acceleration", "0.15s lagged acceleration"), name = "") +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Acceleration", "0.15s lagged acceleration"), name = "") +
  labs(x = "Time (s)", y = "Acceleration (g)") +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    # legend.background = element_rect(fill = "#f6f5f3", color = "#f6f5f3"),
    plot.margin = margin(40, 20, 40, 40),
    axis.title.x = element_text(vjust = 1.5),
    axis.title.y = element_text(vjust = -1.2)
  ) +
  guides(color = guide_legend(nrow = 1),
         linetpe = guide_legend(nrow = 1))
p_anim = p +
  transition_reveal(along = frame) +
  shadow_wake(wake_length = 1, exclude_layer = c(3))

df_anim = df %>%
  filter(!is.na(lag_vm)) %>%
  pivot_longer(cols = c(vm, lag_vm), names_to = "variable", values_to = "value") %>%
  rename(frame = time) %>%
  mutate(variable = factor(variable, levels = c("vm", "lag_vm"),
                           labels = c("Acceleration (g)", "0.15s lagged acceleration")))

# Pivot once, outside of ggplot
df_long = df %>%
  select(vm, lag_vm, ID2, time) %>%
  pivot_longer(cols = c(vm, lag_vm), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("vm", "lag_vm"),
                           labels = c("Acceleration (g)", "0.15s lagged acceleration")))

p = ggplot(df_long, aes(x = time, y = value,  group = variable)) +
  geom_line(linewidth = 1.1, color = "#F0E442") +
  geom_point(data = df_anim, aes(x = frame, y = value, color = variable, group = variable)) +
  scale_color_manual(values = c("black", "darkgrey")) +
  # scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Time (s)", y = "Acceleration (g)") +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(40, 20, 40, 40),
    axis.title.x = element_text(vjust = 1.5),
    axis.title.y = element_text(vjust = -1.2)
  ) +
  transition_reveal(time) +   # reveal along the "time" column
  shadow_wake(wake_length = 0.1) # small wake to keep lines visible

animate(p, nframes = 100, fps = 20)

# suppressWarnings(animate(p_anim, fps = 5, nframes = 101, width = 600, height = 250, renderer = gifski_renderer()))
# anim_save(here::here("docs", "figs", "p1.gif"), renderer = gifski_renderer())

suppressWarnings(animate(p, fps = 15, nframes = 101, width = 1200, height = 600, res = 150, renderer = gifski_renderer()))
anim_save(here::here("docs", "figs", "p1a_v2.gif"), renderer = gifski_renderer())



