# Import Packages ------------------
library(tidyverse)
library(rstatix)
library(ggpubr)


# % Dehydration -----------
df_dehyd = read.csv("dehyd.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
   # names_prefix = "EHI",
    values_to = "dehyd",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_dehyd = df_dehyd %>%
  welch_anova_test(dehyd ~ cond)
# Print Results
#print(anova_dehyd)

# Games-Howell Pairwise Tests
gh_dehyd = df_dehyd %>%
  games_howell_test(dehyd ~ cond)
#print(gh_dehyd)

# Plot Data
plot_dehyd = ggboxplot(
  df_dehyd,
  x = "cond",
  y = "dehyd",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)



# Distance Ran -----------
df_dist_ran = read.csv("dist_ran.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "dist",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_dist_ran = df_dist_ran %>%
  welch_anova_test(dist ~ cond)
# Print Results
#print(anova_dist_ran)

# Games-Howell Pairwise Tests
gh_dist_ran = df_dist_ran %>%
  games_howell_test(dist ~ cond)
#print(gh_dist_ran)

# Plot Data
plot_dist_ran = ggboxplot(
  df_dist_ran,
  x = "cond",
  y = "dist",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)


# Running Time -----------

df_run_time = read.csv("run_time.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "time",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_run_time = df_run_time %>%
  welch_anova_test(time ~ cond)
# Print Results
#print(anova_run_time)

# Games-Howell Pairwise Tests
gh_run_time = df_run_time %>%
  games_howell_test(time ~ cond)
#print(gh_run_time)

# Plot Data
plot_run_time = ggboxplot(
  df_run_time,
  x = "cond",
  y = "time",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)


# Time to Max Temp -----------

df_heat_time = read.csv("heat_time.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "time",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_heat_time = df_heat_time %>%
  welch_anova_test(time ~ cond)
# Print Results
#print(anova_heat_time)

# Games-Howell Pairwise Tests
gh_heat_time = df_heat_time %>%
  games_howell_test(time ~ cond)
#print(gh_heat_time)

# Plot Data
plot_heat_time = ggboxplot(
  df_heat_time,
  x = "cond",
  y = "time",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)

# Tc MAX ----
df_tcmax = read.csv("tcmax.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "tcmax",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_tcmax = df_tcmax %>%
  welch_anova_test(tcmax ~ cond)
# Print Results
#print(anova_tcmax)

# Games-Howell Pairwise Tests
gh_tcmax = df_tcmax %>%
  games_howell_test(tcmax ~ cond)
#print(gh_tcmax)

# Plot Data
plot_tcmax = ggboxplot(
  df_tcmax,
  x = "cond",
  y = "tcmax",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)

# Tc MIN ----
df_tcmin = read.csv("tcmin.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "tcmin",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_tcmin = df_tcmin %>%
  welch_anova_test(tcmin ~ cond)
# Print Results
#print(anova_tcmin)

# Games-Howell Pairwise Tests
gh_tcmin = df_tcmin %>%
  games_howell_test(tcmin ~ cond)
#print(gh_tcmin)

# Plot Data
plot_tcmin = ggboxplot(
  df_tcmin,
  x = "cond",
  y = "tcmin",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)

# Hypothermic Duration ----
df_duration = read.csv("duration.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "duration",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_duration = df_duration %>%
  welch_anova_test(duration ~ cond)
# Print Results
#print(anova_duration)

# Games-Howell Pairwise Tests
gh_duration = df_duration %>%
  games_howell_test(duration ~ cond)
#print(gh_duration)

# Plot Data
plot_duration = ggboxplot(
  df_duration,
  x = "cond",
  y = "duration",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)

# Thermal Load ----
df_thermal_load = read.csv("thermal_load.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "thermal_load",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_thermal_load = df_thermal_load %>%
  welch_anova_test(thermal_load ~ cond)
# Print Results
#print(anova_thermal_load)

# Games-Howell Pairwise Tests
gh_thermal_load = df_thermal_load %>%
  games_howell_test(thermal_load ~ cond)
#print(gh_thermal_load)

# Plot Data
plot_thermal_load = ggboxplot(
  df_thermal_load,
  x = "cond",
  y = "thermal_load",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)

# Time to Tcmax ----
df_heat_time = read.csv("heat_time.csv") %>%
  # mutate(id = 1:length(.data)) %>%
  pivot_longer(
    EHI0:EHI7,
    names_to = "cond",
    # names_prefix = "EHI",
    values_to = "heat_time",
    values_drop_na = TRUE
  ) %>%
  mutate(cond = as.factor(cond)) %>%
  rowid_to_column()

# Welch's one-way ANOVA
anova_heat_time = df_heat_time %>%
  welch_anova_test(heat_time ~ cond)
# Print Results
#print(anova_heat_time)

# Games-Howell Pairwise Tests
gh_heat_time = df_heat_time %>%
  games_howell_test(heat_time ~ cond)
#print(gh_heat_time)

# Plot Data
plot_heat_time = ggboxplot(
  df_heat_time,
  x = "cond",
  y = "heat_time",
  color = "cond",
  palette = c("black", "red", "green", "blue")
)



