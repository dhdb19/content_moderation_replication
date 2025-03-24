source(".Rprofile")

library("arrow")
library("duckdb")
library("tidyverse")
library("duckplyr")
library("ggeffects")
library("marginaleffects")
library("modelsummary")
library("broom")
library("RColorBrewer")
library("viridis")
library("httpgd")
library("ggnewscale")
library("gt")
library("tikzDevice")
library("tinytable")
library("olsrr")
library("cowplot")
library("plm")
library("car")

# packages <- list(
# "arrow",
# "duckdb",
# "tidyverse",
# "duckplyr",
# "ggeffects",
# "marginaleffects",
# "modelsummary",
# "broom",
# "RColorBrewer",
# "viridis",
# "httpgd",
# "ggnewscale",
# "gt",
# "tikzDevice",
# "tinytable",
# "olsrr",
# "cowplot",
# "plm",
# "car"
# )

# for (package in packages ){
# install.packages(package)
# }


options(vsc.use_httpgd = TRUE)


# ---- connect to database ----
con <- dbConnect(duckdb(), dbdir = "data/D2000_eu_tdb_duckdb/eu_tdb.duckdb")



# ---- cast date from datetime to date----
eu_tdb_analysis <- tbl(con, "eu_tdb_analysis") %>%
  collect()

# ---- filter platform ----

eu_tdb_analysis_filtered <- eu_tdb_analysis %>%
  filter(
    !platform_name == "Amazon Store",
    !platform_name == "Threads",
    !platform_name == "Google Shopping",
    !platform_name == "Reddit",
    !platform_name == "X",
    !platform_name == "YouTube",
  ) %>%
  collect()


# ----Results I share_necde LM----

necde_model1 <- lm(
  (necde_share * 100) ~ election_distance,
  data = eu_tdb_analysis_filtered
)

necde_model2 <- lm(
  (necde_share * 100) ~ election_distance + weekend + share_scope,
  data = eu_tdb_analysis_filtered
)

necde_model3 <- lm(
  (necde_share * 100) ~ election_distance * pre_election + weekend + share_scope,
  data = eu_tdb_analysis_filtered
)

necde_model4 <- lm(
  (necde_share * 100) ~ election_distance * pre_election + weekend + share_scope + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)

# necde_plm <- plm(
#   necde_share ~ election_distance * pre_election + weekend + share_scope,
#   data = eu_tdb_analysis_filtered,
#   index = "platform_name",
#   model = "within"
# )

necde_noscope <- lm(
  (necde_share * 100) ~ election_distance * pre_election + weekend + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)

necde_chrono <- lm(
  (necde_share * 100) ~ election_neg * pre_election + weekend + share_scope + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)

necde_point <- avg_slopes(
  necde_model4,
  variables = "election_distance",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()

necde_point_chrono <- avg_slopes(
  necde_chrono,
  variables = "election_neg",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()


necde_fitted_interaction <- predict_response(
  necde_model4,
  terms = c("election_distance [0:50 by=1]", "pre_election"),
  margin = "empirical"
) %>%
  as_tibble() %>%
  mutate(
    election_distance_temp = case_match(
      group,
      "0" ~ x,
      "1" ~ -x
    ),
    created_at = ymd("2024-04-20") + (election_distance_temp + 50)
  ) %>%
  collect()


necde_fitted <- predict_response(
  necde_model2,
  terms = c("election_distance [0:50 by=1]"),
  margin = "empirical"
)


source(".Rprofile")
necde_plot <- necde_fitted_interaction %>%
  ggplot() +
  geom_point(
    data = eu_tdb_analysis_filtered,
    aes(
      x = created_at,
      y = necde_share * 100,
      shape = platform_name,
      # color = platform_name,
    ),
    alpha = 0.8,
    color = "#c7c9c7",
  ) +
  # scale_color_viridis(
  #   name = "Platform",
  #   discrete = TRUE,
  #   option = "plasma"
  # ) +
  new_scale_color() +
  geom_line(
    aes(
      x = created_at,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(
      x = created_at,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group
    ),
    alpha = 0,
    show.legend = FALSE,
    lty = "dashed",
    linewidth = 0.5
  ) +
  scale_shape(
    name = "Platform",
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  geom_vline(
    xintercept = ymd("2024-06-09"),
    lty = "dashed"
  ) +
  scale_y_continuous(
    limits = c(0, 6),
    name = "Percent",
    # breaks = waiver(),
    # n.breaks = 10,
    expand = expansion(add = 0)
  ) +
  # scale_x_continuous(
  #   name = "Election distance",
  #   breaks = waiver(),
  #   n.breaks = 10,
  #   labels = function(x) abs(x),
  #   expand = expansion(add = 0)
  # ) +
  scale_x_date(
    name = "Time",
    expand = expansion(add = 0)
  ) +
  annotate(
    geom = "label",
    x = ymd("2024-06-09"),
    y = 5,
    label = "Election day",
    size = 2.75,
    label.padding = unit(0.5, "lines"),
    label.r = unit(0, "lines"),
    label.size = 0,
    color = "black"
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.8),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
necde_plot

tikz("figures/necde_plot.tex", width = 6.3, height = 2.5, standAlone = FALSE)
necde_plot
dev.off()


source(".Rprofile")
necde_plot_linear <- necde_fitted %>%
  ggplot() +
  geom_line(
    aes(
      x = x,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(
      x = x,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group
    ),
    alpha = 0,
    show.legend = FALSE,
    lty = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  scale_y_continuous(
    limits = c(0, 1.5),
    name = "Percentage points",
    expand = expansion(add = 0)
  ) +
  scale_x_continuous(
    name = "Election distance (absolute)",
    expand = expansion(add = 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.7),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
necde_plot_linear

tikz("figures/necde_plot_linear.tex", width = 3, height = 2, standAlone = FALSE)
necde_plot_linear
dev.off()


necde_plot_interaction <- necde_fitted_interaction %>%
  ggplot() +
  geom_line(
    aes(
      x = x,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
  ) +
  geom_ribbon(
    aes(
      x = x,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group,
      lty = group,
    ),
    alpha = 0,
    show.legend = FALSE,
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  scale_y_continuous(
    limits = c(0, 1.5),
    name = "Percentage points",
    expand = expansion(add = 0)
  ) +
  scale_x_continuous(
    name = "Election distance (absolute)",
    expand = expansion(add = 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.9),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
necde_plot_interaction

tikz("figures/necde_plot_interaction.tex", width = 3, height = 2, standAlone = FALSE)
necde_plot_interaction
dev.off()

necde_plot_combined <- plot_grid(
  necde_plot_linear,
  necde_plot_interaction,
  labels = c("A", "B"),
  label_fontface = "bold",
  label_colour = "#3C714F",
  label_size = 11,
  nrow = 1,
  ncol = 2
)
necde_plot_combined

tikz("figures/necde_plot_combined.tex", width = 6.3, height = 2, standAlone = FALSE)
necde_plot_combined
dev.off()



necde_table_point <- necde_point %>%
  modelsummary(
    shape = group ~ term + statistic:estimate,
    gof_map = "",
    stars = TRUE,
    statistic = c("95% CI" = "[{conf.low}, {conf.high}]"),
    # align = "lcc",
    output = "tinytable",
    escape = TRUE,
    fmt = 4,
    coef_rename = c("election_distance" = "Election distance")
  ) %>%
  format_tt(
    escape = FALSE
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  # style_tt(
  #   i = 3,
  #   italic = TRUE
  # ) %>%
  save_tt(
    "figures/necde_table_point.tex",
    overwrite = TRUE
  )
necde_table_point



necde_table_point_chrono <- necde_point_chrono %>%
  modelsummary(
    shape = group ~ term + statistic:estimate,
    gof_map = "",
    stars = TRUE,
    statistic = c("95% CI" = "[{conf.low}, {conf.high}]"),
    # align = "lcc",
    output = "tinytable",
    escape = TRUE,
    fmt = 4,
    coef_rename = c("election_neg" = "Election distance (chrono)")
  ) %>%
  format_tt(
    escape = FALSE
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  # style_tt(
  #   i = 3,
  #   italic = TRUE
  # ) %>%
  save_tt(
    "figures/necde_table_point_chrono.tex",
    overwrite = TRUE
  )
necde_table_point_chrono


necde_plot_point <- necde_point %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = group,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
    )
  ) +
  geom_hline(
    yintercept = 0,
  ) +
  scale_x_discrete(
    name = "",
    limits = c("Pre-election", "Post-election"),
    # labels = c("Before Election", "After Election"),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    name = "Percentage points",
    limits = c(-0.017, 0.017)
  ) +
  theme_tdb()
necde_plot_point

tikz("figures/necde_plot_point.tex", width = 3, height = 2, standAlone = FALSE)
necde_plot_point
dev.off()


necde_plot_point_chrono <- necde_point_chrono %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = group,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
    )
  ) +
  geom_hline(
    yintercept = 0,
  ) +
  scale_x_discrete(
    name = "",
    limits = c("Pre-election", "Post-election"),
    # labels = c("Before Election", "After Election"),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    name = "Percentage points",
    limits = c(-0.017, 0.017)
  ) +
  theme_tdb()
necde_plot_point_chrono

tikz("figures/necde_plot_point_chrono.tex", width = 3, height = 2, standAlone = FALSE)
necde_plot_point_chrono
dev.off()


# ---- Results III automod LM----
automod_model5 <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_distance,
  data = eu_tdb_analysis_filtered
)

automod_model6 <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_distance + weekend + share_scope,
  data = eu_tdb_analysis_filtered
)

automod_model7 <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_distance * pre_election + weekend + share_scope,
  data = eu_tdb_analysis_filtered
)


automod_model8 <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_distance * pre_election + weekend + share_scope + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)

automod_noscope <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_distance * pre_election + weekend + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)

automod_chrono <- lm(
  ((automod_share_necde - automod_share_all) * 100) ~ election_neg * pre_election + weekend + share_scope + factor(platform_name) - 1,
  data = eu_tdb_analysis_filtered
)



automod_fitted_interaction <- predict_response(
  automod_model8,
  terms = c("election_distance [0:50 by=1]", "pre_election"),
  margin = "empirical"
) %>%
  as_tibble() %>%
  mutate(
    election_distance_temp = case_match(
      group,
      "0" ~ x,
      "1" ~ -x
    ),
    created_at = ymd("2024-04-20") + (election_distance_temp + 50)
  ) %>%
  collect()


automod_fitted <- predict_response(
  automod_model5,
  terms = c("election_distance [0:50 by=1]"),
  margin = "empirical"
) %>%
  as_tibble()


source(".Rprofile")
automod_plot <- automod_fitted_interaction %>%
  ggplot() +
  geom_point(
    data = eu_tdb_analysis_filtered,
    aes(
      x = created_at,
      y = (automod_share_necde - automod_share_all) * 100,
      shape = platform_name,
      # color = platform_name,
    ),
    alpha = 0.8,
    color = "#c7c9c7",
  ) +
  # scale_color_viridis(
  #   name = "Platform",
  #   discrete = TRUE,
  #   option = "plasma"
  # ) +
  new_scale_color() +
  geom_line(
    aes(
      x = created_at,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(
      x = created_at,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group
    ),
    alpha = 0,
    show.legend = FALSE,
    lty = "dashed",
    linewidth = 0.5
  ) +
  scale_shape(
    name = "Platform",
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  geom_vline(
    xintercept = ymd("2024-06-09"),
    lty = "dashed"
  ) +
  scale_y_continuous(
    # limits = c(0, 6),
    name = "Percentage difference",
    # breaks = waiver(),
    # n.breaks = 10,
    expand = expansion(add = 0)
  ) +
  # scale_x_continuous(
  #   name = "Election distance",
  #   breaks = waiver(),
  #   n.breaks = 10,
  #   labels = function(x) abs(x),
  #   expand = expansion(add = 0)
  # ) +
  scale_x_date(
    name = "Time",
    expand = expansion(add = 0)
  ) +
  annotate(
    geom = "label",
    x = ymd("2024-06-09"),
    y = -50,
    label = "Election day",
    size = 2.75,
    label.padding = unit(0.5, "lines"),
    label.r = unit(0, "lines"),
    label.size = 0,
    color = "black"
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.8),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
automod_plot

tikz("figures/automod_plot.tex", width = 6.3, height = 2.5, standAlone = FALSE)
automod_plot
dev.off()


source(".Rprofile")
automod_plot_linear <- automod_fitted %>%
  ggplot() +
  geom_line(
    aes(
      x = x,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
    show.legend = FALSE,
  ) +
  geom_ribbon(
    aes(
      x = x,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group
    ),
    alpha = 0,
    show.legend = FALSE,
    lty = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  scale_y_continuous(
    limits = c(-80, -60),
    name = "Percentage point difference",
    expand = expansion(add = 0)
  ) +
  scale_x_continuous(
    name = "Election distance (absolute)",
    expand = expansion(add = 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.7),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
automod_plot_linear

tikz("figures/automod_plot_linear.tex", width = 3, height = 2, standAlone = FALSE)
automod_plot_linear
dev.off()


automod_plot_interaction <- automod_fitted_interaction %>%
  ggplot() +
  geom_line(
    aes(
      x = x,
      y = predicted,
      color = group
    ),
    linewidth = 1.5,
  ) +
  geom_ribbon(
    aes(
      x = x,
      y = predicted,
      ymin = conf.low,
      ymax = conf.high,
      color = group,
      lty = group,
    ),
    alpha = 0,
    show.legend = FALSE,
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Pre-election",
    values = c("1" = "#3C714F", "0" = "#6fa580"),
    labels = c("1" = "Before election", "0" = "After election")
  ) +
  scale_y_continuous(
    limits = c(-80, -60),
    name = "Percentage point difference",
    expand = expansion(add = 0)
  ) +
  scale_x_continuous(
    name = "Election distance (absolute)",
    expand = expansion(add = 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_tdb(
    legend.position = "inside",
    legend.frame = element_rect(
      linewidth = 1,
      color = "black",
      linetype = "solid",
    ),
    legend.position.inside = c(0.98, 0.9),
    legend.justification = "right",
    legend.background = element_rect(fill = "#f0f0f0", color = NULL),
  )
automod_plot_interaction

tikz("figures/automod_plot_interaction.tex", width = 3, height = 2, standAlone = FALSE)
automod_plot_interaction
dev.off()

automod_plot_combined <- plot_grid(
  automod_plot_linear,
  automod_plot_interaction,
  labels = c("A", "B"),
  label_fontface = "bold",
  label_colour = "#3C714F",
  label_size = 11,
  nrow = 1,
  ncol = 2
)
automod_plot_combined


tikz("figures/automod_plot_combined.tex", width = 6.3, height = 2, standAlone = FALSE)
automod_plot_combined
dev.off()



automod_point <- avg_slopes(
  automod_model8,
  variables = "election_distance",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()

automod_point_chrono <- avg_slopes(
  automod_chrono,
  variables = "election_neg",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()


automod_table_point <- automod_point %>%
  modelsummary(
    shape = group ~ term + statistic,
    gof_map = "",
    stars = TRUE,
    statistic = c("95% CI" = "[{conf.low}, {conf.high}]"),
    align = "lcc",
    output = "tinytable",
    escape = TRUE,
    fmt = 4,
    coef_rename = c("election_distance" = "Election distance")
  ) %>%
  format_tt(
    escape = FALSE
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  # style_tt(
  #   i = 3,
  #   italic = TRUE
  # ) %>%
  save_tt(
    "figures/automod_table_point.tex",
    overwrite = TRUE
  )
automod_table_point


automod_table_point_chrono <- automod_point_chrono %>%
  modelsummary(
    shape = group ~ term + statistic,
    gof_map = "",
    stars = TRUE,
    statistic = c("95% CI" = "[{conf.low}, {conf.high}]"),
    align = "lcc",
    output = "tinytable",
    escape = TRUE,
    fmt = 4,
    coef_rename = c("election_neg" = "Election (chrono)")
  ) %>%
  format_tt(
    escape = FALSE
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  # style_tt(
  #   i = 3,
  #   italic = TRUE
  # ) %>%
  save_tt(
    "figures/automod_table_point_chrono.tex",
    overwrite = TRUE
  )
automod_table_point_chrono



automod_plot_point <- automod_point %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = group,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
    )
  ) +
  geom_hline(
    yintercept = 0,
  ) +
  scale_x_discrete(
    name = "",
    limits = c("Pre-election", "Post-election"),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    name = "Percentage point difference",
    limits = c(-0.2, 0.1)
  ) +
  theme_tdb()
automod_plot_point

tikz("figures/automod_plot_point.tex", width = 3, height = 2, standAlone = FALSE)
automod_plot_point
dev.off()


automod_plot_point_chrono <- automod_point_chrono %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = group,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
    )
  ) +
  geom_hline(
    yintercept = 0,
  ) +
  scale_x_discrete(
    name = "",
    limits = c("Pre-election", "Post-election"),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    name = "Percentage point difference",
    limits = c(-0.1, 0.2)
  ) +
  theme_tdb()
automod_plot_point_chrono

tikz("figures/automod_plot_point_chrono.tex", width = 3, height = 2, standAlone = FALSE)
automod_plot_point_chrono
dev.off()

# ---- latex tables ----
options("modelsummary_format_numeric_latex" = "plain")


necde_models <- list(
  "Model 1" = necde_model1,
  "Model 2" = necde_model2,
  "Model 3" = necde_model3,
  "Model 4" = necde_model4
)

# necde_table_regression <- necde_models %>%
#   modelsummary(
#     # shape = term ~ statistic:estimate,
#     stars = TRUE,
#     coef_map = c(
#       "(Intercept)" = "Constant",
#       "election_distance" = "Election distance",
#       "pre_election" = "Pre-election",
#       "election_distance:pre_election" = "Election distance * \n Pre-election",
#       "weekend" = "Weekend",
#       "share_scope" = "Share of SoRs in \n scope-category"
#     ),
#     # align = "ldddd",
#     output = "tinytable",
#     gof_map = c(
#       "nobs",
#       "r.squared",
#       "adj.r.squared"
#     ),
#   ) %>%
#   theme_tt(
#     "tabular",
#     style = "tabularray"
#   ) %>%
#   group_tt(
#     i = list(
#       "Explanatory variables" = 3,
#       "Controls" = 9
#     ),
#     indent = 0,
#     j = list(
#       "OLS" = 2:4,
#       "OLS + Fixed Effects" = 5
#     )
#   ) %>%
#   style_tt(
#     i = 3,
#     italic = TRUE
#   ) %>%
#   style_tt(
#     i = 10,
#     italic = TRUE
#   ) %>%
#   save_tt(
#     "figures/regression_necde.tex",
#     overwrite = TRUE
#   )



necde_table_regression_wide <- necde_models %>%
  modelsummary(
    shape = term ~ statistic:estimate,
    statistic = c("s.e." = "({std.error})"),
    estimate = c("coef." = "{estimate}{stars}"),
    stars = TRUE,
    coef_map = c(
      "(Intercept)" = "Constant",
      "election_distance" = "Election distance",
      "pre_election" = "Pre-election",
      "election_distance:pre_election" = "El. dist.*Pre-election",
      "weekend" = "Weekend",
      "share_scope" = "\\% SoRs scope-category"
    ),
    align = "ldddddddd",
    output = "tinytable",
    gof_map = c(
      "nobs",
      "r.squared",
      "adj.r.squared"
    ),
    note = "+ p \\num{< 0.1}, * p \\num{< 0.05}, ** p \\num{< 0.01}, *** p \\num{< 0.001}"
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  group_tt(
    i = list(
      "Explanatory variables" = 2,
      "Controls" = 5
    ),
    indent = 0,
    # j = list(
    #   "OLS" = 2:7,
    #   "OLS + Fixed Effects" = 8:9
    # )
  ) %>%
  style_tt(
    i = 2,
    italic = TRUE
  ) %>%
  style_tt(
    i = 6,
    italic = TRUE
  ) %>%
  save_tt(
    "figures/regression_necde_wide.tex",
    overwrite = TRUE
  )

# ---- latex tables automod ----

automod_models <- list(
  "Model 5" = automod_model5,
  "Model 6" = automod_model6,
  "Model 7" = automod_model7,
  "Model 8" = automod_model8
)


# automod_table_regession <- modelsummary(automod_models,
#   stars = TRUE,
#   coef_map = c(
#     "(Intercept)" = "Constant",
#     "election_distance" = "Election distance",
#     "pre_election" = "Pre-election",
#     "election_distance:pre_election" = "Election distance * \n Pre-election",
#     "weekend" = "Weekend",
#     "share_scope" = "Share of SoRs in \n scope-category"
#   ),
#   align = "ldddd",
#   output = "tinytable",
#   gof_map = c(
#     "nobs",
#     "r.squared",
#     "adj.r.squared"
#   ),
# ) %>%
#   theme_tt(
#     "tabular",
#     style = "tabularray"
#   ) %>%
#   group_tt(
#     i = list(
#       "Explanatory variables" = 3,
#       "Controls" = 9
#     ),
#     j = list(
#       "OLS" = 2:4,
#       "OLS + Fixed Effects" = 5
#     )
#   ) %>%
#   style_tt(
#     i = 3,
#     italic = TRUE
#   ) %>%
#   style_tt(
#     i = 10,
#     italic = TRUE
#   ) %>%
#   save_tt(
#     "figures/regression_automod.tex",
#     overwrite = TRUE
#   )



automod_table_regression_wide <- automod_models %>%
  modelsummary(
    shape = term ~ statistic:estimate,
    statistic = c("s.e." = "({std.error})"),
    estimate = c("coef." = "{estimate}{stars}"),
    stars = TRUE,
    coef_map = c(
      "(Intercept)" = "Constant",
      "election_distance" = "Election distance",
      "pre_election" = "Pre-election",
      "election_distance:pre_election" = "El. dist.*Pre-election",
      "weekend" = "Weekend",
      "share_scope" = "\\% SoRs scope-category"
    ),
    align = "ldddddddd",
    output = "tinytable",
    gof_map = c(
      "nobs",
      "r.squared",
      "adj.r.squared"
    ),
    note = "+ p \\num{< 0.1}, * p \\num{< 0.05}, ** p \\num{< 0.01}, *** p \\num{< 0.001}"
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  group_tt(
    i = list(
      "Explanatory variables" = 2,
      "Controls" = 5
    ),
    indent = 0,
    # j = list(
    #   "OLS" = 2:7,
    #   "OLS + Fixed Effects" = 8:9
    # )
  ) %>%
  style_tt(
    i = 2,
    italic = TRUE
  ) %>%
  style_tt(
    i = 6,
    italic = TRUE
  ) %>%
  save_tt(
    "figures/regression_automod_wide.tex",
    overwrite = TRUE
  )

# ---- noscope tables ----

noscope_models <- list(
  "Model 9 (H1)" = necde_noscope,
  "Model 10 (H2)" = automod_noscope
)

noscope_table_regression <- modelsummary(
  noscope_models,
  # shape = term ~ statistic:estimate,
  # statistic = c("s.e." = "({std.error})"),
  # estimate = c("coef." = "{estimate}{stars}"),
  stars = TRUE,
  coef_map = c(
    "(Intercept)" = "Constant",
    "election_distance" = "Election distance",
    "pre_election" = "Pre-election",
    "election_distance:pre_election" = "El. dist*Pre-election",
    "weekend" = "Weekend",
    "share_scope" = "\\% SoRs scope-category"
  ),
  align = "ldd",
  output = "tinytable",
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  # group_tt(
  #   i = list(
  #     "Explanatory variables" = 1,
  #     "Controls" = 4
  #   ),
  #   indent = 0,
  # ) %>%
  # style_tt(
  #   i = 1,
  #   italic = TRUE
  # ) %>%
  # style_tt(
  #   i = 6,
  #   italic = TRUE
  # ) %>%
  save_tt(
    "figures/noscope_table_regerssion.tex",
    overwrite = TRUE
  )


necde_noscope_point <- avg_slopes(
  necde_noscope,
  variables = "election_distance",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()

automod_noscope_point <- avg_slopes(
  automod_noscope,
  variables = "election_distance",
  by = "pre_election"
) %>%
  mutate(
    group = case_when(
      pre_election == 0 ~ "Post-election",
      pre_election == 1 ~ "Pre-election"
    )
  ) %>%
  collect()


models_noscope_point <- list(
  "Model 9 (H1)" = necde_noscope_point,
  "Model 10 (H2)" = automod_noscope_point
)

noscope_table_point <- modelsummary(
  models_noscope_point,
  shape =  group ~ term +model,
  statistic = c("s.e." = "[{conf.low}, {conf.high}]"),
  estimate = c("coef." = "{estimate}{stars}"),
  stars = TRUE,
  escape = TRUE,
  coef_rename = c("election_distance" = "Election distance")
) %>%
  format_tt(
    escape = TRUE
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  save_tt(
    "figures/noscope_table_point.tex",
    overwrite = TRUE
  )


# ---- multicollinearity -----


necde_corr <- necde_model4 %>%
  vcov() %>%
  cov2cor() %>%
  data.frame() %>%
  rownames_to_column(var = "variable") %>%
  mutate(
    variable =
      case_match(
        variable,
        "election_distance" ~ "Election distance",
        "pre_election" ~ "Pre-election",
        "election_distance:pre_election" ~ "Interaction",
        "weekend" ~ "Weekend",
        "share_scope" ~ "Share scope-category",
        "factor(platform_name)Facebook" ~ "Facebook",
        "factor(platform_name)Instagram" ~ "Instagram",
        "factor(platform_name)LinkedIn" ~ "LinkedIn",
        "factor(platform_name)Pinterest" ~ "Pinterest",
        "factor(platform_name)Snapchat" ~ "Snapchat",
        "factor(platform_name)TikTok" ~ "TikTok"
      )
  )

automod_corr <- automod_model8 %>%
  vcov() %>%
  cov2cor() %>%
  data.frame() %>%
  rownames_to_column(var = "variable") %>%
  mutate(
    variable =
      case_match(
        variable,
        "election_distance" ~ "Election distance",
        "pre_election" ~ "Pre-election",
        "election_distance:pre_election" ~ "Interaction",
        "weekend" ~ "Weekend",
        "share_scope" ~ "Share scope-category",
        "factor(platform_name)Facebook" ~ "Facebook",
        "factor(platform_name)Instagram" ~ "Instagram",
        "factor(platform_name)LinkedIn" ~ "LinkedIn",
        "factor(platform_name)Pinterest" ~ "Pinterest",
        "factor(platform_name)Snapchat" ~ "Snapchat",
        "factor(platform_name)TikTok" ~ "TikTok"
      )
  )


necde_table_corr <- tt(necde_corr) %>%
  setNames(
    c(
      "rowname" = "Variable",
      "election_distance" = "Election distance",
      "pre_election" = "Pre-election",
      "weekend" = "Weekend",
      "share_scope" = "Share scope-category",
      "factor(platform_name)Facebook" = "Facebook",
      "factor(platform_name)Instagram" = "Instagram",
      "factor(platform_name)LinkedIn" = "LinkedIn",
      "factor(platform_name)Pinterest" = "Pinterest",
      "factor(platform_name)Snapchat" = "Snapchat",
      "factor(platform_name)TikTok" = "TikTok",
      "election_distance:pre_election" = "Interaction"
    )
  ) %>%
  theme_tt(
    "tabular",
    style = "tabularray"
  ) %>%
  style_tt(
    i = 10,
    italic = TRUE
  ) %>%
  format_tt(
    num_fmt = "decimal",
    digits = 3
  ) %>%
  save_tt(
    "figures/necde_plot_corr.tex",
    overwrite = TRUE
  )


# ---- plots absolute number of SoRs (RD section) ----

eu_tdb_absolute_analysis <- tbl(con, "eu_tdb_absolute_analysis") %>%
  collect()


# ---- filter platform ----
eu_tdb_absolute_filtered <- eu_tdb_absolute_analysis %>%
  filter(
    !platform_name == "Amazon Store",
    # !platform_name == "Threads",
    !platform_name == "Google Shopping",
  ) %>%
  # mutate() %>%
  collect()


plot_sor_log <- eu_tdb_absolute_filtered %>%
  ggplot() +
  geom_col(
    aes(
      x = created_at,
      y = sor,
      fill = category == "negative effects on civic discourse or elections"
    ),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = as.Date("2024-06-09"),
    lty = "dashed",
    color = "#000000"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#c7c9c7", "TRUE" = "#D55E00"),
    labels = c("Total SoRs (log)", "NECDE SoRs (log)"),
    name = "SoR Type"
  ) +
  scale_y_log10(
    name = "Total SoRs (log scale)",
    limits = c(1, NA),
    expand = expansion(add = 0)
  ) +
  scale_x_date(
    expand = expansion(add = 0)
  ) +
  facet_wrap(
    ~ factor(platform_name, c("Facebook", "Instagram", "LinkedIn", "Pinterest", "Reddit", "Snapchat", "TikTok", "Threads", "X", "YouTube")),
    scale = "free_y",
    nrow = 2,
    ncol = 5
  ) +
  labs(
    x = "Time"
  ) +
  theme_tdb(
    strip.background = element_rect(fill = "white")
  )
plot_sor_log

ggsave("plot_sor_log.pdf", plot = plot_sor_log, device = "pdf", width = 10, height = 6)




# FIGURE 2

plot_automod_filter <- eu_tdb_absolute_filtered %>%
  filter(!platform_name %in% c("X", "YouTube", "Reddit")) %>%
  ggplot() +
  geom_col(
    aes(
      x = created_at,
      y = sor,
      fill = "Total SoRs"
    ),
    alpha = 0.6
  ) +
  geom_line(
    aes(
      x = created_at,
      y = automod,
      color = "Automated SoRs"
    ),
    size = 1.2
  ) +
  geom_vline(
    xintercept = as.Date("2024-06-09"),
    lty = "dashed",
    color = "#000000"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Total SoRs" = "#c7c9c7")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("Automated SoRs" = "#1170AA")
  ) +
  scale_y_continuous(
    name = "Total SoRs",
    limits = c(0, NA),
    expand = expansion(add = 0),
    labels = function(x) {
      if_else(x > 900000, glue("{format(x/1000000, digits=2)}M"), as.character(x))
    },
    sec.axis = sec_axis(~., name = "Automated SoRs")
  ) +
  scale_x_date(expand = expansion(add = 0)) +
  facet_wrap(
    ~ factor(platform_name, c("Facebook", "Instagram", "LinkedIn", "Pinterest", "Snapchat", "TikTok", "Threads")),
    scale = "free_y",
    nrow = 3,
    ncol = 3
  ) +
  labs(x = "Time") +
  theme_tdb(strip.background = element_rect(fill = "white"))

# Print the plot
plot_automod_filter
ggsave("plot_automod_filter.pdf", plot = plot_automod_filter, device = "pdf", width = 10, height = 6)
