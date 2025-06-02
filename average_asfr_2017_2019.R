################################################################################
## Age-standardised fertility rates (ASFR) 2011-2023, baseline = mean 2017-2019,
## and percent deviation in 2020-2023
################################################################################

## 1 ────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(knitr)
library(kableExtra)
library(scales)
library(broom)      # still required for consistency
library(htmltools)
library(ggplot2)

## 2 ────────────────────────────────────────────────────────────────────────────
##  European-standard population 2013, women 15-49 (weights sum to 1.0)
std_pop <- tribble(
  ~age_group_5, ~weight,
  "15-19", 5500 / 1e5,
  "20-24", 6000 / 1e5,
  "25-29", 6000 / 1e5,
  "30-34", 6500 / 1e5,
  "35-39", 7000 / 1e5,
  "40-44", 7000 / 1e5,
  "45-49", 7000 / 1e5
)

## 3 ────────────────────────────────────────────────────────────────────────────
##  Read Eurostat file & compute ASFR (per 1 000 women)
dat <- read_csv("data/pop_births_eurostats_2011_2023.csv", show_col_types = FALSE)

asfr <- dat %>%
  left_join(std_pop, by = "age_group_5") %>%
  mutate(fr     = births / population,
         w_rate = fr * weight) %>%
  group_by(country, year) %>%
  summarise(ASFR = sum(w_rate) * 1e3, .groups = "drop") %>%    # *1 000
  arrange(country, year)

## 4 ────────────────────────────────────────────────────────────────────────────
##  BASELINE = arithmetic mean of 2017-2019 for each country
baseline <- asfr %>%
  filter(year %in% 2017:2019) %>%
  group_by(country) %>%
  summarise(baseline_ASFR = mean(ASFR), .groups = "drop")

##  Attach baseline to every year; compute % deviation
asfr_pred <- asfr %>%
  left_join(baseline, by = "country") %>%
  rename(pred_ASFR = baseline_ASFR) %>%        # keep same column name style
  mutate(dev_pct = (ASFR - pred_ASFR) / pred_ASFR * 100)

## 5 ────────────────────────────────────────────────────────────────────────────
##  Table (new colour-band logic: negative = red, positive = blue)
dev_tbl_num <- asfr_pred %>%                     # (unchanged)
  filter(year %in% 2020:2023) %>%
  select(country, year, dev_pct) %>%
  pivot_wider(names_from = year, values_from = dev_pct) %>%
  arrange(country)

dev_tbl_col <- dev_tbl_num %>%
  mutate(across(
    -country,
    ~{
      bg <- dplyr::case_when(          # REVERSED palette
        .x  < -10 ~ "#b71c1c",         # deepest red  (big drop = very bad)
        .x >= -10 & .x <  -5 ~ "#ef5350",
        .x >=  -5 & .x <   0 ~ "#ffcccb",
        .x >=   0 & .x <   1 ~ "#ffffff",
        .x >=   1 & .x <   3 ~ "#bbdefb",
        .x >=   3 & .x <  10 ~ "#42a5f5",
        TRUE                  ~ "#0d47a1"  # deepest blue (big rise = very good)
      )
      fg <- dplyr::case_when(          # keep text legible
        .x < -10                     ~ "white",
        .x >= -10 & .x <  -5         ~ "white",
        .x >=  -5 & .x <   0         ~ "black",
        .x >=   0 & .x <   1         ~ "black",
        .x >=   1 & .x <   3         ~ "black",
        .x >=   3 & .x <  10         ~ "black",
        TRUE                         ~ "white"
      )
      cell_spec(sprintf("%+.1f %%", .x), background = bg, color = fg)
    }
  ))

tbl_html <- dev_tbl_col %>%
  kbl(format = "html", escape = FALSE,
      caption = "Percent deviation of ASFR from the 2017-2019 average baseline  
                 (blue = deficit / red = excess)") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE)

html_print( HTML(tbl_html) )

## 6 ────────────────────────────────────────────────────────────────────────────
##  Plot: observed ASFR, constant 2017-2019 average baseline, % deviation label
ggplot(asfr_pred, aes(year, ASFR)) +
  geom_line(linewidth = 0.7, colour = "steelblue") +
  geom_hline(aes(yintercept = pred_ASFR),
             linetype = "dashed", colour = "grey40") +
  geom_point(size = 2) +
  geom_text(
    data = subset(asfr_pred, year >= 2020),
    aes(label = sprintf("%+.1f%%", dev_pct)),
    vjust = -0.7, size = 3
  ) +
  facet_wrap(~country, scales = "free_y") +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.08))   # 5 % below, 8 % above
  ) +
  coord_cartesian(clip = "off")                 # keep labels visible
labs(
  title    = "Age-standardised fertility rates vs. 2017-2019 average baseline",
  subtitle = "Dashed line = mean ASFR (2017-2019); labels = % deviation",
  x = NULL,
  y = "ASFR (per 1 000 women, ESP-2013 weights)"
) +
  theme_minimal(base_size = 11) +
  theme(plot.margin = margin(5.5, 20, 5.5, 5.5))  # a little right padding

