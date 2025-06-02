################################################################################
## Age-standardised mortality rates (ASMR) 2011-2023, linear trend 2015-2019,  ##
## and percent deviation in 2020-2023                                          ##
################################################################################

## 1 ────────────────────────────────────────────────────────────────────────────
##  Standard US 2000 population weights, adjusted for the 0-4 group
##  (0-4 = <1 + 1-4  ⇒ 0.013818 + 0.055317 = 0.069135)
## -----------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(scales)

std_pop <- tribble(
  ~age_group_5, ~weight,
  "0-4",   0.013818 + 0.055317,   # 0-4 years
  "5-9",   0.072533,
  "10-14", 0.073032,
  "15-19", 0.072169,
  "20-24", 0.066478,
  "25-29", 0.064529,
  "30-34", 0.071044,
  "35-39", 0.080762,
  "40-44", 0.081851,
  "45-49", 0.072118,
  "50-54", 0.062716,
  "55-59", 0.048454,
  "60-64", 0.038793,
  "65-69", 0.034264,
  "70-74", 0.031773,
  "75-79", 0.026999,
  "80-84", 0.017842,
  "85+",   0.015508
) %>%
  arrange(factor(age_group_5, levels = age_group_5))          # keep declared order


## 2 ────────────────────────────────────────────────────────────────────────────
##  Reads Eurostat deaths / population file & calculates ASMR (per 100 000)
## -----------------------------------------------------------------------------
dat <- read_csv("data/pop_deaths_eurostats_2011_2023.csv",
                show_col_types = FALSE)

asmr <- dat %>%
  left_join(std_pop, by = "age_group_5") %>%                 # attach weights
  mutate(mr      = deaths / population,                      # age-specific rate
         w_rate  = mr * weight) %>%                          # weighted rate
  group_by(country, year) %>%
  summarise(ASMR = sum(w_rate, na.rm = TRUE) * 1e5,          # per 100 000
            .groups = "drop") %>%
  arrange(country, year)


## 3 ────────────────────────────────────────────────────────────────────────────
##  Fits 2015-2019 linear trend and derives % deviation for 2020-2023
## -----------------------------------------------------------------------------
##   • one model per country: ASMR ~ year (years 2015-2019)
##   • predict through 2011-2023 to keep a full time-series
trend_models <- asmr %>%
  filter(year %in% 2015:2019) %>%
  nest(data = c(year, ASMR)) %>%
  mutate(model = map(data, ~lm(ASMR ~ year, data = .x)))

asmr_pred <- trend_models %>% 
  mutate(newdata = map(model, ~tibble(year = 2011:2023)),
         pred    = map2(model, newdata, predict)) %>% 
  unnest(c(newdata, pred)) %>% 
  rename(pred_ASMR = pred) %>% 
  left_join(asmr, by = c("country", "year")) %>% 
  mutate(dev_pct = (ASMR - pred_ASMR) / pred_ASMR * 100)



## 4 ────────────────────────────────────────────────────────────────────────────
##  2015-2019 linear-trend deviation table with colour gradient
##    • light-blue  = deficit (negative)
##    • white       = on-trend ( ≈ 0 %)
##    • dark-red    = excess   (positive)
## -----------------------------------------------------------------------------
library(kableExtra)
library(htmltools)   # for HTML() and html_print()

## --- builds the wide numeric table -------------------------------------------
dev_tbl_num <- asmr_pred %>%                       
  filter(year %in% 2020:2023) %>%
  select(country, year, dev_pct) %>%
  pivot_wider(names_from = year, values_from = dev_pct) %>%
  arrange(country)

## --- applies discrete background colours with cell_spec() ---------------------
##  bins & colours
##  < −10          : dark-blue  | white text
##  −10 to < −5    : mid-blue   | black text
##  −5  to <  0    : light-blue | black text
##   0  to <  1    : white      | black text
##   1  to <  3    : light-red  | black text
##   3  to < 10    : mid-red    | white text
##  ≥ 10           : dark-red   | white text

dev_tbl_col <- dev_tbl_num %>%                     # ← keeps country first
  mutate(across(
    -country,                                      # style only the % columns
    ~{
      bg <- dplyr::case_when(
        .x  < -10 ~ "#0d47a1",     # dark blue
        .x >= -10 & .x <  -5 ~ "#42a5f5",  # mid-blue
        .x >=  -5 & .x <   0 ~ "#bbdefb",  # light-blue
        .x >=   0 & .x <   1 ~ "#ffffff",  # white
        .x >=   1 & .x <   3 ~ "#ffcccb",  # light-red
        .x >=   3 & .x <  10 ~ "#ef5350",  # mid-red
        TRUE                  ~ "#b71c1c"  # dark-red (≥10 %)
      )
      fg <- dplyr::case_when(
        .x < -10            ~ "white",
        .x >= -10 & .x <  -5  ~ "black",
        .x >=  -5 & .x <   0  ~ "black",
        .x >=   0 & .x <   1  ~ "black",
        .x >=   1 & .x <   3  ~ "black",
        .x >=   3 & .x <  10  ~ "white",
        TRUE                  ~ "white"
      )
      cell_spec(sprintf("%+.1f %%", .x), background = bg, color = fg)
    }
  ))

## --- rendering & view -----------------------------------------------------------
tbl_html <- dev_tbl_col %>%
  kbl(format = "html", escape = FALSE,
      caption = "Percent deviation of ASMR from the 2015-2019 linear trend  
                 (blue = deficit / red = excess)") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE)

html_print( htmltools::HTML(tbl_html) )

## 5 ────────────────────────────────────────────────────────────────────────────
##  Plot: observed ASMR, fitted linear trend, and % deviation labels
##  ─ one facet per country (free y-axis, shared x-axis)                       ##
## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(asmr_pred, aes(year, ASMR)) +
  geom_line(aes(group = 1), linewidth = 0.7, colour = "steelblue") +
  geom_line(aes(y = pred_ASMR), linetype = "dashed") +
  geom_point(size = 2) +
  geom_text(
    data = subset(asmr_pred, year >= 2020),
    aes(label = sprintf("%+.1f%%", dev_pct)),
    vjust = -0.7, size = 3
  ) +
  facet_wrap(~country, scales = "free_y") +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  facet_wrap(~country, scales = "free_y") +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(                       # ← add 50 units above the max
    expand = expansion(mult = c(0, 0), add = c(0, 50))
  ) +
  labs(
    title    = "Age-standardised mortality rates vs. 2015-2019 linear trend",
    subtitle = "Dashed line = fitted trend (2015-2019); labels = % deviation",
    x = NULL,
    y = "ASMR (per 100 000, US 2000 standard population weights)"
  ) +
  theme_minimal(base_size = 11)

