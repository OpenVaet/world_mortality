################################################################################
## Age-standardised mortality rates (ASMR) 2011-2023, ages 0-19 only
## Baseline = mean 2017-2019, plus % deviation in 2020-2023
################################################################################

## 1 ────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(knitr)
library(kableExtra)
library(scales)
library(htmltools)
library(ggplot2)

## 2 ────────────────────────────────────────────────────────────────────────────
##  US-2000 weights (all ages)  ➜  re-normalise the 0-19 slice so Σw = 1
std_pop_all <- tribble(
  ~age_group_5, ~weight,
  "0-4", 0.013818 + 0.055317,
  "5-9", 0.072533,
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
  "85+",  0.015508
)

std_pop_young <- std_pop_all %>%                       # keep only 0-19 …
  filter(age_group_5 %in% c("0-4", "5-9", "10-14", "15-19")) %>% 
  mutate(weight = weight / sum(weight))                # … and rescale to 1
std_pop_young %>% 
  summarise(total_weight = sum(weight)) %>% 
  print()

## 3 ────────────────────────────────────────────────────────────────────────────
##  Read Eurostat file & compute ASMR (per 100 000) for ages 0-19
dat <- read_csv("data/pop_deaths_eurostats_2011_2023.csv", show_col_types = FALSE)

asmr_young <- dat %>% 
  semi_join(std_pop_young, by = "age_group_5") %>%     # keep rows up to 19 y
  left_join(std_pop_young, by = "age_group_5") %>%     # attach rescaled weights
  mutate(mr     = deaths / population,
         w_rate = mr * weight) %>% 
  group_by(country, year) %>% 
  summarise(ASMR = sum(w_rate) * 1e5, .groups = "drop") %>% 
  arrange(country, year)

## 4 ────────────────────────────────────────────────────────────────────────────
##  BASELINE = arithmetic mean of 2017-2019 for each country (ages 0-19)
baseline_young <- asmr_young %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(country) %>% 
  summarise(baseline_ASMR = mean(ASMR), .groups = "drop")

##  Attach baseline to every year; compute % deviation
asmr_pred_young <- asmr_young %>% 
  left_join(baseline_young, by = "country") %>% 
  rename(pred_ASMR = baseline_ASMR) %>% 
  mutate(dev_pct = (ASMR - pred_ASMR) / pred_ASMR * 100)

## 5 ────────────────────────────────────────────────────────────────────────────
##  Table  (same colour-band logic, ages 0-19)
dev_tbl_num <- asmr_pred_young %>% 
  filter(year %in% 2020:2023) %>% 
  select(country, year, dev_pct) %>% 
  pivot_wider(names_from = year, values_from = dev_pct) %>% 
  arrange(country)

dev_tbl_col <- dev_tbl_num %>% 
  mutate(across(
    -country,
    ~{
      bg <- dplyr::case_when(
        .x  < -10 ~ "#0d47a1",
        .x >= -10 & .x <  -5 ~ "#42a5f5",
        .x >=  -5 & .x <   0 ~ "#bbdefb",
        .x >=   0 & .x <   1 ~ "#ffffff",
        .x >=   1 & .x <   3 ~ "#ffcccb",
        .x >=   3 & .x <  10 ~ "#ef5350",
        TRUE                  ~ "#b71c1c"
      )
      fg <- ifelse(bg %in% c("#0d47a1", "#ef5350", "#b71c1c"), "white", "black")
      cell_spec(sprintf("%+.1f %%", .x), background = bg, color = fg)
    }
  ))

tbl_html <- dev_tbl_col %>% 
  kbl(format = "html", escape = FALSE,
      caption = "Percent deviation of ASMR (ages 0-19) from the 2017-2019 average baseline  
                 (blue = deficit / red = excess)") %>% 
  kable_styling(full_width = FALSE, position = "center") %>% 
  row_spec(0, bold = TRUE)

html_print( HTML(tbl_html) )

## 6 ────────────────────────────────────────────────────────────────────────────
##  Plot: observed ASMR (0-19), constant 2017-2019 baseline, % deviation label
ggplot(asmr_pred_young, aes(year, ASMR)) +
  geom_line(linewidth = 0.7, colour = "steelblue") +
  geom_hline(aes(yintercept = pred_ASMR),
             linetype = "dashed", colour = "grey40") +
  geom_point(size = 2) +
  geom_text(
    data = subset(asmr_pred_young, year >= 2020),
    aes(label = sprintf("%+.1f%%", dev_pct)),
    vjust = -0.7, size = 3
  ) +
  facet_wrap(~country, scales = "free_y") +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(10, 20))) +
  labs(
    title    = "Age-standardised mortality rates (ages 0-19) vs. 2017-2019 average baseline",
    subtitle = "Dashed line = mean ASMR (2017-2019); labels = % deviation relative to that mean",
    x = NULL,
    y = "ASMR (per 100 000, US-2000 standard weights rescaled to ages 0-19)"
  ) +
  theme_minimal(base_size = 11)
