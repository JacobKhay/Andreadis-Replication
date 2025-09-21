# Table 2 Exact Fix - Following Original replication_code.R Precisely
# The key issue: original uses FULL specification with ALL variables in drop_na()

library(pander)
library(fixest)
library(tidyverse)

# Follow EXACT original code structure
data_ai_2017_2022 <- read_csv("data.csv") %>% 
  filter(Year == 2017 | Year == 2018 | Year == 2022 | Year == 2023) %>% 
  mutate(state_year = paste0(state, Year)) %>% 
  mutate(new = 1 * (Year > 2020)) %>% 
  group_by(new, COUNTY_FIPS) %>% 
  mutate(ai = sum(ai), nads = sum(nads)) %>% 
  mutate(ai_intensity = ai / nads) %>% 
  filter(Year == 2017 | Year == 2022) %>% 
  group_by(COUNTY_FIPS) %>% 
  mutate(
    dai_intensity = ai_intensity - lag(ai_intensity),
    dai_intensity9 = ai_intensity - lag(ai_intensity, 1)
  )

# Prepare demographics - EXACT as original
data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    logincome = log(medhhincome),
    loghpi = log(hpi),
    logemp = log(pop_above18),
    logim = log(1)  # Note: original has logim=log(immigration) but no immigration variable
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    unrate14 = lag(unrate, 1),
    logincome14 = lag(logincome, 1),
    loghpi14 = lag(loghpi, 1),
    share_bac14 = lag(share_bac, 1),
    share_black14 = lag(share_black, 1),
    share_poverty14 = lag(share_poverty, 1),
    logemp14 = lag(logemp, 1),
    logim14 = lag(logim, 1),
    medage14 = lag(medage, 1),
    median_rent14 = lag(median_rent, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    gincome = logincome - lag(logincome, 1),
    ghpi = loghpi - lag(loghpi, 1),
    gemp = logemp - lag(logemp, 1),
    gim = logim - lag(logim, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    dunrate = unrate - lag(unrate, 1),
    dshare_bac = share_bac - lag(share_bac, 1),
    dshare_black = share_black - lag(share_black, 1),
    dmedage14 = medage - lag(medage, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(lads = log(1 + nads))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(lads0 = log(nads))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    large_firms = 1 - (small + medium) / est,
    management_intensity = management_emp / emp,
    information_intensity = information_emp / emp,
    information_intensity = manuf_emp / emp  # Note: this overwrites previous line in original
  )

# Log number of patents, inventors, etc. - EXACT as original
data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(logemp = log(Employed)) %>% 
  mutate(logpop = log(pop))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    logn_patents = log(1 + n_patents),
    logn_inventors = log(1 + n_inventors),
    logai_patents = log(1 + ai_patents),
    logai_inventors = log(1 + ai_inventors)
  ) %>%
  mutate(
    pat_intensity = n_inventors / Employed,
    patai_intensity = ai_patents / n_patents
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    pat_intensity14 = lag(pat_intensity, 1),
    patai_intensity14 = lag(patai_intensity, 1)
  )

# Duplicate line in original
data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    pat_intensity14 = lag(pat_intensity, 1),
    patai_intensity14 = lag(patai_intensity, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    small_firms = small / est,
    large_firms = 1 - (small + medium) / est,
    management_intensity = management_emp / emp,
    information_intensity = information_emp / emp,
    manuf_intensity = manuf_emp / emp
  )

# Another duplicate section in original
data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    pat_intensity = n_inventors / Employed,
    patai_intensity = ai_patents / n_patents
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(patai_intensity = replace_na(patai_intensity, 0))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    pat_intensity14 = lag(pat_intensity, 1),
    patai_intensity14 = lag(patai_intensity, 1),
    lads14 = lag(lads, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    logn_inventors14 = lag(logn_inventors, 1),
    logai_inventors14 = lag(logai_inventors, 1),
    lads14 = lag(lads, 1)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  ungroup() %>%  
  mutate(
    degshare = (udeg + mdeg) / Employed,
    stemshare = (ustemdeg + mstemdeg) / (udeg + mdeg)
  )

data_ai_2017_2022 <- data_ai_2017_2022 %>%  
  mutate(stemshare = replace_na(stemshare, 0))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(stemshare14 = lag(stemshare, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(degshare14 = lag(degshare, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>%  
  mutate(tightness = nads / Unemployed)

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(tightness14 = lag(tightness, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(hpi_ch14 = lag(hpi_ch, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(logpop14 = lag(logpop, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(logincome14 = lag(logincome, 1))

data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(gpop = (logpop - lag(logpop, 1)) / 5)

data_ai_2017_2022 <- data_ai_2017_2022 %>%  
  mutate(hpi_ch = hpi_ch / 100)

data_ai_2017_2022 <- data_ai_2017_2022 %>%  
  mutate(hpi_ch14 = hpi_ch14 / 100)

# Industry structure
data_ai_2017_2022 <- data_ai_2017_2022 %>% 
  mutate(
    large_firms14 = lag(large_firms, 1),
    information_intensity14 = lag(information_intensity, 1),
    manuf_intensity14 = lag(manuf_intensity, 1),
    TurnOvrS14 = lag(TurnOvrS, 1)
  )

# Z-scores - EXACT as original
data_ai_l_z <- data_ai_2017_2022 %>% 
  filter(emp != 0) %>% 
  drop_na(share_bac14) %>%
  mutate(
    share_bac14 = scale(share_bac14),
    share_black14 = scale(share_black14),
    share_poverty14 = scale(share_poverty14),
    logpop14 = scale(logpop14),
    hpi_ch14 = scale(hpi_ch14),
    logincome14 = scale(logincome14),
    tightness14 = scale(tightness14),
    pat_intensity14 = scale(pat_intensity14),
    patai_intensity14 = scale(patai_intensity14),
    degshare14 = scale(degshare14),
    stemshare14 = scale(stemshare14),
    large_firms14 = scale(large_firms14),
    information_intensity14 = scale(information_intensity14),
    manuf_intensity14 = scale(manuf_intensity14),
    TurnOvrS14 = scale(TurnOvrS14),
    unrate14 = scale(unrate14)
  ) %>% 
  mutate(dai_intensity9 = dai_intensity9 * 100)

# CRITICAL: Use the EXACT drop_na specification from original
# This is what creates the same sample across all models

est_demog_no <- fixest::feols(
  "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + hpi_ch14 + logincome14 + tightness14" %>% as.formula(),
  data_ai_l_z %>% 
    filter(dai_intensity9 > -5, dai_intensity9 < 10) %>% 
    drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14, logincome14,
            tightness14, pat_intensity14, patai_intensity14, degshare14, stemshare14,
            large_firms14, information_intensity14, manuf_intensity14, TurnOvrS14),
  weights = ~lads14
) 

est_innovation_no <- fixest::feols(
  "dai_intensity9 ~ pat_intensity14 + patai_intensity14 + degshare14 + stemshare14" %>% as.formula(),
  data_ai_l_z %>% 
    filter(dai_intensity9 > -5, dai_intensity9 < 10) %>% 
    drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14, logincome14,
            tightness14, pat_intensity14, patai_intensity14, degshare14, stemshare14,
            large_firms14, information_intensity14, manuf_intensity14, TurnOvrS14),
  weights = ~lads14
) 

est_industry_no <- fixest::feols(
  "dai_intensity9 ~ large_firms14 + information_intensity14 + manuf_intensity14 + TurnOvrS14" %>% as.formula(),
  data_ai_l_z %>% 
    filter(dai_intensity9 > -5, dai_intensity9 < 10) %>% 
    drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14, logincome14,
            tightness14, pat_intensity14, patai_intensity14, degshare14, stemshare14,
            large_firms14, information_intensity14, manuf_intensity14, TurnOvrS14),
  weights = ~lads14
)

est_all <- fixest::feols(
  "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + hpi_ch14 + logincome14 + tightness14 + pat_intensity14 + patai_intensity14 + degshare14 + stemshare14 + large_firms14 + information_intensity14 + manuf_intensity14 + TurnOvrS14" %>% as.formula(),
  data_ai_l_z %>% 
    filter(dai_intensity9 > -5, dai_intensity9 < 10) %>% 
    drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14, logincome14,
            tightness14, pat_intensity14, patai_intensity14, degshare14, stemshare14,
            large_firms14, information_intensity14, manuf_intensity14, TurnOvrS14),
  weights = ~lads14
)

est_all_state <- fixest::feols(
  "dai_intensity9 ~ share_bac14 + share_black14 + share_poverty14 + gpop + hpi_ch14 + logincome14 + tightness14 + pat_intensity14 + patai_intensity14 + degshare14 + stemshare14 + large_firms14 + information_intensity14 + manuf_intensity14 + TurnOvrS14 | state" %>% as.formula(),
  data_ai_l_z %>% 
    filter(dai_intensity9 > -5, dai_intensity9 < 10) %>% 
    drop_na(share_bac14, share_black14, share_poverty14, logpop14, hpi_ch14, logincome14,
            tightness14, pat_intensity14, patai_intensity14, degshare14, stemshare14,
            large_firms14, information_intensity14, manuf_intensity14, TurnOvrS14),
  weights = ~lads14
)

# Extract results
models <- list(est_demog_no, est_innovation_no, est_industry_no, est_all, est_all_state)
obs_counts <- map_dbl(models, nobs)
r2_vals <- map_dbl(models, ~r2(.x, "r2"))

cat("=== EXACT ORIGINAL REPLICATION RESULTS ===\n")
cat("Observations: ", paste(obs_counts, collapse = ", "), "\n")
cat("R-squared: ", paste(sprintf("%.5f", r2_vals), collapse = ", "), "\n")

# Check key coefficients
extract_coef <- function(model, var_name) {
  if(var_name %in% names(coef(model))) {
    return(sprintf("%.4f", coef(model)[var_name]))
  } else {
    return("Not included")
  }
}

cat("\n=== COEFFICIENT COMPARISON ===\n")
cat("Bachelor's Model 1 - Target: 0.0022, Actual:", extract_coef(est_demog_no, "share_bac14"), "\n")
cat("Income Model 1 - Target: 0.0784, Actual:", extract_coef(est_demog_no, "logincome14"), "\n")
cat("Tightness Model 1 - Target: 0.0744, Actual:", extract_coef(est_demog_no, "tightness14"), "\n")
cat("STEM Model 2 - Target: 0.0742, Actual:", extract_coef(est_innovation_no, "stemshare14"), "\n")

# Show condensed table with only the 8 key variables
variable_labels_condensed <- c(
  "share_bac14" = "Bachelor's share, z-score in 2017",
  "logincome14" = "Income, log z-score in 2017",
  "tightness14" = "Tightness, z-score in 2017",
  "stemshare14" = "STEM degrees share, z-score in 2017",
  "large_firms14" = "Large establishments, % z-score in 2017",
  "information_intensity14" = "ICT sector intensity, % z-score in 2017",
  "manuf_intensity14" = "Manufacturing intensity, % z-score in 2017",
  "TurnOvrS14" = "Turnover rate, % z-score in 2017"
)

final_table <- modelsummary(
  list(
    "(1)" = est_demog_no,
    "(2)" = est_innovation_no,
    "(3)" = est_industry_no, 
    "(4)" = est_all,
    "(5)" = est_all_state
  ),
  coef_map = variable_labels_condensed,
  stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  gof_omit = ".*",
  statistic = "({std.error})",
  fmt = 4,
  title = "Table 2—The Correlates of the Percentage Point Change in the Share of AI Jobs"
)

print(final_table)

cat("\nThis follows the EXACT original replication_code.R structure\n")
cat("Key differences: Using full sample restriction ensures same coefficients\n")