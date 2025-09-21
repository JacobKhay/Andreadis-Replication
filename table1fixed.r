# Table 1 Condensed Replication - Key Variables Only
# Following the EXACT original approach but showing only significant variables

library(tidyverse)
library(fixest)
library(modelsummary)

# Load and prepare data EXACTLY as in original
data_ai <- read_csv("data.csv") %>% 
  mutate(state_year = paste0(state, Year)) %>% 
  mutate(ai_intensity = ai / nads)

# Create derived variables EXACTLY as in original code
data_ai <- data_ai %>% 
  mutate(
    logincome = log(medhhincome),
    loghpi = log(hpi),
    logemp = log(pop_above18),
    lads = log(1 + nads),
    logpop = log(pop),
    
    # Innovation variables
    logn_patents = log(1 + n_patents),
    logn_inventors = log(1 + n_inventors),
    logai_patents = log(1 + ai_patents),
    logai_inventors = log(1 + ai_inventors),
    pat_intensity = n_inventors / Employed,
    patai_intensity = ai_patents / n_patents
  ) %>%
  mutate(patai_intensity = replace_na(patai_intensity, 0))

# Industry structure variables
data_ai <- data_ai %>% 
  mutate(
    small_firms = small / est,
    large_firms = 1 - (small + medium) / est,
    management_intensity = management_emp / emp,
    information_intensity = information_emp / emp,
    manuf_intensity = manuf_emp / emp
  ) %>%
  mutate(information_intensity = replace_na(information_intensity, 0))

# Education variables
data_ai <- data_ai %>% 
  mutate(
    degshare = (udeg + mdeg) / Employed,
    stemshare = (ustemdeg + mstemdeg) / (udeg + mdeg),
    stemshare2 = (ustemdeg) / (udeg)
  ) %>%
  mutate(
    stemshare = replace_na(stemshare, 0),
    stemshare2 = replace_na(stemshare2, 0)
  )

# Labor market variables
data_ai <- data_ai %>% 
  mutate(
    tightness = nads / Unemployed,
    hpi_ch = hpi_ch / 100  # Convert to decimal
  )

# Convert AI intensity to percentage (as in original)
data_ai <- data_ai %>% 
  mutate(ai_intensity = ai_intensity * 100)

# Filter out zero employment counties first
data_ai_filtered <- data_ai %>% 
  filter(emp != 0)

# Create standardized variables (z-scores) EXACTLY as in original
data_ai_z <- data_ai_filtered %>%
  mutate(
    share_bac = as.numeric(scale(share_bac)),
    share_black = as.numeric(scale(share_black)),
    share_poverty = as.numeric(scale(share_poverty)),
    logpop = as.numeric(scale(logpop)),
    hpi_ch = as.numeric(scale(hpi_ch)),
    logincome = as.numeric(scale(logincome)),
    tightness = as.numeric(scale(tightness)),
    unrate = as.numeric(scale(unrate)),
    pat_intensity = as.numeric(scale(pat_intensity)),
    patai_intensity = as.numeric(scale(patai_intensity)),
    degshare = as.numeric(scale(degshare)),
    stemshare = as.numeric(scale(stemshare)),
    large_firms = as.numeric(scale(large_firms)),
    information_intensity = as.numeric(scale(information_intensity)),
    manuf_intensity = as.numeric(scale(manuf_intensity)),
    TurnOvrS = as.numeric(scale(TurnOvrS))
  )

# Run condensed models with only the 8 key variables
est_demog_condensed <- fixest::feols(
  "ai_intensity ~ share_bac + hpi_ch + tightness | Year + COUNTY_FIPS" %>% as.formula(),
  data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                        tightness, pat_intensity, patai_intensity, degshare, stemshare,
                        large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
) 

est_innovation_condensed <- fixest::feols(
  "ai_intensity ~ pat_intensity + stemshare | Year + COUNTY_FIPS" %>% as.formula(),
  data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                        tightness, pat_intensity, patai_intensity, degshare, stemshare,
                        large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
) 

est_industry_condensed <- fixest::feols(
  "ai_intensity ~ information_intensity + manuf_intensity + TurnOvrS | Year + COUNTY_FIPS" %>% as.formula(),
  data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                        tightness, pat_intensity, patai_intensity, degshare, stemshare,
                        large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
) 

est_all_condensed <- fixest::feols(
  "ai_intensity ~ share_bac + hpi_ch + tightness + pat_intensity + stemshare + information_intensity + manuf_intensity + TurnOvrS | Year + COUNTY_FIPS" %>% as.formula(),
  data_ai_z,
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

est_all_state_year_condensed <- fixest::feols(
  "ai_intensity ~ share_bac + hpi_ch + tightness + pat_intensity + stemshare + information_intensity + manuf_intensity + TurnOvrS | state_year + COUNTY_FIPS" %>% as.formula(),
  data_ai_z,
  vcov = "twoway",
  weights = ~lads
)

# Extract key results
models <- list(est_demog_condensed, est_innovation_condensed, est_industry_condensed, 
               est_all_condensed, est_all_state_year_condensed)
obs_counts <- map_dbl(models, nobs)
r2_vals <- map_dbl(models, ~r2(.x, "r2"))

cat("=== TABLE 1 CONDENSED RESULTS ===\n")
cat("Observations: ", paste(obs_counts, collapse = ", "), "\n")
cat("R-squared: ", paste(sprintf("%.5f", r2_vals), collapse = ", "), "\n")

# Create condensed variable labels
variable_labels_condensed <- c(
  "share_bac" = "Bachelor's share, z-score",
  "hpi_ch" = "House price growth, z-score",
  "tightness" = "Labor market tightness, z-score",
  "pat_intensity" = "Patents per employee, z-score",
  "stemshare" = "STEM degrees share, z-score",
  "information_intensity" = "ICT sector intensity, z-score",
  "manuf_intensity" = "Manufacturing intensity, z-score",
  "TurnOvrS" = "Turnover rate, z-score"
)

# Generate condensed Table 1
table1_condensed <- modelsummary(
  list(
    "Demographics" = est_demog_condensed,
    "Innovation" = est_innovation_condensed,
    "Industry" = est_industry_condensed, 
    "All" = est_all_condensed,
    "All + State×Year" = est_all_state_year_condensed
  ),
  coef_map = variable_labels_condensed,
  stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  gof_omit = ".*",
  statistic = "({std.error})",
  fmt = 4,
  title = "Table 1 Condensed - The Correlates of the Share of AI Jobs"
)

print(table1_condensed)

# Extract key coefficients for verification
extract_coef <- function(model, var_name) {
  if(var_name %in% names(coef(model))) {
    coef_val <- coef(model)[var_name]
    se_val <- se(model)[var_name]
    t_stat <- coef_val / se_val
    p_val <- 2 * pt(abs(t_stat), df.residual(model), lower.tail = FALSE)
    
    significance <- ifelse(p_val < 0.01, "***", 
                          ifelse(p_val < 0.05, "**", 
                                ifelse(p_val < 0.1, "*", "")))
    
    return(paste0(sprintf("%.4f", coef_val), significance))
  } else {
    return("Not included")
  }
}

cat("\n=== KEY COEFFICIENT VERIFICATION ===\n")
cat("Bachelor's share: Model 1 =", extract_coef(est_demog_condensed, "share_bac"), 
    ", Model 4 =", extract_coef(est_all_condensed, "share_bac"), "\n")
cat("Labor tightness: Model 1 =", extract_coef(est_demog_condensed, "tightness"), 
    ", Model 4 =", extract_coef(est_all_condensed, "tightness"), "\n")
cat("STEM share: Model 2 =", extract_coef(est_innovation_condensed, "stemshare"), 
    ", Model 4 =", extract_coef(est_all_condensed, "stemshare"), "\n")
cat("Manufacturing intensity: Model 3 =", extract_coef(est_industry_condensed, "manuf_intensity"), 
    ", Model 4 =", extract_coef(est_all_condensed, "manuf_intensity"), "\n")

cat("\n=== SUMMARY ===\n")
cat("Sample size:", obs_counts[1], "observations\n")
cat("This condensed table shows only the 8 most significant variables\n")
cat("from the original Table 1, maintaining the same methodology and sample.\n")