## Replication Instructions

To replicate our paper, clone this repository and then, from the command line, type:

`quarto render replication.qmd`


## Background

This project replicates the analysis from Andreadis et al. (2025), "Local Heterogeneity in Artificial Intelligence Jobs over Time and Space," published in AEA Papers and Proceedings. The original study examines spatial and temporal variations in AI-related job postings across U.S. counties.

We obtained the original replication materials and data from the openICPSR repository at https://www.openicpsr.org/openicpsr/project/228344/version/V1/view. The primary data sources include:

- **Lightcast (formerly Emsi Burning Glass)** job postings data, which provides detailed information on AI-related job openings at the county level
- **U.S. Census Bureau** data including County Business Patterns (CBP), American Community Survey (ACS) 5-Year estimates, and Quarterly Workforce Indicators (QWI)
- **U.S. Bureau of Labor Statistics** Local Area Unemployment Statistics (LAUS)
- **Patent data** from the USPTO identifying AI-related inventions at the county level

Data cleaning steps included merging county-level datasets, handling missing values, and creating standardized measures of AI job intensity and demographic controls. We also used the updated Lightcast county CSV file (`updated_lightcast_county.csv`) to merge data points together to obtain Connecticut county-level data, which was not available in the original dataset. All data processing and analysis code is included in this repository.
