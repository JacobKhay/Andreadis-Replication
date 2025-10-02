---
output:
  pdf_document: default
  html_document: default
---
Local Heterogeneity in Artificial Intelligence Jobs over Time and Space

April 2025

This document describes the replication package that accompany the paper “Local Heterogeneity in Artificial Intelligence Jobs over Time and Space":

The tables in the paper are condensed to include only rows of variables of higher significance. The data used in this paper, 
additional results and  a detailed explanation of data construction instructions are also available here:
https://github.com/mhatzikonstantinou/aicounty

References:

>Aysheshim, K., Jacob R. Hinson, and Sharon D. Panek. "A primer on local area gross domestic product methodology." Survey of Current Business 100.3 (2020): 1-13.

>Bureau of Labor Statistics (BLS), Quarterly Census of Employment and Wages.

>U.S. Census Bureau, American Community Survey accessed on July 1, 2024 < https://www.census.gov/data/datasets/time-series/econ/acs/acs-datasets.html>.

>U.S. Census Bureau, County Business Patterns accessed on July 1, 2024 < https://www.census.gov/programs-surveys/cbp/data/datasets.html>.

>U.S. Census Bureau, Quarterly Workforce Indicators (2013-2024). Washington, DC: U.S. Census Bureau, Longitudinal-Employer Household Dynamics Program , accessed on 11/29/2024 at https://qwiexplorer.ces.census.gov.

>U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), 2013-2022. Retrieved on 11/29/2024.

>U.S. Patent and Trademark Office. “Data Download Tables.” PatentsView. Accessed [11/29/2024].https://patentsview.org/ download/data-download-tables.

>Giczy, A. V., Pairolero, N. A., & Toole, A. A. (2022). Identifying artificial intelligence (AI) invention: A novel AI patent dataset. The Journal of Technology Transfer, 47(2), 476-505.

>Bogin, A., Doerner, W., & Larson, W. (2019). Local house price dynamics: New indices and stylized facts. Real Estate Economics, 47(2), 365-398.

>Lightcast, 2024

The folder contains the following:
1) data.csv – This file contains one observation for each year-county observation with the following variables(source):

- state: Represents the state associated with the data record.
- Year: Indicates the year to which the record corresponds.
- COUNTY_FIPS: The Federal Information Processing Standards (FIPS) code for the county.
- ai: The measure of AI activity, as proxied by the number of ads containing AI keywords in the given county. (Lightcast)
- nads: The  count of number of online job posts for any occupation in the private sector  in the given county. (Lightcast)
- share_poverty: The proportion or percentage of the population living below the poverty line in the county. (ACS)
- TurnOvrS: Labor turnover for stable jobs. (QWI)
- medhhincome: The median household income in the county. (ACS)
- hpi:  House Price Index.(Bogin et al)
- hpi_ch: The percentage change of the House Price Index. (Bogin et al)
- pop_above18: The population aged 18 and above in the county. (ACS)
- unrate: The unemployment rate in the county. (LAUS)
- share_bac: The proportion of the population with a bachelor's degree. (ACS)
- share_black: The proportion  of the population identifying as Black. (ACS)
- medage: The median age of the population in the county.(ACS)
- small: The count of small establishments in the county. (CBP)
- medium: The count of medium establishments in the county. (CBP)
- est: The total number of establishments  in the county.  (CBP)
- management_emp: Employment figures specifically for jobs within the management sector. (QCEW)
- manuf_emp: Employment figures for the manufacturing sector.(QCEW)
- information_emp: Employment figures for the information sector.(QCEW)
- emp: total employment in the county.(QCEW)
- Employed: The number of employed individuals in the county.(ACS)
- pop: The total population of the county.(ACS)
- n_patents: The number of patents associated with the county.(USPTO)
- n_inventors: The number of inventors associated with the county.(USPTO)
- ai_patents: The number of patents specifically related to Artificial Intelligence in the county. (USPTO)
- ai_inventors: The number of inventors with Artificial Intelligence specific patents in the county.  (USPTO)
- median_rent: The median cost of renting a property in the county. (ACS)
- information_est: The number of establishments  in the information sector in the county.  (CBP)
- manuf_est: The number of establishments (businesses) in the manufacturing sector in the county.  (CBP)
- private_gdp:  Gross Domestic Product attributed to the private sector within the county. (BEA County GDP)
- information_annual_avg_estabs: The annual average number of establishments in the information sector. (QCEW)
- information_annual_avg_emplvl: The annual average employment level in the information sector. (QCEW)
- manuf_annual_avg_estabs: The annual average number of establishments in the manufacturing sector. (QCEW)
- annual_avg_estabs: The annual average total number of establishments in the county. (QCEW)
- annual_avg_emplvl: The annual average total employment level in the county. (QCEW)
- udeg: Individuals with an undergraduate degree as their highest level of education. (IPEDS)
- mdeg: Individuals with a master's degree as their highest level of education. (IPEDS)
- ustemdeg: Individuals with an undergraduate degree in a Science, Technology, Engineering, or Mathematics (STEM) field. (IPEDS)
- mstemdeg: Individuals with a master's degree in a Science, Technology, Engineering, or Mathematics (STEM) field. (IPEDS)
- Unemployed: The number of unemployed individuals in the county. (LAUS)

2) replication_code.R: contains the code in R that replicates the tables in the paper.

Packages to Install:

You need to install the following R packages:

- pander
- fixest
- tidyverse
You can install them in your R console using the following command:

Code snippet
install.packages(c("pander", "fixest", "tidyverse"))

Short Description of the Code:

This R code performs a regression analysis to investigate the factors associated with AI intensity at the U.S. county level.

The main steps are:

-  for Table 1: Reads county-level data from a CSV file, creates new variables and standardizes the key independent variables by converting them into
z-scores for easier comparison of coefficient magnitudes in regressions. 
Uses the fixest package to run several fixed-effects regression models (feols) to examine how demographic, innovation,
and industry structure variables relate to AI intensity , while controlling for unobserved heterogeneity across years and counties 
(or state-year and county fixed effects).

-  for Table 2: Reads county-level data from a CSV file, creates the long difference of AI intensity for the period 2018-2023, 
where data for 2017 and 2018 are pooled together and similarly data for 2022 and 2023. It then creates the control variables at the first year in the sample(2017) and 
standardizes them by converting them into z-scores. Uses the fixest package to run several fixed-effects regression models (feols) to examine how demographic, 
innovation, and industry structure variables relate to the  change in AI intensity, while controlling for unobserved heterogeneity across years and counties 
(or state-year and county fixed effects).

