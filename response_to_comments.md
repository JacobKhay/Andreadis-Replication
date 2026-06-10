# Response to Reviewer Comments

## Concern 3

**Reviewer concern:** "Third, the unresolved discrepancies need more analysis."

**Revision made:** We expanded the discussion of the two unresolved reproduction issues: Table 2 bachelor's share and Figure 1 Panel B. We now state that the source remains unknown, but we quantify the discrepancies and describe likely classes of causes.

**Where revised:** Replication, Table 2.

**Text added:** "The source of this discrepancy remains unknown, but the pattern is systematic rather than random. In column 1, the published bachelor's-share coefficient is 0.0022 with a standard error of 0.0027, while rerunning AKCLM's released code and data gives 0.0196 with a standard error of 0.0250. In columns 4 and 5, the published coefficients are -0.0035 and -0.0044, while the corresponding rerun values are -0.0322 and -0.0401. Thus, both the coefficient and standard error are approximately nine times larger in the rerun than in the published table. This suggests a reporting or scaling discrepancy specific to bachelor's share, because the same rerun reproduces the remaining reported coefficients."

**Where revised:** Replication, Figure 1.

**Text added:** "Panel A is reproducible from the released data: the generated county values match exactly across all 3,130 counties in the comparison file, and no county changes bins. Panel B is not reproducible in the same way. When we compare the available AKCLM map-code construction (2023 annual intensity minus 2018 annual intensity) with the article's stated pooled-window construction (2022--2023 minus 2017--2018), 2,562 of 3,130 counties have a nonzero difference, 1,453 counties change color bins, and the median absolute difference is 0.138 percentage points. The differences exceed 0.10 percentage points in 1,805 counties, 0.25 points in 1,076 counties, 0.50 points in 548 counties, 1 point in 204 counties, 2 points in 58 counties, and 5 points in 11 counties; the maximum absolute difference is 9.890 percentage points."

**Text added:** "These discrepancies are most consistent with a coding or definition difference in the Panel B time window rather than a data-vintage difference, because both calculations use the same released AKCLM `data.csv`. County definitions also matter for coverage: the released data include 27 Alaska counties and 4 Hawaii counties but no Connecticut county FIPS rows, so Connecticut cannot be mapped with values from the released data."

## Concern 4

**Reviewer concern:** "Fourth, the robustness extension needs a cleaner design."

**Revision made:** We revised the log-population weighting extension so weighted and unweighted comparisons use the same no-tightness specification. This follows the reviewer's first suggested option.

**Where revised:** Extension: Log-Population Weighting Analysis.

**Text added:** "Because labor market tightness shares total job postings with the denominator of AI intensity, we omit labor market tightness from both the equal-weighted and log-population-weighted extension models. The weighting comparison therefore holds the specification fixed and changes only the weighting scheme."

**Where revised:** Notes to extension figures.

**Text added:** "Labor market tightness is omitted from both equal-weighted and log-population-weighted models due to denominator contamination."

## Concern 5

**Reviewer concern:** "Fifth, the decision to report only variables that are statistically significant in at least one specification should be reconsidered."

**Revision made:** We retained AKCLM's selective reporting rule for comparability with the original article, but we made that choice explicit in both the replication and extension sections.

**Where revised:** Replication.

**Text retained:** "Following AKCLM, we report only variables that have statistically significant coefficients in at least one model specification."

**Where revised:** Extension: Log-Population Weighting Analysis.

**Text added:** "Following AKCLM, we report only variables that have statistically significant coefficients in at least one model specification."

## Substantive Interpretation

**Reviewer concern:** "Finally, on the substantive interpretation, your critique of causal language in the original paper is sensible."

**Revision made:** We separated confirmed reproduction results, partial/non-reproduction results, and new methodological concerns in the conclusion. We also weakened the interpretation of the weighting exercise so coefficient attenuation is not presented as proof that relationships are stronger in smaller counties.

**Where revised:** Extension: Log-Population Weighting Analysis.

**Text added:** "We do not interpret these shifts as proof that relationships are systematically stronger in smaller counties; attenuation under population weighting could also reflect high-variance observations or outliers among less-populous counties. A direct population-heterogeneity test, such as estimating models separately by population quartile, would be needed to distinguish those mechanisms."

**Where revised:** Conclusion.

**Text added:** "This replication of AKCLM produces three sets of findings. First, several core regression results are reproduced. Table 1 is reproduced from the released analysis-ready data and code to the displayed precision, and most Table 2 coefficients also match when the released code is rerun."

**Text added:** "Second, two results are only partially reproduced. In Table 2, the bachelor's-share coefficient and standard error differ systematically from the published table in columns 1, 4, and 5, with the rerun values approximately nine times larger than the published values. In Figure 1, Panel A is reproducible from the released data, but Panel B is not: 1,453 of 3,130 counties change bins when comparing the available map-code construction with the pooled-window definition used in our replication, and 548 counties differ by more than 0.50 percentage points."

**Text added:** "Third, our methodological extensions raise separate concerns. AKCLM employ causal language that the observational fixed-effects design cannot support without an explicit identification strategy. In addition, labor market tightness mechanically shares total job postings with the denominator of AI intensity, creating a denominator-contamination concern. Finally, the log-population weighting exercise shows that some coefficient magnitudes change when more weight is given to populous counties, but those shifts should be interpreted as descriptive robustness evidence rather than proof of systematically stronger relationships in smaller counties."
