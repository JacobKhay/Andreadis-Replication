# Submission Materials

Revised submission to the *Journal of Comments and Replications in Economics* (JCRE), round 2.

**Manuscript:** "Local Heterogeneity in Artificial Intelligence Jobs Over Time and Space: A Replication Study of Andreadis et al. (AEA Papers and Proceedings, 2025)"

**Authors:** Jacob Khaykin (Solon High School), David Kane (New College of Florida)

## Contents

| File | Purpose |
|---|---|
| `replication.qmd` | Manuscript source. Renders to all three manuscript files below. |
| `replication.pdf` | Rendered manuscript (regular PDF). |
| `replication.docx` | Rendered manuscript (Word). |
| `replication_anon.pdf` | Anonymized manuscript for blind review, with author names, affiliations and email addresses removed. |
| `make_anon.R` | Produces `replication_anon.pdf`. See the note below on why the Quarto `anon` profile is not used. |
| `cover_letter.md` | Cover letter to the editor. |
| `response_to_editors.md` | Point-by-point response to the round-2 referee reports. |
| `data.csv` | AKCLM's released analysis-ready dataset — the only data input the manuscript reads. |
| `references.bib` | Bibliography. |
| `aea.csl` | AEA citation style. |
| `_quarto.yml` | Quarto project settings (bibliography, CSL, `nocite`). |
| `_quarto-anon.yml` | Profile producing the anonymized PDF. |
| `figures/published_figure1_panel_a.png` | Published AKCLM Figure 1, Panel A, reproduced in Figure 1 of the manuscript. |
| `figures/published_figure1_panel_b.png` | Published AKCLM Figure 1, Panel B, reproduced in Figure 1 of the manuscript. |
| `figures/akclm_published_table2.png` | Page image of the published AKCLM Table 2 (journal page 33), retaining the "VOL. 115 ... 33" running head. Exhibit supporting our response to referee comment R1. |
| `figures/akclm_published_table2_annotated.png` | The same page image with the three disputed bachelor's-share coefficients and the page number marked. |

## Reproducing the manuscript

From this directory:

```bash
quarto render replication.qmd --to pdf
```

For the Word version:

```bash
quarto render replication.qmd --to docx
```

For the anonymized PDF required by JCRE:

```bash
Rscript make_anon.R
```

Note that the Quarto `anon` profile declared in `_quarto.yml` does **not** anonymize
this document. A document's own YAML takes precedence over project-level profile
configuration, so `quarto render replication.qmd --to pdf --profile anon` produces a
PDF that still carries both author names, both affiliations and both email addresses.
`make_anon.R` edits a copy of the source instead, and aborts if any identifying string
survives into it. The GitHub URL in the Data Availability statement is retained by
choice, so that the statement remains complete in the blinded copy.

### Requirements

- R (developed under 4.6.1)
- A LaTeX installation. `quarto install tinytex` suffices.
- R packages: `tidyverse`, `fixest`, `modelsummary`, `kableExtra`, `sf`, `scales`, `tigris`, `ggplot2`, `patchwork`, `png`, `usmap`, `viridis`, `knitr`, `rmarkdown`

```r
install.packages(c("tidyverse","fixest","modelsummary","kableExtra","sf","scales",
                   "tigris","ggplot2","patchwork","png","usmap","viridis",
                   "knitr","rmarkdown"))
```

County boundaries are downloaded once by `tigris` on first render and cached thereafter, so the first render requires an internet connection.

### A note on package versions

Two `fixest` defaults changed in recent releases and would otherwise alter reported statistics without any change to the manuscript:

- `fixef.rm` now drops fixed-effect singletons by default, which changes reported sample sizes.
- `feols()` no longer falls back to clustering on the first fixed effect when no cluster is named, which changes standard errors in the state fixed-effects specification.

Both are now set explicitly in the source, so output is stable across `fixest` versions. Neither affected any coefficient.

## Data provenance

`data.csv` is AKCLM's released dataset, obtained from the openICPSR archive for the original article (project 228344, version V1) at <https://www.openicpsr.org/openicpsr/project/228344/version/V1/view>. The reproduction begins from this analysis-ready file rather than reconstructing the county-year panel from the underlying Lightcast, ACS, QWI, CBP, LAUS, FHFA, and patent sources; this scope is stated in the manuscript.

The original article is Andreadis, Kalotychou, Chatzikonstantinou, Louca and Makridis, "Local Heterogeneity in Artificial Intelligence Jobs over Time and Space," *AEA Papers and Proceedings* 115 (2025): 29–34, DOI [10.1257/pandp.20251001](https://doi.org/10.1257/pandp.20251001).

Data and code for this replication are also deposited with the ZBW Journal Data Archive; a development copy is at <https://github.com/JacobKhay/Andreadis-Replication>.
