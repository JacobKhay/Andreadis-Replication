# Round 2 Submission — Upload Checklist

JCRE submission portal. Files are numbered in the order they were uploaded last round,
so each one maps to the same slot.

| # | File | Portal category | Notes |
|---|---|---|---|
| 1 | `1_Manuscript_anonymized.pdf` | Paper/Manuscript anonymized | 25 pp. No author names, affiliations or emails. |
| 2 | `2_Manuscript.pdf` | Paper/Manuscript | 29 pp. |
| 3 | `3_Response_to_comments.pdf` | Other | Point-by-point response to both referees. |
| 4 | `4_replication.qmd` | Research Materials | Manuscript source. |
| 5 | `5_published_figure1_panel_a.png` | Research Materials | Same file as last round. |
| 6 | `6_published_figure1_panel_b.png` | Research Materials | Same file as last round. |
| 7 | `7_Cover_letter.pdf` | Cover Letter | |

## Optional additions this round

These two are new. They are the evidence for Referee A's comment R1, and the editor
asked for this information to be included in the resubmission. They are already
reproduced inside the manuscript as appendix Figures 6 and 7, so uploading them
separately is belt-and-braces rather than required.

| # | File | Portal category | Notes |
|---|---|---|---|
| 8 | `8_published_table2_page33.png` | Research Materials | Table 2 of the published version of record, journal page 33. The running head "VOL. 115 ... 33" is retained in the image, so the volume and page are visible on the exhibit itself. |
| 9 | `9_preprint_table2_page11.png` | Research Materials | Table 2 of the AEA conference preprint, page 11, for comparison. |

## What changed since the last submission

- **R1** — The manuscript now names the version, page and DOI of the source for the
  "Original" column wherever the claim appears, records that the AEA conference
  preprint and the SSRN version agree with the released code, and shows the scaling
  exactly: each preprint coefficient divided by 9.114 returns the published
  coefficient in all three columns. Both tables are reproduced in the appendix.
- **R2(a)** — Table 5 reformatted; it printed at roughly 3pt and now prints at 9pt.
- **R2(b)** — Map colour bins rebuilt as half-open intervals on the published
  cut-offs. All 3,130 counties are now drawn; four with valid values were previously
  rendered as "No data".
- **R3** — Table 10 reformatted from roughly 3pt to 7.5pt with a spanning header.
  Solon, Haider and Wooldridge (2015) added and discussed where the extension is
  motivated.
- **r1** — All figure and table labels standardized to one sentence-case set.
- **Tables 7, 8 and 9** were also enlarged, from 6pt and 4.6pt to 7.5pt and 8pt.
- **Reproducibility** — Two `fixest` defaults that had changed in recent versions were
  pinned, so reported sample sizes and the state fixed-effects standard errors no
  longer vary with the installed package version. No coefficient was affected.

Every reported statistic in Tables 1-10 was checked against the previous submission
and matches.

## Before uploading

- [ ] Read the "On the substance" paragraph in `3_Response_to_comments.pdf`. It is
      firmer than the wording in David's interim email and should be his call.
- [ ] Decide whether to include items 8 and 9.
