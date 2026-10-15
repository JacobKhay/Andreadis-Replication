# Interim Response to Referee A, Comment R1

*Draft email to the editor. A full point-by-point response to all comments will follow.*

---

**To:** Prof. Marianne Saam, Editor, Journal of Comments and Replications in Economics
**Cc:** David Kane (dakane@ncf.edu)
**Subject:** Re: JCRE submission — source of the "Original" column in Table 4 (Referee A, R1)

**Attachments:**
1. `Andreadis_et_al_published_AEA_PP_2025.pdf` — the published article
2. `Andreadis_et_al_preprint_AEA_conference.pdf` — the preprint Referee A appears to have used
3. `akclm_published_table2.png` / `akclm_published_table2_annotated.png` — page image of Table 2 from the published article, page 33
4. `akclm_preprint_table2.png` / `akclm_preprint_table2_annotated.png` — page image of Table 2 from the preprint, page 11

---

Dear Professor Saam,

Thank you for the decision and for forwarding both reports. We will submit a full point-by-point response and a revised manuscript within the next week or so. In the meantime, we would like to settle Referee A's R1 straight away, since it concerns our headline claim and is quickly resolved.

Referee A writes:

> Your central partial-reproduction claim (Table 4, abstract, conclusion) asserts that AKCLM's published Table 2 reports bachelor's-share coefficients of 0.0022 / -0.0035 / -0.0044 (Cols 1/4/5), differing ninefold from the released code's output.

and asks us to:

> Please state exactly which document your "Original" column is transcribed from (version, DOI, page) and provide a page image of that table.

**The source of our "Original" column.** Our transcription is from the published version of record:

> Andreadis, Lefteris, Eleni Kalotychou, Manolis Chatzikonstantinou, Christodoulos Louca, and Christos A. Makridis. 2025. "Local Heterogeneity in Artificial Intelligence Jobs over Time and Space." *AEA Papers and Proceedings* 115: 29–34.
> DOI: [10.1257/pandp.20251001](https://doi.org/10.1257/pandp.20251001)

The exhibit is **Table 2, "The Correlates of the Percentage Point Change in the Share of AI Jobs," on page 33.** Its first row reads:

| Row | (1) | (4) | (5) |
|---|---|---|---|
| Bachelor's share, *z*-score in 2017 | 0.0022 | −0.0035 | −0.0044 |
| | (0.0027) | (0.0032) | (0.0029) |

Here is the top of Table 2 from the published version (*AEA Papers and Proceedings* 2025, 115: 29–34), from page 33, with the three coefficients marked:

![Table 2 from the published version of Andreadis et al. (2025), AEA Papers and Proceedings 115: 29-34, page 33. The row "Bachelor's share, z-score in 2017" reports 0.0022, -0.0035 and -0.0044 in columns 1, 4 and 5.](figures/akclm_published_table2_annotated.png)

The running head of the page is retained in the image, so the volume and page number are visible on the exhibit itself: "VOL. 115 ... 33". An unmarked copy of the same image is attached as the formal exhibit, along with the full published article.

**Why Referee A could not find these values.** The referee is working from the preprint distributed through the AEA conference program at <https://www.aeaweb.org/conference/2025/program/paper/EFEnFGBY>, which is marked "FORTHCOMING IN AE P&P 2025" on its first page. We attach it for comparison. Its Table 2, "The Correlates of the Change in the Share of Artificial Intelligence Jobs," appears on page 11, and the corresponding row reads:

![Table 2 from the preprint version of Andreadis et al., page 11. The row "Bachelors,% z-score in 2017" reports 0.0196, -0.0322 and -0.0401 in columns 1, 4 and 5.](figures/akclm_preprint_table2_annotated.png)

Those values — 0.0196 (0.0250), −0.0322 (0.0292), −0.0401 (0.0263) — are precisely what AKCLM's released code produces, and precisely the values in our "Replication" column. So in the preprint there is indeed no discrepancy, exactly as the referee observed: there the table and the code agree.

The discrepancy we report is therefore specific to the **published** article: between the version of record and the released replication code. We accept that our manuscript did not make that scope explicit, referring only to "AKCLM's published Table 2" without identifying the version. We are grateful to the referee for pressing on this, and the revision will state the version, page, and DOI wherever the claim appears, together with a note that readers working from the preprint or the SSRN version will not encounter the discrepancy.

**On the substance.** Having both documents side by side makes the mechanical relationship between them exact. In AKCLM's released `data.csv` the 2017 bachelor's-share variable has a standard deviation of 0.0911, or 9.114 percentage points. Dividing each preprint coefficient by that figure reproduces the published coefficient in every column:

| Column | Preprint | ÷ 9.114 | Published |
|---|---|---|---|
| (1) | 0.0196 | 0.00215 | **0.0022** |
| (4) | −0.0322 | −0.00353 | **−0.0035** |
| (5) | −0.0401 | −0.00440 | **−0.0044** |

The agreement holds to the full displayed precision in all three columns. The row was therefore rescaled between the preprint and the published article, from a per-standard-deviation coefficient to a per-percentage-point coefficient. The row label, however, still reads "*z*-score in 2017" in the published version, exactly as it did in the preprint — the preprint labels it "Bachelors,% z-score in 2017" and the published version "Bachelor's share, *z*-score in 2017."

The published table therefore reports this coefficient on a different scale from the one its own label describes, and from the one every other row in that table uses. That is the discrepancy our manuscript documents. We wrote to the authors about it and received no reply, so we make no claim about how it arose; we report only that the published row cannot be reproduced from the released code as labeled, and that the rescaling above accounts for the difference exactly.

We hope this resolves R1. The remaining points from Referee A (R2, R3, and r1) are formatting and labeling matters, and all are already addressed in the revision now being prepared.

With thanks to you and both referees for the care taken with the manuscript,

Jacob Khaykin
Solon High School
jacobkhaykin27@solonschools.net

David Kane
New College of Florida
dakane@ncf.edu

---

## Checklist before sending

- [x] **Published article** — `Andreadis_et_al_published_AEA_PP_2025.pdf` in this folder. First page header: "AEA Papers and Proceedings 2025, 115: 29–34" with the DOI. Table 2 on PDF page 5 = journal page 33.
- [x] **Preprint** — `Andreadis_et_al_preprint_AEA_conference.pdf` in this folder. First page marked "FORTHCOMING IN AE P&P 2025". Table 2 on page 11.
- [x] **Preprint values verified** by reading them directly off the attached preprint: 0.0196 (0.0250), −0.0322 (0.0292), −0.0401 (0.0263).
- [x] **Scaling verified** against the released `data.csv`: sd(share_bac, 2017) = 0.0911, and each preprint coefficient divided by 9.114 reproduces the published coefficient to the displayed precision.
- [x] Page images prepared, both retaining their running heads.
- [ ] Cc David Kane.
- [ ] David to confirm he is content with the wording of "On the substance" before this goes out.
