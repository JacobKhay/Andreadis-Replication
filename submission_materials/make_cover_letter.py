"""Generate the JCRE cover letter PDF.

The layout reproduces the cover letter submitted in the previous round, which was
built with ReportLab in Helvetica rather than through Quarto. Every measurement
below (positions, sizes, colours, leading, the header rule and the footer) was
read back off that PDF so this round's letter matches it.

Note that "Cover letter for revised submission | JCRE Manuscript No. 60" is a
footer in that design, not a title block at the top of the page.

Usage:  python make_cover_letter.py
Output: submission_materials/cover_letter.pdf
"""

from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor
from reportlab.pdfbase.pdfmetrics import stringWidth

PAGE_W, PAGE_H = 612.0, 792.0
LEFT = 67.2                 # body left margin
RIGHT = 544.8               # body right edge (header rule ends here)
FOOT_L, FOOT_R = 61.2, 550.8

NAVY = HexColor("#1f2937")  # author line
GREY = HexColor("#4b5563")  # header metadata lines
BODY = HexColor("#111827")  # body text
RULE = HexColor("#9ca3af")  # header rule
FOOT_RULE = HexColor("#e5e7eb")
FOOT_TXT = HexColor("#6b7280")

BODY_SIZE, BODY_LEAD = 10.8, 16.2
PARA_GAP = 26.2             # baseline-to-baseline between paragraphs

AUTHORS = "Jacob Khaykin and David Kane"
META1 = "Manuscript No. 60 | Journal of Comments and Replications in Economics"
META2 = "Local Heterogeneity in Artificial Intelligence Jobs Over Time and Space"
DATE = "September 6, 2026"
FOOTER_L = "Cover letter for revised submission"
FOOTER_R = "JCRE Manuscript No. 60"

RE_LINE = ('Re: Revised submission of "Local Heterogeneity in Artificial Intelligence Jobs '
           'Over Time and Space: A Replication Study of Andreadis et al. '
           '(AEA Papers and Proceedings, 2025)"')

PARAGRAPHS = [
    'We are pleased to submit a revised version of our manuscript, "Local Heterogeneity in '
    'Artificial Intelligence Jobs Over Time and Space: A Replication Study of Andreadis et al. '
    '(AEA Papers and Proceedings, 2025)," for further consideration by the Journal of Comments '
    'and Replications in Economics.',

    'We thank you and the reviewers for the continued and constructive feedback, and we are '
    'grateful that Reviewer B considers the previous version ready for publication. In response '
    'to the remaining points from Reviewer A, we have documented the source of the disputed '
    'published values, reformatted the tables that were printing at an illegibly small size, '
    'rebuilt the colour bins of the replication maps so that every county with a value is drawn, '
    'framed the weighting extension with reference to Solon, Haider and Wooldridge (2015), and '
    'standardized the variable labels across all figures and tables. A detailed point-by-point '
    'response accompanies this resubmission.',

    'Following your request that the version information be included in the resubmission, the '
    'manuscript now states the version, page, and DOI of the source for the "Original" column '
    'wherever the claim appears, and records that the AEA conference preprint and the SSRN '
    'version instead agree with the released code. Page images of the published table and of the '
    'preprint table are reproduced in the appendix. The revised source file and supporting figure '
    'materials have been uploaded with this resubmission, and the stable archival materials are '
    "referenced in the manuscript's Data Availability statement.",

    'Thank you for considering our revised submission.',
]


def wrap(text, font, size, width):
    """Greedy wrap on measured string widths."""
    words, lines, cur = text.split(), [], ""
    for w in words:
        trial = w if not cur else cur + " " + w
        if stringWidth(trial, font, size) <= width:
            cur = trial
        else:
            if cur:
                lines.append(cur)
            cur = w
    if cur:
        lines.append(cur)
    return lines


def main(out="submission_materials/cover_letter.pdf"):
    c = canvas.Canvas(out, pagesize=(PAGE_W, PAGE_H))
    c.setTitle("Cover letter for revised submission")
    avail = RIGHT - LEFT

    def draw(text, y, font="Helvetica", size=BODY_SIZE, color=BODY):
        c.setFont(font, size)
        c.setFillColor(color)
        c.drawString(LEFT, PAGE_H - y, text)

    # --- header block ---
    draw(AUTHORS, 74.0, "Helvetica-Bold", 14.0, NAVY)
    draw(META1, 88.5, "Helvetica", 9.5, GREY)
    draw(META2, 101.5, "Helvetica", 9.5, GREY)
    c.setStrokeColor(RULE)
    c.setLineWidth(1.0)
    c.line(LEFT, PAGE_H - 115.0, RIGHT, PAGE_H - 115.0)

    # --- date and addressee ---
    draw(DATE, 144.5, "Helvetica", 10.5)
    draw("Professor Marianne Saam", 172.8)
    draw("Journal of Comments and Replications in Economics", 189.0)

    # --- subject line (bold, wrapped) ---
    y = 215.2
    for ln in wrap(RE_LINE, "Helvetica-Bold", BODY_SIZE, avail):
        draw(ln, y, "Helvetica-Bold")
        y += 15.0

    # --- salutation and body ---
    y = 259.2
    draw("Dear Professor Saam,", y)
    y += PARA_GAP
    for para in PARAGRAPHS:
        for ln in wrap(para, "Helvetica", BODY_SIZE, avail):
            draw(ln, y)
            y += BODY_LEAD
        y += PARA_GAP - BODY_LEAD

    # --- sign-off ---
    y += 8.0
    draw("Sincerely,", y)
    y += 34.2
    draw("Jacob Khaykin", y)
    y += BODY_LEAD
    draw("David Kane", y)

    # --- footer ---
    c.setStrokeColor(FOOT_RULE)
    c.setLineWidth(1.0)
    c.line(FOOT_L, PAGE_H - 752.4, FOOT_R, PAGE_H - 752.4)
    c.setFont("Helvetica", 8.5)
    c.setFillColor(FOOT_TXT)
    c.drawString(FOOT_L, PAGE_H - 764.6, FOOTER_L)
    c.drawRightString(FOOT_R, PAGE_H - 764.6, FOOTER_R)

    c.showPage()
    c.save()
    print("wrote", out)


if __name__ == "__main__":
    main()
