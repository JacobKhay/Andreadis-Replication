# Generates the anonymized manuscript that JCRE requires for blind review.
#
# The Quarto `anon` profile in _quarto.yml does not do this. A document's own
# YAML takes precedence over project-level profile configuration, so rendering
# with `--profile anon` still produced a PDF carrying both author names, both
# affiliations and both email addresses. This script strips the identifying
# content from a copy of the source instead, then renders that copy.
#
# Removed:
#   1. the author names in the YAML header
#   2. the affiliation footnotes and email addresses attached to them
#
# Deliberately kept: the GitHub URL in the Data Availability statement. It
# contains an author's username, but the authors have chosen to retain it so the
# data-availability requirement is met in the blinded copy as well.
#
# Usage:  Rscript make_anon.R
# Output: replication_anon.qmd (intermediate) and replication_anon.pdf

suppressPackageStartupMessages(library(readr))

# Read as raw bytes so the file's UTF-8 byte-order mark is preserved. Writing
# the copy without it made Quarto treat the document differently and render the
# anonymized PDF at a different type size from the main one.
src <- rawToChar(read_file_raw("replication.qmd"))
Encoding(src) <- "UTF-8"

# 1 + 2. Replace the two author entries with a single anonymous one.
author_block <- paste0(
  '  - name: "Jacob Khaykin^[Solon High School, ',
  '[jacobkhaykin27@solonschools.net](mailto:jacobkhaykin27@solonschools.net)]"\r\n',
  '  - name: "David Kane^[New College of Florida, ',
  '[dakane@ncf.edu](mailto:dakane@ncf.edu)]"'
)
stopifnot(grepl(author_block, src, fixed = TRUE))
src <- sub(author_block, '  - name: "Anonymous"', src, fixed = TRUE)

writeBin(charToRaw(src), "replication_anon.qmd")

# Verify nothing identifying survived into the source before rendering.
# The Data Availability URL is intentionally retained, so "JacobKhay" is not
# checked here.
leaks <- c("Khaykin", "Kane", "Solon High", "New College",
           "jacobkhaykin", "dakane")
found <- leaks[vapply(leaks, function(x) grepl(x, src, fixed = TRUE), logical(1))]
if (length(found)) {
  stop("Identifying text still present in anonymized source: ",
       paste(found, collapse = ", "))
}

cat("replication_anon.qmd written and checked clean. Rendering...\n")
# Use the Quarto CLI rather than the `quarto` R package, which is not a
# dependency of this project. RStudio and Positron each ship a copy.
quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    file.path(Sys.getenv("LOCALAPPDATA"),
              "Programs/Positron/resources/app/quarto/bin/quarto.exe")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) quarto_bin <- hit[1]
}
if (!nzchar(quarto_bin)) {
  stop("Could not find the Quarto CLI. Render manually with:
",
       "  quarto render replication_anon.qmd --to pdf")
}
status <- system2(quarto_bin, c("render", "replication_anon.qmd", "--to", "pdf"))
if (status != 0) stop("quarto render failed with status ", status)
cat("replication_anon.pdf written.
")
