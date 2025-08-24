# Install quarto package if not already installed
if (!require("quarto", quietly = TRUE)) {
  install.packages("quarto")
}
library(quarto)

# Render Word (DOCX) version with names (default profile)
quarto_render(
  input = "replication.qmd",
  output_format = "docx",
  output_file = "replication.docx"
)

# Render PDF version with names (default profile)
quarto_render(
  input = "replication.qmd",
  output_format = "pdf",
  output_file = "replication.pdf"
)

# Render anonymized PDF version without names (using 'anon' profile)
quarto_render(
  input = "replication.qmd",
  output_format = "pdf",
  output_file = "replication_anon.pdf",
  profile = "anon"
)

# Optional: Print confirmation
cat("Rendering complete. Files produced:\n- replication.docx\n- replication.pdf\n- replication_anon.pdf\n")
