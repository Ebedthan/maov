clean_qmd_files <- function() {
  qmd_files <- list.files(pattern = "\\.qmd$", recursive = TRUE)
  
  for (path in qmd_files) {
    content <- readLines(path, warn = FALSE, encoding = "UTF-8")
    
    # Remove non-printable ASCII control characters
    # Keep: tab (\t = \x09), newline (\n = \x0a), carriage return (\r = \x0d)
    # Remove: all other control characters \x00-\x08, \x0b, \x0c, \x0e-\x1f, \x7f
    cleaned <- gsub(
      "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]",
      "",
      content
    )
    
    if (!identical(content, cleaned)) {
      writeLines(cleaned, path, useBytes = FALSE)
      cat("Cleaned:", path, "\n")
      
      # Show which lines were affected
      bad_lines <- which(content != cleaned)
      cat("  Lines affected:", paste(bad_lines, collapse = ", "), "\n")
    }
  }
  cat("Done.\n")
}

clean_qmd_files()
