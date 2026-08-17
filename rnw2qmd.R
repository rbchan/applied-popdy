rnw2qmd <- function(rnw_path, qmd_path) {
  # 1. Read the Rnw file
  if (!file.exists(rnw_path)) stop("The specified .Rnw file does not exist.")
  lines <- readLines(rnw_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  
  # Helper function to extract metadata using base regex
  extract_meta <- function(pattern, text, default) {
    match <- regexec(pattern, text)
    matches <- regmatches(text, match)[[1]]
    if (length(matches) > 1) return(matches[2])
    return(default)
  }
  
  # 2. Extract metadata (Title, Author) for the YAML header
  title <- extract_meta("\\\\title\\{(.*?)\\}", content, "Converted Presentation")
  author <- extract_meta("\\\\author\\{(.*?)\\}", content, "Author Name")
  
  # Remove core LaTeX template wrappers using base gsub
  content <- gsub("\\\\documentclass\\[.*?\\]\\{.*?\\}", "", content)
  content <- gsub("\\\\documentclass\\{.*?\\}", "", content)
  content <- gsub("\\\\begin\\{document\\}", "", content)
  content <- gsub("\\\\end\\{document\\}", "", content)
  content <- gsub("\\\\title\\{.*?\\}", "", content)
  content <- gsub("\\\\author\\{.*?\\}", "", content)
  content <- gsub("\\\\maketitle", "", content)
  
  # 3. Parse and translate Sweave chunks to Quarto chunks
  lines <- unlist(strsplit(content, "\n", fixed = TRUE))
  in_chunk <- FALSE
  new_lines <- c()
  
  for (line in lines) {
    # Match Sweave block start: <<label, option=value>>=
    if (!in_chunk && grepl("^\\s*<<", line) && grepl(">>=\\s*$", line)) {
      in_chunk <- TRUE
      
      # Extract text inside << >>
      match_header <- regexec("^\\s*<<(.*)>>=\\s*$", line)
      header_content <- regmatches(line, match_header)[[1]]
      
      # Split by commas and trim whitespace
      opts <- unlist(strsplit(header_content[2], ",", fixed = TRUE))
      opts <- trimws(opts)
      
      label <- NULL
      quarto_opts <- c()
      
      for (i in seq_along(opts)) {
        opt <- opts[i]
        if (!grepl("=", opt, fixed = TRUE)) {
          if (i == 1) label <- opt # First nameless option is the label
        } else {
          kv <- unlist(strsplit(opt, "=", fixed = TRUE))
          key <- trimws(kv[1])
          val <- trimws(kv[2])
          # Convert booleans to lowercase for YAML compatibility
          if (tolower(val) == "true") val <- "true"
          if (tolower(val) == "false") val <- "false"
          quarto_opts <- c(quarto_opts, paste0("#| ", key, ": ", val))
        }
      }
      
      new_lines <- c(new_lines, "```{r}")
      if (!is.null(label) && label != "") {
        new_lines <- c(new_lines, paste0("#| label: ", label))
      }
      if (length(quarto_opts) > 0) {
        new_lines <- c(new_lines, quarto_opts)
      }
      
    } else if (in_chunk && grepl("^\\s*@\\s*\\$", line)) {
      # End of Sweave block: @
      in_chunk <- FALSE
      new_lines <- c(new_lines, "```")
    } else {
      # Document content or code body
      new_lines <- c(new_lines, line)
    }
  }
  
  interim_content <- paste(new_lines, collapse = "\n")
  
  # 4. Run Pandoc on the text to convert remaining LaTeX environments to Markdown
  tmp_tex <- tempfile(fileext = ".tex")
  tmp_md <- tempfile(fileext = ".md")
  writeLines(interim_content, tmp_tex)
  
  system2("pandoc", args = c(tmp_tex, "-f", "latex", "-t", "markdown", "-o", tmp_md))
  md_content <- readLines(tmp_md, warn = FALSE)
  
  # 5. Build Quarto format YAML header using revealjs
  yaml_frontmatter <- c(
    "---",
    paste0("title: \"", title, "\""),
    paste0("author: \"", author, "\""),
    "format: revealjs",
    "---",
    ""
  )
  
  # Combine everything together
  final_qmd <- c(yaml_frontmatter, md_content)
  
  # 6. Output file
  writeLines(final_qmd, qmd_path)
  message("Successfully converted file to Revealjs presentation: ", qmd_path)
}
