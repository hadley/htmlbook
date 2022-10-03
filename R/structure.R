
process_book <- function(path = "_book") {
  files <- dir(path, pattern = "\\.html$", full.names = TRUE)
  htmls <- lapply(files, read_html)

  lapply(htmls, extract_main)

  out_path <- file.path("oreilly/", basename(files))
  dir.create("oreilly", showWarnings = FALSE)

  for (i in seq_along(htmls)) {
    write_html(htmls[[i]], out_path[[i]])
  }

  invisible()

}

extract_main <- function(html, type = c("chapter", "part", "bibliography", "glossary", "preface")) {
  type <- arg_match(type)

  content <- xml_children(xml_find_first(html, "//main[@id='quarto-document-content']"))

  # Replace <html> with <section data-type="">, and move in nodes from
  # #quarto-document-content
  html_node <- xml_find_first(html, "//html")
  section <- xml_replace(html_node, "section", "data-type" = type)
  for (node in content) {
    xml_add_child(section, node)
  }

  invisible()
}

