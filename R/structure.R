
process_book <- function(path = "_book") {
  files <- dir(path, pattern = "\\.html$", full.names = TRUE)
  htmls <- lapply(files, read_html)

  lapply(htmls, extract_main)
  lapply(htmls, header_update)

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

header_update <- function(html) {
  # Replace <header> with single <h1>
  h1 <- xml_find_first(html, "//h1")
  xml_attr(h1, "class") <- NULL
  header <- xml_find_first(html, "//header")
  xml_replace(header, h1)

  # Remove title metadata block
  title_meta <- xml_children(xml_find_first(html, "//main[@id='quarto-title_meta']"))
  xml_remove(title_meta)

}
