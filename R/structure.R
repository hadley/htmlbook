
process_book <- function(path = "_book") {
  files <- dir(path, pattern = "\\.html$", full.names = TRUE)
  files <- files[basename(files) != "index.html"]

  htmls <- lapply(files, read_html)

  lapply(htmls, extract_main)
  lapply(htmls, header_update)
  lapply(htmls, section_update)
  lapply(htmls, callout_update)

  out_path <- file.path("oreilly/", basename(files))
  dir.create("oreilly", showWarnings = FALSE)

  for (i in seq_along(htmls)) {
    write_html(htmls[[i]], out_path[[i]])
  }

  # strip doc type declaration
  for (path in out_path) {
    lines <- brio::read_lines(path)
    brio::write_lines(lines[-1], path)
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

  # Strip chapter-number and following space
  chapter_nums <- xml_find_all(html, "//span[@class='chapter-number']")
  padding <- xml_find_all(chapter_nums, "following-sibling::text()")
  xml_remove(chapter_nums)
  xml_remove(padding)

  # Strip section-number and following space
  section_nums <- xml_find_all(html, "//span[@class='header-section-number']")
  text <- xml_find_all(section_nums, "following-sibling::text()[1]")
  xml_remove(section_nums)
  xml_text(text) <- trimws(xml_text(text))

  # Strip class and unneeded data attributes
  headings <- xml_find_all(html, "//h1|//h2|//h3|//h4|//h5")
  xml_add_sibling(headings, text("\n"), .where = "before")
  xml_attr(headings, "class") <- NULL
  xml_attr(headings, "data-number") <- NULL
  xml_attr(headings, "data-anchor-id") <- NULL

  # HTML book uses h1 for sect1 titles, so we need to move every heading up a
  # level
  xml_set_name(xml_find_all(html, "//h2"), "h1")
  xml_set_name(xml_find_all(html, "//h3"), "h2")
  xml_set_name(xml_find_all(html, "//h4"), "h3")
  xml_set_name(xml_find_all(html, "//h5"), "h4")
}

section_update <- function(html) {
  # Find all sections within top-level "page" section
  section <- xml_find_all(html, "//section//section")

  # figure out depth, which is one less than quarto's
  class <- xml_attr(section, "class")
  depth <- as.numeric(gsub("level", "", class)) - 1
  # TODO: process footnotes first to avoid NAs introduced by coercion warning

  # set data-type attribute and clear others
  xml_attr(section, "data-type") <- paste0("sect", depth)
  xml_attr(section, "class") <- NULL
  xml_attr(section, "data-number") <- NULL

  # Add a little padding
  xml_add_sibling(section, text("\n"), .where = "before")
  xml_add_sibling(section, text("\n"), .where = "after")
}

callout_update <- function(html) {
  update_type <- function(type, title = type) {
    div <- xml_find_all(html, paste0("//div[contains(@class,'callout-", type, "')]"))
    if (length(div) == 0) return()

    # Use O'Reilly data-type instead of class
    xml_attr(div, "class") <- NULL
    xml_attr(div, "data-type") <- type
    xml_add_sibling(div, text("\n"), .where = "after")

    # Move non-default head to <h1>
    head <- xml_find_all(div, ".//div[contains(@class,'callout-caption-container')]")
    if (tolower(xml_text(head, trim = TRUE)) != title) {
      xml_name(head) <- "h1"
      xml_attr(head, "class") <- NULL
      xml_add_child(div, head)
    } else {
      xml_remove(head)
    }

    # Hoist body out of div
    body <- xml_find_all(div, "./div[contains(@class,'callout-body-container')]")
    for (node in xml_contents(body)) {
      xml_add_child(div, node)
    }
    xml_remove(body)

    # Remove excess new lines
    blank <- xml_find_all(div, "text()[.='\n']")
    xml_remove(blank)
  }

  update_type("note")
  update_type("tip")
  update_type("warning")
  update_type("caution", "danger")
  update_type("important")

  header <- xml_find_all(html, "//div[contains(@class, 'callout-header')]")
  xml_remove(header)
}

text <- function(x) {
  xml_contents(read_xml(paste0("<node>", x, "</node>")))
}
