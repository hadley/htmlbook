chapter_types <- function(path = "_quarto.yml") {
  yaml <- yaml::read_yaml(path)

  chapters <- yaml$book$chapters

  df <- do.call("rbind", lapply(chapters, function(x) {
    if (is.character(x)) {
      tibble::tibble(path = x, type = "chapter")
    } else {
      tibble::tibble(
        path = c(x$part, x$chapters),
        type = c("part", rep("chapter", length(x$chapters)))
      )
    }
  }))

  appendices <- yaml$book$appendices %||% character()
  df <- rbind(df, tibble::tibble(path = appendices, type = "appendix"))

  df <- df[df$path != "index.qmd", , drop = FALSE]

  setNames(df$type, tools::file_path_sans_ext(df$path))
}
