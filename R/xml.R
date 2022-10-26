xml_hoist_children <- function(nodes) {
  for (i in seq_along(nodes)) {
    children <- xml_contents(nodes[i])
    for (node in children) {
      xml_add_sibling(nodes[i], node, .where = "before")
    }
  }
  xml_remove(nodes)
}

