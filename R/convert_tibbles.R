# Functions to convert tibbles to other formats

tibble_to_matrix <- function(tbl, ..., row_names=NULL){
  cols <- enquos(...)

  if (!is.null(row_names)){
    if (length(row_names) == 1 & is.character(row_names)){
      row_names <- tbl[[row_names]]
    }
  }

  tbl <- select(tbl, !!! cols) %>%
    as.matrix()
  if (!is.null(row_names)){
    tbl <- set_rownames(tbl, row_names)
  }
  return(tbl)
}

transpose_tibble <- function(tbl, col, name_col = 'rows'){
  col <- enquo(col)

  tibble_to_matrix(tbl, -!!col, row_names = pull(tbl, !!col)) %>%
    t() %>%
    as_tibble(rownames = name_col) %>%
    return()
}
