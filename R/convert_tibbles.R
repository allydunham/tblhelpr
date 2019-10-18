# Functions to convert tibbles to other formats

#' Convert columns from a tibble into a matrix.
#'
#' Convert subsections of tibbles into matrices, with the option of adding
#' row names from another column or a character vector
#'
#' @param tbl Tibble to covert.
#' @param ... Tidy selection of columns to include in matrix.
#' @param row_names String of column name to pull row names from or a character
#'     vector of row names.
#'
#' @export
tibble_to_matrix <- function(tbl, ..., row_names=NULL){
  cols <- rlang::enquos(...)

  if (!is.null(row_names)){
    if (length(row_names) == 1 & is.character(row_names)){
      row_names <- tbl[[row_names]]
    }
  }

  mat <- as.matrix(dplyr::select(tbl, !!! cols))

  if (!is.null(row_names)){
    if (length(row_names) == 1 & row_names %in% colnames(tbl)){
      row_names <- tbl[row_names]
    }
    rownames(mat) <- row_names
  }
  return(mat)
}

#' Transpose a tibble.
#'
#' Transpose tibble, adding a new column to store the old column names and
#' drawing new column names from a given column. Currently doesn't have type
#' checking so new types have to be assigned to columns manually if there
#' is more than one type in the tibble.
#'
#' @param tbl Tibble to transpose.
#' @param col_names Column containing the transposed column headers.
#' @param id_col Name of the new column containing the old column headers.
#'
#' @importFrom dplyr "%>%"
#' @export
transpose_tibble <- function(tbl, col_names, id_col = "columns"){
  col_names <- rlang::enquo(col_names)

  tibble_to_matrix(tbl, -!!col_names,
                   row_names = dplyr::pull(tbl, !!col_names)) %>%
    t() %>%
    dplyr::as_tibble(rownames = name_col) %>%
    return()
}
