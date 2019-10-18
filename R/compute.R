# Raw computation functions for working with tibbles

#' Add \code{hclust} order to columns.
#'
#' Add orders to two character columns in a tibble, based on heirarchical
#' clustering of a third column.
#'
#' @param tbl A tibble.
#' @param col1,col2 Columns to add order to.
#' @param value Column giving a value from each pair of levels in \code{col1}
#' and \code{col2}.
#' @param sym Is the relationship between \code{col1} and \code{col2} symetric.
#' Meaning they represent the same physical property and the expanded matrix of
#' value pairs will be symetrical across the diagonal.
#'
#' @importFrom dplyr "%>%"
#' @export
add_factor_order <- function(tbl, col1, col2, value, sym=FALSE){
  col1 <- rlang::enquo(col1)
  col2 <- rlang::enquo(col2)
  value <- rlang::enquo(value)

  mat <- dplyr::select(tbl, !!col1, !!col2, !!value) %>%
    tidyr::pivot_wider(names_from = !!col2, values_from = !!value) %>%
    tibble_to_matrix(-!!col1, row_names = pull(., !!col1))

  if (sym){
    order1 <- rownames(mat)[hclust(dist(mat))$order]
    order2 <- order1
  } else {
    mat2 <- t(mat)

    order1 <- rownames(mat)[hclust(dist(mat))$order]
    order2 <- rownames(mat2)[hclust(dist(mat2))$order]
  }

  return(dplyr::mutate(tbl,
                       !!col1 := factor(!!col1, levels = order1),
                       !!col2 := factor(!!col2, levels = order2))
  )
}

#' Count the number of unique entries in each column
#'
#' Shortcut for applying \code{\link[dplyr]{n_distinct}} to each tibble column.
#'
#' @param tbl A data frame.
#'
#' @export
col_unique_counts <- function(tbl){
  return(apply(tbl, 2, function(x) dplyr::n_distinct(x) ))
}

#' Enumerate unique and duplicate rows in a matrix like object.
#'
#' Identify which rows of a matrix are unique and which are duplicates,
#' including identifying which row they are a duplicate of. Unique rows are
#' numbered sequentially.
#'
#' @param mat A matrix like object
#'
#' @return A list containing the a vector of unique row indices
#'     \code{indeces}, corresponding to each rows coresponding unique row in
#'     a deduplicated matrix, and a logical vector showing whether each row is
#'     duplicate (\code{duplicated}). These two vectors are sufficient to
#'     extract a  deduplcated matrix (\code{deduped <- mat[!duplicate,]}) and
#'     reconstruct  the original (\code{mat == deduped[indeces,]}).
#'
#' @export
enumerate_unique_rows <- function(mat){
  dupe_rows <- duplicated(mat)

  unique_inds <- rep(0, length(dupe_rows))
  ind <- 1
  for (i in 1:length(dupe_rows)){
    if (!dupe_rows[i]){
      # enumerate non-dupe rows with unused index
      unique_inds[i] <- ind
      ind <- ind + 1

    } else if (unique_inds[i] == 0) {
      # If its a dupe row, set indeces to first occurance
      dupe_row_nums <- which(apply(mat, 1, identical, y = mat[i, ]))
      unique_inds[dupe_row_nums] <- unique_inds[dupe_row_nums[1]]
    }
  }
  return(list(indeces = unique_inds, duplicate = dupe_rows))
}

#' Sort matric columns and/or rows alphabetically
#'
#' Sort data matrices alphabetically based on row and/or column names. A
#' convinience function for inserting into \code{%>%} pipe chains
#'
#' @param x A matrix.
#' @param by Whether to sort by rows, columns or both. Supports argument
#'     matching on any unique substring.
#'
#' @export
alphabetise_matrix <- function(x, by=c("rows", "columns", "both")){
  by <- match.arg(by)

  if (by %in% c("rows", "both")){
    x <- x[sort(rownames(x)), ]
  }

  if (by %in% c("columns", "both")){
    x <- x[, sort(colnames(x))]
  }

  return(x)
}
