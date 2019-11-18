# Perform common calculations on tibbles

#' Calculate a PCA from selected columns in a tibble
#'
#' Use \code{prcomp} to generate a principal component analysis from a
#' selection of columns in a tibble.
#'
#' @param tbl A tibble.
#' @param ... Tidy selection of columns to pass to \code{prcomp}
#'
#' @export
tibble_pca <- function(tbl, ...){
  cols <- rlang::enquos(...)

  pca <- prcomp(tibble_to_matrix(tbl, !!! cols))

  return(pca)
}

#' Calculate a tSNE from selected columns
#'
#' Pass a selection of columns to \code{\link[Rtsne]{Rtnse}}, deduplicating
#' them and then reconstructing the projection for duplicate rows.
#'
#' @param tbl A tibble.
#' @param ... A tidy selection of columns
#' @param tsne_kwargs Additional arguments to pass to \code{Rtsne}
#'
#' @export
tibble_tsne <- function(tbl, ..., tsne_kwargs=list()){
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop("Package \"Rtsne\" is required for this function.",
         call. = FALSE)
  }

  prof_cols <- rlang::enquos(...)

  mat <- tibble_to_matrix(tbl, !!! prof_cols)

  mat_dupe_rows <- enumerate_unique_rows(mat)
  mat_deduped <- mat[!mat_dupe_rows$duplicate, ]

  tsne <- do.call(Rtsne::Rtsne, c(list(X = mat_deduped), tsne_kwargs))

  tsne_tbl <- tsne$Y[mat_dupe_rows$indeces, ]
  colnames(tsne_tbl) <- c("tSNE1", "tSNE2")
  tbl <- dplyr::bind_cols(tbl, dplyr::as_tibble(tsne_tbl))

  return(list(tbl = tbl, tsne = tsne,
              dupe_rows = mat_dupe_rows$duplicate,
              unique_row_indeces = mat_dupe_rows$indeces))
}

#' Calculate correlations between tibble columns
#'
#' Calculate the correlation between all pairs from a selection of columns.
#' The column names are stored in factor columns \code{cat1} and \code{cat2},
#' with order based upon \code{hclust}
#'
#' @param tbl A tibble.
#' @param x Tidy selection of variables for x
#' @param y Optional tidy selection of variables for y. If given correlations
#'     between x and y are calculated, otherwise all correlations between x are
#'     calculated.
#' @param filter_diag Whether to filter diagonal entries
#' @param ... Other arguments to pass to \code{cor}
#'
#' @importFrom dplyr "%>%"
#' @export
tibble_correlation <- function(tbl, x, y = NULL, filter_diag=FALSE, ...){
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  x_mat <- tibble_to_matrix(tbl, !!x)

  if (!rlang::quo_is_null(y)){
    y_mat <- tibble_to_matrix(tbl, !!y)
  } else {
    y_mat <- NULL
  }

  cor_mat <- cor(x = x_mat, y = y_mat, ...)

  cors <- dplyr::as_tibble(cor_mat, rownames = "cat1") %>%
    tidyr::pivot_longer(-cat1, names_to = "cat2", values_to = "cor") %>%
    add_factor_order(cat1, cat2, cor, sym = rlang::quo_is_null(y))

  if (filter_diag & rlang::quo_is_null(y)){
    cors <- dplyr::filter(cors, !cat1 == cat2)
  }

  return(cors)
}
