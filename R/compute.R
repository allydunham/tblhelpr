# Raw computation functions for working with tibbles

add_factor_order <- function(tbl, col1, col2, value, sym=FALSE){
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  value <- enquo(value)

  mat <- select(tbl, !!col1, !!col2, !!value) %>%
    spread(key = !!col2, value = !!value) %>%
    tibble_to_matrix(-!!col1, row_names = pull(., !!col1))

  if (sym){
    order1 <- rownames(mat)[hclust(dist(mat))$order]
    order2 <- order1
  } else {
    mat2 <- t(mat)

    order1 <- rownames(mat)[hclust(dist(mat))$order]
    order2 <- rownames(mat2)[hclust(dist(mat2))$order]
  }

  return(mutate(tbl,
                !!col1 := factor(!!col1, levels=order1),
                !!col2 := factor(!!col2, levels=order2))
  )
}

col_unique_counts <- function(tbl){
  return(apply(tbl, 2, function(x){dplyr::n_distinct(x)}))
}

# returns a list with a vector of unique row labels and a vector of bools as from duplicated(mat)
# combined these can map rowwise results from de-duplicated matrix back to original as int labels correspond to
# row numbers in de-duped matrix
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
      # If its a dupe row, set indeces to first occurance (which will have been previously set)
      dupe_row_nums <- which(apply(mat, 1, identical, y=mat[i,]))
      unique_inds[dupe_row_nums] <- unique_inds[dupe_row_nums[1]]
    }
  }
  return(list(indeces=unique_inds, duplicate=dupe_rows))
}

alphabetise_matrix <- function(x, by=c('rows', 'columns', 'both')){
  by <- match.arg(by)

  if (by %in% c('rows', 'both')){
    x <- x[sort(rownames(x)),]
  }

  if (by %in% c('columns', 'both')){
    x <- x[,sort(colnames(x))]
  }

  return(x)
}

