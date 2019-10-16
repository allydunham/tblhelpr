# Perform common calculations on tibbles

# Calc PCA based on selected columns from a tibble
tibble_pca <- function(tbl, ...){
  cols <- enquos(...)

  pca <- tibble_to_matrix(tbl, !!! cols) %>%
    prcomp()

  return(pca)
}

# generate tSNE from selected columns
tibble_tsne <- function(tbl, ..., tsne_kwargs=list()){
  prof_cols <- enquos(...)

  mat <- tibble_to_matrix(tbl, !!! prof_cols)

  mat_dupe_rows <- enumerate_unique_rows(mat)
  mat_deduped <- mat[!mat_dupe_rows$duplicate,]

  tsne <- do.call(Rtsne, c(list(X=mat_deduped), tsne_kwargs))

  tbl <- bind_cols(tbl, as_tibble(set_colnames(tsne$Y[mat_dupe_rows$indeces,], c('tSNE1', 'tSNE2'))))

  return(list(tbl=tbl, tsne=tsne, dupe_rows=mat_dupe_rows$duplicate, unique_row_indeces=mat_dupe_rows$indeces))
}

# Calculate correlation of tibble columns
tibble_correlation <- function(tbl, ..., filter_diag=FALSE){
  prof_cols <- enquos(...)

  cor_mat <- tibble_to_matrix(tbl, !!! prof_cols) %>%
    cor()

  group_order <- rownames(cor_mat)[hclust(dist(cor_mat))$order]

  cors <- as_tibble(cor_mat, rownames = 'cat1') %>%
    gather(key = 'cat2', value = 'cor', -cat1) %>%
    mutate(cat1 = factor(cat1, levels = group_order),
           cat2 = factor(cat2, levels = group_order))

  if (filter_diag){
    cors[cors$cat1 == cors$cat2, 'cor'] <- NA
  }

  return(cors)
}
