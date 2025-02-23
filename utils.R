# fisher's test (used to very COG overrepresentation...) ----
runFisher <- function(allAnnotations = cogs,
                      goiList = affected_loci.rs_codes,
                      labelColumn = "PATHWAY",
                      label = "Glycolysis") {

  shifted.labeled <- allAnnotations |>
    dplyr::filter(GENE_ID %in% goiList,
                  get(labelColumn) == label) |>
    dplyr::pull(GENE_ID) |>
    unique()

  shifted.unlabeled <- allAnnotations |>
    dplyr::filter(GENE_ID %in% goiList,
                  get(labelColumn) != label,
                  !GENE_ID %in% shifted.labeled) |> #some genes have more than 1 annot
    dplyr::pull(GENE_ID) |>
    unique()

  unshifted.labeled <- allAnnotations |>
    dplyr::filter(!GENE_ID %in% goiList,
                  get(labelColumn) == label) |>
    dplyr::pull(GENE_ID) |>
    unique()

  unshifted.unlabeled <- allAnnotations |>
    dplyr::filter(!GENE_ID %in% goiList,
                  get(labelColumn) != label,
                  !GENE_ID %in% unshifted.labeled) |>
    dplyr::pull(GENE_ID) |>
    unique()


  table <- matrix(c(length(shifted.labeled), length(unshifted.labeled),
                    length(shifted.unlabeled), length(unshifted.unlabeled)),
                  nrow = 2, ncol = 2, byrow = TRUE)

  rownames(table) <- c('labeled', 'not.labeled')
  colnames(table) <- c('shifted', 'unshifted')

  res <- fisher.test(table, alternative = "greater")

  print(label)
  print(table)

  out <- tibble::tibble(pathway = label, p.value = res$p.value)
  return(out)
}
