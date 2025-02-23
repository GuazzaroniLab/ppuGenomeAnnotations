getKEGG <- function(query) {

  specific_pathway <- KEGGREST::keggGet(query)

  path_name <- specific_pathway[[1]]$NAME[1] |> unlist()

  message(glue::glue("getting data for {query} - {path_name}"))

  path_genes <- specific_pathway[[1]]$GENE |>
    stringr::str_subset(pattern = "^PP_\\d")

  output <- tibble::tibble(pathway = path_name, genes = path_genes)
  return(output)
}

# get all genes in all pathways annotated in KEGG
all_pathways <- KEGGREST::keggList("pathway", organism = "ppu") |> names()

complete_pathways <- all_pathways |>
                     purrr::map(getKEGG) |>
                     purrr::list_rbind()

complete_pathways <- complete_pathways |> # a lil formatting...
  dplyr::mutate(pathway = stringr::str_remove_all(pathway, "\\s-\\sPseudomonas putida KT2440"))

complete_pathways_summary <- complete_pathways |>
  dplyr::group_by(pathway) |>
  dplyr::tally(sort = TRUE)

# getting the genes that were shifted in position
shifted_pathways <- complete_pathways |>
  dplyr::filter(genes %in% diffLoci.tags)


shifted_pathways_summary <- shifted_pathways |>
  dplyr::group_by(pathway) |>
  dplyr::tally(sort = TRUE)

dplyr::inner_join(complete_pathways_summary,
                  shifted_pathways_summary,
                  by = "pathway",
                  suffix = c(".all", ".shifted")) |>
        dplyr::mutate(ratio = (n.shifted / n.all) * 100) |>
        View()


# chi squared test ----

runChi2Test <- function(label = "Porphyrin metabolism") {

  shifted.labeled <- shifted_pathways |>
                     dplyr::filter(pathway == label) |>
                     dplyr::pull(genes) |>
                     unique()

  shifted.unlabeled <- shifted_pathways |>
                     dplyr::filter(pathway != label,
                                   !genes %in% shifted.labeled) |>
                     dplyr::pull(genes) |>
                     unique()

  unshifted.labeled <- complete_pathways |>
                     dplyr::filter(!genes %in% shifted_unique_genes, pathway == label) |>
                     dplyr::pull(genes) |>
                     unique()

  unshifted.unlabeled <- complete_pathways |>
                     dplyr::filter(!genes %in% shifted_unique_genes, pathway != label, !genes %in% unshifted.labeled) |>
                     dplyr::pull(genes) |>
                     unique()


  table <- matrix(c(length(shifted.labeled), length(unshifted.labeled),
                    length(shifted.unlabeled), length(unshifted.unlabeled)),
                  nrow = 2, ncol = 2, byrow = TRUE)

  rownames(table) <- c('labeled', 'not.labeled')
  colnames(table) <- c('shifted', 'unshifted')

  res <- chisq.test(table)

  print(table)

  out <- tibble::tibble(pathway = label, p.value = res$p.value)
  return(out)
}

a <-  unique(shifted_pathways_summary$pathway) |>
      purrr::map(~runChi2Test(label = .x)) |>
      purrr::list_rbind() |>
      dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))


View(a)


