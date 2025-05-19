source('utils.R')
source('createdb.R')
cogs <- readr::read_tsv("ppuCog.tsv")

annotated_aff_genes <- affected_loci |> dplyr::left_join(cogs, by = c("locus_tag" = "GENE_ID"))



functional_categories <-  unique(cogs$FUNC_CATEGORY) |>
  purrr::map(~runFisher(labelColumn = "FUNC_CATEGORY", label = .x)) |>
  purrr::list_rbind() |>
  dplyr::mutate(p.adj = p.adjust(p.value, method = "fdr"))

cellular_processes <-  cogs |>
  dplyr::filter(FUNC_CATEGORY == "CELLULAR PROCESSES AND SIGNALING" ) |>
  dplyr::pull(COG_CATEGORY_DESCRIPTION) |>
  unique() |>
  purrr::map(~runFisher(labelColumn = "COG_CATEGORY_DESCRIPTION", label = .x)) |>
  purrr::list_rbind() |>
  dplyr::mutate(p.adj = p.adjust(p.value, method = "fdr"))


locustag_func_class <- annotated_aff_genes |> dplyr::group_by(FUNC_CATEGORY) |> dplyr::tally()

locustag_cellproc_class <- annotated_aff_genes |>
                           dplyr::filter(FUNC_CATEGORY == "CELLULAR PROCESSES AND SIGNALING" ) |>
                           dplyr::group_by(COG_CATEGORY_DESCRIPTION) |>
                           dplyr::tally(sort = TRUE) |>
                           dplyr::left_join(cellular_processes, by = c(COG_CATEGORY_DESCRIPTION = "pathway"))
