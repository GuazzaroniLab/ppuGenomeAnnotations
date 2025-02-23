all_pathways <- KEGGREST::keggList("pathway", "ppu")

all_pathway_genes <- NULL

for( path in names(all_pathways)) {

  print(path)

  res <- KEGGREST::keggGet(path)

  genes <- res[[1]]$GENE |>
           unlist() |>
           stringr::str_subset(pattern = "PP_")

  print(genes)

  all_pathway_genes <- append(all_pathway_genes, genes)

}

# all genes present in the ppu KEGG database
entries <- KEGGREST::keggInfo("ppu") |>
  stringr::str_split(pattern = "\n") |>
  unlist() |>
  stringr::str_squish() |>
  stringr::str_subset(pattern = "entries")

# all genes assocaited with pathways
length(unique(all_pathway_genes))
