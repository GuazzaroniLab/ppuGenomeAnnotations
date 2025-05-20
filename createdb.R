library(ggplot2)
ppu <- DBI::dbConnect(RSQLite::SQLite(), "") # initiate the database connection

# add genbank and refseq tables
for (file in list.files("./output",full.names = TRUE)) {
  dbname <-  stringr::str_extract(file, "(?<=output\\/).+(?=.tsv)")
  DBI::dbWriteTable(ppu, dbname, file, sep = "\t")
}

# retrieve refseq (rs) and genbank (gb)  tables (focusing on protein coding features)
rs <- dplyr::tbl(ppu, "refseq") |>  # refseq database
  dplyr::filter(class_gene == "protein_coding") |>
  dplyr::select(locus_tag, old_locus_tag, start, end, feature_interval_length, name_product) |>
  tibble::as_tibble()

gb <- dplyr::tbl(ppu, "genbank") |> # genbank database
  dplyr::filter(class_gene == "protein_coding") |>
  dplyr::select(locus_tag, start, end, feature_interval_length, name_product) |>
  tibble::as_tibble()

DBI::dbDisconnect(ppu)

# Compare both annotations side by side (and save it)
fulljoin <- dplyr::full_join(rs,gb, by = c("old_locus_tag" = "locus_tag"), suffix = c(".refseq", ".genbank"))
readr::write_tsv(fulljoin, "../output/tables/refseq_genbank_comparison.tsv")

# Compare start codons, end codons, etc...
# here, refseq will always be the "reference" (i.e. positive value)
rsxgb <- dplyr::inner_join(rs,gb, by = c("old_locus_tag" = "locus_tag"), suffix = c(".refseq", ".genbank")) |>
  dplyr::mutate(
    same_start = start.refseq == start.genbank,
    same_end =  end.refseq == end.genbank,
    delta_start_bp = abs(start.refseq - start.genbank),
    delta_end_bp =  abs(end.refseq - end.genbank),
    difference_type = dplyr::case_when(
      delta_start_bp != 0 & delta_end_bp != 0 ~ "both",
      delta_start_bp != 0 ~ "start",
      delta_end_bp != 0 ~ "end",
                          .default = "none"),
    relative_size = dplyr::case_when(
      feature_interval_length.refseq > feature_interval_length.genbank ~ "rs.bigger",
      feature_interval_length.refseq < feature_interval_length.genbank ~  "rs.smaller",
      .default = NA),
    difference_bp = dplyr::case_when(
      delta_start_bp != 0 & relative_size == "rs.bigger" ~ delta_start_bp,
      delta_start_bp != 0 & relative_size == "rs.smaller" ~ delta_start_bp * -1,
      delta_end_bp != 0 & relative_size == "rs.bigger"   ~ delta_end_bp,
      delta_end_bp != 0 & relative_size == "rs.smaller" ~ delta_end_bp * -1,
      .default = 0))

# outputs a table with differences in the size of the loci with shifted positions
affected_loci <- rsxgb |> dplyr::filter(difference_type != "none") |>
  dplyr::select(- dplyr::starts_with("name_product"))

# saves a pretty table for supplemental material
affected_loci |>
  dplyr::select(-c(same_start, same_end, delta_start_bp, delta_end_bp, relative_size)) |>
  dplyr::rename(
    "Refseq-to-GenBank length variation (bp)" = difference_bp,
    "RefSeq locus tag" = locus_tag,
    "GenBank locus tag" = old_locus_tag,
    "RefSeq start position" = start.refseq,
    "GenBank start position" = start.genbank,
    "RefSeq end position" = end.refseq,
    "GenBank end position" = end.genbank,
    "RefSeq feature length" = feature_interval_length.refseq,
    "GenBank feature length" = feature_interval_length.genbank,
    "Affected extremity" = difference_type
    ) |>
  readr::write_tsv("../output/tables/affected_loci.tsv")


  # Other lists/variables that will be useful in other functions

affected_loci.old_codes <- affected_loci |> dplyr::pull(old_locus_tag) |> unique()
affected_loci.rs_codes <- affected_loci |> dplyr::pull(locus_tag) |> unique()
save(affected_loci.old_codes, file = "affected_oldformat")
save(affected_loci.rs_codes, file = "affected_newformat")

# for the venn diagram
gb_genes <- dplyr::pull(gb, locus_tag) |>
  unique() |>
  sort()

rs_genes <- rs |>
  dplyr::mutate(old_locus_tag = dplyr::if_else(old_locus_tag == "", locus_tag, old_locus_tag)) |>
  dplyr::pull(old_locus_tag) |>
  unique() |>
  sort()

gb_exclusive <- setdiff(gb_genes, rs_genes)
rs_exclusive <- setdiff(rs_genes, gb_genes)
intersection <- intersect(gb_genes, rs_genes)

# frameshift?
# rsxgb |> dplyr::filter(difference_bp != 0) |>
#   dplyr::mutate(frameshift = difference_bp %% 3) |>
#   View()

# new start codons?
#rsxgb |> dplyr::filter(delta_end_bp != 0) |> nrow()
