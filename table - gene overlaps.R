source("./createdb.R")

# ordering by start positions
ordered.loci <- rsxgb |>
  dplyr::arrange(start.refseq, start.genbank)

# check if locus n ends AFTER locus n + 1 for gb and rs
overlapping_refseq <- ordered.loci$end.refseq[-length(ordered.loci$end.refseq)] > ordered.loci$start.refseq[-1]
overlapping_genbank <- ordered.loci$end.genbank[-length(ordered.loci$end.genbank)] > ordered.loci$start.genbank[-1]

# retrieve TRUE positions
positions.overlapping_refseq <- which(overlapping_refseq)
positions.overlapping_genbank <- which(overlapping_genbank)

# find those that are unique to either data set
positions.overlapping_diff_genbank <- setdiff(positions.overlapping_genbank, positions.overlapping_refseq)
positions.overlapping_diff_refseq <- setdiff(positions.overlapping_refseq, positions.overlapping_genbank)

# gets all unique positions from both sources
positions.overlapping_diff_all <- c(positions.overlapping_diff_genbank, positions.overlapping_diff_refseq) |> sort()

# slices the whole database and formats the table for a pretty output:

# prepares the data for gene "n"
genen <- ordered.loci |>
  dplyr::slice(positions.overlapping_diff_all)|>
  dplyr::select(locus_tag, old_locus_tag, start.refseq, end.refseq, start.genbank, end.genbank) |>
  dplyr::mutate(og_position = positions.overlapping_diff_all,
                `gene 1 refseq span (start → end)` = glue::glue("{start.refseq} → {end.refseq}"),
                `gene 1 genbank span (start → end)` = glue::glue("{start.genbank} → {end.genbank}"))

# prepares for gene "n + 1"
genen1 <- ordered.loci |>
  dplyr::slice(positions.overlapping_diff_all + 1) |>
  dplyr::select(locus_tag, old_locus_tag, start.refseq, end.refseq, start.genbank, end.genbank) |>
  dplyr::mutate(og_position = positions.overlapping_diff_all,
                `gene 2 refseq span (start → end)` = glue::glue("{start.refseq} → {end.refseq}"),
                `gene 2 genbank span (start → end)` = glue::glue("{start.genbank} → {end.genbank}"))

# final formatting
overlaps_table <- dplyr::left_join(genen, genen1, by = "og_position", suffix = c(".1", ".2")) |>
  dplyr::mutate(`gene pair (genbank codes)` = glue::glue("{old_locus_tag.1}–{old_locus_tag.2}"),
                `gene pair (refseq codes)` = glue::glue("{locus_tag.1}–{locus_tag.2}"),
                `overlaps in` = dplyr::case_when(
                  end.refseq.1 > start.refseq.2 & end.genbank.1 > start.genbank.2 ~ "BOTH", # sanity check
                  end.refseq.1 > start.refseq.2 ~ "RefSeq",
                  end.genbank.1 > start.genbank.2 ~ "GenBank",
                  .default = "NONE" # sanity check
                ),
                `overlap size (bp)` = dplyr::case_when(
                  `overlaps in` == "GenBank" ~ abs(end.genbank.1 - start.genbank.2) + 1,
                  `overlaps in` == "RefSeq" ~ abs(end.refseq.1 - start.refseq.2) + 1
                )) |>
  dplyr::select(-tidyselect::starts_with(c("locus_tag", "old_locus_tag", "start", "end")), -og_position) |>
  dplyr::relocate(tidyselect::contains("pair")) |>
  dplyr::relocate(tidyselect::contains("genbank"))

# saving the final table
readr::write_tsv(overlaps_table, "../output/tables/table - summary of sequence overlaps.tsv")

