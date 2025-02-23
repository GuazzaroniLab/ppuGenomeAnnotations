# this script takes a long-ish time to run
source("./createdb.R")

aledb <- readr::read_csv('aledb.csv') |> dplyr::select(`Reference Seq`:Details)

# categorize each affected gene by a lower and an upper boundary using both annotations
intervals <- affected_loci |>
  dplyr::select(start.refseq, start.genbank, end.refseq, end.genbank) |>
  dplyr::mutate(
    index = 1:dplyr::n(),
    start_l_bound = dplyr::if_else(start.refseq < start.genbank, start.refseq, start.genbank),
    start_r_bound = dplyr::if_else(start.refseq > start.genbank, start.refseq, start.genbank),
    end_l_bound = dplyr::if_else(end.refseq < end.genbank, end.refseq, end.genbank),
    end_r_bound = dplyr::if_else(end.refseq > end.genbank, end.refseq, end.genbank)

  ) |>
  dplyr::select(-c(start.refseq:end.genbank))


# find all genes in the database that match an affected gene (longest span)
matches_in_genes <- NULL

for (i in unique(aledb$Position)) {
  for (j in 1:nrow(intervals)) {

    message(glue::glue("testing position {which(x = unique(aledb$Position) == i, arr.ind = TRUE)}/{length(unique(aledb$Position))} against interval {j}/{nrow(intervals)}..."))

    if (dplyr::between(i, intervals$start_l_bound[j], intervals$end_l_bound[j])) {
      row <- affected_loci |>
        dplyr::slice(intervals$index[j]) |>
        dplyr::mutate(mutation_pos = i)

      matches_in_genes <- dplyr::bind_rows(matches_in_genes, row)
    }
  }
}

# find the matches specific to the regions that differ between the two annotations
matches_in_regions <- NULL

for (i in unique(aledb$Position)) {
  for (j in 1:nrow(intervals)) {

    message(glue::glue("testing position {which(x = unique(aledb$Position) == i, arr.ind = TRUE)}/{length(unique(aledb$Position))} against interval {j}/{nrow(intervals)}..."))
    if (dplyr::between(i, intervals$start_l_bound[j],intervals$start_r_bound[j]) |
        dplyr::between(i, intervals$end_l_bound[j],intervals$end_l_bound[j])) {

      row <- affected_loci |>
        dplyr::slice(intervals$index[j]) |>
        dplyr::mutate(mutation_pos = i)

      matches_in_regions <- dplyr::bind_rows(matches_in_regions, row)
    }
  }
}
