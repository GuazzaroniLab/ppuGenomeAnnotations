total_number <- affected_loci |>
  dplyr::select(same_start:difference_bp) |>
  tidyr::pivot_longer(cols = c("same_start", "same_end"), names_to = "difference", values_to = "class") |>
  dplyr::group_by(relative_size, difference) |>
  dplyr::summarise("Total" = sum(!class)) |> # we are summing up the FALSE values
  dplyr::mutate(
    difference = dplyr::case_when(
      difference == "same_end" ~ "5'",
      difference == "same_start" ~ "3'"
    )
  )


median_difference <- affected_loci |>
  dplyr::select(same_start:difference_bp) |>
  tidyr::pivot_longer(cols = c("delta_start_bp", "delta_end_bp"), names_to = "difference", values_to = "class") |>
  dplyr::group_by(relative_size, difference) |>
  dplyr::filter(class != 0) |>
  dplyr::summarise("Difference min (bp)" = min(class),
                   "Difference max (bp)" = max(class),
                   "Difference median (bp)" = median(class)) |>
  dplyr::mutate(
    difference = dplyr::case_when(
      difference == "delta_end_bp" ~ "3'",
      difference == "delta_start_bp" ~ "5'"
    )
  )

table_s2 <- dplyr::left_join(total_number, median_difference) |>
  dplyr::mutate(relative_size = dplyr::case_when(
    relative_size == "rs.bigger" ~ "RefSeq ORF is bigger",
    relative_size == "rs.smaller" ~ "RefSeq ORF is smaller"
    )) |>
  dplyr::rename(Outcome = relative_size,
                `Affected extremity` = difference,
                `Number of affected loci` = Total) |>
  dplyr::relocate(`Affected extremity`) |>
  dplyr::relocate(Outcome, .after = `Difference max (bp)`) |>
  dplyr::arrange(`Affected extremity`)

readr::write_tsv(table_s2, "../output/tables/table 2 - summary of shifted genes.tsv")
