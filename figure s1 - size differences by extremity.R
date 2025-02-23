## Piramid ----

lollipop_data <- diffLoci |>
  dplyr::select(diffStart, diffEnd, diffKind) |>
  dplyr::group_by(diffStart, diffEnd, diffKind) |>
  dplyr::tally(sort = TRUE) |>
  dplyr::mutate(
    type = dplyr::case_when(
      diffStart < 0 ~ "Before", # #"refseq starts before"
      diffStart > 0 ~ "After", # "refseq starts after"
      diffEnd < 0 ~ "Before", # "refseq ends before"
      diffEnd > 0 ~ "After"), # "refseq ends after"
    diffStart = abs(diffStart),
    diffEnd = abs(diffEnd))

# start position
lollipop_data |>
  dplyr::filter(diffKind == "start") |>
  ggplot(aes(x = as.factor(diffStart),
             y = dplyr::if_else(type == "After", n * -1, n),
             fill = type,
             color = type)) +
  geom_col(width = 1, color = "black", linewidth = 0.1, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  annotate("text",
           label = c("RefSeq start position > Genbank start position",
                     "RefSeq start position <  Genbank start position"),
           x = 28,
           y = c(-40,12),
           color = c("#18547e","#d99255"),
           size = 5,
           hjust = 0.5) +
  scale_y_continuous(limits = c(-40, 12), labels = \(x) abs(x)) +
  scale_fill_manual(values = c("#3382ae","#f4b66b")) +
  labs(x = "Start position shift amount (bp)", y = "Number of occurrences") +
  theme_light(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(angle = -90, size = 14, color = "black")
  )


# end position
lollipop_data |>
  dplyr::filter(diffKind == "end") |>
  ggplot(aes(x = as.factor(diffEnd),
             y = dplyr::if_else(type == "After", n * -1, n),
             fill = type,
             color = type)) +
  geom_col(width = 1, color = "black", linewidth = 0.1, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  annotate("text",
           label = c("RefSeq end position > Genbank end position",
                     "RefSeq end position <  Genbank end position"),
           x = 28,
           y = c(-10,30),
           color = c("#18547e","#d99255"),
           size = 5,
           hjust = 0.5) +
  scale_y_continuous(limits = c(-10, 30), labels = \(x) abs(x)) +
  scale_fill_manual(values = c("#3382ae","#f4b66b")) +
  labs(x = "End position shift amount (bp)", y = "Number of occurrences") +
  theme_light(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(angle = -90, size = 14, color = "black")
