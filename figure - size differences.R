source("./createdb.R")

size_groups <- affected_loci |>
  dplyr::group_by(difference_bp, relative_size) |>
  dplyr::tally(sort = TRUE) |>
  dplyr::mutate(
    difference_bp = abs(difference_bp),
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(difference_bp = as.factor(difference_bp))


sizeplot <- ggplot(size_groups, aes(y = forcats::fct_rev(difference_bp), x = n, fill = relative_size)) +
 geom_col() +
  geom_hline(yintercept = 0) +
  coord_fixed(3) +
  labs(y = "Length difference (bp)", x = "Number of ORFs") +
  scale_fill_manual(name = "Relative size", values = c("rs.bigger" = "#8cba9d", "rs.smaller" = "steelblue"), labels = c("GenBank ORF is smaller", "Refseq ORF is smaller")) +
  scale_x_continuous(expand = expansion(c(0,0))) +
  theme_light(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),

    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 0, size = 14, color = "black", hjust = 0, vjust = 0.5),

    legend.position = "inside",
    legend.position.inside = c(0.75, 0.5)
  )

ggsave(sizeplot, filename = "Fig - Relative sizes.svg", device = svg, path = "../output/panels/", width = 6, height =12, units = "in", dpi = 300)
