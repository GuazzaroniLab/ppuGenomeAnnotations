source("./createdb.R")

manual_colors <- c("Same position in GenBank and RefSeq" = "#eaeaea",
                   "Different position in GenBank and RefSeq" = "#18547e",
                   "Absent from RefSeq" = "#f5aa6a")

genomic_ruler <- gb |>
  dplyr::mutate(category = dplyr::case_when(
    locus_tag %in% gb_exclusive ~ "Absent from RefSeq",
    locus_tag %in% affected_loci.old_codes ~ "Different position in GenBank and RefSeq",
    .default = "Same position in GenBank and RefSeq",
  )) |>
  ggplot() +
  geom_bar(aes(fill = category,
               x = locus_tag,
               y = "1"),
           stat="identity",
           width = 1) +
  scale_x_discrete(breaks = c(min(gb_genes), "PP_1437", "PP_2874", "PP_4311", max(gb_genes))) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(values = manual_colors,
                    breaks = names(manual_colors)) +
  labs(x="", y="", fill = "") +
  coord_fixed(500) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 0.5, r = 1.5, b = 0.2, l = .8, "cm"),
    panel.border = element_rect(fill = NA, color="black", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),

    axis.title.x = element_text(size = 24, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length.x = unit(3, "mm"),

    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.size = unit(0.7, "cm"),
    legend.text = element_text(size = 14,
                               margin = margin(l=0.1, r = 0.4, t= 0, b = 0, unit = "cm"))
  )

ggsave(genomic_ruler, filename = "Fig 1. - Genomic ruler", device = svg, path = "../output/panels/", width = 12, height = 4, units = "in", dpi = 300)
