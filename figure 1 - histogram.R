source('./createdb.R')
all_groups <- unique(affected_loci$difference_bp) |> abs() |>  sort()
bins <- c(all_groups[seq(1, length(all_groups), by = 15)], max(all_groups)) |> unique()

histogram <- affected_loci |>
  dplyr::mutate(bins = cut(abs(difference_bp), breaks = bins,include.lowest = TRUE ,right = TRUE)) |>
  ggplot(aes(x = bins))+
  geom_bar(color = "black", fill = "#3382ae", width = 1) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(x = "Position shift (bp)", y = "Number of affected loci") +
  coord_fixed(0.008) +
  theme_light() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.line.y = element_line(color = "black", linewidth = 0.5)

  )


ggsave(histogram, filename = "Fig 1. - Histogram", device = Cairo::CairoSVG, path = "../output/panels/", width = 8, height = 6, units = "in", dpi = 300)
