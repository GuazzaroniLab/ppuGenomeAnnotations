source("./createdb.R")
source("./figure 2 - cog - data.R")

overrepresentation <- locustag_cellproc_class |>
  dplyr::mutate(
    COG_CATEGORY_DESCRIPTION = stringr::str_trunc(COG_CATEGORY_DESCRIPTION, width = 30)
  ) |>
  dplyr::arrange(-p.value) |>
  ggplot(aes(x = n, y = forcats::fct_inorder(COG_CATEGORY_DESCRIPTION), fill = -log10(p.value))) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Number of genes", y = "", fill = "*p*-values (-log<sub>10</sub>)") +
  coord_fixed(15) +
  scale_fill_gradient2(low = "darkblue",
                       mid = "darkorchid4",
                       high = "red2",
                       midpoint = 1.075,
                       limits = c(0.25, 1.9)) +
  theme_minimal(base_size = 16) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = ggtext::element_markdown(),
    axis.text = element_text(color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
   plot.background = element_rect(fill = "white", color = NA),
   legend.title = ggtext::element_markdown(angle = 0, hjust = 0.5)
  )

ggsave(overrepresentation, filename = "Fig 2. - Overrepresentation.svg", device = svg, path = "../output/panels/", width = 12, height = 8, units = "in", dpi = 300)

