library(ggplot2)

aledb <- readr::read_csv('aledb.csv') |> dplyr::select(`Reference Seq`:Details) |>
  dplyr::group_by(`Reference Seq`) |>
  dplyr::tally()

aledb_mutations <- ggplot(aledb, aes(x =`Reference Seq`, y = n, fill = `Reference Seq`)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  coord_fixed(1.2) +
  scale_x_discrete(limits = c("NC_002947", "AE015451", "JE3692-4", "KT2440")) +
  scale_y_log10(expand = expansion(mult = c(0, 0), add = c(0, .1))) +
  scale_fill_manual(values = c("KT2440" = "gray80",
                               "JE3692-4" = "gray80",
                               "AE015451" = "#8cba9d",
                               "NC_002947" = "steelblue")) +
  labs(x = "Reference sequence", y = "Number of mutations in ALEdb (log<sub>10</sub>)") +
  theme_light(base_size = 16) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = ggtext::element_markdown(),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

ggsave(aledb_mutations, filename = "Fig 4. - ALEdb", device = svg, path = "../output/panels/", width = 10, height = 5, units = "in", dpi = 300)
