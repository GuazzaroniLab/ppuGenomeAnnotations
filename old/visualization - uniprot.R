library(ggplot2)
#visualizations

uniprot <- dplyr::tbl(ppu, "uniprot")

existenceLevels <- uniprot |> dplyr::pull(proteinExistence) |> unique()
databaseLevels <- uniprot |> dplyr::pull(dataset) |> unique()

manual_colors_names <- append(existenceLevels, databaseLevels)

manual_colors <- c("#3382ae", "#e4e8eb","#d99255", "#9f3e3b", "#d99255", "#e4e8eb", "#649790", "#7fac90", "#d89053", "#f4a968", "#9f3d39", "#c9625b", "#18547e")
names(manual_colors) <-manual_colors_names

ggplot(data = uniprot) +
  geom_bar(aes(fill = proteinExistence,
               x = ordered.locus,
               y = "1"),
               stat="identity",
               width = 1) +
  scale_x_discrete(breaks = c("PP_0001", "PP_5748")) +
  labs(x="Ordered locus", y="") +
  coord_fixed(500) +
  scale_fill_manual(values = manual_colors) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 0, r = .8, b = 0, l = .2, "cm"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.title.position = "top"
  )


