source('./createdb.R')

vennInfo <- data.frame(x = c(0.65, 1.5, 2.35),
                       y = rep(0, 3),
                       label = c(length(gb_exclusive), length(intersection), length(rs_exclusive)),
                       title = c("GenBank", NA, "RefSeq"),
                       x_title = c(0.4, NA, 2.5))

circles <- data.frame(x0 = c(1,2), y0 = c(0,0), r = c(1,1), groups = c("1", "2"))

custom_colors <- c("#8cba9d", "steelblue")

vennDiag <- ggplot(vennInfo) +
  ggforce::geom_circle(data = circles,
                       aes(x0 = x0, y0 = y0, r = r, fill = groups),
                       radius = 3, color = NA, alpha = 0.7, show.legend = FALSE) +
  geom_text(data = vennInfo,
            aes(x = x, y = y, label = label), size = 12) +
  geom_text(data = vennInfo,
            aes(x = x_title, y = y + 1.1, label = title), size = 12) +
  scale_fill_manual(values = custom_colors) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(vennDiag, filename = "Fig - VennDiagram.svg", device = svg, path = "../output/panels/", width = 6, height = 6, units = "in", dpi = 300)
