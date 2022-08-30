# Q:
# - How do communities from different TP relate to each other? Ex: C1@B vs. C1@FV
# - Different number of communities
# - TFI
library(tidyverse)
library(glue)
library(igraph)
library(ggraph)
library(ggforce)
library(ggiraph)
library(patchwork)
# extrafont::loadfonts(device = "win", quiet = TRUE)

source("utils.R", echo = FALSE)

center <- "berlin"
files <- c("Baseline" = glue("TFI_0_baseline_{center}.gml"),
           "Interim Visit" = glue("TFI_0_interim_visit_{center}.gml"),
           "Final Visit" = glue("TFI_0_final_visit_{center}.gml")
)
col_score <- "tfi_score" # name of column for node coloring
col_edge_weight <- "value" # name of column representing the edge weights
edge_weight_treshold <- 3 # edges with a weight below this threshold are not shown

target_file <- glue("{center}_{col_score}-b-iv-fv.rds")

if (!file.exists(paste0("data/", target_file))) {
  df <- map2_dfr(files, names(files), prep_g,
                 col_score = col_score, col_edge_weight = col_edge_weight) |>
    mutate(layer = factor(layer, levels = names(files)))
  write_rds(df, paste0("data/", target_file))
} else {
  df <- read_rds(paste0("data/", target_file))
}

composite_graphs <- function(interactive = FALSE) {
  names(files) |>
    map(~ df |> filter(layer == .x)) |>
    map(~ plot_graph(
      df = .x,
      interactive = interactive,
      edge_weight_treshold = edge_weight_treshold,
      title = .x$layer[[1]],
      node_color_legend_title = col_score,
      rng_score = c(0, 100)
    )) |>
    wrap_plots(nrow = 1, guides = "collect") +
    patchwork::guide_area() +
    plot_layout(widths = c(rep(0.85 / length(files), length(files)), 0.15))
}

pi <- composite_graphs(interactive = TRUE)

gi <- girafe(
  ggobj = pi,
  options = list(
    ggiraph::opts_toolbar(saveaspng = FALSE),
    ggiraph::opts_hover_inv(css = "fill:#B3B3B3; stroke:#B3B3B3;"),
    ggiraph::opts_hover(css = "fill:blue;stroke:blue;"),
    ggiraph::opts_tooltip(css = paste(
      "background-color:white",
      "font-family: sans-serif",
      "padding: 5px 5px 5px 5px",
      sep = ";"
    )),
    ggiraph::opts_sizing(rescale = FALSE, width = 9/16),
    ggiraph::opts_selection(
      only_shiny = FALSE,
      type = "single",
      css = "fill:green; stroke:green;"
      ),
    ggiraph::opts_zoom(max = 5)
  ),
  width_svg = 45/2.54, height_svg = 16/2.54
)

out_name <- file.path("figures", xfun::sans_ext(target_file))
# gi
htmlwidgets::saveWidget(gi, file = glue("{out_name}.html"), 
                        title = glue("communities-{center}"))

ps <- composite_graphs(interactive = FALSE)
# ps
ggsave(
  plot = ps,
  filename = glue("{out_name}.pdf"),
  width = 30, height = 10, units = "cm",
  bg = "white",
  device = grDevices::cairo_pdf
)
ggsave(
  plot = ps,
  filename = glue("{out_name}.png"),
  width = 30, height = 10, units = "cm",
  bg = "white", dpi = 300 #!600
)
