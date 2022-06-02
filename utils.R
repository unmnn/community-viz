prep_g <- function(file, layer, col_score, col_edge_weight) {

  file_exp <- dir("data", pattern = xfun::sans_ext(file), full.names = TRUE)

  # g contains edge data
  g <- igraph::read.graph(file_exp, format = "gml") |>
    igraph::as_long_data_frame() |>
    tibble::as_tibble()

  # needed for node positions
  gg <- multiplex::read.gml(file_exp, as = "srt", coords = TRUE)

  gg_cord <- gg$coord |>
    tibble::as_tibble() |>
    dplyr::select(matching_id = L1, from_x = X, from_y = Y)

  g <- g |>
    dplyr::rename(from_matching_id = from_id, to_matching_id = to_id) |>
    dplyr::select(
      from_id = from, to_id = to,
      from_matching_id, to_matching_id,
      from_score = !!sym(paste0("from_", col_score)),
      edge_weight = !!sym(col_edge_weight),
      from_community # !!! currently hard-coded
    ) |>
    dplyr::mutate(dplyr::across(c(from_id, to_id,
                                  from_matching_id, to_matching_id),
                                ~ as.character(.x))) |>
    dplyr::mutate(from_community = as.factor(as.integer(from_community))) |>
    dplyr::mutate(from_score = as.double(from_score)) |>
    dplyr::left_join(gg_cord, by = c("from_matching_id" = "matching_id")) |>
    dplyr::left_join(
      gg_cord |>
        dplyr::rename_with(~ stringr::str_replace(.x, "from", "to")),
      by = c("to_matching_id" = "matching_id")
    ) |>
    dplyr::mutate(layer = layer)
  g
}

plot_graph <- function(
    df,
    interactive = FALSE,
    edge_weight_treshold = 5,
    title = title,
    node_color_legend_title = "Score",
    rng_score = NULL
) {

  custom_colors <- tab10_colors[1:nlevels(df$from_community)]
  # df_colors <- levels(df$from_community) |>
  #   str_extract("^[:digit:]+?") |>
  #   as_tibble() |>
  #   mutate(group = cumsum(value == 1)) |>
  #   mutate(color = tab10_colors[as.integer(value)])
  #
  # custom_colors <- df_colors|>
  #   pull(color)

  # custom_colors <- janitor::tabyl(df, layer, from_community) |>
  #   pivot_longer(-layer) |>
  #   filter(value > 0) |>
  #   group_by(layer) |>
  #   mutate(color = tab10_colors[row_number()]) |>
  #   pull(color)

  if (is.null(rng_score)) {
    rng_score <- range(df[["from_score"]])
  }

  # cap edge weights
  # df <- df |>
  #   mutate(
  #     edge_weight = ifelse(
  #       edge_weight > (quantile(edge_weight, 0.75) + 2 * IQR(edge_weight)),
  #       NA_real_, edge_weight
  #     )
  #   ) |>
  #   mutate(edge_weight = ifelse(is.na(edge_weight), max(edge_weight, na.rm = TRUE), edge_weight))

  p <- ggplot() +
    coord_equal() +
    ggforce::geom_voronoi_tile(
      data = df |> distinct(from_id, .keep_all = TRUE),
      aes(x = from_x, y = from_y, fill = from_community, group = -1L),
      alpha = 0.75, max.radius = 50, color = "white"
    ) +
    geom_segment(
      data = df |>
        filter(edge_weight >= edge_weight_treshold),
      aes(x = from_x, xend = to_x, y = from_y, yend = to_y,
          alpha = edge_weight, size = edge_weight), #size = 0.3
      color = "gray70"
    ) +
    scale_alpha_continuous(
      limits = c(edge_weight_treshold, min(10, max(df$edge_weight))),
      breaks = 3,
      range = c(0.05, 0.5)
    ) +
    scale_size_binned(
      limits = c(edge_weight_treshold, min(10, max(df$edge_weight))),
      breaks = 3,
      range = c(0.05, 0.5)
    ) +
    scale_fill_manual(
      values = custom_colors,
      drop = FALSE
    ) +
    # colorspace::scale_fill_discrete_qualitative("Warm") +
    # colorblindr::scale_fill_OkabeIto() +
    # scale_fill_brewer(palette = "Pastel2") +
    guides(size = "none", alpha = "none", fill = "none") +
    labs(
      title = title,
      color = paste0(node_color_legend_title, "\n(node color)"),
      fill = "Community\n(cell color)"
    ) +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))

  if (!interactive) {
    p <- p +
      geom_point(
        data = df |> distinct(from_id, .keep_all = TRUE),
        aes(x = from_x, y = from_y, color = from_score),
        size = 3
      ) +
      scale_color_distiller(
        palette = "YlOrRd",
        direction = 1,
        limits = rng_score,
        breaks = round(seq(rng_score[1], rng_score[2], length.out = 5))
      )
  }

  if (interactive) {
    p <- p +
      ggiraph::geom_point_interactive(
        data = df |> distinct(from_id, .keep_all = TRUE),
        aes(
          x = from_x,
          y = from_y,
          color = from_score,
          tooltip = paste0("ID = ", from_id, "\n",
                           node_color_legend_title, " = ", from_score, "\n",
                           "Community = ", from_community),
          data_id = if (TRUE) from_id else from_community
        ),
        size = 5
      ) +
      scale_color_distiller(
        palette = "YlOrRd",
        direction = 1,
        limits = rng_score,
        breaks = round(seq(rng_score[1], rng_score[2], length.out = 5))
      )
  }

  p
}

tab10_colors <- c("#bbbc28", "#7c7e7a", "#e74f1c", "#e474c4", "#14bcd3",
                  "#9464bc", "#2ba32c", "#1c7cb4", "#8c544c", "#2674b1",
                  "#fcdcf8"
)
