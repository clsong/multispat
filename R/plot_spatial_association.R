#' Plot the spatial associations
#'
#' @return A graph visualizing spatial associations
#' @spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @plot_type Either using the package ggraph or DiagrammR
#' @export

plot_spatial_association <- function(spatial_association, plot_type = 'ggraph'){
  if(plot_type == 'ggraph'){
    p <- plot_spatial_association_ggraph(spatial_association)
  }
  if(plot_type == 'DiagrammeR'){
    p <- plot_spatial_association_DiagrammeR(spatial_association)
  }


  p
}

#' @import DiagrammeR
plot_spatial_association_DiagrammeR <- function(spatial_association){
  length <- .8
  n <- tibble(
    node = LETTERS[1:gamma]
  ) %>%
    mutate(
      label = node
    ) %>%
    rename(
      id = node
    ) %>%
    mutate(
      width = .3,
      height = .3,
      alpha = 354.91
    )

  e <- spatial_association %>%
    as.table() %>%
    as.data.frame() %>%
    transmute(
      from = Var1,
      to = Var2,
      weight = Freq
    ) %>%
    mutate(
      association = case_when(
        weight > 0 ~ "attractive",
        weight == 0 ~ "null",
        weight < 0 ~ "repulsive"
      )
    ) %>%
    filter(
      match(from, LETTERS[1:gamma]) <= match(to, LETTERS[1:gamma])
    ) %>%
    mutate(from = factor(from), to = factor(to)) %>%
    mutate(color = case_when(
      association == 'null' ~ "gray",
      association == 'repulsive' ~ "#4594B8",
      association == 'attractive' ~ "#8E5750",
      TRUE ~ 'white'))  %>%
    mutate(arrowhead = case_when(
      weight < 0 ~ "inv",
      weight == 0 ~ "none",
      weight > 0 ~ "normal"
    )) %>%
    mutate(
      arrowtail = arrowhead,
      fontcolor = color,
      dir = 'both',
      penwidth = ifelse(weight == 0, .5,  abs(weight)),
      label = weight)

  create_graph() %>%
    add_nodes_from_table(
      table = n
    ) %>%
    add_edges_from_table(
      table = e,
      from_col = from,
      to_col = to,
      from_to_map = id_external
    ) %>%
    set_node_attr_to_display(attr = id_external) %>%
    render_graph()
}

#' @import ggraph tidygraph
plot_spatial_association_ggraph <- function(spatial_association){
  spatial_association %>%
    as.table() %>%
    as.data.frame() %>%
    transmute(
      from = Var1,
      to = Var2,
      weight = Freq
    ) %>%
    mutate(
      association = case_when(
        weight > 0 ~ "attractive",
        weight == 0 ~ "null",
        weight < 0 ~ "repulsive"
      )
    ) %>%
    mutate(
      transparency = ifelse(weight == 0, 1, abs(weight))
    ) %>%
    filter(
      match(from, LETTERS[1:gamma]) <= match(to, LETTERS[1:gamma])
    ) %>%
    as_tbl_graph() %>%
    ggraph(layout = 'linear', circular = TRUE) +
    geom_edge_link(aes(color = association,
                       alpha = transparency),
                   # arrow = arrow(
                   #   length = unit(4, 'mm'), type ='open', ends = 'both'
                   #   ),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +
    geom_edge_loop(aes(color = association, alpha = transparency),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +
    geom_node_point(size = 4) +
    geom_node_label(aes(label = name)) +
    scale_edge_colour_manual(
      values=c("#4594B8", "gray", "#8E5750")
    ) +
    scale_edge_alpha_identity() +
    coord_fixed() +
    theme_graph(foreground = "white",
                fg_text_colour = "white",
                base_family = 'Helvetica') +
    theme(
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text=element_text(size=12)
    )
}
