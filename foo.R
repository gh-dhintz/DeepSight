combine_plot_B_components <- function(plots, values) {  # ← CHANGE: Accept values parameter
  theme_border <- create_border_theme_B()
  
  plots$p1 / (plots$p2 + (plots$p3 / plots$p4)) + 
    plot_annotation(
      title = values$title,      # ← CHANGE: Use annotation title
      subtitle = values$subtitle, # ← CHANGE: Use annotation subtitle
      caption = values$figOvCap,  # ← CHANGE: Use annotation overall caption
      tag_levels = 'A', 
      theme = theme_border
    ) +
    plot_layout(
      heights = c(1, 1, 1), 
      widths = c(1, 1, 2),
      guides = "collect"
    )
}

