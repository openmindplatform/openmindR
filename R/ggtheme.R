#' A ggplot2 theme
#'
#' This function can be added to a ggplot2 call and creates the official OpenMind ggplot theme
#' @param legend_position Specify Legend position c(x, y)
#' @export
theme_om <- function(legend_position = c(.55, .93)){
  theme(panel.background = element_rect(fill="white"),
        text = element_text(family="Poppins",size=24),
        legend.text = element_text(size=18),
        legend.position = legend_position,
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=26),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(color = "grey20", size = 20),
        axis.text.y = element_text(color = "grey20", size = 20),
        axis.title.x = element_text(color = "grey20", size = 20),
        axis.title.y = element_text(color = "grey20", size = 20))
}

# Blue : #2a98db
# Purple : #3d4fa1
# Yellow : #e8df15
# Turquoise : #65c6c3
# Pink : #ec145b
# Green : #94cfa1
# Black : #414042


#' A ggplot2 palette
#'
#'  Blue : #2a98db
#'  Purple : #3d4fa1
#'  Yellow : #e8df15
#'  Turquoise : #65c6c3
#'  Pink : #ec145b
#'  Green : #94cfa1
#'  Black : #414042
#' This object contains a palette with the official OpenMind colors
#' @export
pal_om <- scales::manual_pal(c("#2a98db","#3d4fa1","#e8df15","#65c6c3","#ec145b","#94cfa1", "#414042"))

#' A ggplot2 color palette
#'
#'  Blue : #2a98db
#'  Purple : #3d4fa1
#'  Yellow : #e8df15
#'  Turquoise : #65c6c3
#'  Pink : #ec145b
#'  Green : #94cfa1
#'  Black : #414042
#' This object contains a palette with the official OpenMind colors
#' @export
scale_color_om <- function(...) { discrete_scale("colour", "om", pal_om, ...) }


#' A ggplot2 fill palette
#'
#'  Blue : #2a98db
#'  Purple : #3d4fa1
#'  Yellow : #e8df15
#'  Turquoise : #65c6c3
#'  Pink : #ec145b
#'  Green : #94cfa1
#'  Black : #414042
#' This object contains a palette with the official OpenMind colors
#' @export
scale_fill_om <- function(...) { discrete_scale("fill", "om", pal_om, ...) }
