

#' A ggplot2 theme
#'
#' This function can be added to a ggplot2 call and creates the official OpenMind ggplot theme
#' @param legend_position Specify Legend position c(x, y)
#' @param axis.text Specify Axis Text size
#' @param axis.title Specify Title Text size
#' @param legend.text.size Specify Legend Text size
#' @param title.size Specify Title Text size
#' @export
theme_om <- function(legend_position = c(.55, .93),
                     axis_text_size = 20,
                     axis_title_size = 20,
                     legend_text_size = 24,
                     title_size = 26,
                     overall_text_size = 24, ...){
  theme(panel.background = element_rect(fill="white"),
        legend.text = element_text(size=legend_text_size),
        legend.position = legend_position,
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=title_size),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(color = "grey20", size = axis_text_size),
        axis.text.y = element_text(color = "grey20", size = axis_text_size),
        axis.title.x = element_text(color = "grey20", size = axis_title_size),
        axis.title.y = element_text(color = "grey20", size = axis_title_size),
        text = element_text(family="Poppins",size=24, ...))
}


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


