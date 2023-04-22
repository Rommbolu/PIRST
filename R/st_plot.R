##State-Trace-Plot for a single case##
#' st_plot
#'
#' @param simdata A dataframe in the form c(X, Y, Condition)
#'
#' @return A State-Trace-Plot (probably for your single case data)
#' @export
#'
#' @examples st_plot(data)
#'
#'           Best to be tested out with:
#'
#'           data = pirstsim_sc(nhalf = 4,
#'                              overlap = 1,
#'                              interaction_constant = 0.2,
#'                              noise = 0,
#'                              curve = 1,)
st_plot <- function(simdata) {
  ggplot2::ggplot(simdata, ggplot2::aes(x = X, y = Y, xax)) +
    ggplot2::geom_point(ggplot2::aes(color = Condition)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("State-Trace-Plot") +
    ggplot2::xlab("Variable A") +
    ggplot2::ylab("Variable B") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
