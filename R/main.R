#' Visualize the summary of a data frame
#'
#' @param x Data frame
#' @param base Character. "raw" or "hist"
#' @param if_box Logical. Whether plot the box
#' @param if_message Logical. Whether display the progress message
#'
#' @return a figure
#' @export
#'
#' @examples
#' plot_summary(iris)
#' plot_summary(iris, if_box = TRUE)
plot_summary <- function(x, base = c("raw", "hist"), if_box = FALSE, if_message = FALSE){
  base <- match.arg(base, base)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vn <- ncol(x)
  rown <- floor(sqrt(vn))
  coln <- ceiling(vn / rown)
  oldpar <- par(mfrow = c(rown, coln),
                mar = c(3, 2, 0.5, 0.5))
  for (i in 1:vn) {
    vname <- colnames(x)[i]
    if (if_message) message(i, "/", vn, " ", vname)
    v <- unlist(x[, i])
    if (is.numeric(v)) {
      if (base == "raw"){
        dotchart(v, xlim = range(v, na.rm = TRUE),
                 pch = 16, lcolor = NULL, labels = NA,
                 col = rgb(0, 0, 0, alpha = .3))
      }
      if (base == "hist"){
        plothist(v, show_density = FALSE, show_normline = TRUE, eightlines = FALSE, myxlim = range(v, na.rm = TRUE))
      }
      if (if_box){
        par(new = TRUE)
        boxplot(v, ylim = range(v, na.rm = TRUE), axes = FALSE,
                col = NA, border = "blue", outline = FALSE, horizontal = TRUE)
        abline(v = mean(v, na.rm = TRUE), col = "red")
        text(fivenum(v, na.rm = TRUE), labels = fivenum(v, na.rm = TRUE), y = 1.25, col = 'blue')
        text(mean(v, na.rm = TRUE), labels = round(mean(v, na.rm = TRUE), digits = decimal_digits(median(v, na.rm = TRUE))), y = 1.3, col = 'red')
      }
    } else {
      barplot(table(v))
      box()
    }
    legend("topright",
           legend = paste0(i, ". ", vname),
           bty = "n",
           text.col = "blue")
  }
  par(oldpar)
}
