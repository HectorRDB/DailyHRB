# Plot a smiley face
#' plotSmiley function
#'
#' Cute drawing for a family joke.
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' DailyHRB::plotSmiley()
plotSmiley <- function() {
  mouth <- lapply(seq(-1, 1, length.out = 100), function(x) {
    -sqrt(1 - x^2)
  }) %>% unlist()
  mouth <- data.frame("x" = seq(-1, 1, length.out = 100),
                      "y" = mouth) %>%
    filter(y < -(.5)) %>%
    mutate(y = y + .5) %>%
    mutate(type = "mouth")

  nose <- data.frame("x" = 0,
                     y = seq(-.05, .1, length.out = 5),
                     type = "nose")
  eye <- c(lapply(seq(-1, 1, length.out = 1000), function(x) {
                -sqrt(1 - x^2)
              }) %>% unlist(),
           lapply(seq(1, -1, length.out = 1000), function(x) {
                sqrt(1 - x^2)
              }) %>% unlist()
  )

  eye <- eye / 5
  eye <- data.frame(x = c(seq(-1, 1, length.out = 1000) / 5,
                          seq(1, -1, length.out = 1000) / 5),
                    y = eye)
  smiley <- rbind(mouth, nose,
                  eye %>% mutate(x = x - .5, y = y + .25,type = "left_eye"),
                  eye %>% mutate(x = x + .5, y = y + .25, type = "right_eye")
                  )

  ggplot(smiley, aes(x = x, y = y)) +
    geom_path(aes(group = type)) +
    theme_void()
}
