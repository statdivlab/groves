#' Scales MDS results
#' Scales MDS results of trees for each distance so that all coordinates for each axis and distance
#' are between -1 and 1.
#'
#' @param df A dataframe with the first two MDS coordinates for a set of trees and distances.
#' @param n The number of trees in the dataframe.
#' @param group_var The name of the variable within the dataframe that specifies the distance.
#' @param x_dim The name of the variable within the dataframe that specifies the value on the x axis.
#' @param y_dim The name of the variable within the dataframe that specifies the value on the y axis.
#'
#' @return A scaled version of the original dataframe.
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#' @export
scale_MDS <- function(df, n, group_var = "method", x_dim = "MDS1", y_dim = "MDS2") {
  scale <- df %>% group_by(get(group_var)) %>%
    summarise(max_absx = max(abs(get(x_dim))),
              max_absy = max(abs(get(y_dim))),
              max_abs = max(max_absx, max_absy))
  scale_vec <- rep(scale$max_abs, each = n)
  scale_df <- df %>%
    mutate(scale_x_dim = get(x_dim)/scale_vec,
           scale_y_dim = get(y_dim)/scale_vec)
  return(scale_df)
}
