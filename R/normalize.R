#' Normalize weighting targets to sum to 1
#'
#' @inheritParams harvest
#' @return A list of targets ready to be passed to
#'   \code{\link[harvest]{harvest}}
#' @export

normalize <- function(target) {
  if (is.data.frame(target)) {
    target = df_targets_to_list(target)
  }

  sums   = lapply(target, sum)
  target = mapply(`/`, target, sums)

  lapply(
    target,
    function(x) {
      x[[length(x)]] = 1 - sum(x) + x[[length(x)]]
      x
    }
  )
}
