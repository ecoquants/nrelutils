#' Limits used for NREL ocean uses analysis.
#'
#' A hierarchical list with limits used.
#'
#' @format A nested list with the following items:
#' \describe{
#'   \itemize{
#'     \item{field}{: character of either "depth", "tide", "wave" or "wind"}
#'     \itemize{
#'       \item{breaks}{: numeric vector}
#'       \item{break_labels}{: weight of the diamond, in carats}
#'       \item{depth}{: "depth" present if field is "tide", "wave" or "wind"}
#'       \itemize{
#'         \item{min}{: numeric minimum depth}
#'         \item{max}{: numeric maximum depth}
#'       }
#'     }
#'   }
#' }
#' @source \url{https://github.com/ecoquants/nrelutils}
"nrel_limits"
