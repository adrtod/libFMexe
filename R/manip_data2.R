#' Converts a data frame into a format digestable for libFM
#'
#' This can only deal with factors as covariate.
#' It is important to make sure your covariates are all factors (not indicators),
#' or else it will not work correctly
#'
#' @param formula the formula with the response on the left hand side
#'   and the covariates on the right hand side
#' @param data data.frame
#' @param ... optional, currently unused, arguments
#'
#' @return a character vector with one value per observation
#'
#' @details If your data is sparse, \code{sp_matrix_libFM} is about 100 times faster than
#'   \code{matrix_libFM}. I recommend using the sparse version over the standard version
#'   whenever possible. If your data consists of factor variables with a lot of levels,
#'   \code{model_frame_libFM} is faster than \code{sp_matrix_libFM}.
#'
#' @examples
#' data(movie_lens)
#'
#' movie_lens_libFM2 = model_frame_libFM(Rating ~ User + Movie, movie_lens)
#' tail(movie_lens_libFM, 10)
#'
#' @seealso \code{\link{sp_matrix_libFM}}, \code{\link{matrix_libFM}}
#' @export
model_frame_libFM2 <- function(formula, data, scale_factors = FALSE, ...) {
  # the independent variables should all be factors
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data.frame")
  }
  is.response = attr(terms(formula), "response")
  if (is.response) {
    response_name = attr(attr(terms(formula), "factors"), "dimnames")[[1]][1]
    if (any(colnames(data) == response_name)) {
      out.string = paste0(data[[response_name]])
    } else {
      warning("Response variable not in dataset. Using constant 1 as response.")
      out.string = paste0(rep(1, nrow(data)))
    }
  } else {
    warning("No response variable. Using constant 1 as response")
    out.string = paste0(rep(1, nrow(data)))
  }

  vars = attr(attr(terms(formula), "factors"), "dimnames")[[2]]
  col.num = 0
  data_weights = list()
  for (var in vars) {
    var_data = data[[var]]
    if (is.factor(var_data) || is.character(var_data)) {
      if (!is.factor(var_data)) {
        if (is.list(scale_factors) || scale_factors) {
          stop(var, " has character class and is not a factor.\n",
                  "This can cause issues if not all of the levels are present in a subset ",
                  "of the data.")
        }
        warning(var, " has character class and is not a factor.\n",
                "This can cause issues if not all of the levels are present in a subset ",
                "of the data.")
        var_data = factor(var_data)
      }
      nlev = nlevels(var_data)
      if (is.list(scale_factors) || scale_factors) {
        if (is.list(scale_factors)) {
          weights = scale_factors[[var]]
        } else {
          weights = sqrt(sum(!is.na(var_data))/table(var_data))
        }
        var_data = as.numeric(var_data)
        var_weights = unname(weights)[var_data]
        data_weights[[var]] = weights
      } else {
        var_data = as.numeric(var_data)
        var_weights = 1
        data_weights = scale_factors
      }
      out.string = paste0(out.string, " ", sprintf("%i", col.num + var_data - 1), ":", var_weights)
      col.num = col.num + nlev
    } else if (is.numeric(var_data) | is.logical(var_data)) {
      out.string = paste0(out.string, " ", sprintf("%i", col.num), ":", as.numeric(var_data))
      col.num = col.num + 1
    } else {
      stop(var, " has an unknown variable type.")
    }
  }
  return(list(out.string = out.string,
              scale_factors = data_weights))
}
