#' Get a reference grid-like object from all model terms
#'
#' @param terms character vector
#' @param data data.frame
#'
#' @return
#' @export
get_term_levels <- function(terms, data) {
    float_reducer <- function(vals) {
        range_vals <- round(range(vals), 2)
        c(range_vals[1], round(mean(vals, na.rm=T),2), range_vals[2])
    }
    int_reducer <- function(vals) {
        range_vals <- round(range(vals), 2)
        c(range_vals[1], stats::median(vals, na.rm=T), range_vals[2])
    }

    ref_levels <- list()

    for (term in terms) {
        x <- data[[term]]

        # data types: factor, character, logical, integer, numeric
        if (is.factor(x))  {
            ref_levels[[term]] <- levels(factor(x))
        }
        else if (is.character(x) || is.logical(x)) {
            ref_levels[[term]] <- sort(unique(x))
        }
        else if (is.integer(x)) {
            ref_levels[[term]] <- int_reducer(as.numeric(x))
        }
        else {
            ref_levels[[term]] <- float_reducer(as.numeric(x))
        }
    }
    return(ref_levels)
}
