#' @export
vetiver_grid <- function(model, ...) {
    UseMethod("vetiver_grid")
}


#' Title
#'
#' @param model
#' @param ...
#'
#' @return
#' @rdname vetiver_grid
#' @export
#'
#' @examples
#' # lm
#' m <- lm(mpg ~ ., data=mtcars)
#' m <- lm(mpg ~ cyl*disp + hp, data=mtcars)
#' m <- lm(mpg ~ cyl + I(cyl*2) + disp, data=mtcars)
vetiver_grid.default <- function(model, ...) {

    if (!is.null(model$data)) {
        data <-  model$data
    } else if (!is.null(model$model)) {
        data <- model$model
    } else {
        data <- model$trainingData
    }

    # get predictors
    terms <- all.vars(stats::formula(model)[[3]])

    # get outcome
    outcome <- all.vars(formula(model)[[2]])

    # infer types and values from data for each term
    term_levels <- get_term_levels(c(outcome,terms), data)

    return(term_levels)
}


float_reducer <- function(vals) {
    range_vals <- round(range(vals), 2)
    c(range_vals[1],
      round(mean(vals, na.rm=T),2),
      range_vals[2],
      round((range_vals[2] - range_vals[1]) / 10))
}

int_reducer <- function(vals) {
    range_vals <- round(range(vals), 2)
    c(range_vals[1], stats::median(vals, na.rm=T), range_vals[2])
}

#' Get a reference grid-like object from all model terms
#'
#' @param terms character vector
#' @param data data.frame
#'
#' @return
#' @export
get_term_levels <- function(terms, data) {

    ref_levels <- list()

    for (term in terms) {
        x <- data[[term]]

        # data types: factor, character, logical, integer, numeric
        if (is.null(x)) {
            values <- NULL
        }
        else if (is.factor(x))  {
            values <- levels(factor(x))
        }
        else if (is.character(x) || is.logical(x)) {
            values <- sort(unique(x))
        }
        else if (is.integer(x)) {
            values <- as.integer(int_reducer(as.numeric(x)))
        }
        else {
            values <- float_reducer(as.numeric(x))
        }

        ref_levels[[term]] <- values
    }

    return(ref_levels)
}

#' Title
#'
#' @param grid
#' @param x_axis
#'
#' @return
#' @export
#'
#' @examples
sample_grid <- function(grid, n=1, x_axis=NULL) {
    # remove outcome
    grid <- grid[-1]

    newdata <- data.frame(sample=seq_len(n))
    for (grid_name in names(grid)) {
        grid_vals <- grid[[grid_name]]

        if (is.integer(grid_vals)) {
            value <- as.integer(runif(n, min=grid_vals[1], max=grid_vals[3]))
        } else if (is.numeric(grid_vals)) {
            value <- runif(n, min=grid_vals[1], max=grid_vals[3])
        } else {
            value <- sample(grid_vals, n)
        }
        newdata[[grid_name]] <- value
    }
    newdata$sample <- NULL
    newdata
}

