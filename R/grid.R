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
