#' @rdname vetiver_create_description
#' @export
vetiver_create_description.glm <- function(model) {
    glue("A generalized linear model ({model$family$family} family, {model$family$link} link)")
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.glm <- function(model) {
    model <- butcher::axe_data(model)
    model
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.glm <- function(model, ...) {
    vetiver_ptype.lm(model, ...)
}

#' @rdname handler_startup
#' @export
handler_predict.glm <- function(vetiver_model, ...) {
    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }
        ret <- predict(vetiver_model$model, newdata = newdata, type='response', se.fit=TRUE, ...)
        list(.pred = 100 * ret$fit,
             .conf_lo = pmax(100 * (ret$fit - 1.96 * ret$se.fit), 0),
             .conf_hi = pmin(100 * (ret$fit + 1.96 * ret$se.fit)), 100)
    }
}

