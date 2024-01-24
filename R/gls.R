#' @rdname vetiver_create_description
#' @export
vetiver_create_description.gls <- function(model) {
    glue("An Generalized Least-Squares model fit with the nlme package")
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.gls <- function(model) {
    model <- butcher::axe_data(model)
    model <- butcher::axe_fitted(model)
    model
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.gls <- function(model, ...) {
    pred_names <- vetiver:::preds_lm_ish(model)
    prototype <- vctrs::vec_ptype(model$data[pred_names])

    if (is.null(prototype)) {
        return(NULL)
    }
    tibble::as_tibble(prototype)
}

#' @rdname handler_startup
#' @export
handler_predict.gls <- function(vetiver_model, ...) {
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

