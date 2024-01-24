#' @rdname vetiver_create_description
#' @export
vetiver_create_description.lm <- function(model) {
    "An OLS linear regression model"
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.lm <- function(model) {
    model <- butcher::axe_data(model)
    model
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.lm <- function(model, ...) {
    pred_names <- preds_lm_ish(model)
    prototype <- vctrs::vec_ptype(model$model[pred_names])
    tibble::as_tibble(prototype)
}

#' @rdname handler_startup
#' @export
handler_predict.lm <- function(vetiver_model, ...) {

    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }
        ret <- predict(vetiver_model$model,
                       newdata = newdata,
                       interval='confidence',
                       ...)
        list(.pred = as.numeric(ret[,'fit']),
             .conf_lo = as.numeric(ret[,'lwr']),
             .conf_hi = as.numeric(ret[,'upr']))
    }

}




