#' @rdname vetiver_create_description
#' @export
vetiver_create_description.coxph <- function(model) {
    "A Cox regression model"
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.coxph <- function(model) {
    #model <- butcher::axe_data(model)
    model
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.coxph <- function(model, ...) {
    pred_names <- preds_lm_ish(model)
    prototype <- vctrs::vec_ptype(model.frame(model)[pred_names])
    tibble::as_tibble(prototype)
}

#' @rdname handler_startup
#' @export
handler_predict.coxph <- function(vetiver_model, ...) {

    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }

        # add status and time variable as dummy
        time_var <- all.vars(formula(model))[1]
        newdata[[status_var]] <- NA
        status_var <- all.vars(formula(model))[2]
        newdata[[status_var]] <- NA

        res <- survival::survfit(model, newdata=newdata)

        list(.pred = 100 * res$surv,
             .conf_lo = 100 * res$lower,
             .conf_hi = 100 * res$upper,
             .time = res$time)
    }

}
