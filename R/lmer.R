


#' @rdname vetiver_create_description
#' @export
vetiver_create_description.lmer <- function(model) {
    "A mixed effects model fit with the lme4 package"
}

#' @rdname vetiver_prepare_model
#' @export
vetiver_prepare_model.lmer <- function(model) {
    model <- butcher::butcher(model)
    return(model)
}

#' this is mainly need for keeping track of all the levels for factor variables
#' which itself is mainly needed for generating confidence intervals
#' @rdname vetiver_ptype
#' @export
vetiver_ptype.lmer <- function(model, ...) {
    pred_names <- all.vars(formula(model, fixed.only=T)[-2])
    prototype <- vctrs::vec_ptype(model@frame[pred_names])

    if (is.null(prototype)) {
        return(NULL)
    }
    tibble::as_tibble(prototype)
}




#' Title
#'
#' @param vetiver_model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
handler_predict.lmer <- function(vetiver_model, ...) {

    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!purrr::is_null(ptype)) {
            newdata <- vetiver::vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }

        model <- vetiver_model$model

        newdata$pred <- lme4:::predict.merMod(model,
                                               newdata=newdata,
                                               type='response',
                                               re.form=NA)

        if (!purrr::is_null(ptype)) {
            Designmat <- model.matrix(formula(model, fixed.only=TRUE)[-2], newdata)
            predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat))
            newdata$SE <- sqrt(predvar + model@sigma^2)
            newdata$conf_lo = newdata$pred - 1.96*newdata$SE
            newdata$conf_hi = newdata$pred + 1.96*newdata$SE

            res <- list(.pred = newdata$pred,
                        .conf_lo = newdata$conf_lo,
                        .conf_hi = newdata$conf_hi)
        } else {
            res <- list(.pred = newdata$.pred)
        }

        return(res)
    }
}

