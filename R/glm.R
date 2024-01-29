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
    print('GOT GLM HANDLER')
    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }
        ret <- predict_glm(vetiver_model$model,
                           newdata = newdata,
                           type='response',
                           se.fit=TRUE, ...)

        list(.pred = 100 * ret$fit,
             .conf_lo = pmax(100 * (ret$fit - 1.96 * ret$se.fit), 0),
             .conf_hi = pmin(100 * (ret$fit + 1.96 * ret$se.fit)), 100)
    }
}


#' Title
#'
#' # predict glm function that gives se.fit even when data is missing
#' # this was needed because dispersion fails to be calculated without the
#' # original training data. dispersion is basically always = 1 though so
#' # it's not very necessary
#'
#' @param object
#' @param newdata
#' @param type
#' @param se.fit
#' @param dispersion
#' @param terms
#' @param na.action
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict_glm <- function (object, newdata = NULL,
                         type = c("link", "response",
                                  "terms"), se.fit = FALSE,
                         dispersion = NULL, terms = NULL,
                         na.action = na.pass, ...)
{
    type <- match.arg(type)
    na.act <- object$na.action

    dispersion <- 1
    residual.scale <- as.vector(sqrt(dispersion))
    pred <- predict.lm(object, newdata, se.fit, scale = residual.scale,
                       type = if (type == "link")
                           "response"
                       else type, terms = terms, na.action = na.action)
    fit <- pred$fit
    se.fit <- pred$se.fit
    switch(type, response = {
        se.fit <- se.fit * abs(family(object)$mu.eta(fit))
        fit <- family(object)$linkinv(fit)
    }, link = , terms = )
    if (missing(newdata) && !is.null(na.act)) {
        fit <- napredict(na.act, fit)
        se.fit <- napredict(na.act, se.fit)
    }
    pred <- list(fit = fit, se.fit = se.fit, residual.scale = residual.scale)

    pred
}

