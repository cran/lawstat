brunner.munzel.test <- function(x, ...) UseMethod("brunner.munzel.test")

brunner.munzel.test.default <- function (x, y, alternative = c("two.sided", "greater", "less"), 
    alpha = 0.05) 
{
    alternative <- match.arg(alternative)
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    x <- na.omit(x)
    y <- na.omit(y)
    n1 = length(x)
    n2 = length(y)
    r1 = rank(x)
    r2 = rank(y)
    r = rank(c(x, y))
    m1 = mean(r[1:n1])
    m2 = mean(r[n1 + 1:n2])
    pst = (m2 - (n2 + 1)/2)/n1
    v1 = sum((r[1:n1] - r1 - m1 + (n1 + 1)/2)^2)/(n1 - 1)
    v2 = sum((r[n1 + 1:n2] - r2 - m2 + (n2 + 1)/2)^2)/(n2 - 1)
    statistic = n1 * n2 * (m2 - m1)/(n1 + n2)/sqrt(n1 * v1 + 
        n2 * v2)
    dfbm = ((n1 * v1 + n2 * v2)^2)/(((n1 * v1)^2)/(n1 - 1) + 
        ((n2 * v2)^2)/(n2 - 1))
    if ((alternative == "greater") | (alternative == "g")) {
        p.value = pt(statistic, dfbm)
    }
    else if ((alternative == "less") | (alternative == "l")) {
        p.value = 1-pt(statistic, dfbm)
    }
    else {
        alternative = "two.sided"
        p.value = 2 * min(pt(abs(statistic), dfbm), (1 - pt(abs(statistic), 
            dfbm)))
    }
    conf.int = c(pst - qt(1 - alpha/2, dfbm) * sqrt(v1/(n1 * 
        n2^2) + v2/(n2 * n1^2)), pst + qt(1 - alpha/2, dfbm) * 
        sqrt(v1/(n1 * n2^2) + v2/(n2 * n1^2)))
    estimate = pst
    ESTIMATE = pst
    names(ESTIMATE) = "P(X<Y)+.5*P(X=Y)"
    STATISTIC = statistic
    names(STATISTIC) = "Brunner-Munzel Test Statistic"
    PARAMETER = dfbm
    names(PARAMETER) = "df"
    CONF.INT = conf.int
    names(CONF.INT) = c("lower", "upper")
    attr(CONF.INT, "conf.level") = (1 - alpha)
    METHOD = "Brunner-Munzel Test"
    structure(list(estimate = ESTIMATE, conf.int = CONF.INT, 
        statistic = STATISTIC, parameter = PARAMETER, p.value = p.value, 
        method = METHOD, data.name = DNAME), class = "htest")
}

brunner.munzel.test.formula <-
    function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
    DATA <<- setNames(split(mf[[response]], g), c("x", "y"))
    y <- do.call("brunner.munzel.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
