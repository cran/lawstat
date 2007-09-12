"ltrend.test"<-
function (y, group, score=NULL, option = c("mean", "median", "trim.mean"), 
    trim.alpha = 1) 
{

    if(is.null(score)){score=group}

    option <- match.arg(option)
    DNAME = deparse(substitute(y))
    y <- na.omit(y)
    if ((option == "trim.mean") & (trim.alpha == 1)) {
        stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean option")
    }
    gr<-score
    group <- as.factor(group)
    if (option == "mean") {
        means <- tapply(y, group, mean)
        resp.mean <- abs(y - means[group])
        mu <- mean(resp.mean)
        n <- tapply(y, group, length)
        dbar <- sum(as.numeric(factor(levels(group))) * n)/sum(n)
        z <- as.vector(resp.mean - mu)
        d <- as.numeric(gr) - dbar
        statistic = summary(lm(z ~ d))$coefficients[2, 1]
        METHOD = "ltrend test based on classical Levene's procedure using the group means"
        p.value = summary(lm(z ~ d))$coefficients[2, 4]
    }
    else if (option == "median") {
        means <- tapply(y, group, median)
        resp.mean <- abs(y - means[group])
        mu <- mean(resp.mean)
        n <- tapply(y, group, length)
        dbar <- sum(as.numeric(factor(levels(group))) * n)/sum(n)
        z <- as.vector(resp.mean - mu)
        d <- as.numeric(gr) - dbar
        statistic = summary(lm(z ~ d))$coefficients[2, 1]
        METHOD = "ltrend test based on the modified Brown-Forsythe Levene-type procedure using the group medians"
        p.value = summary(lm(z ~ d))$coefficients[2, 4]
    }
    else {
        option = "trim.mean"
        trimmed.mean <- function(y) mean(y, trim = trim.alpha)
        means <- tapply(y, group, trimmed.mean)
        resp.mean <- abs(y - means[group])
        resp.mean <- abs(y - means[group])
        mu <- mean(resp.mean)
        n <- tapply(y, group, length)
        dbar <- sum(as.numeric(factor(levels(group))) * n)/sum(n)
        z <- as.vector(resp.mean - mu)
        d <- as.numeric(gr) - dbar
        statistic = summary(lm(z ~ d))$coefficients[2, 1]
        METHOD = "ltrend test based on the modified Levene-type procedure using the group trimmed means"
        p.value = summary(lm(z ~ d))$coefficients[2, 4]
    }
    STATISTIC = statistic
    names(STATISTIC) = "Test Statistic"
    structure(list(statistic = STATISTIC, p.value = p.value, 
        method = METHOD, data.name = DNAME), class = "htest")
}


