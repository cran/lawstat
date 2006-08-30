"rjb.test" <-
function(x, option=c("RJB", "JB"))
{

option<-match.arg(option) 

   if (NCOL(x) > 1)
        stop("x is not a vector or univariate time series")
    if (any(is.na(x)))
        stop("NAs in x")
    DNAME <- deparse(substitute(x))


    ### Calculate the first 4 central moments ###
    n <- length(x)
    m1 <- sum(x)/n
    m2 <- sum((x - m1)^2)/n
    m3 <- sum((x - m1)^3)/n
    m4 <- sum((x - m1)^4)/n


    ### User can choose the Standard Jarque Bera Test or Robust Jarque Bera Test ###
    ### Robust Jarque Bera Test is default ###
    if(option=="JB")
    {
    b1 <- (m3/m2^(3/2))^2;
    b2 <- (m4/m2^2);
    vk<-24/n;
    METHOD <- "Standard Jarque Bera Test"
    statistic<-n * b1/6 + n * (b2 - 3)^2/24
    }

    else
    {
    option="RJB";
    J<-sqrt(pi/2)*mean(abs(x-median(x)));
    J2<-J^2;
    b1 <- (m3/(J2)^(3/2))^2;
    b2 <- (m4/(J2)^2);
    vk<-70/n;
    METHOD <- "Robust Jarque Bera Test"
    vs<-6/n
    ek<-3
    statistic <- b1/vs + (b2 - ek)^2/vk
    }

    ### Display Output ###
    STATISTIC=statistic
    names(STATISTIC) <- "X-squared"
    PARAMETER <- 2
    names(PARAMETER) <- "df"
    p.value <- 1 - pchisq(statistic, df = 2)

    structure(list(statistic = STATISTIC, parameter = PARAMETER,
        p.value = p.value, method = METHOD, data.name = DNAME),
        class = "htest")
}