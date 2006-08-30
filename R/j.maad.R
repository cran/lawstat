"j.maad" <-
function (x, na.rm = FALSE) 
{

### User can choose to remove NA elements
if (na.rm) { x <- x[!is.na(x)]}


### Robust Standard Deviation J
J<-sqrt(pi/2)*mean(abs(x-median(x))) 

# return(J)

paste("MAAD estimated J =", J, "for data", deparse(substitute(x)))

}

