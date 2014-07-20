
countdays <- function() {
    cnt <- 0
    d <- data[1,2]
    for (di in data[,2]) {
        if (di != d) {
            cnt <- cnt+1
            d <- di
        }
    }
    cnt
}

getdays <- function() {
    d <- data[1,2]
    days <- c(d)
    ## days <- cbind(days,d)
    for (di in data[,2]) {
        if (di != d) {
            days <- cbind(days,di)
            d <- di
        }
    }
    days
}

stepsperday <- function() {
    days <- levels(data[,2])
    
    isna <- is.na(data[,1])
    s1 <- data[,1]
    d1 <- data[,2]
    s2 <- s1[!isna]
    d2 <- d1[!isna]
    
   ## x <- sapply(split(st,da), sum)
    v <- vector(mode = "numeric", length = 61)
    i <- 1
    for (d in days) {
        isd <- d2 == d
        sk <- s2[isd]
        v[i] <- sum(sk)
        i <- i + 1
    }
    
    v
}
