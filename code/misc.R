# Copy values in x until the next non-NA value
copyforward <- function(x) {
    r <- base::rle(is.na(x))
    for (i in 2:length(r$lengths)) {
        if (r$values[i]) {
            istart <- sum(r$lengths[1:(i - 1)]) + 1
            iend <- sum(r$lengths[1:i])
            x[istart:iend] <- x[istart - 1]
        }
    }
    x
}

# Compute mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
