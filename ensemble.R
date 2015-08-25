library(pracma)

softmax <- function(m) {
    s <- exp(m)/sum(exp(m))
    s
}
loss_ensemble <- function(wp, val_matrix, val_target) {
    w <- softmax(wp)
    p <- val_matrix %*% w
    e <- loss(p, val_target)
    e
}
ensemble <- function(val_matrix, val_target, test_matrix, loss) {
    x0 <- matrix(ncol(val_matrix), 1, data=0.0)
    result <- fminsearch(loss_ensemble, x0, val_matrix, val_target, dfree=TRUE)
    wp <- as.matrix(result$xval)
    print(c("VAL_LOSS",result$fval))
    w <- softmax(wp)
    print(t(w))
    test_p <- test_matrix %*% w
    test_p
}
