te_test_stats_new_fast <- function(x,y, mu = 0) {
    m <- length(x)
    n <- length(y)
    if (m < 2) {
        stop("not enough 'x' observations")
    }
    if (n < 2) {
        stop("not enough 'y' observations")
    }
    swap <- FALSE
    if (m < n) {
        swap <- TRUE
        tmp <- x 
        x <- y 
        y <- tmp 
        tmp <- m
        m <- n 
        n <- tmp
        mu <- -mu
    }
    mul_vec_by_mat <- function(v,takefirst) {
        s <- sum(v)
        n <- length(v)
        a <- 1/sqrt(n)
        b <- -1 / (n * (1 + a))
        v1 <- v[1]
        template <- (s - v1)*b - v1*a
        ret <- v[1:takefirst] + template 
        ret[1] <- s*a 
        ret
    }
    qx <- mul_vec_by_mat(x,n) / sqrt(m)
    py <- mul_vec_by_mat(y,n) / sqrt(n)
    z <- qx - py 
    z1 <- z[1]
    z1_minus_mu <- z1 - mu
    z_rest <- z[2:length(z)]
    z_rest_sum <- sum(z_rest^2)
    df <- n - 1
    meanx <- mean(x)
    meany <- mean(y)
    t_denom <- sqrt(z_rest_sum/df)
    xy_ulp <- 10 * .Machine$double.eps * max(abs(meanx), abs(meany))
    if (t_denom < xy_ulp) {
        stop("data are essentially constant")
    }
    t <- z1_minus_mu / t_denom
    if (swap) {
        c(-t,df,meany,meanx)
    } else {
        c(t,df,meanx,meany)
    }
}

make_t_test_result <- function(m,n,t,df,
                               meanx,meany,
                               varx,vary,
                               alternative=c("two.sided", "less", "greater"),
                               mu,conf.level,
                               xname,yname,
                               method) {
    stderrx <- sqrt(varx/m)
    stderry <- sqrt(vary/n)
    stderr <- sqrt(stderrx^2 + stderry^2)
    diff_of_means_estimate <- meanx - meany

    alternative <- match.arg(alternative)
    if (alternative == "less") {
        nudge <- qt(conf.level,df)*stderr
        cint <- c(-Inf,diff_of_means_estimate + nudge)
        pval <- pt(t,df)
    } else if (alternative == "greater") {
        nudge <- qt(conf.level,df)*stderr
        cint <- c(diff_of_means_estimate - nudge,Inf)
        pval <- pt(t,df,lower.tail = FALSE)
    } else {
        alpha <- 1 - conf.level 
        radius <- -qt(alpha/2,df) * stderr 
        cint <- c(diff_of_means_estimate - radius, diff_of_means_estimate + radius)
        pval <- 2 * pt(-abs(t),df)
    }
    
    estimate <- c("mean of x" = meanx, "mean of y" = meany)
    names(t) <- "t"
    names(df) <- "df"
    names(mu) <- "difference in means"
    attr(cint,"conf.level") <- conf.level 
    dname <- paste(xname,"and",yname)
    rval <- list(statistic = t, parameter = df, p.value = pval,
                 conf.int = cint, estimate=estimate, null.value = mu,
                 stderr=stderr,
                 alternative = alternative,
                 method = method, data.name = dname)
    class(rval) <- "htest"
    rval
}

#' Te T-test
#' 
#' `te.test` is a fast implementation of the \eqn{T_e} Test, as described in the paper "Te Test: A New Non-asymptotic T-test for Behrens-Fisher Problems". Can be used as a drop-in replacement for `stats::t.test`, where appropriate.
#' 
#' `te.test` should be used only in situations where Welch's T-test would otherwise be appropriate (unequal variances, unequal sample sizes). Its benefit is that it is an [exact test](https://en.wikipedia.org/wiki/Exact_test).
#' 
#' If the sample sizes of the data are equal, this test has been shown to be equivalent to the paired t-test (though the confidence intervals won't be the same, since they will still have different `stderr`s). 
#' Therefore, it will also fail in the same circumstances: when the sample sizes are equal, and the pairwise differences between corresponding data are constant (e.g. x=c(1,2,7),y=c(11,12,17)), it will generate an error.
#' It may also do this in other circumstances (even when sample sizes are not equal) but the likelihood of this should be vanishingly small.
#' 
#' Unlike the paper, this uses the matrix:
#' 
#' \eqn{\big(P^T\big)_{n \times n} = 
#'   \begin{bmatrix}
#'     \frac{1}{\sqrt{n}} & \frac{1}{\sqrt{n}} & \frac{1}{\sqrt{n}} & \dots  &  \frac{1}{\sqrt{n}} & \frac{1}{\sqrt{n}} \\ 
#'     -\frac{1}{\sqrt{n}} & 1 + \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \dots & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} \\ 
#'     -\frac{1}{\sqrt{n}} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & 1 + \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \dots & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} \\ 
#'     \vdots & \vdots & \vdots & \ddots & \vdots & \vdots \\ 
#'     -\frac{1}{\sqrt{n}} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \dots & 1 + \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} \\ 
#'     -\frac{1}{\sqrt{n}} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & \dots & \frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} & 1 +\frac{-1}{n\big(1 + \frac{1}{\sqrt{n}}\big)} 
#'   \end{bmatrix}
#' }
#' 
#' Which has the same required properties as the one in the paper, but is easier to calculate with. \eqn{(Q^T)_{n \times m}} is defined similarly.
#' @usage 
#' te.test(x,...)
#' 
#' ##Default S3 method:
#' te.test(x,y,
#'         alternative=c("two.sided", "less", "greater"),
#'         mu = 0, conf.level = 0.95,...)
#' 
#' ## S3 method for class 'formula'
#' te.test(formula, data, subset, na.action = na.pass, ...)
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of `"two.sided"` (default), `"greater"` or `"less"`. You can specify just the initial letter.
#' @param mu a number indicating the true value of the difference in means (i.e. `mean(x) - mean(y)`).
#' @param conf.level confidence level of the interval.
#' @param formula a formula of the form `lhs ~ rhs` where `lhs` is a numeric variable giving the data values and `rhs` is a factor with two levels giving the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see [`model.frame`]) containing the variables in the formula `formula`. By default the variables are taken from `environment(formula)`.
#' @param subset an optional vector specifying a subset of observations to be used.
#' @param na.action a function which indicates what should happen when the data contain `NA`s.
#' @seealso [t.test]
#' @return A list with class "htest" containing the following components (just like `t.test`):
#' * `statistic`: the value of the t-statistic.
#' * `parameter`: the degrees of freedom for the t-statistic.
#' * `p.value`: the p-value for the test.
#' * `conf.int`: a confidence interval for the mean appropriate to the specified alternative hypothesis.
#' * `estimate`: the estimated difference in means.
#' * `null.value`: the specified hypothesized value of the mean difference.
#' * `stderr`: the standard error of the mean difference, always \eqn{\sqrt{{\sigma_x}^4+{\sigma_y}^4}}
#' * `alternative`: a character string describing the alternative hypothesis.
#' * `method`: a character string indicating what type of t-test was performed, always `"Te Two Sample t-test"`
#' * `data.name`: a character string giving the names of the data.
#' @references Chang Wang, & Jinzhu Jia. (2022). Te Test: A New Non-asymptotic T-test for Behrens-Fisher Problems. <https://arxiv.org/abs/2210.16473>
#' @examples 
#' 
#' te.test(1:10,7:20)        # P = .0000001456466 (would be 0.00001855282 with t.test)
#' te.test(1:10,c(7:20,200)) # P = .00009977306   (would be 0.1245135     with t.test)
#' 
#' ## Traditional interface
#' with(mtcars, te.test(mpg[am == 0], mpg[am == 1]))
#' 
#' ## Formula interface
#' te.test(mpg ~ am, data = mtcars)
#' 
#' 
#' 
#' @export
te.test <- function(x,...) UseMethod("te.test")

#' @export 
te.test.default <- function(x,y,alternative=c("two.sided", "less", "greater"), mu = 0, conf.level = 0.95,...) {
    tdfmxy <- te_test_stats_new_fast(x,y,mu)
    t <- tdfmxy[1]
    df <- tdfmxy[2]
    meanx <- tdfmxy[3]
    meany <- tdfmxy[4]
    varx <- var(x)
    vary <- var(y)
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))
    make_t_test_result(length(x),length(y),t,df,meanx,meany,varx,vary,alternative,mu,conf.level,xname,yname,"Te Two Sample t-test")
}

#mostly copied from t.test.formula
#' @export 
te.test.formula <- 
function(formula, data, subset, na.action = na.pass, ...) 
{
    if (missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    if ("paired" %in% ...names())
        stop("cannot use 'paired' in formula method")    
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) 
        if (formula[[3L]] == 1L)
            oneSampleOrPaired <- TRUE
        else
            stop("'formula' missing or incorrect")
    if (oneSampleOrPaired) {
        stop("Te test doesn't support one sample or paired tests")
    }
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ") # works in all cases
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L) 
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    ## Call the default method.
    y <- te.test(x = DATA[[1L]], y = DATA[[2L]], ...)
    if (length(y$estimate) == 2L) {
        names(y$estimate) <- paste("mean in group", levels(g))
        names(y$null.value) <-
            paste("difference in means between",
                  paste("group", levels(g), collapse = " and "))
    }
    y$data.name <- DNAME
    y
}