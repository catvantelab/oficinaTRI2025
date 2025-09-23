rm(list = ls())

# banco 2pl ----

## T1 ----

# parâmetros originais dos itens
# a1.original <- c(1.0295999, 1.2833717, 1.8143789, 2.9310132, 1.0113734,
#                  2.4803165, 1.0453012, 1.9615977, 1.5082983, 2.3080323)
#
# b.original <- c(1.36330492, 1.38053410, 1.23284018, -0.12924484, -0.88812405,
#                 0.04909726, 0.45853523, 0.27753228, 0.91737338, 0.59221396)


set.seed(1234)
# parâmetros originais dos itens
# a1.original <- rlnorm(50, 0, .5)
a1.original <- runif(50, .5, 1.5)
b.original <- rnorm(50)

# amostra.normal <- rnorm(500)
amostra.superior <- rnorm(500, 2)
amostra.normal <- c(amostra.superior, rnorm(500))

d <- -a1.original*b.original

banco2pl.t1 <- mirt::simdata(a = a1.original, d = d, itemtype = '2PL', Theta = amostra.normal)
banco2pl.t1 <- data.frame(banco2pl.t1)

usethis::use_data(banco2pl.t1, internal = FALSE, overwrite = TRUE)

## T2 ----

# parâmetros originais dos itens
# set.seed(1234)
# a1.sim <- rlnorm(5)
# a1.sim <- c(a1.original[6:10], a1.sim)
#
# # b.sim <- rnorm(5)
# # b.sim <- b.sim + rlnorm(5)*2
# b.sim <- runif(5, 1, 2)
# b.sim <- c(b.original[6:10], b.sim)


set.seed(1234)
a1.sim <- runif(20, .5, 1.5)
a1.sim <- c(a1.original[21:50], a1.sim)

b.sim <- runif(20, 1, 2)
b.sim <- c(b.original[21:50], b.sim)

# amostra.superior <- rnorm(500, 2)
# amostra.normal <- c(amostra.superior, rnorm(500))
amostra.normal <- rnorm(1000)

d.sim <- -a1.sim*b.sim

banco2pl.t2 <- mirt::simdata(a = a1.sim, d = d.sim, itemtype = '2PL', Theta = amostra.normal)
banco2pl.t2 <- data.frame(banco2pl.t2)

names(banco2pl.t2) <- paste0('Item_', 6:15)
names(banco2pl.t2) <- paste0('Item_', 21:70)
usethis::use_data(banco2pl.t2, internal = FALSE, overwrite = TRUE)


# banco graded ----

set.seed(1234)

# parâmetros originais dos itens
# a1.original <- rlnorm(10)
# d1.original <- rnorm(10)
# d2.original <- d1.original - rlnorm(10)
# d <- cbind(d1.original, d2.original)
#
# amostra.normal <- rnorm(500)
#
# banco.poli <- data.frame(mirt::simdata(a = a1.original, d = d, itemtype = 'graded', Theta = amostra.normal))
#
# usethis::use_data(banco.poli, internal = FALSE, overwrite = TRUE)


set.seed(1234)
# a
a <- rlnorm(10, 0.5, 0.5)
a[4] <- .150
# b
b1 <- rnorm(10)
b2 <- b1 + rlnorm(10, -1, 0.1)
b3 <- b2 + rlnorm(10, -1, 0.1)

d <- as.matrix(data.frame(
  d1 = -b1*a,
  d2 = -b2*a,
  d3 = -b3*a
))

banco.poli <- data.frame(mirt::simdata(a = a, d = d, itemtype = 'graded', Theta = rnorm(500)))

usethis::use_data(banco.poli, internal = FALSE, overwrite = TRUE)

fit.graded <- mirt(banco.poli, 1, 'graded')

coef(fit.graded, IRTpars = TRUE, simplify = TRUE)

a

mirt::plot(fit.graded)
mirt::plot(fit.graded, type = 'info')
mirt::plot(fit.graded, type = 'rxx')
mirt::plot(fit.graded, type = 'SE')
mirt::plot(fit.graded, type = 'infoSE')
mirt::plot(fit.graded, type = 'infotrace')
mirt::plot(fit.graded, type = 'trace')
mirt::plot(fit.graded, type = 'itemscore')
mirt::plot(fit.graded, type = 'score')

