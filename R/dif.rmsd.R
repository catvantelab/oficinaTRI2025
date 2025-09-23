#' @title RMSD
#' @name dif.rmsd
#'
#' @description Análise de DIF usando Root Mean Square Deviaton (RMSD)
#'
#' @param calib objeto do tipo `SingleGroupClass` gerado com a função `mirt::mirt`
#' @param flag ponto de corte para sinalizar itens com DIF
#' @param probfun argumento lógico. Se FALSE, o função de escore esperado
#' será integrada
#' @param dentype densidade para usar para o traço latente. Pode ser `'norm'`
#'  para densidade normal com média e variância extraídas do objeto,
#'  `'snorm'` para normal padrão e `'empirical'` para usar a densidade
#'  estimada obtida no passo E
#'
#' @examples
#' data(banco2pl.t1, package = 'oficinaTRI2025')
#' fit.t1 <- mirt(data = banco2pl.t1, model = 1, itemtype = '2PL', TOL = .001)
#' data(banco2pl.t2, package = 'oficinaTRI2025')
#' tab.pars <- mirt(data = banco2pl.t2, model = 1, itemtype = '2PL', TOL = .001, pars = 'values')
#' itens.comuns <- paste0('Item_', 21:50)
#' pars.comuns <- mod2values(fit.t1)
#' for(i in 1:length(itens.comuns))
#' {
#'   tab.pars[tab.pars$item == itens.comuns[i] & tab.pars$name == 'a1', 'value'] <- pars.comuns[pars.comuns$item == itens.comuns[i] & pars.comuns$name == 'a1', 'value']
#'   tab.pars[tab.pars$item == itens.comuns[i] & tab.pars$name == 'd', 'value'] <- pars.comuns[pars.comuns$item == itens.comuns[i] & pars.comuns$name == 'd', 'value']
#'   tab.pars[tab.pars$item == itens.comuns[i], 'est'] <- FALSE
#' }
#' tab.pars[tab.pars$name == 'MEAN_1', 'est'] <- TRUE
#' tab.pars[tab.pars$name == 'COV_11', 'est'] <- TRUE
#' fit.t2 <- mirt(data = banco2pl.t2, model = 1, itemtype = '2PL', TOL = .001, pars = tab.pars)
#' rmsd.pisa(calib = fit.t2, flag = .1)
#'
#' @export

dif.rmsd <- function(calib, flag = 0, probfun = TRUE, dentype = "norm"){
  dat <- mirt::extract.mirt(calib, "data")
  ret <- data.frame()
  nfact <- mirt::extract.mirt(calib, "nfact")
  stopifnot(nfact == 1L)
  sv <- mirt::mod2values(calib)
  sv$est <- FALSE
  Theta <- matrix(seq(-6, 6, length.out = 61))
  mod_g <- mirt::mirt(dat, nfact, itemtype = mirt::extract.mirt(calib,
                                                                "itemtype"), pars = sv, technical = list(storeEtable = TRUE,
                                                                                                         customTheta = Theta, customK = mirt::extract.mirt(calib,
                                                                                                                                                           "K")))
  Etable <- mod_g@Internals$Etable[[1]]$r1
  if (dentype %in% c("norm", "snorm")) {
    mu <- sv$value[sv$name == "MEAN_1"]
    sigma2 <- sv$value[sv$name == "COV_11"]
    if (dentype == "snorm") {
      mu <- 0
      sigma2 <- 1
    }
    f_theta <- dnorm(Theta, mean = mu, sd = sqrt(sigma2))
    f_theta <- as.vector(f_theta/sum(f_theta))
  }
  else if (dentype == "empirical") {
    f_theta <- rowSums(Etable)/sum(Etable)
  }
  else stop("dentype not supported", call. = FALSE)
  itemloc <- mirt::extract.mirt(mod_g, "itemloc")
  which.items <- 1L:ncol(dat)
  ret2 <- vector("list", ncol(dat))
  names(ret2) <- mirt::extract.mirt(mod_g, "itemnames")
  for (i in seq_len(length(which.items))) {
    pick <- itemloc[which.items[i]]:(itemloc[which.items[i] +
                                               1L] - 1L)
    O <- Etable[, pick]
    P_o <- O/rowSums(O)
    item <- mirt::extract.item(calib, which.items[i])
    P_e <- mirt::probtrace(item, Theta)
    ret2[[i]] <- if (probfun) {
      sqrt(colSums((P_o - P_e)^2 * f_theta))
    }
    else {
      S <- 1L:ncol(P_o) - 1L
      c(`S(theta)` = sqrt(sum((colSums(S * t(P_o -
                                               P_e)))^2 * f_theta)))
    }
  }
  nms <- lapply(ret2, names)
  unms <- unique(do.call(c, nms))
  items <- matrix(NA, length(ret2), length(unms))
  rownames(items) <- names(ret2)
  colnames(items) <- unms
  for (i in seq_len(nrow(items))) items[i, nms[[i]]] <- ret2[[i]]
  if (flag > 0)
    items[items < flag] <- NA
  items[is.nan(items)] <- NA
  items <- as.data.frame(items)
  items <- mirt:::as.mirt_df(items)
  items
}

