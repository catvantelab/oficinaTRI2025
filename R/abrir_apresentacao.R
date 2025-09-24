#' @title Abrir apresentação da oficina
#'
#' @name abrir_apresentacao
#'
#' @description Abre a apresentação da oficina
#'
#' @return A apresentação é aberta em formato PDF no navegador
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' abrir_apresentacao()
#'
#' @export

abrir_apresentacao <- function() {

  url <- "https://catvantelab.github.io/oficinaTRI2025/ampliando_regua.pdf"

  utils::browseURL(url)

  message("Material da oficina aberto no navegador!")
}
