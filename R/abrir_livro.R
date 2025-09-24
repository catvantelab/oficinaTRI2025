#' @title Abrir livro da oficina
#'
#' @name abrir_livro
#'
#' @description Abre o livro que contém os comandos da oficina
#'
#' @return O livro é aberto no navegador
#'
#' @author Alexandre Jaloto
#'
#' @examples
#' abrir_livro()
#'
#' @export

abrir_livro <- function() {

  url <- "https://catvantelab.github.io/oficinaTRI2025/"

  utils::browseURL(url)

  message("Material da oficina aberto no navegador!")
}
