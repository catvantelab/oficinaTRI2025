#' @title Testar pacote
#'
#' @name teste
#'
#' @description Testa se o pacote foi instalado corretamente,
#' bem como as dependências dos pacotes.
#'
#' @return Caso o teste não tenha identificado erro, uma
#' mensagem será emitida no console e o livro do curso será
#' aberto no navegador.
#'
#' @author Alexandre Jaloto
#'
#' @examples
#'
#' teste()
#'
#' @export

teste <- function() {

  nzchar(find.package('mirt'))
  nzchar(find.package('latticeExtra'))
  nzchar(find.package('utils'))

  data(banco2pl.t1, package = 'oficinaTRI2025')
  fit.t1 <- mirt::mirt(data = banco2pl.t1, model = 1, itemtype = '2PL', TOL = 0.1)

  data.frame(latticeExtra::ancestry)

  escore.t1 <- data.frame(mirt::fscores(object = fit.t1, full.scores.SE = TRUE))

  info_t1 <- mirt::testinfo(fit.t1, seq(-3, 3, .01))

  grafico_t1_info <- ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x = escore.t1$F1)) +
    ggplot2::labs(title='Densidade e informação (T1)', x= "escore", y = "densidade") +
    ggplot2::geom_line(ggplot2::aes(x = seq(-3, 3, .01), y = info_t1 / 45), linetype = 2) +
    ggplot2::geom_vline(xintercept = c(-1.5, 1.5), linetype = 2, linewidth = .3) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                       sec.axis = ggplot2::sec_axis(~ . * 45, name = 'informação'))

  erro_t1 <- 1/sqrt(info_t1 + 1)

  grafico_t1_erro <- ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x = escore.t1$F1)) +
    ggplot2::labs(title='Densidade e erro (T1)', x= "escore", y = "densidade") +
    ggplot2::geom_line(ggplot2::aes(x = seq(-3, 3, .01), y = erro_t1 / .8), linetype = 2) +
    ggplot2::geom_vline(xintercept = c(-1.5, 1.5), linetype = 2, linewidth = .3) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                       sec.axis = ggplot2::sec_axis(~ . * .8, name = 'erro padrão'))

  cowplot::plot_grid(grafico_t1_info, grafico_t1_erro,
            labels = c('A', 'B'),
            ncol = 2)

  if(exists('fit.t1'))
  {
    abrir_livro()
    print('Os pacotes foram instalados com sucesso')
  }
}
