# 3. fazer o commit
usethis::use_git('criação')
3

# 2. ignorar os arquivos e pastas
usethis::use_git_ignore('rascunho')

# indicar os pacotes que serão importados na instalação -------------------

usethis::use_package('ggplot2')

# renderizar --------

quarto::quarto_render(input = 'quarto')

# alterar o nome da pasta para o github pages reconhecer

# Caminho da pasta original
pasta_original <- "quarto/_book"

# Caminho do novo diretório
novo_diretorio <- getwd()

# Copia a pasta e seu conteúdo
file.copy(pasta_original, novo_diretorio, recursive = TRUE)

# Remove a pasta original (após a cópia bem-sucedida)
# unlink(pasta_original, recursive = TRUE)

# Remove a pasta docs
file.remove(list.files("docs", full.names = TRUE, recursive = TRUE))
unlink('docs', recursive = TRUE)

# Renomeia a pasta
file.rename('_book', 'docs')

# copiar a apresentação
file.copy('apresentacao/ampliando_regua.pdf', 'docs', recursive = TRUE)
unlink('apresentacao/ampliando_regua.pdf', recursive = TRUE)


# funções gerais ----------------------------------------------------------

# carregar o pacote
devtools::load_all()

# atualizar documentação
devtools::document()

# criar manual
devtools::build_manual()

# para instalar pacote privado
devtools::install_github('usuario/pacote', auth_token = 'chave')

