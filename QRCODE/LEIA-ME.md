# Análise complementar ao pôster da 70ª RBras

## Começar

1. Descompacte este ZIP inteiro em uma pasta.
2. Abra `Analise_Sobrevivencia.Rproj` no RStudio.
3. Execute `source("Preparar_e_Renderizar.R")` no console. Esse script instala somente os pacotes ausentes e gera `index.html`. É necessário acesso à internet para instalar pacotes.
4. Para edições posteriores, abra `Analise_Criterios_Sobrevivencia.Rmd` e clique em **Knit → Knit to HTML**. Pelo Knit, a saída usa o mesmo nome do Rmd. O script de preparação usa `index.html` por conveniência para hospedagem.

O arquivo Rmd sozinho depende dos oito CSVs na pasta `dados/`. Prefira baixar o ZIP completo. O relatório não precisa de LaTeX, do RDS ou dos PNGs antigos: ele redesenha os gráficos diretamente dos CSVs.

## Conteúdo

- Resultados dos tamanhos amostrais 50, 100, 250 e 500 em abas, cada uma com seis distribuições e seis critérios.
- Comparação entre tamanhos amostrais com a censura fixada.
- Evolução do acerto por critério e censura para uma distribuição configurável.
- Matrizes de seleção e principais escolhas fora da diagonal.
- Conferência entre taxas e frequências, denominadores e erros padrão de Monte Carlo.
- Tabelas completas com busca, filtros e paginação.
- Espaço para novas análises, metadados científicos e referências.
- Instruções de publicação e código para gerar um QR code depois de definir a URL.

## Alterar recortes

No cabeçalho YAML do Rmd, altere:

```yaml
params:
  diretorio_dados: "dados"
  censura_comparacao: 30
  distribuicao_foco: "Generalized gamma"
  criterio_matriz: "AIC"
  censura_matriz: 30
  exigir_matrizes: false
```

Use os nomes originais de distribuição: `Exponential`, `Lognormal`, `Loglogistic`, `Gompertz`, `Weibull`, `Generalized gamma`. Critérios: `AIC`, `AIC SURV`, `BIC Tradicional`, `BIC Volinsky`, `KIC`, `KICc`.

## Acrescentar tamanhos amostrais

Copie um novo `Resultados_Simulacao_n1000.csv` para `dados/`, mantendo as colunas originais. Se disponível, inclua também `Matriz_Confusao_n1000.csv`. Compile novamente. Os novos tamanhos são detectados automaticamente, sem editar as abas.

As probabilidades dos arquivos originais são percentuais de 0 a 100, com ponto decimal. O leitor aceita separador vírgula ou ponto e vírgula, inclusive a matriz n250 fornecida. Não misture o resumo de uma execução com a matriz de outra. Alterações de distribuição, covariáveis, geradores ou métodos devem ser documentadas.

Se acrescentar uma sétima família, ela receberá o nome original. Para incluir mais de seis critérios, amplie as cores e os tipos de linha no bloco setup, como indicado no código. Se não existir uma matriz, o resumo ainda pode ser visualizado, mas seu erro de Monte Carlo não será calculado. Com `exigir_matrizes: true`, a compilação para quando alguma matriz estiver ausente.

## Publicar e usar no pôster

Revise autores, afiliações, referências, conteúdo e os detalhes metodológicos pendentes. Clique em **Publish** na visualização HTML do RStudio e escolha **RPubs**. A publicação exige sua conta no serviço e será pública. Consulte https://rpubs.com/about/getting-started . Este projeto não publica nada automaticamente.

Depois de publicar, copie a URL definitiva. O bloco `gerar-qrcode` do Rmd está desativado para não gerar um código com um endereço fictício. Preencha `url_relatorio`, instale o pacote `qrcode` e execute o bloco para gerar `qrcode_analise.pdf`. No LaTeX do pôster, substitua o quadro tracejado por:

```latex
\node[anchor=north west,inner sep=0pt] at (1928,625) {
  \includegraphics[width=330bp,height=330bp,keepaspectratio]{qrcode_analise.pdf}
};
```

Apague também o texto provisório que estava dentro do quadro, mantendo a legenda abaixo. Teste o endereço em outro navegador e leia o QR code pelo celular antes da impressão. O caminho de um arquivo no computador não é uma URL pública. Ao atualizar os resultados, atualize a publicação mantendo o endereço utilizado no pôster.

## Limites da análise

Os dados são agregados. Não permitem recuperar o processo gerador, a calibração efetiva da censura, as sementes, as falhas de ajuste, as fórmulas dos critérios nem a associação entre escolhas de critérios na mesma repetição. O relatório não reexecuta a simulação nem realiza testes pareados de superioridade.

A nomenclatura BIC_SURV para BIC Volinsky segue o resumo submetido e precisa ser conferida no código original. A leitura usa os denominadores das matrizes; 10.000 seleções registradas não provam ausência de falhas ou exclusões anteriores.

Documentação de saída HTML: https://rmarkdown.rstudio.com/docs/reference/html_document.html . Documentação de QR code: https://search.r-project.org/CRAN/refmans/qrcode/html/plot.qr_code.html .
