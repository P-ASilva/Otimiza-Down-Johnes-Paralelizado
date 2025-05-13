# Otimiza-Down-Jones-Paralelizado

Este projeto possui duas versões: uma em Python como prova de conceito mais acessível e outra em Haskell focada em paradigma funcional.

## Instalação

### Requisitos e Dependências
- **Haskell Stack** (versão recomendada: 2.9.3)
- **Bibliotecas Usadas**:
  - random
  - parallel
  - vector
  - bytestring
- **Adições a Configuração do GHC**
  - threaded 
  - rtsopts
  - with-rtsopts=-N
- **Python 3.8+** (para versão alternativa)
  - Checar requirements.txt para depedências
- **Arquivos de dados**:
  - `data/stock_combinations.txt` (combinações de ações) - Gerado com Script python, equivalente em Haskell seré adicionado depois
  - `data/prices.csv` (histórico de preços) - Extraido em um Script python, cheque a branch alternativa

## Execução

1. Antes de compilar
```bash
cd ./sharpe-calculator
stack setup
```
2. Compile o projeto:
```bash
stack build
```
3. Execute o programa principal:
```bash
stack exec
```
4. Os resultados serão salvos em:
- `results/sharpeRatios.csv` (melhores Sharpe Ratios)
- Logs de progresso no console (Descomentar Código)


### Fluxo do Programa
1. Carrega todas as combinações de 25/30 ações do Dow Jones
2. Para cada combinação:
  - Gera 1000 vetores de pesos aleatórios (Pre-Loaded)
  - Calcula variação percentual dos preços (Pre-Loaded)
  - Calcula retornos e volatilidade            |
  - Computa o Sharpe Ratio para cada carteira  |-> (Paralelisado)
3. Armazena os melhores resultados (e pesos) para a combinação

## Resultados

### Saída Esperada
O programa produzirá:
- Arquivo CSV com estrutura:
  ```
  ComboIndex,SharpeRatio,Weights
  1,1.423,[0.12;0.08;...;0.15]
  ...
  ```
- ComboIndex representa a qual combinação em `./datas/stock_combinations.txt` aquele resultado e pesos pertencem.

<!-- - Relatório no console indicando:
  - Progresso das simulações 
  - Melhor Sharpe Ratio encontrado
  - Tempo de execução -->

### Métricas Calculadas
1. **Retornos Anualizados**: Média dos retornos diários × 252
2. **Volatilidade Anualizada**: Desvio padrão × √252
3. **Sharpe Ratio**: Retorno anualizado / Volatilidade

### Paralelização
O código utiliza estratégias de paralelismo:
- `parMap` para processar combinações concorrentemente
- Avaliação estrita com `rdeepseq`
- Divisão automática de carga de trabalho

## Módulos Principais

### Loader.hs
Responsável por:
- Carregar combinações de ações (`loadCombinations`)
- Ler dados históricos (`loadAllWallets`)
- Salvar resultados (`saveBestSharpe`)

### Simulate.hs
Contém a lógica de:
- Geração de pesos aleatórios (`generateWeights`)
- Cálculo de métricas (`calculateSharpe`)
- Simulação paralela (`computeBestSharpeAndWeights`)

### Main.hs
Orquestra:
- Carregamento de dados
- Execução das simulações
- Gerenciamento de saída