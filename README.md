# Projeto de Linguagens de Programação - Registros
Feito por:
  -  BRUNO FRANCISCO NECKEL - 2221101035
  -  HENRIQUE EDUARDO SIMONATO - 2221101008
  -  WELLINTON MATHEUS GONÇALVES BAO - 2221101045

Este projeto é um interpretador simples de **expressões com registros** (`record { ... }`) usando **Haskell** e **Happy** para parsing.

## Funcionalidades

- Criação de registros com campos nomeados.
- Acesso aos valores de campos (`.campo`).
- Tipos básicos: `Int`, `Bool`.

## Como executar

1. Compile executar o happy para gerar o Parser.hs:

```bash
happy Parser.hs
```
2. Executar o GHCI:

```bash
ghc Main.hs
```
4. Carregar e executar os exemplos:
E para executar os exemplos, so copiar os codigos no exemplos.txt.
 
