# 50 Questões de Programação Funcional

Esta é a minha resolução dos exercicios propostos na cadeira de Programação Funcional, os exercicios não são nada de muito complexo, contudo penso que sejam importantes para fazer uma boa introdução à cadeira. Além disso o foco deste repositório não é a minha resolução, mas sim a palete de testes aqui existente

## Testes

Incluí alguns testes que abrangem todos os exercicios, assim podemos testar tudo de uma só vez sem termos de perder muito tempo a correr os testes manualmente (btw, a minha resolução não passa em dois dos testes :smiling_face_with_tear:).

## Biblioteca utilizada nos Testes

Caso ainda não tenham instalado o [HUnit](https://hackage.haskell.org/package/HUnit) basta correr o seguinte comando

```bash
$ cabal v2-install --lib HUnit
```
## Correr os Testes

Para testarem as vossas resoluções basta mudar o import do ficheiro .hs

```bash
$ ghci Testes.hs
*Testes> correrTestes
```
