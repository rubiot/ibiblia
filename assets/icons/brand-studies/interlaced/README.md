# iBiblia · Letras entrelaçadas

Quatro estudos adicionais da marca, numerados de 07 a 10 para acompanhar a galeria anterior. Os próprios traços das letras alternam passagens por cima e por baixo. Os intervalos nos cruzamentos são transparentes, inclusive nas versões monocromáticas.

| Estudo | Letras | Composição |
| --- | --- | --- |
| 07 · Alfa em trama | α / A | Alfa curva ao redor do A, com cruzamento na travessa. |
| 08 · Alfa diagonal | α / A | Alfa inclinada atravessa um A vertical. |
| 09 · Mem e M · trama | מ / M | Letras sobrepostas, com diagonais entrelaçadas. |
| 10 · Mem e M · diagonal | מ / M | Mem deslocada acima e à esquerda do M. |

Abra `index.html` para comparar tamanhos e superfícies. A prancha completa está em `preview/contact-sheet.png`. Os SVGs coloridos e monocromáticos estão em `source/svg/`; os PNGs transparentes estão em `png/`, de 16 a 256 pixels. As letras são contornos vetoriais e não dependem de fontes instaladas para sua exibição.

Estes arquivos são propostas para avaliação; não são carregados pelo aplicativo. A marca atual e os quatro conjuntos de ícones permanecem independentes destes estudos.

## Regeneração

Com Node.js e `sharp` disponíveis:

```powershell
node assets/icons/brand-studies/interlaced/generate.cjs
```

É possível indicar a instalação de `sharp` com `--sharp-module <caminho>`. O gerador verifica dimensões, transparência, conteúdo e margens dos 36 PNGs; também confere se os arquivos da marca ativa permanecem inalterados durante a geração. Os resultados são gravados em `validation.json`.
