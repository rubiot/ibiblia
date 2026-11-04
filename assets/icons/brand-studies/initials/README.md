# iBiblia · Iniciais entre alfabetos

Estudos 11–14: aproximações visuais de `iB` com formas inspiradas em vav (ו), yod (י) e beta minúsculo (β). A composição é uma interpretação gráfica das iniciais, não sua transliteração. O ponto acrescentado à proposta 12 é uma adaptação da marca.

| Estudo | Direção |
| --- | --- |
| 11 · Vav e beta | Duas letras lado a lado, com um beta tipográfico. |
| 12 · Inicial pontuada | Um ponto reforça a leitura de iB. |
| 13 · Vav e beta · trama | As próprias hastes se entrelaçam. |
| 14 · Yod e beta · ponto | O yod compacto acompanha um beta gestual. |

Abra `index.html` para comparar as versões coloridas e monocromáticas em diferentes tamanhos e superfícies. `preview/contact-sheet.png` reúne as quatro propostas e simulações na barra de título.

Os SVGs estão em `source/svg/`. Os PNGs transparentes estão em `png/`, nas resoluções 16, 20, 24, 32, 40, 48, 64, 128 e 256. Os contornos incorporados dispensam fontes instaladas para a exibição. O beta tipográfico de 11 e 12 foi extraído de Segoe UI Bold; os demais elementos são desenhos vetoriais próprios.

Com Node.js e `sharp` disponíveis, regenere com:

```powershell
node assets/icons/brand-studies/initials/generate.cjs
```

Se necessário, indique `--sharp-module <caminho>`. O gerador registra em `validation.json` a conferência dos 36 PNGs e verifica que os arquivos da marca ativa não mudaram durante a geração. A extração tipográfica original pode ser reproduzida no Windows por `scripts/extract-glyphs.ps1`.

Estes estudos não são recursos carregados pelo aplicativo. Nenhuma marca foi aplicada ao executável.
