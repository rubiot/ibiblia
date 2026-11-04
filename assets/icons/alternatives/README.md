# Famílias adicionais de ícones do iBiblia

Este estudo acrescenta três famílias completas ao catálogo existente. Cada
proposta contém os mesmos 68 símbolos e preserva o significado dos comandos.
As três famílias também estão disponíveis no aplicativo, junto com a família
original Textos conectados. A escolha altera os ícones de comandos, preservando
as cores da interface e os estados personalizados dos projetos.

**A marca do aplicativo é única.** A barra de título, a janela Sobre e o ícone
do Windows usam sempre o livro original com o elo dourado. As variantes de
`app-ibiblia` mostradas nesta galeria são estudos históricos; não entram nos
recursos das famílias selecionáveis. Cada família oferece 67 ícones de comandos.

Abra [a comparação interativa](index.html) para comparar as quatro famílias,
procurar um comando e alternar entre tamanhos de 16 a 64 pixels. A opção
"Comandos principais" mostra uma seleção inicial de 12 símbolos. Desative-a
para ver o catálogo completo. A busca também percorre todos os comandos.

| Família | Direção | Prancha completa |
| --- | --- | --- |
| Atual: Textos conectados | Silhuetas coloridas, páginas claras e volume discreto. | [PNG](../preview/contact-sheet.png) |
| Clareza | Contornos leves, espaços abertos e acentos de cor. | [PNG](clareza/preview/contact-sheet.png) |
| Mosaico | Formas geométricas, cores sólidas e recortes amplos. | [PNG](mosaico/preview/contact-sheet.png) |
| Fólio | Papel ilustrado, contornos de tinta e cores quentes. | [PNG](folio/preview/contact-sheet.png) |

A [comparação resumida](preview/comparison.png) coloca as quatro famílias lado
a lado nos mesmos 12 comandos. As pranchas completas usam a ordem e as células
da prancha original, com cada símbolo em 16, 24 e 48 pixels.

## Arquivos de cada família

- `source/svg/`: 68 desenhos vetoriais originais, com área de 32 × 32 unidades.
- `png/16/`, `20/`, `24/`, `32/`, `40/`, `48/` e `64/`: PNGs transparentes.
- `preview/contact-sheet.svg` e `.png`: prancha completa.
- `manifest.json`: significado, paleta e localização de cada símbolo.
- `validation.json`: contagem, resolução, transparência e verificação das bordas.

Os desenhos usam primitivas vetoriais, sem imagens, fontes ou serviços externos.
Os estados de processamento de IA pertencem ao fluxo de IA. Não são atribuídos
significados fixos aos números dos estados personalizados dos projetos.

## Regeneração

O gerador precisa de Node.js e do módulo `sharp`:

```powershell
node assets/icons/alternatives/generate.cjs --sharp-module "caminho/para/node_modules/sharp"
```

Use `--family clareza`, `--family mosaico` ou `--family folio` para regenerar uma
única proposta. Sem essa opção, o gerador também atualiza a comparação e a
galeria. As definições editáveis ficam em `designs/*.cjs`; o catálogo simbólico
ativo é lido apenas como referência.

Para incorporar os PNGs atualizados ao executável, compile os recursos:

```powershell
node assets/icons/scripts/build-icon-families.cjs --windres C:/Apps/lazarus/fpc/3.2.2/bin/x86_64-win64/windres.exe
```

O empacotador valida os 1.407 PNGs necessários (3 famílias × 67 comandos ×
7 tamanhos), exclui a marca e produz `../generated/ui_icon_families.rc` e
`.res`. A compilação normal do iBiblia usa esse RES sem depender de arquivos
soltos, Node.js ou sharp. O ICO e os recursos da família original permanecem
independentes.

A visualização de ícones desabilitados na galeria é uma simulação gráfica.
A validação da aplicação deve incluir os controles nativos e suas escalas de
100%, 150% e 200%.
