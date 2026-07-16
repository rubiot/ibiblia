# Inventory Validation

Validacao do inventario em `assets/icons` contra o codigo Lazarus atual.

Arquivos revisados:

- `assets/icons/manifest/icons.json`
- `assets/icons/STYLEGUIDE.md`
- `assets/icons/legacy-map/legacy-image-index.md`
- `formprincipal.lfm`
- `formprincipal.pas`
- `Project.pas`
- `bibletreeview.pas`
- `formaiprocessing.pas`
- `FormProjectProperties.lfm`
- `Verse.pas`
- `chapterview.pas`
- formularios com `TPageControl`, `TTabControl` e `TTabSheet`

## Resultado Geral

- Bloqueadores encontrados: 0.
- O `icons.json` esta bem formado e foi lido com `ConvertFrom-Json`.
- Todos os `TAction` que hoje possuem `ImageIndex` no codigo/LFM foram mapeados.
- Todos os `ImageIndex` ativos de toolbar e arvore/status foram documentados.
- Nao ha `ImageIndex` em abas no codigo atual.

## Checklist Dos Objetivos

| Objetivo | Status | Observacao |
| --- | --- | --- |
| Verificar todos os `TAction` com `ImageIndex` | OK | Todos estao em `formprincipal.lfm` e aparecem no manifesto. |
| Verificar `ImageIndex` em toolbar | OK | Indices 0..10 de `ImageList1` cobertos. |
| Verificar `ImageIndex` em menus | OK com ressalva | Menu principal nao tem `Images`; menu de contexto da arvore usa `ImageIndex := status`. |
| Verificar `ImageIndex` em arvore/status | OK | `FStatusImages` e `TBibleTreeView.StateImages` documentados. |
| Verificar `ImageIndex` em abas | OK | Nao foi encontrado uso de `ImageIndex` em abas. |
| Apontar orfaos/duplicados | OK | Listados abaixo. |
| Apontar acoes sem icone | OK | Listadas abaixo. |
| Conferir coerencia entre os tres arquivos | OK com recomendacoes | O conteudo esta coerente, mas pode ganhar campos para reduzir ambiguidade. |

## Bloqueador

Nenhum bloqueador encontrado.

O manifesto pode ser usado como inventario inicial porque cobre todos os
`TAction.ImageIndex` reais:

| Pascal action | ImageIndex atual | Simbolo no manifesto |
| --- | --- | --- |
| `ActionAbrirProjeto` | 0 | `project-open` |
| `ActionFecharProjeto` | 1 | `project-close` |
| `ActionSalvarProjeto` | 2 | `project-save` |
| `ActionSalvarProjetoComo` | 3 | `project-save-as` |
| `ActionSugerirAssociacao` | 4 | `association-auto` |
| `ActionLimparAssociacoes` | 5 | `association-clear` |
| `ActionNovoProjeto` | 6 | `project-new` |
| `ActionVersoAnterior` | 7 | `verse-previous` |
| `ActionVersoSeguinte` | 8 | `verse-next` |
| `ActionReverterAssociacoes` | 9 | `association-rollback` |
| `ActionSair` | 10 | `app-exit` |

`ToolButtonExit` tambem usa `ImageIndex = 10` diretamente, sem `Action`, e o
inventario documenta a equivalencia semantica com `ActionSair`.

## Recomendacao

### 1. Tornar `legacyImageIndex` menos ambiguo

O campo `legacyImageIndex` esta correto isoladamente, mas e ambiguo quando lido
sem o contexto. O mesmo numero aparece em canais diferentes:

| Indice | Simbolos no `icons.json` |
| --- | --- |
| 0 | `project-open`, `status-not-associated`, `status-awaiting-translation`, `checkbox-unchecked` |
| 1 | `project-close`, `status-associating`, `status-translating`, `checkbox-checked` |
| 2 | `project-save`, `status-needs-review`, `status-ready-for-review`, `checkbox-partial` |
| 3 | `project-save-as`, `status-associated`, `status-pending-issues`, `ai-pending` |
| 4 | `association-auto`, `status-reviewed`, `ai-processing` |
| 5 | `association-clear`, `ai-success` |
| 6 | `project-new`, `ai-error` |

Isso nao e erro de mapeamento: os indices pertencem a listas/canais diferentes
(`ImageList1`, `FStatusImages`, `TBibleTreeView.StateImages`). Ainda assim,
recomenda-se adicionar um campo futuro como `legacyImageList` ou
`legacyChannel`, por exemplo:

```json
{
  "symbol": "project-open",
  "legacyImageList": "TFrmPrincipal.ImageList1",
  "legacyImageIndex": 0
}
```

### 2. Registrar o botao de saida direto no `icons.json`

`app-exit` esta semanticamente correto como `ActionSair`, mas o botao da toolbar
nao usa `ActionSair`; ele usa `ToolButtonExit.ImageIndex = 10` e
`ToolButtonExitClick`.

Recomenda-se adicionar, no futuro, um campo de origem ou controle:

- `pascalAction`: `ActionSair`
- `directControls`: [`ToolButtonExit`]
- `legacyImageIndex`: 10
- `legacyImageList`: `TFrmPrincipal.ImageList1`

### 3. Diferenciar acoes internas de acoes visiveis

Algumas entradas existem como `TAction`, mas nao sao acoes visiveis diretas no
menu/toolbar:

- `ActionQuandoNovoVersiculo`
- `ActionChangeTab`
- `ActionSyncTheWordVerse`
- `ActionMostrarTags`
- `ActionExportarDestinoComStrongs`
- `ActionExportarTextoInterlinear`

O manifesto ja marca `ActionQuandoNovoVersiculo` e `ActionChangeTab` como
internas. Recomenda-se aplicar a mesma clareza nas demais quando forem
puramente atalhos, wrappers ou chamadas programaticas.

### 4. Cobrir menus diretos sem `TAction`

O inventario identifica que o menu principal possui itens diretos com `OnClick`
e sem `TAction`. Alguns ja aparecem parcialmente no manifesto, mas outros ainda
nao possuem simbolo dedicado.

Itens diretos que merecem icone caso os menus passem a exibir imagens:

| Menu item | Contexto | Simbolo sugerido |
| --- | --- | --- |
| `MenuItemAbout` | Help | `help-about` |
| `MenuItemLangEn`, `MenuItemLangPt` | Options > Language | `language-english`, `language-portuguese` |
| `MenuItemMouseHover` | Options > Dictionary popup trigger | `popup-trigger-hover` |
| `MenuItemAltMouseHover` | Options > Dictionary popup trigger | `popup-trigger-alt-hover` |
| `MenuItemCtrlMouseHover` | Options > Dictionary popup trigger | `popup-trigger-ctrl-hover` |
| `MenuItemStrongsCountNone` | Options > Strongs count | `strongs-count-hide` |
| `MenuItemStrongsCountWords` | Options > Strongs count | `strongs-count-words` |
| `MenuItemStrongsCountStrongs` | Options > Strongs count | `strongs-count-tags` |
| `MenuItemStatisticalSuggestion` | Options > Association engine | `association-engine-statistical` |
| `MenuItemAISuggestion` | Options > Association engine | `association-engine-ai` |
| `MenuItem22` | Options | `association-auto-unassociated` |
| `MenuItemAutoSave` | Options | `autosave` |
| `MenuItemSyncTheWord` | Options | `sync-theword-to-ibiblia` |
| `MenuItemSynciBiblia` | Options | `sync-ibiblia-to-theword` |
| `MenuItemStrongNegrito` | Options | `strongs-bold` |
| `MenuItemAlwaysOnTop` | Options | `window-always-on-top` |

O `icons.json` ja tem `file-association-register` e
`file-association-unregister`, que tambem sao menus diretos sem `TAction`.

### 5. Corrigir/clarear o simbolo de sincronizacao

O manifesto tem `sync-theword-verse` ligado a `ActionSyncTheWordVerse`.
No codigo real, os itens visiveis de sincronizacao do menu sao:

- `MenuItemSyncTheWord`: Sync theWord to iBiblia verse
- `MenuItemSynciBiblia`: Sync iBiblia to theWord verse

Recomenda-se trocar a entrada unica por duas entradas visiveis ou documentar
`ActionSyncTheWordVerse` como acao interna/programatica.

### 6. Alinhar tamanhos de abas entre `STYLEGUIDE.md` e `icons.json`

O `STYLEGUIDE.md` recomenda `Tab/panel` com tamanhos 16 e 24. No `icons.json`,
as entradas:

- `tab-source`
- `tab-destination`
- `tab-reference`

estao com `sizes: [16]`.

Isso nao quebra o inventario, porque hoje nao ha icones em abas. Mas, para a
nova identidade, recomenda-se escolher uma regra unica:

- manter abas somente em 16px; ou
- exportar abas em 16px e 24px como o guia ja sugere.

### 7. Documentar constantes obsoletas de AI no legado

`Project.pas` ainda declara:

- `ImgIndexPending = 8`
- `ImgIndexProcessing = 9`

Essas constantes aparecem como comentario/legado de um fluxo antigo e nao foram
encontradas como uso ativo. O fluxo atual de AI usa `TBibleTreeView.StateIndex`
com constantes em `bibletreeview.pas`:

- `ImgIndexAIPending = 3`
- `ImgIndexAIProcessing = 4`
- `ImgIndexAISuccess = 5`
- `ImgIndexAIError = 6`

Recomenda-se registrar as constantes antigas em `legacy-image-index.md` como
obsoletas, ou remove-las em uma etapa futura de limpeza de codigo.

## Melhoria Futura

### 1. ImageIndex orfaos

Os seguintes indices/assets existem mas nao tem uso ativo encontrado:

| Origem | Indices orfaos ou legados | Observacao |
| --- | --- | --- |
| `ImageList1` | 11..23 | Sprites 32x32 embutidos, sem `TAction` ou controle usando esses indices. |
| `ImageList64x64` | 0..6 | Lista 64x64 inteira sem binding atual encontrado. |
| `ImageListTreeView` | 0..9 | Lista 16x16 de design time; e substituida por `FStatusImages` quando um projeto abre. |

O `legacy-image-index.md` documenta esses casos. O `icons.json` atualmente nao
inclui simbolos para spares, o que e aceitavel para um manifesto de identidade
nova. Se o objetivo passar a ser preservacao completa de assets legados,
convem adicionar entradas `legacy-spare-*`.

### 2. Duplicidade direta no indice 10 da toolbar

O indice 10 aparece em dois lugares:

- `ActionSair.ImageIndex = 10`
- `ToolButtonExit.ImageIndex = 10`

A duplicidade e intencional/semantica, nao um erro. Em uma migracao futura,
seria melhor fazer `ToolButtonExit.Action := ActionSair` ou documentar
explicitamente o controle direto no manifesto.

### 3. Inventario de abas sem icone pode ser mais completo

Nao ha `ImageIndex` em abas, mas o inventario de abas em
`legacy-image-index.md` pode listar tambem estes controles dinamicos:

- `FormNewProject.FWizard`
- `FormProjectProperties.FPageControl`
- `FormProjectProperties.FDictionaryTextTabs`
- `formflowexamples.FPageControl`

Isso melhora completude documental, embora nao altere o mapeamento de icones
atuais.

### 4. Popup menus sem iconografia

Os popup menus existem e nao tem `Images` hoje. Se a nova identidade visual for
mais ampla, vale criar simbolos para:

- `TVerse.InitSyntagmPopupMenu`: copiar tags, colar tags, colar Strong,
  colar morfologia.
- `TVerse.InitVersePopupMenu`: direita-para-esquerda, salvar texto em arquivo,
  copiar tokens/JSON/XML em debug.
- `TProject.AttachProjectVersePopupMenu`: usar verso como exemplo, exemplo de
  associacao, exemplo de traducao.
- `TChapterView.InitPopupMenu`: fonte, modo paragrafo, modo interlinear,
  modo intralinear, regras de verso, imprimir capitulo.

Esses itens nao precisam bloquear a primeira troca de icones da toolbar, mas
sao bons candidatos para a segunda fase.

### 5. Metadados de rastreabilidade

O `icons.json` ficaria mais facil de auditar se cada entrada pudesse apontar
para a origem real no codigo:

- `sourceUnit`
- `sourceControl`
- `sourceProperty`
- `legacyImageList`
- `status`: `current`, `proposed`, `internal`, `legacy-spare`

Isso reduziria a necessidade de inferir contexto apenas por `contexts`.

## Consistencia Entre Arquivos

### `icons.json` vs codigo

Consistente para os pontos ativos:

- Todos os `TAction.ImageIndex` atuais aparecem no manifesto.
- `ToolButtonExit.ImageIndex = 10` esta coberto semanticamente por `app-exit`.
- Status de projeto e `TBibleTreeView.StateImages` estao representados.
- Abas aparecem como lacunas/propostas, nao como uso ativo.

Ressalvas:

- `legacyImageIndex` precisa de campo de canal/lista para evitar ambiguidade.
- `sync-theword-verse` deve ser separado dos menu items reais de sync, ou
  marcado como interno.

### `STYLEGUIDE.md` vs `icons.json`

Coerente em direcao visual, paleta e formatos. A unica divergencia pratica e
a regra de tamanho para abas/painel: o guia fala em 16 e 24, enquanto algumas
abas no manifesto pedem apenas 16.

### `legacy-image-index.md` vs codigo

Coerente com os usos ativos encontrados:

- `ImageList1` tem 24 sprites e usa 0..10 na toolbar.
- `ImageList64x64` nao tem binding atual encontrado.
- `ImageListTreeView` e design-time/legado na pratica.
- `FStatusImages` e gerado em runtime com 16x16.
- `TBibleTreeView` usa `StateImages` 0..6.

Melhorias documentais sugeridas:

- adicionar as constantes obsoletas `Project.pas` `ImgIndexPending` e
  `ImgIndexProcessing`;
- listar os controles de abas dinamicos omitidos;
- listar menus diretos sem `TAction` que ainda nao estao no manifesto.

## Conclusao

O inventario esta apto como base inicial para gerar a nova identidade visual.
Nao ha bloqueadores. A principal recomendacao antes de automatizar uma troca de
icones e enriquecer o manifesto com o canal legado (`ImageList1`,
`FStatusImages`, `StateImages`, etc.), pois `legacyImageIndex` sozinho nao e
unico no projeto.
