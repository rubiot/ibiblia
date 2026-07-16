# Pilot Icon Redesign Set

Lote piloto selecionado a partir do inventario validado. O objetivo e redesenhar
primeiro os icones que mais definem a identidade visual do iBiblia: aplicativo,
toolbar principal, associacao de textos e status da arvore.

## Criterios Atendidos

- Toolbar principal: inclui todos os 11 icones hoje visiveis em `ToolBar1`.
- Status da arvore: inclui os 4 status padrao do fluxo de associacao.
- Associacao de textos: inclui `association-auto`, `association-clear` e
  `association-rollback`.
- Icone principal do aplicativo: inclui `app-ibiblia`.
- Identidade visual: prioriza Biblia/texto em duas colunas, ligacoes entre
  sintagmas, revisao e progresso.

## Lote Piloto

### `app-ibiblia`

- `symbol`: `app-ibiblia`
- `pascalAction`: none
- `legacyImageIndex`: none
- Contexto de uso: icone principal do aplicativo, Windows resource, janela,
  executavel, associacao de arquivos `.bib`.
- Tamanho necessario: ICO com 16, 32, 48, 64, 128 e 256; PNG fonte em 256.
- Descricao visual recomendada: Biblia aberta ou modulo de texto em duas
  colunas, com um elo discreto conectando as colunas. O desenho deve comunicar
  "texto biblico associado" antes de comunicar apenas "livro".
- Observacoes para legibilidade em 16px: reduzir para silhueta de livro aberto
  com uma unica ponte/linha central; evitar detalhes de paginas, versiculos ou
  texto microscopico.

### `project-open`

- `symbol`: `project-open`
- `pascalAction`: `ActionAbrirProjeto`
- `legacyImageIndex`: 0
- Contexto de uso: toolbar principal, Project menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: Biblia/documento aberto com seta entrando ou
  canto levantado, sugerindo abrir projeto existente.
- Observacoes para legibilidade em 16px: manter apenas contorno de documento e
  seta grossa; nao usar pasta como metafora principal.

### `project-close`

- `symbol`: `project-close`
- `pascalAction`: `ActionFecharProjeto`
- `legacyImageIndex`: 1
- Contexto de uso: toolbar principal, Project menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: documento/projeto com um `x` pequeno no canto,
  indicando fechamento do projeto atual sem sugerir apagar arquivo.
- Observacoes para legibilidade em 16px: o `x` deve ser o acento dominante; o
  documento pode virar uma forma simples com pouca textura.

### `project-save`

- `symbol`: `project-save`
- `pascalAction`: `ActionSalvarProjeto`
- `legacyImageIndex`: 2
- Contexto de uso: toolbar principal, Project menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: documento/modulo com marca de confirmacao ou
  disco discreto, em linguagem moderna. Deve comunicar persistencia do projeto.
- Observacoes para legibilidade em 16px: usar check claro e area de documento
  simples; evitar detalhes internos de disquete se ficarem datados ou confusos.

### `project-save-as`

- `symbol`: `project-save-as`
- `pascalAction`: `ActionSalvarProjetoComo`
- `legacyImageIndex`: 3
- Contexto de uso: toolbar principal.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: documento duplicado com lapis ou pequena seta
  lateral, indicando salvar com outro nome.
- Observacoes para legibilidade em 16px: usar apenas dois planos de documento
  e um acento de lapis/seta; nao combinar muitos simbolos.

### `project-new`

- `symbol`: `project-new`
- `pascalAction`: `ActionNovoProjeto`
- `legacyImageIndex`: 6
- Contexto de uso: toolbar principal, Project menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: documento/projeto novo com sinal de `+`, ou
  Biblia em branco com `+`. Deve parecer inicio de fluxo, nao arquivo generico.
- Observacoes para legibilidade em 16px: o `+` precisa ter espessura alta e
  ficar separado do contorno do documento.

### `association-auto`

- `symbol`: `association-auto`
- `pascalAction`: `ActionSugerirAssociacao`
- `legacyImageIndex`: 4
- Contexto de uso: toolbar principal, Edit menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: duas pequenas colunas/blocos de texto com uma
  ligacao central e um brilho/spark discreto. Deve comunicar sugestao automatica
  de associacao entre sintagmas.
- Observacoes para legibilidade em 16px: representar as colunas como dois
  blocos curtos e a associacao como uma linha unica; o brilho deve virar um
  ponto/estrela simples, nao uma varinha detalhada.

### `association-clear`

- `symbol`: `association-clear`
- `pascalAction`: `ActionLimparAssociacoes`
- `legacyImageIndex`: 5
- Contexto de uso: toolbar principal, Edit menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: elo quebrado entre dois blocos de texto, com
  borracha ou acento de limpeza. Deve indicar remover associacoes da selecao ou
  do verso atual.
- Observacoes para legibilidade em 16px: priorizar elo quebrado; a borracha
  pode ser omitida no 16px se poluir o icone.

### `association-rollback`

- `symbol`: `association-rollback`
- `pascalAction`: `ActionReverterAssociacoes`
- `legacyImageIndex`: 9
- Contexto de uso: toolbar principal, Edit menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: associacao/elo com seta circular voltando para
  tras. Deve substituir a metafora atual pouco precisa de arquivo aberto.
- Observacoes para legibilidade em 16px: usar uma seta circular forte e uma
  indicacao minima de elo; evitar duas setas ou formas que parecam sincronizar.

### `verse-previous`

- `symbol`: `verse-previous`
- `pascalAction`: `ActionVersoAnterior`
- `legacyImageIndex`: 7
- Contexto de uso: toolbar principal, Navigation menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: seta para esquerda com pequena linha de texto
  ou marcador de versiculo, mantendo a metafora de navegacao no texto biblico.
- Observacoes para legibilidade em 16px: seta simples e grossa; qualquer linha
  de texto deve ser secundaria e nao competir com a direcao.

### `verse-next`

- `symbol`: `verse-next`
- `pascalAction`: `ActionVersoSeguinte`
- `legacyImageIndex`: 8
- Contexto de uso: toolbar principal, Navigation menu.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: seta para direita com pequena linha de texto ou
  marcador de versiculo, par visual direto de `verse-previous`.
- Observacoes para legibilidade em 16px: espelhar `verse-previous` exatamente;
  manter area livre suficiente ao redor da ponta da seta.

### `app-exit`

- `symbol`: `app-exit`
- `pascalAction`: `ActionSair`
- `legacyImageIndex`: 10
- Contexto de uso: toolbar principal via `ToolButtonExit.ImageIndex = 10`,
  Project menu via `ActionSair`.
- Tamanho necessario: 16, 24 e 32.
- Descricao visual recomendada: porta/contorno de janela com seta saindo, em
  tom neutro. Deve comunicar sair do app, nao fechar projeto.
- Observacoes para legibilidade em 16px: usar uma unica seta de saida; evitar
  `x`, pois `project-close` ja usa essa metafora.

### `status-not-associated`

- `symbol`: `status-not-associated`
- `pascalAction`: none
- `legacyImageIndex`: 0 no canal runtime `FStatusImages`
- Contexto de uso: arvore lateral principal, menu de contexto da arvore,
  status padrao de projeto de associacao.
- Tamanho necessario: 16.
- Descricao visual recomendada: circulo vazio ou elo ausente com acento
  vermelho. Deve representar verso ainda sem associacoes.
- Observacoes para legibilidade em 16px: nao depender apenas da cor vermelha;
  usar forma aberta/vazia para distinguir de erro.

### `status-associating`

- `symbol`: `status-associating`
- `pascalAction`: none
- `legacyImageIndex`: 1 no canal runtime `FStatusImages`
- Contexto de uso: arvore lateral principal, menu de contexto da arvore,
  status padrao de projeto de associacao.
- Tamanho necessario: 16.
- Descricao visual recomendada: circulo parcial, pequeno elo em construcao ou
  indicador de progresso em amarelo/ambar.
- Observacoes para legibilidade em 16px: usar uma forma incompleta ou meia-lua;
  evitar spinner detalhado demais.

### `status-needs-review`

- `symbol`: `status-needs-review`
- `pascalAction`: none
- `legacyImageIndex`: 2 no canal runtime `FStatusImages`
- Contexto de uso: arvore lateral principal, menu de contexto da arvore,
  status padrao de projeto de associacao.
- Tamanho necessario: 16.
- Descricao visual recomendada: losango/alerta com ponto central ou olho de
  revisao em laranja. Deve indicar que a associacao existe mas requer revisao.
- Observacoes para legibilidade em 16px: um losango com ponto funciona melhor
  que um triangulo complexo; manter borda forte.

### `status-associated`

- `symbol`: `status-associated`
- `pascalAction`: none
- `legacyImageIndex`: 3 no canal runtime `FStatusImages`
- Contexto de uso: arvore lateral principal, menu de contexto da arvore,
  status padrao de projeto de associacao.
- Tamanho necessario: 16.
- Descricao visual recomendada: check verde combinado a pequeno elo ou bloco de
  texto. Deve comunicar associacao concluida.
- Observacoes para legibilidade em 16px: check deve ser a forma primaria; elo
  pode virar apenas uma curva curta ou ser omitido se ficar apertado.

## Fora Do Lote Piloto

Os seguintes icones ficam para a proxima fase, apesar de importantes:

- `project-settings`
- `project-export`
- `patch-create`
- `patch-apply`
- `llm-options`
- `database-vacuum`
- `ai-pending`
- `ai-processing`
- `ai-success`
- `ai-error`

Eles dependem menos da primeira impressao visual da janela principal ou podem
ser redesenhados depois que a linguagem visual do app estiver definida pelos
icones piloto.
