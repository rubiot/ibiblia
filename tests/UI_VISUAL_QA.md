# QA da identidade visual do iBiblia

Verificação em 10/09/2026, no Windows x64 com o Lazarus/FPC do projeto.

## Resultado

- Compilação Release concluída com código de saída **0**.
- Suíte FPCUnit: **798 testes, 0 erros e 0 falhas**. Evidência local: [test-results-final.log](../.codex-visual-work/test-results-final.log).
- Matriz visual com otimização **O3**, como na Release: português e inglês em **96, 144 e 192 PPI** (100%, 150% e 200%). Cada combinação produziu **49 capturas**, código de saída **0** e **nenhum `REVIEW_BOUNDS` ou `REVIEW_TEXT`**: 294 PNGs ao todo.
- Evidência da matriz: [summary.csv](../.codex-visual-work/capture-20260910-113154/summary.csv). Cada subdiretório `<idioma>-<PPI>` contém `stdout.log`, `stderr.log` e `captures/report.tsv`, junto das imagens. Esses artefatos locais podem ser recriados.

## Reprodução

Na raiz do repositório, em PowerShell:

```powershell
.\tests\Run-VisualProbe.ps1
```

Para verificar somente português a 100%:

```powershell
.\tests\Run-VisualProbe.ps1 -Language pt -Dpi 96
```

O lançador compila uma sonda separada com otimização O3, prepara DLLs e catálogos de idioma e cria um ambiente novo sob `.codex-visual-work`. O ambiente tem INI, projeto bíblico e patch sintéticos próprios. Use `-SkipBuild` somente se a sonda já tiver sido recompilada após a última alteração Pascal, LFM ou de ícones. Consulte [VISUAL_PROBE.md](VISUAL_PROBE.md) para os registros e a operação detalhada.

## Cobertura

- Janela principal e largura reduzida; páginas de criação, propriedades e exportação; preferências, configurações e revisão de IA; exemplos; mesclagem e patches; regras de versículos; cores; estados; dicionário; notas, prévias, progresso e Sobre.
- Auditoria das classes `TForm` e dos diálogos criados dinamicamente não encontrou formulário em uso na aplicação principal fora de `ApplyFormStyle`/`ApplyDialogStyle`. Inclui os diálogos de prompt e remapeamento de estados. O exemplo independente `ONTMemo/main.pas` e os diálogos comuns nativos do Windows não fazem parte dessa lista.
- Os testes de identidade carregam cada ícone incorporado nas sete resoluções de 16 a 64 px, verificam a ligação dos símbolos entre menus e ferramentas e distinguem histórico de navegação bíblica.
- O ICO usa nove imagens DIB BGRA de 32 bits, sem compressão e com máscaras AND, de 16 a 256 px. O leitor nativo `TIcon` carregou tanto o arquivo quanto o recurso incorporado; a suíte também verifica o recurso incorporado.
- A sonda verifica **12 estados personalizados**, suas cores exatas, ordem de exibição, transparência e indicadores proporcionais. Inclui preto e magenta e confirma que cancelar o remapeamento de estados de um patch preserva o projeto sintético.
- Cada combinação da matriz validou a troca **PT → EN → PT nas mesmas instâncias** de propriedades, exportação, aplicação de patch e regras de versículos/interlinear. Os títulos e botões Cancelar/Cancel foram reaplicados corretamente e o atalho de Cancelar nas regras foi preservado. Também verificou o filtro por clique e Espaço, mantendo a seleção por ID do estado e a posição de rolagem. Evidência: [report.tsv](../.codex-visual-work/capture-20260910-113154/pt-96/captures/report.tsv).
- A fonte grega configurada (`Times New Roman`) e a fonte/tamanho do destino (`Georgia`, 22 pt) são preservadas. Os testes também cobrem fontes de editores especializados, cores definidas pelo usuário, conteúdo, edição e formatação de notas. O material sintético inclui grego, português e hebraico RTL.

## Verificação nativa da Release

A execução normal da aplicação, sem instrumentação de diagnóstico e com otimização O3 mantida, confirmou:

- Lista de revisão de estados com cores, marcadores e textos corretos. Uma versão anterior pintava um retângulo com a cor do estado na lista nativa, apesar das capturas corretas. Cálculos explícitos de coordenadas e isolamento do estado do Canvas resolveram o sintoma; a causa exata não foi comprovada.
- Ícones no menu Ferramentas e apresentação dos títulos e rodapés das propriedades do projeto. A aba Textos preservou a fonte grega configurada; o diálogo de exportação apresentou campos, abas e rodapé organizados.
- Diálogo de preferências em português e inglês, com mudança do idioma em tempo real.
- Navegação de foco com Tab e fechamento do diálogo com Esc.
- Na lista de estados, clicar no primeiro estado alterou a contagem para 7956/7957, mantendo destaque azul e foco. Espaço restaurou 7957/7957 sem perder a seleção.
- Botão de próximo versículo levando de Mateus 1:1 a 1:2 e Alt+Esquerda retornando pelo histórico a 1:1.
- Maximizar e restaurar a janela preservou a disposição dos painéis.
- Ctrl+F4 fechou o projeto sintético; os ícones de projeto, associação e navegação ficaram desabilitados, cinza e nítidos, e a interface exibiu o estado sem projeto aberto.

## Limites

A matriz aplica o caminho real de escala do Lazarus (`AutoAdjustLayout`) e captura formulários fora da área visível com `GetFormImage`/`PaintTo`. A matriz, por si só, não equivale a validar todas as resoluções físicas, áreas úteis de monitor, mudanças entre monitores, molduras, menus abertos ou todos os percursos de teclado. As verificações nativas acima complementam essa cobertura, mas não a tornam exaustiva. Algumas legendas gráficas sobre abas temáticas podem não aparecer no `PaintTo`; o conteúdo dos controles e os registros ajudam a distinguir essa limitação de um texto ausente. A análise automática de limites complementa a inspeção visual, sem substituí-la.

**Nenhum serviço externo theWord ou de IA foi acionado pela sonda.** Sincronização, salvamento automático e sugestões automáticas ficam desabilitados no ambiente isolado. Estados de IA são exercitados na interface; não houve chamada LLM nem validação de comunicação com uma instalação externa do theWord. A sonda trabalha com dados sintéticos, sem aplicar alterações aos projetos do usuário.
