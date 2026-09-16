# Alternância de famílias de ícones — 14/09/2026

O menu **Ferramentas → Preferências do iBiblia → Conjunto de ícones** oferece
Textos conectados, Clareza, Mosaico e Fólio, com prévia de seis comandos.
A escolha é aplicada imediatamente e salva no INI do aplicativo, seção
`opcoes`, chave `ui.icon.family`. Os identificadores persistidos são
`connected`, `clareza`, `mosaico` e `folio`; valor ausente ou desconhecido
usa a família original.

O livro aberto com elo dourado continua sendo a única marca, inclusive em
Sobre, nas janelas e no ICO. Os recursos alternativos contêm apenas os 67
comandos. As listas completas, subconjuntos de IA e botões já abertos são
atualizados sem mudar índices. Nomes, cores e proporções dos estados de
projeto continuam sob controle de cada catálogo.

## Verificação

- Compilação Release pelo comando obrigatório do projeto: sucesso. O único
  aviso na compilação incremental final é o aviso já existente sobre
  `MainFormOnTaskBar`.
- Oito testes de identidade: zero erros/falhas. Cobrem os recursos em quatro
  famílias e sete resoluções, igualdade dos pixels da marca, diferença entre
  comandos, troca em listas existentes e liberação/reutilização das listas.
- Matriz PT/EN × 96/144/192 PPI: 150 capturas, sem falhas ou alertas automáticos
  de limites. Mesmas instâncias de janela principal, propriedades, IA e Sobre
  permanecem abertas durante as trocas.
- Teste adicional iniciado com Fólio: 25 capturas. A ação real de Preferências
  abre o diálogo; a sonda seleciona cada item do combobox e dispara seu evento.
  Confere o valor em memória e no INI, a prévia e a preservação das outras
  preferências. Restauração inicial da família salva confirmada.
- As quatro capturas de Sobre têm o mesmo SHA-256. Os catálogos de idiomas
  anteriores foram preservados byte a byte, com apenas três novas entradas.

Evidências locais desta execução:

- [Testes](../.codex-icon-family-work/test-icon-families-final.log)
- [Release](../.codex-icon-family-work/build-release-final.log)
- [Matriz](../.codex-icon-family-work/matrix/summary.csv)
- [Preferências e persistência](../.codex-icon-family-work/persistence-final/pt-96/captures/report.tsv)

## Reprodução

```powershell
.\tests\Run-VisualProbe.ps1 -IconFamilies
.\tests\Run-VisualProbe.ps1 -IconFamilies -InitialIconFamily folio -Language pt -Dpi 96
```

As capturas usam o Lazarus real com `AutoAdjustLayout` e `GetFormImage/PaintTo`.
A ferramenta de controle interativo do Windows não iniciou nesta sessão:
`windows sandbox failed: helper_unknown_error: apply deny-read ACLs`.
Por isso esta execução não confirma interação por teclado/mouse nem todas as
particularidades de molduras e menus abertos no desktop. Mantêm-se os limites
de captura descritos em [VISUAL_PROBE.md](VISUAL_PROBE.md). Os dados são
sintéticos, com sincronização e chamadas de IA desabilitadas.
## Marca Clareza adotada — 14/09/2026

O desenho `app-ibiblia` do Clareza foi adotado como marca única de todas as
famílias. A fonte canônica é `assets/icons/brand/app-ibiblia.svg`, idêntica à
proposta escolhida. O gerador principal deriva dela os nove PNGs, o ICO e os
recursos embutidos; os 536 arquivos conferidos de ícones de comandos não mudaram.

A janela Sobre recarrega a marca após mudanças de DPI e suaviza sua redução.
Isso evita ampliar a imagem de 48 pixels ao passar para 150% ou 200%.

Validação desta adoção:

- Release compilada pelo comando obrigatório, inclusive após o ajuste de DPI.
- Oito testes de identidade aprovados; marca invariável em quatro famílias e
  nove tamanhos. ICO embutido e ICO em arquivo carregados pelo Lazarus.
- 75 capturas finais em português, em 96/144/192 PPI, sem falhas ou alertas
  automáticos de limites. Inspeção de Sobre confirmou os contornos nítidos nas
  três escalas; as capturas de Sobre são idênticas entre famílias em cada DPI.

Evidências locais em `.codex-visual-work/brand-adoption/`: `build-release-final.log`,
`tests/results.log`, `tests/icon-file-load.log`, `visual-final/summary.csv` e
`visual-final/brand-hashes.json`.
