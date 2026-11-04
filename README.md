# iBiblia

[Português](#português) · [English](#english)

## Português

O iBiblia prepara módulos bíblicos anotados para o [theWord](https://www.theword.net/). Associe sintagmas do texto origem aos do texto destino para transferir números de Strong e morfologia, produzir interlineares e gerar concordâncias. Projetos de tradução permitem começar com o destino vazio e escrevê-lo no próprio aplicativo.

A versão **1.1.0 para Windows 10/11 x64** inclui editor visual de versículos e notas, memória de tradução com ocorrências, revisão por estados, Salvar como, integração com theWord 7 e fluxos opcionais de tradução/associação por IA. A IA é **experimental**; exige internet e uma chave de API própria de um provedor suportado.

- [Baixar a versão publicada](https://github.com/rubiot/ibiblia/releases)
- [Instalação e atualização](https://github.com/rubiot/ibiblia/wiki/Instala%C3%A7%C3%A3o-e-atualiza%C3%A7%C3%A3o-(Portugu%C3%AAs))
- [Manual de utilização](https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%C3%A7%C3%A3o-(Portugu%C3%AAs))
- [Novidades da versão 1.1.0 (English)](docs/releases/1.1.0.md)

Extraia o ZIP inteiro para uma pasta em que seu usuário possa gravar e execute `iBiblia.exe`. Mantenha todos os arquivos fornecidos juntos. Antes de atualizar, salve e feche o projeto, preserve um backup do `.bib` e instale a nova versão em outra pasta. O pacote contém [instruções portáteis](docs/README-portable.md). Projetos e preferências pessoais não fazem parte do pacote.

## English

iBiblia prepares annotated Bible modules for [theWord](https://www.theword.net/). Associate source and destination Syntagms to transfer Strong's numbers and morphology, produce interlinears and generate concordances. Translation projects let you start with an empty destination and write it inside the application.

**Version 1.1.0 for Windows 10/11 x64** adds visual verse/note editing, translation-memory occurrences, status review, Save As, theWord 7 integration and optional AI translation/Association workflows. AI is **experimental** and requires internet access and your own supported-provider API key.

- [Download the published release](https://github.com/rubiot/ibiblia/releases)
- [Installation and updating](https://github.com/rubiot/ibiblia/wiki/Installation-and-updating-(English))
- [User manual](https://github.com/rubiot/ibiblia/wiki/User-manual-(English))
- [What's new in 1.1.0](docs/releases/1.1.0.md) · [Changelog](CHANGELOG.md)

Extract the complete ZIP into a folder your user can write to and run `iBiblia.exe`. Keep all supplied files together. Before updating, save and close your project, preserve a `.bib` backup and extract the new version into another folder. The package includes [portable instructions](docs/README-portable.md). Personal projects and settings are not included.

## Formats and model catalog

Bible imports use unlocked theWord `.ont`, `.nt` and `.ot` files, preferably UTF-8. Encrypted modules are unsupported. Dictionary imports require unencrypted, uncompressed `.dct.twm` modules with RTF content. Projects remain `.bib` files. See the manuals for scope, import rules and export options.

`iBiblia.models.json`, beside the executable, is the editable model catalog for the implemented OpenAI, DeepSeek and Google providers. For example:

```json
{
  "name": "gpt-4.1-mini",
  "provider": "OpenAI",
  "supports_struct_output": true
}
```

`supports_struct_output` controls requests for structured JSON responses when supported. A missing or invalid catalog falls back to built-in defaults. Adding a model does not implement a new provider. API keys are configured in the application and stored for the current Windows user; do not put keys in this catalog.

## Development and support

This is a Lazarus/Free Pascal desktop application. [Release preparation](docs/releasing.md) describes the clean-source build, dependency lock, tests, portable packaging and publication checks. Repository agent guidance is in [AGENTS.md](AGENTS.md); domain terminology is in [CONTEXT.md](CONTEXT.md).

Report reproducible problems through [GitHub Issues](https://github.com/rubiot/ibiblia/issues), including version, steps and a small non-private example. Omit API keys. The portable package includes third-party notices and licenses for its bundled components.
