# iBiblia (Português)
iBiblia é uma ferramenta que visa simplificar o trabalho de se adicionar marcações de números de Strong e morfologia a um módulo de Bíblia do [theWord](http://theword.net/). Esta ferramenta vai poupá-lo do que seria um tedioso trabalho de digitação de caracteres especiais e números sem sentido imediato, permitindo-lhe focar-se em conceitos mais inteligíveis. O conceito é simples: tudo o que você precisa fazer é sinalizar ao iBiblia que palavras de uma tradução bíblica traduzem quais palavras do texto original. De posse desta informação, o iBiblia será capaz de gerar um módulo de Bíblia do texto traduzido com todas as marcações de números de Strong e morfologia do texto original, além de outros recursos. Para mais detalhes, visite a nossa [Wiki](https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%C3%A7%C3%A3o-(Portugu%C3%AAs)).

# iBiblia (English)
iBiblia helps you simplify the work of adding Strong's and morphology tags to a [theWord](http://theword.net/) bible module. Its goal is to save you a great amount of time from having to manually type the special characters and/or number tags; thus, letting you focus on your more important tasks. The concept is simple. All you have to do is tell iBiblia which words from one bible text (the source) translate to which words on the other (the destination). With this information iBiblia will be able to generate a bible module with all the Strong's and morphology tags coming from the original source module.
For more detail check the [Wiki](https://github.com/rubiot/ibiblia/wiki/User-manual-(English)).

## LLM Model Catalog
The supported LLM model list is now loaded from `iBiblia.models.json`, located alongside the executable. Users can edit this file to add new models for the currently supported providers (`OpenAI`, `DeepSeek`, `Google`) without waiting for an application update.

Each entry uses this shape:

```json
{
	"name": "gpt-4.1-mini",
	"provider": "OpenAI",
	"supports_struct_output": true
}
```

`supports_struct_output` controls whether iBiblia requests JSON-structured responses from providers that support it. If the file is missing or invalid, the application falls back to its built-in default catalog.

This file does not add support for new providers by itself. New providers still require code changes.
