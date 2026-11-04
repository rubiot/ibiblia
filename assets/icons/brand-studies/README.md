# Novas propostas para a marca do iBiblia

Seis propostas exploram duas ideias: pergaminhos e a ligação entre alfabetos.
São estudos para escolher uma marca única; não alteram o ícone instalado nem
a seleção das quatro famílias de comandos.

Abra a [galeria](index.html) para mudar o tamanho da visualização e a superfície,
ou a [prancha completa](preview/contact-sheet.png) para comparar todas as opções.

| Nº | Direção | Ideia |
| --- | --- | --- |
| 01 | Pergaminho e elo | Um texto antigo aberto e conectado. |
| 02 | Pergaminhos paralelos | Dois textos ligados lado a lado. |
| 03 | Faixa de leitura | Um rolo horizontal com a inicial i. |
| 04 | Alfa e A | A letra grega α ligada à latina A. |
| 05 | Mem e M | A letra hebraica מ ligada à latina M. |
| 06 | Pergaminho e alfabetos | O suporte antigo combinado com a ligação entre letras. |

Os vetores ficam em `source/svg/`; os PNGs transparentes em `png/`, nas
resoluções 16, 20, 24, 32, 40, 48, 64, 128 e 256. A prancha mostra os desenhos
ampliados e em 16, 24, 32 e 48 pixels, acompanhados de uma simulação simples
da barra de título.

As letras são contornos vetoriais: os SVGs não dependem de fontes instaladas
nem da ordem de escrita RTL do sistema. Alfa, A, mem e M usam contornos da
Segoe UI Bold local; os suportes, elos e composições são desenhos originais.

Para regenerar, usando Node.js e Sharp:

```powershell
node assets/icons/brand-studies/generate.cjs --sharp-module "caminho/para/node_modules/sharp"
```

O gerador grava somente nesta pasta e verifica dimensões, transparência e
bordas dos PNGs. `validation.json` registra a execução. A referência da marca
atual é somente lida.
