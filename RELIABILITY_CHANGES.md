# Correções de confiabilidade

## Patches

Uma lista explícita vazia aplica zero versículos. O parâmetro público omitido
(nil) continua significando todos; internamente o modo de seleção é explícito.
A janela desabilita **Aplicar** sem seleção. A aplicação devolve a quantidade
real de referências encontradas e aplicadas.

## Resultados de IA

Associações exigem um objeto JSON com "data". Pares individuais inválidos ainda
podem ser ignorados quando restam pares úteis, mas uma resposta sem associação
aplicável não pode substituir associações existentes. Traduções vazias não podem
apagar conteúdo, inclusive notas e metadados sem texto visível.

Os campos aceitos só são publicados depois de materializar o resultado. Falhas
restauram o texto e as associações anteriores. Um resultado vazio só é neutro
quando os dois versículos estão realmente vazios.

## Exportação

theWord, MySword e concordância são escritos em arquivo temporário na mesma
pasta do destino. O destino só é substituído depois de concluir e fechar a
gravação. Uma falha mantém o arquivo anterior e remove os temporários.

Os chamadores recebem TExportResult: sucesso, sucesso com avisos ou falha,
quantidade de versículos processados, caminho publicado, relatório e erro.
Falhas recuperáveis por versículo continuam permitindo exportar com avisos.
Se o relatório não puder ser salvo, a janela informa essa falha separadamente
do sucesso da exportação.

## Cancelamento de IA

Cancelar sinaliza o lote sem aguardar HTTP na interface. Itens concluídos antes
do cancelamento permanecem disponíveis para revisão; respostas posteriores,
itens pendentes e tentativas adiadas são descartados como cancelados.

A janela mantém seu timer durante “Cancelling...” e só libera fechamento e
aplicação depois de esvaziar as filas e encerrar os workers. A chamada HTTP em
andamento pode precisar terminar ou atingir seu timeout. Destruição forçada de
um lote que possui seus próprios versículos transfere sua liberação para uma
thread de limpeza; versículos emprestados não podem seguir esse caminho.

## Credenciais de IA

No Windows, os segredos ficam no Gerenciador de Credenciais do usuário, sob
referências opacas iBiblia/ai/UUID. O projeto armazena referências distintas
para a configuração comum, associação e tradução.

Chaves antigas continuam funcionando e migram no próximo salvamento bem-sucedido,
inclusive autosave. Primeiro o serviço grava e relê os segredos locais. Depois
confirma as referências e remove as chaves antigas numa transação SQLite. A
limpeza usa secure_delete, VACUUM e checkpoint do WAL; se falhar, uma marca
persistente solicita nova tentativa no próximo salvamento. Falhas não removem
a única cópia utilizável da chave nem descartam alterações pendentes da chave.

Uma referência configurada, mas indisponível neste computador, produz erro e
pede reconfiguração nas opções de LLM. Abrir e confirmar a tela sem editar o campo
não apaga essa referência. Um override desabilitado não tenta ler sua chave.

Copiar um projeto no mesmo computador pode reutilizar sua referência. Trocar a
chave cria uma referência nova, preservando as cópias anteriores. Limpar o campo
desvincula a chave; entradas antigas no cofre não são excluídas automaticamente.
Backups e outras cópias existentes do projeto não são modificados.

Em outras plataformas, novas chaves permanecem apenas na sessão. Credenciais
antigas não são removidas sem um armazenamento local persistente disponível.

## Verificação

Validação em 06/09/2026: build Release concluído e 740 testes executados,
sem erros nem falhas (69 regressões adicionadas à base de 671 testes).
A cópia isolada do conteúdo deste commit também compilou em Release e passou
nos seus 732 testes; as demais alterações preexistentes ficaram fora do commit.
Inclui leitor mantendo WAL antigo e escritor concorrente bloqueando a
transação de referências, com retomada e verificação dos bytes após liberar
os bloqueios.

As regressões estão no runner tests/fpcunitibiblia_console.lpi, com fixtures
locais e clientes de IA controlados, sem chamadas a provedores. Cobrem seleção
vazia, respostas malformadas, rollback, cancelamento e fechamento, publicação
de arquivos e falhas de migração de credenciais.

O teste do cofre Windows usa uma referência aleatória e um segredo fictício,
removendo essa entrada ao terminar.

Build de integração:

~~~powershell
& "C:\Apps\lazarus\lazbuild.exe" --lazarusdir="C:\Apps\lazarus" --os=win64 --cpu=x86_64 --build-mode=Release iBiblia.lpi
~~~
