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
pasta do destino. A geração e o fechamento desse arquivo terminam antes de
alterar o destino.

Atualizar módulos abertos no theWord não depende de renomear o arquivo:

- Texto theWord: a substituição por renomeação continua sendo a primeira
  opção. Quando o Windows bloqueia essa operação, mas permite escrever, o
  exportador abre o arquivo existente sem truncá-lo, impede outros escritores
  e salva um backup completo antes de copiar o novo conteúdo. Uma falha
  detectada restaura os bytes e o tamanho anteriores no mesmo arquivo.
- Concordância e MySword: um banco existente é atualizado pela API de backup
  do SQLite, numa transação que respeita conexões abertas e o journal/WAL.
  Uma transação conflitante continua produzindo erro, preservando a saída.
  Bancos SQLite nunca passam pela cópia direta usada para texto.

A atualização SQLite invalida consultas preparadas anteriormente. Clientes
com a API legada precisam prepará-las novamente ao receber SQLITE_SCHEMA;
o exportador não mascara essa mudança de esquema. Os testes usam a mesma
conexão aberta com sqlite3_prepare_v2, que faz essa recuperação. A atualização
visual e os caches internos do theWord ainda dependem de recarregar o módulo
quando necessário; a validação automatizada não controla a interface dele.

A atualização de texto no arquivo aberto não é uma troca instantânea para
leitores. Se houver encerramento abrupto ou falha na restauração, o backup
permanece ao lado do destino, com sufixo ".export-backup-UUID". Uma falha de
restauração informa esse caminho. Backups são removidos somente após confirmar
a gravação ou a restauração. Arquivos que também bloqueiam escrita continuam
protegidos; o exportador não encerra o theWord nem força a liberação de locks.

Referências técnicas: [compartilhamento de arquivos no Windows](https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew)
e [transação da API de backup do SQLite](https://sqlite.org/c3ref/backup_finish.html).

Os chamadores recebem TExportResult: sucesso, sucesso com avisos ou falha,
quantidade de versículos processados, caminho publicado, relatório e erro.
Falhas recuperáveis por versículo continuam permitindo exportar com avisos.
Se o relatório não puder ser salvo, a janela informa essa falha separadamente
do sucesso da exportação.

### Destinos locais por projeto

O diálogo "Salvar como" lembra separadamente os destinos theWord de texto com
Strong, interlinear, concordância analítica e sintética. O histórico fica no
iBiblia.ini local, por caminho absoluto do projeto, sem gravar caminhos no
banco do projeto. Arquivos de projeto em caminhos diferentes têm históricos
independentes, inclusive quando seus nomes são iguais.

Confirmar "Salvar" registra o destino antes de exportar, permitindo repetir
uma tentativa que falhou. Cancelar não altera o registro. MySword continua
disponível no diálogo e não substitui o histórico theWord. Destinos globais
antigos não são herdados, pois não identificam seu projeto de origem.

Uma pasta indisponível faz o diálogo usar a sugestão baseada no projeto, mas
não apaga o histórico. Falha ao salvar a preferência produz um aviso junto
ao resultado da exportação, sem impedir a geração nem alterar seu resultado.

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

Validação do histórico local em 10/09/2026: build Release concluído e
795 testes executados, sem erros nem falhas. As 31 novas regressões cobrem
preferências reabertas do disco, os quatro destinos, separação de projetos,
comparação ordinal de nomes Unicode no Windows, cancelamento, confirmação
antes da exportação, MySword e avisos de preferências. Os testes do formulário
substituem o diálogo nativo e a geração do módulo, usando projeto e INI
temporários. Logs em outputs/export-history-2026-09-10/.

A correção de módulos em uso foi commitada como 3700ab1 após a cópia isolada
do conteúdo exato do commit compilar em Release e passar em 756 testes.
As outras alterações locais não foram incluídas nesse commit.

Validação da atualização de módulos em uso em 10/09/2026: build Release
concluído e 764 testes executados, sem erros nem falhas. Foram adicionadas 24
regressões para leitores mantendo o arquivo aberto, publicação SQLite,
bloqueios reais do Windows, falhas de gravação/restauração e retenção de backup.
Os testes de integração exportam texto com Strong, interlinear e concordância,
verificando os dados pelo mesmo handle/conexão aberto antes da atualização.
Todos usam arquivos temporários; nenhum módulo de trabalho foi sobrescrito
durante essa validação. Logs em outputs/export-in-use-2026-09-10/.

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
