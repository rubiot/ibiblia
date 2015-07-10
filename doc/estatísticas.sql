select
  pare_situacao,
  count(*),
  round((count(*)/7957.0)*100,2) percentual
from pares
group by pare_situacao;