/* Evaluation assets only. This script never writes the active icon catalog. */
'use strict';
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const root = __dirname;
const active = path.resolve(root, '..');
const args = process.argv.slice(2);
const option = name => { const i = args.indexOf(name); return i < 0 ? undefined : args[i + 1]; };
const sharp = require(option('--sharp-module') || 'sharp');
const catalog = JSON.parse(fs.readFileSync(path.join(active, 'manifest/icons.json'), 'utf8')).icons;
const sizes = [16, 20, 24, 32, 40, 48, 64];
const ids = ['clareza', 'mosaico', 'folio'];
const selected = option('--family');
const escape = value => String(value).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const digest = value => crypto.createHash('sha256').update(value).digest('hex');
const svgURI = svg => 'data:image/svg+xml;base64,' + Buffer.from(svg).toString('base64');
const write = (relative, content) => {
  const file = path.join(root, relative);
  fs.mkdirSync(path.dirname(file), {recursive: true});
  fs.writeFileSync(file, content);
};
const labels = {
  'app-ibiblia':'Marca iBiblia', 'project-new':'Novo projeto', 'project-open':'Abrir projeto',
  'project-close':'Fechar projeto', 'project-save':'Salvar', 'project-save-as':'Salvar como',
  'project-settings':'Propriedades do projeto', 'project-export':'Exportar projeto',
  'association-auto':'Associar sintagmas', 'association-clear':'Remover associações',
  'association-rollback':'Reverter associações', 'history-back':'Voltar no histórico',
  'history-forward':'Avançar no histórico', 'verse-previous':'Versículo anterior',
  'verse-next':'Próximo versículo', 'verse-first':'Primeiro versículo', 'verse-last':'Último versículo',
  'app-exit':'Sair', 'projects-merge':'Mesclar projetos',
  'association-memory-rebuild':'Reconstruir memória de associação',
  'sync-theword-verse':'Sincronizar com theWord', 'tags-show':'Exibir etiquetas',
  'patch-create':'Criar patch', 'patch-apply':'Aplicar patch', 'database-vacuum':'Compactar banco',
  'keyboard-shortcuts':'Atalhos de teclado', 'layout-reset':'Restaurar disposição',
  'guide-mode':'Modo guiado', 'theword-highlighter-colors':'Cores de destaque',
  'llm-options':'Opções de IA', 'preferences':'Preferências',
  'file-association-register':'Registrar tipo de arquivo',
  'file-association-unregister':'Remover registro do arquivo', 'text-load':'Carregar texto',
  'text-clear':'Limpar texto', 'dictionary-strong-select':'Dicionário Strong',
  'dictionary-morphology-select':'Dicionário de morfologia', 'font-select':'Escolher fonte',
  'checkbox-unchecked':'Caixa desmarcada', 'checkbox-checked':'Caixa marcada',
  'checkbox-partial':'Caixa parcialmente marcada', 'ai-pending':'IA: aguardando',
  'ai-processing':'IA: processando', 'ai-success':'IA: concluído', 'ai-error':'IA: erro',
  'tab-source':'Texto fonte', 'tab-destination':'Texto destino', 'tab-reference':'Texto de referência',
  'comments':'Comentários', 'status-review':'Revisar', 'help':'Ajuda', 'about':'Sobre',
  'confirm':'Confirmar', 'cancel':'Cancelar', 'add':'Adicionar', 'remove':'Remover',
  'edit':'Editar', 'browse':'Procurar arquivo', 'refresh':'Atualizar', 'search':'Pesquisar',
  'print':'Imprimir', 'copy':'Copiar', 'paste':'Colar', 'stop':'Parar', 'translate':'Traduzir',
  'link':'Vincular textos', 'export-destination-strongs':'Exportar com Strong',
  'export-interlinear':'Exportar interlinear'
};
const showcase = ['app-ibiblia', 'project-open', 'project-save', 'association-auto',
  'association-clear', 'association-rollback', 'history-back', 'verse-previous',
  'sync-theword-verse', 'llm-options', 'comments', 'preferences'];

function loadFamily(id) {
  const family = require(path.join(root, 'designs', id + '.cjs'));
  if (family.id !== id) throw new Error('Incorrect family ID: ' + id);
  const missing = catalog.filter(i => !family.icons[i.symbol]);
  const extra = Object.keys(family.icons).filter(s => !catalog.some(i => i.symbol === s));
  if (missing.length || extra.length) throw new Error(`${id}: missing ${missing.map(i => i.symbol)}, extra ${extra}`);
  for (const [symbol, svg] of Object.entries(family.icons)) {
    if (!/<svg\b/.test(svg) || !/viewBox=["']0 0 32 32["']/.test(svg) ||
        /<(script|image|text|foreignObject|filter)\b/i.test(svg) || /\b(?:href|src)\s*=/i.test(svg))
      throw new Error('Expected self-contained vector icon: ' + id + '/' + symbol);
  }
  return family;
}

function contactSheet(family) {
  const cards = catalog.map((icon, n) => {
    const x = n % 6 * 176, y = 110 + Math.floor(n / 6) * 132;
    const href = svgURI(family.icons[icon.symbol]);
    return `<g transform="translate(${x} ${y})"><rect x="8" width="160" height="122" rx="10" fill="#fff"/>
      <image x="20" y="41" width="16" height="16" href="${href}"/>
      <image x="51" y="37" width="24" height="24" href="${href}"/>
      <image x="96" y="25" width="48" height="48" href="${href}"/>
      <text x="88" y="97" text-anchor="middle" font-size="${icon.symbol.length > 23 ? 9 : 11}" fill="#475569">${escape(icon.symbol)}</text></g>`;
  }).join('');
  return `<svg xmlns="http://www.w3.org/2000/svg" width="1056" height="${110 + Math.ceil(catalog.length / 6) * 132}" font-family="Segoe UI, sans-serif">
    <rect width="100%" height="100%" fill="#F3F5F8"/>
    <text x="28" y="43" font-size="28" fill="#1F2937">iBiblia · ${escape(family.name)}</text>
    <text x="28" y="74" font-size="14" fill="#64748B">${escape(family.subtitle)} · 16 / 24 / 48 px</text>${cards}</svg>`;
}

async function generateFamily(family) {
  const report = {id: family.id, icons: catalog.length, sizes, pngCount: 0, empty: [], clipped: [], distinctSVGs: new Set(Object.values(family.icons).map(digest)).size};
  for (const icon of catalog) {
    const svg = family.icons[icon.symbol];
    write(`${family.id}/source/svg/${icon.symbol}.svg`, svg + '\n');
    for (const size of sizes) {
      const file = `${family.id}/png/${size}/${icon.symbol}.png`;
      const png = await sharp(Buffer.from(svg)).resize(size, size).png().toBuffer();
      write(file, png);
      report.pngCount++;
      const {data, info} = await sharp(png).ensureAlpha().raw().toBuffer({resolveWithObject: true});
      if (info.width !== size || info.height !== size) throw new Error('Wrong dimensions: ' + file);
      let visible = 0, transparent = 0, border = 0;
      for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) {
        const a = data[(y * size + x) * 4 + 3];
        if (a > 0) visible++;
        if (a === 0) transparent++;
        if ((x === 0 || y === 0 || x === size - 1 || y === size - 1) && a > 96) border++;
      }
      if (!visible || !transparent) report.empty.push(file);
      if (size >= 32 && border) report.clipped.push(file);
    }
  }
  if (report.empty.length) throw new Error('Empty or opaque icons: ' + report.empty.join(', '));
  const sheet = contactSheet(family);
  write(`${family.id}/preview/contact-sheet.svg`, sheet);
  write(`${family.id}/preview/contact-sheet.png`, await sharp(Buffer.from(sheet)).png().toBuffer());
  write(`${family.id}/manifest.json`, JSON.stringify({name: family.name, description: family.description, palette: family.palette,
    provenance: 'Original alternative vector artwork for iBiblia. Evaluation only.',
    icons: catalog.map(i => ({symbol: i.symbol, meaning: i.meaning, source: `source/svg/${i.symbol}.svg`, sizes}))}, null, 2) + '\n');
  write(`${family.id}/validation.json`, JSON.stringify(report, null, 2) + '\n');
  console.log(`${family.name}: ${report.icons} SVGs; ${report.pngCount} PNGs; ${report.clipped.length} edge warnings.`);
  return report;
}

function overview(families) {
  const w = 1320, first = 244, cw = 260, header = 162, rh = 74;
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${header + showcase.length * rh + 38}" font-family="Segoe UI, sans-serif">
  <rect width="100%" height="100%" fill="#F3F5F8"/>
  <text x="28" y="43" font-size="28" fill="#1F2937">iBiblia · Quatro linguagens para os mesmos comandos</text>
  <text x="28" y="74" font-size="14" fill="#64748B">A família atual e três alternativas originais. Cada célula mostra o ícone em 24 e 48 pixels.</text>
  ${families.map((f, n) => `<text x="${first + n * cw + cw / 2}" y="119" text-anchor="middle" font-size="21" font-weight="600" fill="#1F2937">${escape(f.name)}</text><text x="${first + n * cw + cw / 2}" y="143" text-anchor="middle" font-size="12" fill="#64748B">${['Família atual', 'Contorno e cor pontual', 'Geometria e cores sólidas', 'Papel e tinta'][n]}</text>`).join('')}
  ${showcase.map((s, i) => `<text x="28" y="${header + i * rh + 40}" font-size="14" fill="#334155">${escape(labels[s])}</text>${families.map((f, n) => {
    const x = first + n * cw, y = header + i * rh, uri = svgURI(f.icons[s]);
    return `<rect x="${x}" y="${y}" width="${cw - 12}" height="${rh - 8}" rx="9" fill="#fff"/><image x="${x + 72}" y="${y + 21}" width="24" height="24" href="${uri}"/><image x="${x + 131}" y="${y + 9}" width="48" height="48" href="${uri}"/>`;
  }).join('')}`).join('')}
  <text x="28" y="${header + showcase.length * rh + 19}" font-size="12" fill="#64748B">68 símbolos por família · Catálogos completos e arquivos SVG acompanham esta comparação.</text></svg>`;
}

function gallery(families) {
  const imagePath = (f, s) => f.id === 'current' ? `../source/svg/${s}.svg` : `${f.id}/source/svg/${s}.svg`;
  const links = families.map(f => `<a href="${f.id === 'current' ? '../preview' : f.id + '/preview'}/contact-sheet.png">${escape(f.name)}: prancha completa ↗</a>`).join('');
  const captions = ['Silhuetas coloridas e volume discreto.', 'Traço leve e aberto, com acentos de cor.',
    'Geometria forte, blocos de cor e recortes amplos.', 'Papel ilustrado, contornos de tinta e tons quentes.'];
  const header = families.map((f, n) => `<th scope="col"><span class="number">${n === 0 ? 'ATUAL' : 'OPÇÃO ' + n}</span>${escape(f.name)}<small>${captions[n]}</small></th>`).join('');
  const rows = catalog.map(i => `<tr data-search="${escape(labels[i.symbol] + ' ' + i.symbol + (i.symbol.startsWith('association-') ? ' associação associações associar' : ''))}" data-essential="${showcase.includes(i.symbol)}"><th scope="row">${escape(labels[i.symbol])}<code>${i.symbol}</code></th>${families.map(f => `<td><img width="48" height="48" src="${imagePath(f, i.symbol)}" alt="${escape(labels[i.symbol] + ' — ' + f.name)}" loading="lazy"></td>`).join('')}</tr>`).join('');
  return `<!doctype html><html lang="pt-BR"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>iBiblia — Famílias de ícones</title>
  <style>
  :root{font:15px/1.5 "Segoe UI",system-ui,sans-serif;color:#253247;background:#f3f5f8;--icon-size:48px;--surface:#fff}*{box-sizing:border-box}body{margin:0}main{max-width:1440px;margin:auto;padding:32px 28px 64px}h1{font-size:32px;line-height:1.2;margin:8px 0 12px;letter-spacing:-.6px}p{max-width:900px;margin:8px 0;color:#56657a}.eyebrow{color:#2563eb;font-weight:700;letter-spacing:2px;font-size:12px}.controls{display:flex;flex-wrap:wrap;align-items:end;gap:16px;margin:28px 0 18px;padding:20px;background:#fff;border:1px solid #dde3ec;border-radius:12px}label{display:flex;flex-direction:column;gap:6px;font-size:13px;font-weight:600}input,select,button{font:inherit;color:inherit;border:1px solid #9eacc0;border-radius:6px;background:#fff;padding:8px 12px;min-height:40px}input[type=search]{width:250px}button{cursor:pointer}button[aria-pressed=true]{color:#fff;background:#2563eb;border-color:#2563eb}button:focus-visible,a:focus-visible,input:focus-visible,select:focus-visible{outline:3px solid #d49a28;outline-offset:3px}.count{font-size:13px;color:#64748b;margin-left:auto;align-self:center}.table-wrap{overflow:auto;border-radius:12px;border:1px solid #dde3ec;background:var(--surface)}table{border-collapse:separate;border-spacing:0;width:100%;min-width:960px;table-layout:fixed}thead th{position:sticky;top:0;z-index:1;vertical-align:top;background:#edf1f7;font-size:22px;padding:20px 16px;text-align:center}thead th:first-child{width:236px;text-align:left;font-size:14px}thead small{display:block;font-size:12px;font-weight:400;line-height:1.5;max-width:220px;margin:8px auto 0;color:#5b677a}.number{display:block;font-size:10px;letter-spacing:1.5px;color:#64748b;margin-bottom:4px}tbody th{font-size:14px;text-align:left;font-weight:600;padding:14px 18px}code{display:block;font:10px/1.4 Consolas,monospace;color:#64748b;margin-top:4px;overflow-wrap:anywhere}tbody td{text-align:center;padding:15px;height:92px}tbody th,tbody td{border-top:1px solid #e5e9f0}td+td{border-left:1px solid #eef1f5}tbody tr:hover{background:#eff5ff}td img{width:var(--icon-size);height:var(--icon-size);vertical-align:middle}.disabled td img{filter:grayscale(1);opacity:.42}.downloads{display:flex;flex-wrap:wrap;gap:12px;margin:24px 0}.downloads a{background:#fff;border:1px solid #d7e0ec;border-radius:8px;padding:10px 14px;font-size:13px;color:#2254ae;text-decoration:none}.downloads a:hover{text-decoration:underline}footer{margin-top:20px;font-size:12px;color:#64748b}[hidden]{display:none!important}@media(max-width:600px){main{padding:20px 12px}h1{font-size:26px}.controls{padding:12px;gap:12px}input[type=search]{width:220px}}@media print{.controls{display:none}main{padding:0}.table-wrap{overflow:visible}table{min-width:0}thead th:first-child{width:160px}.downloads{display:none}}
  </style><main><div class="eyebrow">IBIBLIA · ESTUDO DE IDENTIDADE</div><h1>Três novas famílias de ícones</h1><p>Compare as três propostas com a família atual. Todas usam o mesmo catálogo de 68 símbolos; as diferenças estão no desenho, no peso e no tratamento da cor.</p><div class="downloads">${links}</div>
  <div class="controls"><label>Encontrar comando<input id="search" type="search" placeholder="Ex.: associação, salvar, IA"></label><label>Tamanho<select id="size">${sizes.map(s => `<option value="${s}"${s === 48 ? ' selected' : ''}>${s} px</option>`).join('')}</select></label><label>Superfície<select id="surface"><option value="#fff">Branca</option><option value="#f3f5f8">Cinza-claro</option></select></label><button id="essential" aria-pressed="true">Comandos principais</button><button id="disabled" aria-pressed="false">Simular desabilitados</button><span class="count" id="count" role="status" aria-live="polite"></span></div>
  <div class="table-wrap"><table><thead><tr><th scope="col">Comando</th>${header}</tr></thead><tbody>${rows}</tbody></table></div>
  <footer>Desenhos vetoriais originais. PNGs transparentes em 16, 20, 24, 32, 40, 48 e 64 px. A simulação de desabilitados serve para comparação visual; o resultado nativo depende do Lazarus e do Windows. A interface do aplicativo continua usando a família atual.</footer></main>
  <script>
  const search=document.getElementById('search'),essential=document.getElementById('essential'),disabled=document.getElementById('disabled');
  const normalized=value=>value.normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').toLowerCase();
  function update(){const q=normalized(search.value),only=essential.getAttribute('aria-pressed')==='true'&&!q;let count=0;document.querySelectorAll('tbody tr').forEach(row=>{row.hidden=(!normalized(row.dataset.search).includes(q))||(only&&row.dataset.essential!=='true');if(!row.hidden)count++});document.getElementById('count').textContent=count+' de 68 comandos'}
  search.addEventListener('input',update);essential.addEventListener('click',()=>{essential.setAttribute('aria-pressed',essential.getAttribute('aria-pressed')!=='true');update()});disabled.addEventListener('click',()=>{const on=disabled.getAttribute('aria-pressed')!=='true';disabled.setAttribute('aria-pressed',on);document.body.classList.toggle('disabled',on)});document.getElementById('size').addEventListener('change',e=>document.documentElement.style.setProperty('--icon-size',e.target.value+'px'));document.getElementById('surface').addEventListener('change',e=>document.documentElement.style.setProperty('--surface',e.target.value));update();
  </script></html>`;
}

async function main() {
  if (selected && !ids.includes(selected)) throw new Error('Unknown family: ' + selected);
  const activeFiles = ['manifest/icons.json', 'preview/contact-sheet.png', ...catalog.map(i => i.source)];
  const before = activeFiles.map(f => digest(fs.readFileSync(path.join(active, f))));
  const families = (selected ? [selected] : ids).map(loadFamily);
  const reports = [];
  for (const family of families) reports.push(await generateFamily(family));
  if (!selected) {
    const current = {id: 'current', name: 'Textos conectados', description: 'Silhuetas coloridas e volume discreto.',
      icons: Object.fromEntries(catalog.map(i => [i.symbol, fs.readFileSync(path.join(active, i.source), 'utf8')]))};
    const all = [current, ...families];
    const sheet = overview(all);
    write('preview/comparison.svg', sheet);
    write('preview/comparison.png', await sharp(Buffer.from(sheet)).png().toBuffer());
    write('index.html', gallery(all));
  }
  activeFiles.forEach((f, n) => { if (before[n] !== digest(fs.readFileSync(path.join(active, f)))) throw new Error('Active asset changed: ' + f); });
  write(selected ? `${selected}/validation.json` : 'validation.json', JSON.stringify(selected ? reports[0] : {
    catalogSize: catalog.length, familyCount: families.length, svgCount: families.length * catalog.length,
    pngCount: reports.reduce((n, r) => n + r.pngCount, 0), activeAssetsUnchanged: true, families: reports
  }, null, 2) + '\n');
}
main().catch(e => { console.error(e); process.exitCode = 1; });
