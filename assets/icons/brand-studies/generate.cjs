'use strict';
// Brand exploration only: no application resources or active artwork are written.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const args = process.argv.slice(2);
const option = name => { const i = args.indexOf(name); return i < 0 ? undefined : args[i + 1]; };
const sharp = require(option('--sharp-module') || 'sharp');
const root = __dirname;
const sizes = [16, 20, 24, 32, 40, 48, 64, 128, 256];
const escape = text => String(text).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const hash = content => crypto.createHash('sha256').update(content).digest('hex');
const currentPath = path.resolve(root, '../source/svg/app-ibiblia.svg');
const current = fs.readFileSync(currentPath, 'utf8');
const currentHash = hash(current);
const designs = [...require('./designs/scrolls.cjs'), ...require('./designs/letters.cjs')];
const write = (relative, content) => { const file = path.join(root, relative); fs.mkdirSync(path.dirname(file), {recursive:true}); fs.writeFileSync(file, content); };
const uri = svg => 'data:image/svg+xml;base64,' + Buffer.from(svg).toString('base64');
const image = (svg, x, y, width, height = width) => `<image x="${x}" y="${y}" width="${width}" height="${height}" href="${uri(svg)}"/>`;
const short = [
  'Um pergaminho aberto, unido pelo elo dourado.',
  'Dois textos lado a lado, com uma ligação central.',
  'Uma faixa de pergaminho com silhueta própria.',
  'Alfa e A como duas faces da mesma leitura.',
  'Mem e M aproximam os alfabetos hebraico e latino.',
  'Pergaminho e alfabetos reunidos em uma só marca.'
];
function contactSheet() {
  return `<svg xmlns="http://www.w3.org/2000/svg" width="1440" height="1148" font-family="Segoe UI, sans-serif">
    <rect width="100%" height="100%" fill="#F3F6F8"/>
    <text x="36" y="50" font-size="31" fill="#22344D">iBiblia · Estudos para a marca</text>
    <text x="36" y="82" font-size="16" fill="#637389">Pergaminhos e alfabetos conectados · seis propostas originais</text>
    ${image(current, 1260, 26, 48)}<text x="1320" y="49" font-size="13" fill="#637389">Marca atual</text>
    <text x="36" y="128" font-size="12" font-weight="600" letter-spacing="2" fill="#2563EB">01–03 · PERGAMINHOS</text>
    <text x="36" y="624" font-size="12" font-weight="600" letter-spacing="2" fill="#2563EB">04–06 · ALFABETOS CONECTADOS</text>
    ${designs.map((d, i) => { const x = 36 + i % 3 * 456, y = i < 3 ? 148 : 644;
      return `<g transform="translate(${x} ${y})">
      <rect width="432" height="440" rx="18" fill="#FFF" stroke="#E0E7EF"/>
      <text x="24" y="37" font-size="12" font-weight="600" fill="#2563EB">${String(i+1).padStart(2,'0')}</text>
      <text x="56" y="39" font-size="21" font-weight="600" fill="#22344D">${escape(d.title)}</text>
      <text x="24" y="66" font-size="13" fill="#637389">${short[i]}</text>
      ${image(d.svg, 152, 91, 128)}
      <line x1="24" x2="408" y1="241" y2="241" stroke="#EDF1F6"/>
      ${[16,24,32,48].map((size,n) => image(d.smallSvg && size <= 24 ? d.smallSvg : d.svg, 56 + n*92 - size/2, 284-size/2, size) + `<text x="${56+n*92}" y="325" text-anchor="middle" font-size="11" fill="#637389">${size} px</text>`).join('')}
      <rect x="24" y="350" width="384" height="44" rx="7" fill="#F5F7FB" stroke="#E3E9F0"/>
      ${image(d.smallSvg || d.svg, 36, 364, 16)}
      <text x="62" y="377" font-size="13" fill="#334155">iBiblia · Mateus 1:1</text>
      <path d="M385 366l9 9m0-9l-9 9" stroke="#64748B" stroke-width="1.2"/>
      <text x="24" y="419" font-size="11" fill="#8A96A8">Simulação na barra de título · ícone de 16 px</text></g>`;
    }).join('')}
    <text x="36" y="1125" font-size="13" fill="#637389">Propostas para avaliação. A marca instalada continua sendo a atual.</text>
  </svg>`;
}
function gallery() {
  return `<!doctype html><html lang="pt-BR"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>iBiblia · Estudos para a marca</title>
  <style>:root{font:16px/1.5 "Segoe UI",system-ui,sans-serif;color:#22344d;background:#f3f6f8;--size:128px;--tile:#fff}*{box-sizing:border-box}body{margin:0}main{max-width:1440px;margin:auto;padding:36px}h1{font-size:36px;letter-spacing:-.8px;margin:8px 0}p{color:#62728a;max-width:850px;margin:8px 0}header{display:flex;gap:32px;justify-content:space-between;align-items:center}.eyebrow{font-size:12px;font-weight:700;letter-spacing:2px;color:#2563eb}.current{flex:none;text-align:center;font-size:12px;color:#64748b}.current img{display:block;width:64px;margin:0 auto 8px}.controls{display:flex;align-items:center;gap:20px;flex-wrap:wrap;margin:30px 0;padding:18px 22px;background:white;border:1px solid #dfe6ee;border-radius:12px}label{display:flex;gap:10px;align-items:center;font-size:14px}select,a{font:inherit}select{border:1px solid #a2b1c4;border-radius:6px;padding:7px 12px;color:inherit;background:white}a{color:#215abc}a:focus-visible,select:focus-visible{outline:3px solid #d49a28;outline-offset:3px}.download{margin-left:auto;font-size:14px}h2{margin:30px 0 16px;font-size:13px;letter-spacing:2px;color:#2563eb}.grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:24px}.card{background:var(--tile);border:1px solid #dfe6ee;border-radius:16px;padding:24px;min-width:0}.card h3{font-size:21px;margin:0 0 8px;display:flex;gap:16px;align-items:center}.number{font-size:12px;color:#2563eb}.description{font-size:14px;min-height:65px}.display{display:grid;place-items:center;height:max(200px,calc(var(--size) + 32px))}.display img{width:var(--size);height:var(--size);max-width:100%;object-fit:contain}.sizes{display:flex;justify-content:space-around;align-items:end;border-top:1px solid #e7edf4;padding-top:20px;height:104px}.sizes figure{margin:0;text-align:center}.sizes figcaption{font-size:11px;color:#64748b;margin-top:8px}.titlebar{display:flex;align-items:center;gap:10px;padding:12px;margin-top:24px;background:#edf2f8;border-radius:7px;font-size:12px;color:#334155}.titlebar img{width:16px;height:16px}.titlebar span:last-child{margin-left:auto}.note{font-size:11px;margin-top:8px;color:#8793a3}.links{display:flex;gap:16px;margin-top:18px;font-size:12px}footer{font-size:13px;color:#64748b;margin-top:30px;max-width:1000px}@media(max-width:1100px){.grid{grid-template-columns:repeat(2,minmax(0,1fr))}}@media(max-width:700px){main{padding:22px}.grid{grid-template-columns:1fr}header{align-items:start}h1{font-size:28px}.current{display:none}.controls{align-items:start;gap:12px}.download{margin-left:0}}
  </style><main><header><div><div class="eyebrow">IBIBLIA · ESTUDO DE IDENTIDADE</div><h1>Pergaminhos e alfabetos conectados</h1><p>Seis caminhos para uma marca única. Compare a presença do desenho ampliado e a leitura em tamanhos pequenos.</p></div><div class="current"><img src="../source/svg/app-ibiblia.svg" alt="Livro com elo dourado">Marca atual</div></header><nav aria-label="Rodadas de estudos" style="display:flex;flex-wrap:wrap;gap:12px 24px;margin-top:24px;font-size:14px"><span aria-current="page">01–06 · Pergaminhos e alfabetos</span><a href="interlaced/index.html">07–10 · Letras entrelaçadas ↗</a><a href="initials/index.html">11–14 · Iniciais iB ↗</a></nav>
  <div class="controls"><label>Ícone principal<select id="size">${sizes.map(s => `<option value="${s}"${s===128?' selected':''}>${s} px</option>`).join('')}</select></label><label>Superfície<select id="surface"><option value="#fff">Branca</option><option value="#f3f6f8">Cinza-claro</option><option value="#fff5de">Papel</option></select></label><a class="download" href="preview/contact-sheet.png">Abrir a prancha completa ↗</a></div>
  ${[0,1].map(group => `<h2>${group===0?'01–03 · PERGAMINHOS':'04–06 · ALFABETOS CONECTADOS'}</h2><section class="grid">${designs.slice(group*3,group*3+3).map((d,n) => `<article class="card"><h3><span class="number">${String(group*3+n+1).padStart(2,'0')}</span>${escape(d.title)}</h3><p class="description">${escape(d.description)}</p><div class="display"><img src="source/svg/${d.id}.svg" data-id="${d.id}" data-small="${!!d.smallSvg}" alt="${escape(d.title)}"></div><div class="sizes">${[16,24,32,48].map(s=>`<figure><img src="png/${s}/${d.id}.png" width="${s}" height="${s}" alt="${s} pixels"><figcaption>${s} px</figcaption></figure>`).join('')}</div><div class="titlebar"><img src="png/16/${d.id}.png" alt=""><span>iBiblia · Mateus 1:1</span><span aria-hidden="true">×</span></div><div class="note">Simulação na barra de título · ícone de 16 px</div><div class="links"><a href="source/svg/${d.id}.svg">SVG</a><a href="png/256/${d.id}.png">PNG 256 px</a></div></article>`).join('')}</section>`).join('')}
  <footer>As letras escolhidas são alfa (α) e A, e mem (מ) e M. A ligação representa o trabalho entre textos e alfabetos. São propostas de marca para avaliação; os ícones instalados não foram alterados.</footer></main>
  <script>document.getElementById('size').addEventListener('change',e=>{document.documentElement.style.setProperty('--size',e.target.value+'px');document.querySelectorAll('.display img').forEach(img=>img.src='source/svg/'+img.dataset.id+(img.dataset.small==='true'&&Number(e.target.value)<=24?'-small':'')+'.svg')});document.getElementById('surface').addEventListener('change',e=>document.documentElement.style.setProperty('--tile',e.target.value));</script></html>`;
}
async function main() {
  if (designs.length !== 6 || new Set(designs.map(d=>d.id)).size !== 6) throw new Error('Expected six unique proposals');
  const report = [];
  for (const d of designs) {
    if (!/viewBox=["']0 0 64 64["']/.test(d.svg) || /<(text|image|script|foreignObject|filter)\b/i.test(d.svg)) throw new Error('Expected standalone vector artwork: '+d.id);
    write(`source/svg/${d.id}.svg`, d.svg+'\n');
    if (d.smallSvg) write(`source/svg/${d.id}-small.svg`, d.smallSvg+'\n');
    const warnings=[];
    for (const size of sizes) {
      const svg=d.smallSvg&&size<=24?d.smallSvg:d.svg;
      const png=await sharp(Buffer.from(svg)).resize(size,size).png().toBuffer();
      write(`png/${size}/${d.id}.png`,png);
      const {data,info}=await sharp(png).ensureAlpha().raw().toBuffer({resolveWithObject:true});
      let visible=0,clear=0,edge=0;
      for(let y=0;y<size;y++)for(let x=0;x<size;x++){const alpha=data[(y*size+x)*4+3];if(alpha)visible++;else clear++;if((x===0||y===0||x===size-1||y===size-1)&&alpha>96)edge++;}
      if(!visible||!clear||info.width!==size||info.height!==size)throw new Error('Invalid raster: '+d.id+'/'+size);
      if(edge)warnings.push(size);
    }
    report.push({id:d.id,sizes,edgeWarnings:warnings});
    console.log(d.id+': '+sizes.length+' PNGs, edge warnings '+warnings.join(','));
  }
  const sheet=contactSheet();write('preview/contact-sheet.svg',sheet);write('preview/contact-sheet.png',await sharp(Buffer.from(sheet)).png().toBuffer());
  write('index.html',gallery());
  write('manifest.json',JSON.stringify({purpose:'Original brand proposals for evaluation; not installed.',designs:designs.map(({svg,smallSvg,...d})=>({...d,source:`source/svg/${d.id}.svg`,smallSource:smallSvg?`source/svg/${d.id}-small.svg`:undefined,sizes}))},null,2)+'\n');
  if(hash(fs.readFileSync(currentPath,'utf8'))!==currentHash)throw new Error('Current brand was modified');
  write('validation.json',JSON.stringify({proposals:6,pngCount:6*sizes.length,activeBrandUnchanged:true,results:report},null,2)+'\n');
}
main().catch(e=>{console.error(e);process.exitCode=1});
