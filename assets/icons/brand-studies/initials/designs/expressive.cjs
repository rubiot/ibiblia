'use strict';

const blue = '#2563EB';
const gold = '#D49A28';
const ribbon = (d, color, width) => `<path d="${d}" fill="none" stroke="${color}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"/>`;
const wrap = content => `<svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" viewBox="0 0 64 64">${content}</svg>`;

// These are drawn letterforms, not a spelling or a transliteration of iBiblia.
const betaWeave = 'M24 56L30 18C31 10 37 7 43 10C52 14 48 25 32 29C42 26 54 31 51 41C48 52 33 54 26 44';
const vavWeave = 'M10 15L20 12L32 47';
const betaCompact = 'M20 56L27 19C28 11 34 8 41 10C52 14 48 25 30 29C40 26 55 31 51 42C47 53 30 53 23 44';

function woven() {
  const beta = (color, gap = false) => ribbon(betaWeave, color, gap ? 10.2 : 6.6);
  const vav = (color, gap = false) => ribbon(vavWeave, color, gap ? 10.2 : 6.6);
  return wrap(`<defs>
    <clipPath id="ib13-lower-crossing"><rect x="24" y="38" width="16" height="16"/></clipPath>
    <mask id="ib13-gold" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64" style="mask-type:luminance"><rect width="64" height="64" fill="white"/>${beta('black', true)}<rect x="24" y="38" width="16" height="16" fill="white"/></mask>
    <mask id="ib13-blue" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64" style="mask-type:luminance"><rect width="64" height="64" fill="white"/><g clip-path="url(#ib13-lower-crossing)">${vav('black', true)}</g></mask>
    </defs><g mask="url(#ib13-gold)">${vav(gold)}</g><g mask="url(#ib13-blue)">${beta(blue)}</g>`);
}

module.exports = [
  {
    id: '13-vav-beta-trama',
    title: 'Vav e beta · trama',
    description: 'Um vav inclinado atravessa o beta: a haste azul passa à frente no centro e a dourada retorna à frente na curva inferior. Uma interpretação gráfica das iniciais iB.',
    svg: woven()
  },
  {
    id: '14-yod-beta',
    title: 'Yod e beta · ponto',
    description: 'O yod compacto evoca o ponto do i junto à haste longa do beta. As letras compartilham a composição numa interpretação gráfica de iB, sem pretensão de transliteração.',
    svg: wrap(`<path d="M10 8L18 10Q21 11 20 14L19 19L14 18L15 15L9 13Z" fill="${gold}"/>${ribbon(betaCompact, blue, 7)}`)
  }
];
