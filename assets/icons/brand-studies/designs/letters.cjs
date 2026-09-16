'use strict';

// Glyph outlines: Segoe UI Bold, installed Windows font (segoeuib.ttf),
// converted once with System.Drawing.Drawing2D.GraphicsPath.AddString.
// The exported marks contain only vector paths and have no font dependency.
const glyphs = {
  alpha: {
    x: 2.28125, y: 36.28125, width: 38.21875, height: 33.5625,
    d: 'M26.969 62.375C24.406 67.354 20.542 69.844 15.375 69.844C11.313 69.844 8.115 68.375 5.781 65.438C3.448 62.5 2.281 58.677 2.281 53.969C2.281 48.865 3.635 44.641 6.344 41.297C9.052 37.953 12.688 36.281 17.25 36.281C22.396 36.281 25.875 38.479 27.688 42.875L27.844 42.875L29.594 37.063L39.563 37.063L32.656 53.281C33.448 56.24 34.266 58.37 35.109 59.672C35.953 60.974 36.927 61.625 38.031 61.625C39.01 61.625 39.833 61.458 40.5 61.125L40.375 68.688C39 69.25 37.427 69.531 35.656 69.531C33.365 69.531 31.547 68.974 30.203 67.859C28.859 66.745 27.844 64.917 27.156 62.375ZM24.844 52.625C23.635 46.729 21.521 43.781 18.5 43.781C16.521 43.781 14.969 44.698 13.844 46.531C12.719 48.365 12.156 50.646 12.156 53.375C12.156 55.979 12.609 58.099 13.516 59.734C14.422 61.37 15.74 62.188 17.469 62.188C20.427 62.188 22.729 59.49 24.375 54.094Z'
  },
  latinA: {
    x: 0.3125, y: 24.25, width: 44.21875, height: 44.8125,
    d: 'M44.531 69.063L33.531 69.063L30.344 59.094L14.406 59.094L11.25 69.063L0.313 69.063L16.625 24.25L28.594 24.25ZM28.031 51.344L23.219 36.281C22.865 35.156 22.615 33.813 22.469 32.25L22.219 32.25C22.115 33.563 21.854 34.865 21.438 36.156L16.563 51.344Z'
  },
  latinM: {
    x: 5.125, y: 24.25, width: 51.0625, height: 44.8125,
    d: 'M56.188 69.063L46.219 69.063L46.219 42.25C46.219 39.354 46.344 36.156 46.594 32.656L46.344 32.656C45.823 35.406 45.354 37.385 44.938 38.594L34.438 69.063L26.188 69.063L15.5 38.906C15.208 38.094 14.74 36.01 14.094 32.656L13.813 32.656C14.083 37.073 14.219 40.948 14.219 44.281L14.219 69.063L5.125 69.063L5.125 24.25L19.906 24.25L29.063 50.813C29.792 52.938 30.323 55.073 30.656 57.219L30.844 57.219C31.406 54.74 32 52.583 32.625 50.75L41.781 24.25L56.188 24.25Z'
  },
  mem: {
    x: 3.40625, y: 29.96875, width: 37.46875, height: 39.6875,
    d: 'M40.875 55C40.875 58.292 40.323 60.964 39.219 63.016C38.115 65.068 36.453 66.688 34.234 67.875C32.016 69.062 29.552 69.656 26.844 69.656C25.427 69.656 24.089 69.542 22.828 69.313C21.568 69.083 20.323 68.615 19.094 67.906L20.844 60.969C22.365 61.49 23.76 61.75 25.031 61.75C28.906 61.75 30.844 59.49 30.844 54.969L30.844 47.25C30.844 44.396 30.104 42.156 28.625 40.531C27.146 38.906 25.052 38.094 22.344 38.094C20.99 38.094 19.693 38.411 18.453 39.047C17.214 39.682 16.24 40.516 15.531 41.547C14.823 42.578 14.469 43.531 14.469 44.406L14.469 69.063L4.375 69.063L4.375 38.813C4.375 37.854 4.328 36.938 4.234 36.063C4.141 35.188 3.865 34.271 3.406 33.313L12.094 29.969C13.094 31.156 13.771 32.677 14.125 34.531C15.833 32.948 17.563 31.818 19.313 31.141C21.063 30.464 22.979 30.125 25.063 30.125C29.792 30.125 33.609 31.646 36.516 34.688C39.422 37.729 40.875 41.833 40.875 47Z'
  }
};

const C = { blue: '#2563EB', gold: '#D49A28', ink: '#22344D', cream: '#FFF4DC', white: '#FFFFFF' };
const path = (d, fill, extra = '') => `<path d="${d}" fill="${fill}"${extra ? ` ${extra}` : ''}/>`;
const stroke = (d, color, width) => path(d, 'none', `stroke="${color}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"`);
function letter(name, x, y, width, height, color) {
  const g = glyphs[name];
  const s = Math.min(width / g.width, height / g.height);
  const tx = x + (width - g.width * s) / 2 - g.x * s;
  const ty = y + (height - g.height * s) / 2 - g.y * s;
  return path(g.d, color, `transform="translate(${tx.toFixed(4)} ${ty.toFixed(4)}) scale(${s.toFixed(6)})"`);
}
function svg(body, transform) {
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">${transform ? `<g transform="${transform}">${body}</g>` : body}</svg>`;
}

module.exports = [
  {
    id: '04-alfa-a',
    title: 'Alfa e A',
    subtitle: 'Alfabetos conectados',
    description: 'O alfa grego e o A latino formam uma assinatura direta, unidos por um elo azul e dourado. A composição sem moldura dá protagonismo às letras.',
    svg: svg(
      letter('alpha', 4, 10, 26, 30, C.blue) +
      letter('latinA', 35, 8, 26, 32, C.gold) +
      stroke('M28 45H20a6 6 0 0 0 0 12h9a6 6 0 0 0 6-6', C.blue, 4.5) +
      stroke('M36 57h8a6 6 0 0 0 0-12h-9a6 6 0 0 0-6 6', C.gold, 4.5)
    )
  },
  {
    id: '05-mem-m',
    title: 'Mem e M',
    subtitle: 'Uma ponte entre escritas',
    description: 'O M latino e a letra hebraica mem ocupam duas placas ligadas. A forma compacta enfatiza a convivência de alfabetos e a conexão entre textos.',
    svg: svg(
      path('M10 7h17a7 7 0 0 1 7 7v30H10a7 7 0 0 1-7-7V14a7 7 0 0 1 7-7Z', C.blue) +
      path('M37 17h17a7 7 0 0 1 7 7v27a7 7 0 0 1-7 7H30V24a7 7 0 0 1 7-7Z', C.gold) +
      letter('latinM', 8, 17, 20, 19, C.white) +
      letter('mem', 36, 27, 20, 23, C.ink) +
      stroke('M19 42v4a6 6 0 0 0 6 6h12', C.cream, 6) +
      stroke('M19 42v4a6 6 0 0 0 6 6h12', C.ink, 3.5)
    )
  },
  {
    id: '06-pergaminho-alfabetos',
    title: 'Pergaminho dos alfabetos',
    subtitle: 'Texto antigo, conexão presente',
    description: 'Um pergaminho claro abriga o alfa grego; uma pequena página azul traz o A latino. O elo dourado une as duas superfícies em uma só marca.',
    svg: svg(
      path('M16 5h34c5 0 8 4 8 8v4H25v32c0 5-4 10-10 10H8c-4 0-7-3-7-7v-4h10V13c0-4 2-8 5-8Z', C.gold) +
      path('M18 5h28c-3 2-4 5-4 9v37c0 5-4 8-9 8H15c6 0 10-5 10-10V14c0-5-3-9-7-9Z', C.cream) +
      path('M18 5c-5 0-7 4-7 8v35H5v4c0 2 1 3 3 3h7c4 0 6-3 6-6V14c0-3-1-6-3-9Z', C.cream) +
      stroke('M7 48h12', C.gold, 3) +
      letter('alpha', 14, 17, 24, 24, C.ink) +
      path('M41 30h16a5 5 0 0 1 5 5v23a4 4 0 0 1-4 4H37a4 4 0 0 1-4-4V38a8 8 0 0 1 8-8Z', C.blue) +
      letter('latinA', 37, 36, 21, 22, C.white) +
      stroke('M27 46h5a5 5 0 0 0 5-5v-1', C.cream, 7) +
      stroke('M27 46h5a5 5 0 0 0 5-5v-1', C.gold, 4),
      'translate(4 2) scale(0.88)'
    )
  }
];
