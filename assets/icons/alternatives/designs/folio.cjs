'use strict';

// Original editorial artwork. Broad paper planes and an ink keyline stay readable at menu size.
const palette = {
  ink: '#293E50', paper: '#FFF5DE', sand: '#E5D3AE', blue: '#9EC6D6',
  primary: '#397C9F', gold: '#E8B856', clay: '#D97859', green: '#79AA91',
  purple: '#B49AC1', white: '#FFFFFF', shadow: '#CFBEA0', red: '#B74E40'
};
const C = palette;
const path = (d, fill = C.paper, sw = 1.7, stroke = C.ink) => `<path d="${d}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}" stroke-linecap="round" stroke-linejoin="round"/>`;
const line = (d, color = C.ink, width = 1.8) => path(d, 'none', width, color);
const rect = (x, y, w, h, fill = C.paper, radius = 1.2, stroke = C.ink, width = 1.7) => `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${radius}" fill="${fill}" stroke="${stroke}" stroke-width="${width}"/>`;
const circle = (x, y, r, fill = C.paper, stroke = C.ink, width = 1.7) => `<circle cx="${x}" cy="${y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${width}"/>`;
const group = (body, transform) => `<g transform="${transform}">${body}</g>`;
const svg = body => `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">${body}</svg>`;
const icons = {};
const add = (name, body) => { icons[name] = svg(body); };
const check = (color = C.ink) => line('M20.8 24.2l2.3 2.3 4.2-5', color, 2.2);
const plus = (color = C.ink) => line('M24 20.5v7M20.5 24h7', color, 2);
const cross = (color = C.ink) => line('M21.5 21.5l5 5m0-5l-5 5', color, 2);
const badge = (body, color = C.gold) => circle(24, 24, 6.4, color) + body;
const paper = (accent = C.blue) =>
  path('M8 5h15l6 6v19H8z', C.shadow, 0, 'none') +
  path('M5 3h16l6 6v19H5z') + path('M21 3v7h6', C.sand) +
  path('M5 3h4v25H5z', accent, 0, 'none') + line('M12 13h9m-9 5h9m-9 5h5');
const plainPaper = (fill = C.paper) => path('M5 4h16l6 6v18H5z', fill) + path('M21 4v6h6', C.sand);
const folder = () => path('M3 7h9l3 4h13v17H3z', C.sand) + path('M4 13l26-1-4 16H2z', C.gold) + line('M7 17h9', C.paper, 2);
const disk = () => path('M6 5h18l5 5v19H6z', C.shadow, 0, 'none') + path('M3 3h19l6 6v19H3z', C.blue) + rect(8, 3, 12, 8, C.paper, 0) + rect(8, 18, 15, 10, C.paper, 0) + rect(16, 5, 2, 4, C.ink, 0, C.ink, 0) + line('M11 22h9');
const book = (left = C.paper, right = C.paper) =>
  path('M3 7l11 3 3 1 10-4 3 1-1 20-12 2-3-1-11-2z', C.shadow, 0, 'none') +
  path('M2 5l11 2 3 3 3-3 10-3-1 21-9 2-3 2-3-3-10-2z', C.primary) +
  path('M3 4l10 2 3 3v18l-3-3-10-2z', left) +
  path('M16 9l3-3 9-2-1 18-8 2-3 3z', right) + line('M7 10l5 1m-5 4l5 1m8-5l4-1m-4 6l4-1', C.ink, 1.6);
const smallBook = () => group(book(), 'translate(2 0) scale(.83)');
const connector = () => path('M11 17h5a3 3 0 010 6h-5a3 3 0 010-6z', C.gold) + path('M17 17h5a3 3 0 010 6h-5', C.gold) + line('M13 20h7');
const textPair = () => path('M2 4l11 1-1 22-10-1z', C.paper) + path('M20 4l10-1-1 23-10 1z', C.blue) + line('M5 9h5m-5 5h5m12-5h5m-5 5h4', C.ink, 1.7);
const rightArrow = (fill = C.primary) => path('M3 14h15V8l11 10-11 9v-7H3z', fill);
const history = () => path('M12 4L3 13l9 8v-6h5c5 0 7 4 6 9 4-2 7-6 5-11-2-5-7-6-12-6h-4z', C.sand);
const backSmall = () => path('M17 15l-6 6 6 6v-4h5c4 0 5 3 4 6 4-3 4-8 0-10-2-1-4-1-9-1z', C.gold);
const gear = (fill = C.sand) => path('M13 3h6l1 4 3 1 4-1 3 5-3 3v3l3 3-3 5-4-1-3 1-1 4h-6l-1-4-3-1-4 1-3-5 3-3v-3l-3-3 3-5 4 1 3-1z', fill) + circle(16, 16, 5, C.paper);
const smallGear = () => group(gear(C.gold), 'translate(15 15) scale(.47)');
const pencil = () => group(path('M5 24L22 5l6 5-17 19-7 1z', C.gold) + path('M22 5l2-2c1-1 2-1 3 0l3 3c1 1 1 2 0 3l-2 2z', C.clay) + path('M5 24l6 5-7 1z', C.paper) + line('M9 23L24 7'), 'translate(1 1) scale(.92)');
const db = () => path('M5 7h21v17c0 5-21 5-21 0z', C.sand) + `<ellipse cx="15.5" cy="7" rx="10.5" ry="4" fill="${C.paper}" stroke="${C.ink}" stroke-width="1.7"/>` + line('M5 15c0 5 21 5 21 0m-21 7c0 5 21 5 21 0');
const tag = (fill = C.gold) => path('M3 6h13l14 13-12 11L3 17z', fill) + circle(9, 12, 2, C.paper);
const refresh = () => path('M26 5v9h-9l3-3a8 8 0 00-12 4l-5-1A13 13 0 0123 7z', C.blue) + path('M6 27v-9h9l-3 3a8 8 0 0012-4l5 1a13 13 0 01-20 7z', C.gold);
const dictionary = (fill = C.blue) => path('M7 4h19v24H7c-5 0-5-6 0-6h19', fill) + path('M7 4c-5 0-5 5 0 5h1v14', fill) + line('M7 25h16', C.paper, 2.4);
const seal = (fill = C.blue) => path('M10 3h12l7 7v12l-7 7H10l-7-7V10z', fill);
const smallArrowOut = () => path('M19 16h6v-4l4.5 6-4.5 6v-4h-6z', C.gold);
const sparkle = () => path('M23 3l2 5 4.5 2-4.5 2-2 5-2-5-5-2 5-2z', C.gold);

add('app-ibiblia', book() + connector());
add('project-new', paper() + badge(plus(), C.green));
add('project-open', folder());
add('project-close', folder() + badge(cross(), C.clay));
add('project-save', disk());
add('project-save-as', group(disk(), 'translate(0 0) scale(.9)') + group(pencil(), 'translate(13 13) scale(.57)'));
add('project-settings', paper() + smallGear());
add('project-export', paper() + smallArrowOut());
add('association-auto', textPair() + connector() + group(sparkle(), 'translate(-1 -1) scale(.72)'));
add('association-clear', textPair() + path('M11 18h3v5h-4a3 3 0 010-6', C.clay) + path('M23 18h-3v5h4a3 3 0 000-6', C.clay) + line('M16 16l2-3m-2 14l2-3', C.red, 2));
add('association-rollback', textPair() + backSmall());
add('history-back', history());
add('history-forward', group(history(), 'translate(32 0) scale(-1 1)'));
add('verse-previous', smallBook() + group(rightArrow(), 'translate(30 14) scale(-.62 .62)'));
add('verse-next', smallBook() + group(rightArrow(), 'translate(12 14) scale(.62)'));
add('verse-first', smallBook() + group(rightArrow(), 'translate(31 14) scale(-.55 .62)') + line('M12.5 19v11', C.primary, 2.4));
add('verse-last', smallBook() + group(rightArrow(), 'translate(8 14) scale(.55 .62)') + line('M26.5 19v11', C.primary, 2.4));
add('app-exit', path('M4 5h17v23H4z', C.sand) + path('M4 5l12-2v27L4 28z', C.paper) + circle(12, 17, 1, C.ink, C.ink, 0) + group(rightArrow(C.clay), 'translate(14 5) scale(.56 .7)'));
add('projects-merge', group(paper(), 'translate(-.7 0) scale(.54)') + group(paper(C.gold), 'translate(14.5 0) scale(.54)') + path('M7 19v4h7v-4h4v4h7v-4h4v8H18v2h-4v-2H3v-8z', C.blue));
add('association-memory-rebuild', group(db(), 'translate(0 0) scale(.82)') + group(refresh(), 'translate(14 14) scale(.52)'));
add('sync-theword-verse', group(book(), 'translate(4 5) scale(.73)') + path('M3 10V3h7L8 5h14l-2-3h4l5 7-7 4V9H7l3 3z', C.gold) + path('M29 22v7h-7l2-2H10l2 2H8l-5-6 7-5v5h14l-3-3z', C.blue));
add('tags-show', tag());
add('patch-create', paper(C.sand) + path('M12 10h6v5h5v6h-5v5h-6v-5H7v-6h5z', C.gold) + badge(plus(), C.green));
add('patch-apply', paper(C.sand) + path('M12 10h6v5h5v6h-5v5h-6v-5H7v-6h5z', C.gold) + badge(check(), C.green));
add('database-vacuum', db() + path('M16 17v6h-4l6 6 6-6h-4v-6z', C.blue));
add('keyboard-shortcuts', path('M4 7h24l2 19H2z', C.sand) + rect(5, 10, 22, 13, C.paper, 0) + line('M9 13h1m4 0h1m4 0h1m4 0h1M9 17h1m4 0h1m4 0h1m4 0h1M11 21h10', C.ink, 2));
add('layout-reset', rect(3, 4, 26, 24, C.paper) + rect(3, 4, 26, 6, C.blue, 0) + path('M3 10h8v18H3z', C.sand) + line('M12 18h17') + group(backSmall(), 'translate(10 10) scale(.6)'));
add('guide-mode', path('M9 4h14l6 12-13 13L3 16z', C.paper) + path('M20 9l-3 12-5 2 3-12z', C.gold) + line('M16 4V2m12 14h2M4 16H2'));
add('theword-highlighter-colors', path('M3 28h26', C.gold, 3, C.gold) + path('M7 20L21 4l7 6-14 16-6 1z', C.gold) + path('M7 20l7 6-6 1-4 3H2z', C.ink) + line('M18 8l7 6') + circle(24, 24, 4, C.blue) + circle(16, 27, 3, C.clay));
add('llm-options', path('M6 4h20l-1 22H5z', C.paper) + path('M13 10h7l3 6-3 6h-7l-4-6z', C.purple) + line('M16 7v3m0 12v3M6 16h3m14 0h4') + sparkle());
add('preferences', gear());
add('file-association-register', paper() + group(connector(), 'translate(-2 -4) scale(.83)') + badge(plus(), C.green));
add('file-association-unregister', paper() + group(connector(), 'translate(-2 -4) scale(.83)') + badge(line('M20.5 24h7', C.ink, 2), C.clay));
add('text-load', plainPaper() + line('M9 12h10m-10 5h7m-7 5h7') + path('M21 3h5v11h3.5l-6 7-6-7H21z', C.blue));
add('text-clear', paper() + badge(cross(), C.clay));
add('dictionary-strong-select', dictionary() + line('M12 10h10m-10 6h10m-7-9l-2 12m8-12l-2 12', C.ink, 1.8));
add('dictionary-morphology-select', dictionary(C.green) + line('M17 8v6m-5 6v-6h10v6') + circle(17, 9, 2.5, C.paper) + circle(12, 19, 2.5, C.paper) + circle(22, 19, 2.5, C.paper));
add('font-select', path('M5 4h23v24H3z', C.paper) + path('M9 23L16 8l7 15h-4l-1-4h-5l-1 4z', C.primary) + path('M14 16h3l-1.5-4z', C.paper, 0, 'none'));
add('checkbox-unchecked', path('M7 7h21v22H7z', C.shadow, 0, 'none') + rect(4, 4, 22, 22, C.paper, .5));
add('checkbox-checked', path('M7 7h21v22H7z', C.shadow, 0, 'none') + rect(4, 4, 22, 22, C.green, .5) + line('M9 15l5 5L23 9', C.ink, 2.8));
add('checkbox-partial', path('M7 7h21v22H7z', C.shadow, 0, 'none') + rect(4, 4, 22, 22, C.gold, .5) + line('M9 15h13', C.ink, 3));
add('ai-pending', seal(C.sand) + circle(16, 16, 8, C.paper) + line('M16 10v7l4 2'));
add('ai-processing', seal(C.paper) + line('M16 7a9 9 0 109 9', C.purple, 4) + path('M24 5l1.6 4.4 3.9 1.6-3.9 1.6L24 17l-1.6-4.4L18 11l4.4-1.6z', C.gold));
add('ai-success', seal(C.green) + line('M8 16l6 6L25 9', C.ink, 3));
add('ai-error', seal(C.clay) + line('M10 10l12 12m0-12L10 22', C.ink, 3));
add('tab-source', book(C.blue, C.paper) + path('M5 4l6 1v12l-3-3-3 2z', C.primary));
add('tab-destination', book(C.paper, C.green) + path('M21 6l5-1v13l-2-2-3 3z', C.green));
add('tab-reference', book(C.paper, C.paper) + path('M21 6l5-1v13l-2-2-3 3z', C.gold));
add('comments', path('M5 6h25v17H18l-9 7v-7H5z', C.shadow, 0, 'none') + path('M2 3h26v18H15l-9 7v-7H2z', C.paper) + line('M7 9h16M7 14h12'));
add('status-review', paper() + group(path('M9 16l6 6L27 7', 'none', 4, C.green), 'translate(10 12) scale(.62)'));
add('help', path('M8 3h17l4 4v18l-4 4H7l-4-4V8z', C.blue) + line('M11 11c0-7 12-7 12 0 0 4-7 4-7 9', C.ink, 2.7) + circle(16, 24, 1.4, C.ink, C.ink, 0));
add('about', circle(16, 16, 13, C.paper) + path('M12 14h6v11h-6v-2h2v-7h-2z', C.primary, 0, 'none') + circle(16, 9, 2, C.primary, C.primary, 0));
add('confirm', path('M4 15l8 7L27 4l3 5-18 21L2 20z', C.green));
add('cancel', path('M7 3l9 9 9-9 5 5-9 8 9 9-5 5-9-9-9 9-5-5 9-9-9-8z', C.clay));
add('add', path('M12 3h8v9h9v8h-9v9h-8v-9H3v-8h9z', C.green));
add('remove', path('M3 12h26v8H3z', C.clay));
add('edit', pencil());
add('browse', folder() + circle(11, 21, 1.3, C.ink, C.ink, 0) + circle(16, 21, 1.3, C.ink, C.ink, 0) + circle(21, 21, 1.3, C.ink, C.ink, 0));
add('refresh', refresh());
add('search', path('M20 18l9 8-3.5 3.5L17 20z', C.gold) + circle(13, 12, 9.5, C.paper) + circle(13, 12, 6.5, C.blue) + line('M9 11l3-3', C.white, 2));
add('print', rect(7, 2, 18, 11, C.paper, 0) + path('M5 10h23l2 3v11H2V13z', C.blue) + rect(7, 19, 18, 11, C.paper, 0) + line('M11 23h10m-10 4h7') + circle(24, 15, 1.1, C.ink, C.ink, 0));
add('copy', path('M4 3h18v22H4z', C.sand) + path('M10 8h14l5 5v16H10z', C.paper) + path('M24 8v6h5', C.blue) + line('M14 18h10m-10 5h8'));
add('paste', path('M6 5h21v25H6z', C.shadow, 0, 'none') + rect(3, 4, 23, 24, C.sand, .5) + rect(7, 8, 16, 17, C.paper, 0) + path('M10 4h3V2h5v2h3v5H10z', C.blue) + line('M11 14h8m-8 5h8'));
add('stop', path('M6 5h23v23H6z', C.shadow, 0, 'none') + rect(3, 3, 24, 24, C.clay, 1) + rect(10, 10, 10, 10, C.paper, 0));
add('translate', path('M3 3h17v19H3z', C.paper) + path('M14 11h15v19H14z', C.blue) + line('M6 17l5-11 5 11m-8-4h6', C.ink, 1.8) + line('M18 17h8m-4-3v3m3 0c-1 5-3 7-7 9m1-7c1 4 4 6 7 7', C.ink, 1.7));
add('link', group(rect(2, 10, 17, 12, C.blue, 6) + rect(13, 10, 17, 12, C.gold, 6) + rect(6, 14, 9, 4, C.paper, 2) + rect(17, 14, 9, 4, C.paper, 2) + line('M12 16h8', C.ink, 2.4), 'rotate(-40 16 16)'));
add('export-destination-strongs', paper(C.green) + group(tag(), 'translate(5 10) scale(.51)') + smallArrowOut());
add('export-interlinear', plainPaper() + line('M9 13h3m4 0h6M9 19h3m4 0h6', C.primary, 2) + line('M9 16h3m4 0h6M9 22h3m4 0h6', C.clay, 1.8) + smallArrowOut());

module.exports = {
  id: 'folio',
  name: 'Fólio',
  subtitle: 'Papel ilustrado · traço de tinta · acentos quentes',
  description: 'Uma família editorial de páginas assimétricas, recortes de papel e contornos de tinta azul-escura. Creme, azul lavado e dourado dão calor aos comandos; terracota e verde distinguem remoção e confirmação.',
  palette,
  icons
};
