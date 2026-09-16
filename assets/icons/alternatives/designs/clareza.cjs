'use strict';

// Original iBiblia drawings: an open, monoline alternative to the active family.
const palette = {
  blue: '#2563EB', ink: '#3D5268', gold: '#B8811E', teal: '#128575',
  red: '#C94650', purple: '#7653B5', paleBlue: '#EAF1FF',
  paleGold: '#FFF2D3', paleTeal: '#E5F4EF', paleRed: '#FCEAEC', white: '#FFFFFF',
};
const c = palette;
const path = (d, stroke = c.ink, fill = 'none', width = 2) =>
  `<path d="${d}" fill="${fill}" stroke="${stroke}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"/>`;
const rect = (x, y, w, h, stroke = c.blue, fill = 'none', r = 2) =>
  `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${r}" fill="${fill}" stroke="${stroke}" stroke-width="2"/>`;
const circle = (x, y, r, stroke = c.blue, fill = 'none', width = 2) =>
  `<circle cx="${x}" cy="${y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${width}"/>`;
const group = (body, transform) => `<g transform="${transform}">${body}</g>`;
const svg = body => `<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32">${body}</svg>`;
const icons = {};
const add = (id, body) => { icons[id] = svg(body); };
const plus = (x = 24, y = 24, stroke = c.teal, size = 3) =>
  path(`M${x - size} ${y}h${size * 2}M${x} ${y - size}v${size * 2}`, stroke);
const cross = (x = 24, y = 24, stroke = c.red, size = 2.8) =>
  path(`M${x - size} ${y - size}l${size * 2} ${size * 2}M${x + size} ${y - size}l${-size * 2} ${size * 2}`, stroke);
const check = (x = 24, y = 24, stroke = c.teal) =>
  path(`M${x - 3.5} ${y}l2.5 2.5 4.5-5`, stroke);
const badge = (body, stroke = c.blue, fill = c.white) => circle(24, 24, 6, stroke, fill) + body;
const document = (stroke = c.blue, fill = c.white) =>
  path('M7 3h11l7 7v17H7z', stroke, fill) + path('M18 3v7h7', stroke) + path('M11 14h9M11 19h6', stroke);
const folder = (front = true) =>
  path('M3 24V8a2 2 0 0 1 2-2h7l3 4h12v14', c.gold, c.paleGold) +
  (front ? path('M3 14h26l-3 12H6z', c.gold, c.white) : path('M3 24h24', c.gold));
const disk = () =>
  path('M5 4h19l4 4v20H5z', c.blue, c.white) +
  path('M10 4v8h12V4', c.blue, c.paleBlue) + rect(10, 19, 13, 9, c.blue, c.paleBlue, 1) + path('M18 7v2', c.blue);
const book = (stroke = c.blue, small = false) => {
  const drawing = path('M3 6c5-2 9-1 13 2 4-3 8-4 13-2v20c-5-2-9-1-13 2-4-3-8-4-13-2z', stroke, c.white) +
    path('M16 8v20', stroke) + path('M7 11l5 1M7 16l5 1M20 12l5-1M20 17l5-1', stroke);
  return small ? group(drawing, 'translate(3 0) scale(.8)') : drawing;
};
const linkedPages = () =>
  path('M3 5c5-1 9 0 13 3 4-3 8-4 13-3v21c-5-1-9 0-13 3-4-3-8-4-13-3z', c.blue, c.white) +
  path('M16 8v6M7 10l5 1M20 11l5-1M7 15l3 .5M22 15.5l3-.5', c.blue) +
  path('M14 18h-3a3 3 0 0 0 0 6h3M18 18h3a3 3 0 0 1 0 6h-3M12 21h8', c.gold);
const panelPair = () =>
  rect(3, 5, 10, 22, c.blue, c.paleBlue, 2) + rect(19, 5, 10, 22, c.teal, c.paleTeal, 2) +
  path('M6 10h4M6 22h4', c.blue) + path('M22 10h4M22 22h4', c.teal);
const panelLink = (broken = false) =>
  path('M12 13h-1a3 3 0 0 0 0 6h2M20 13h1a3 3 0 0 1 0 6h-2', broken ? c.red : c.gold, c.white) +
  (broken ? path('M14 12l4 8', c.red) : path('M12 16h8', c.gold));
const straight = (direction = 1, first = false) => {
  const drawing = path('M11 6h15v20H11', c.blue, c.paleBlue) + path('M15 10h7M15 14h7', c.blue) +
    path('M4 21h16m-5-5 5 5-5 5', c.blue) + (first ? path('M29 17v11', c.blue) : '');
  return direction === 1 ? drawing : group(drawing, 'translate(32 0) scale(-1 1)');
};
const backArrow = () => path('M11 7 4 14l7 7M4 14h14a9 9 0 0 1 9 9v2', c.ink);
const sliders = (stroke = c.blue) =>
  path('M6 8h20M6 16h20M6 24h20', stroke) +
  circle(12, 8, 3, stroke, c.white) + circle(21, 16, 3, stroke, c.white) + circle(10, 24, 3, stroke, c.white);
const database = () =>
  path('M5 8v16c0 3 22 3 22 0V8', c.blue, c.white) +
  `<ellipse cx="16" cy="8" rx="11" ry="4" stroke="${c.blue}" stroke-width="2" fill="${c.paleBlue}"/>` +
  path('M5 15c0 5 22 5 22 0M5 21c0 5 22 5 22 0', c.blue);
const sparkle = (x = 23, y = 8, stroke = c.gold) =>
  path(`M${x} ${y - 5}q0 5 5 5-5 0-5 5 0-5-5-5 5 0 5-5z`, stroke, c.paleGold);
const miniTurn = (stroke = c.gold) => path('M21 21h5a4 4 0 0 1 0 8h-3M24 18l-3 3 3 3', stroke);
const patch = () =>
  path('M4 9h7c-2-6 8-6 6 0h7v6c6-2 6 8 0 6v7h-7c2-6-8-6-6 0H4v-7c6 2 6-8 0-6z', c.blue, c.paleBlue);
const dictionary = (stroke = c.blue) =>
  path('M8 4h18v24H8a4 4 0 0 1-4-4V8a4 4 0 0 1 4-4z', stroke, c.white) +
  path('M8 4v20M8 24h18', stroke);
const search = (stroke = c.blue) => circle(13, 13, 9, stroke, c.paleBlue) + path('M20 20l8 8', stroke, 'none', 2.5);
const refresh = (stroke = c.blue) =>
  path('M27 12A11 11 0 0 0 7 8L3 12M3 5v7h7M5 20a11 11 0 0 0 20 4l4-4M29 27v-7h-7', stroke);
const exportArrow = () => path('M16 24h13m-5-5 5 5-5 5', c.teal);

add('app-ibiblia', linkedPages());
add('project-new', document() + badge(plus(), c.teal));
add('project-open', folder());
add('project-close', folder() + badge(cross(), c.red));
add('project-save', disk());
add('project-save-as', disk() + badge(plus(24, 24, c.blue), c.blue));
add('project-settings', document() + circle(24, 24, 6, 'none', c.white, 0) + path('M19 21h10M19 27h10', c.blue) + circle(22, 21, 2, c.blue, c.white) + circle(26, 27, 2, c.blue, c.white));
add('project-export', document() + circle(24, 24, 6, 'none', c.white, 0) + exportArrow());
add('association-auto', panelPair() + panelLink() + circle(24, 8, 6, 'none', c.white, 0) + sparkle(24, 8));
add('association-clear', panelPair() + panelLink(true));
add('association-rollback', panelPair() + panelLink() + circle(24, 24, 6.5, 'none', c.white, 0) + miniTurn());
add('history-back', backArrow());
add('history-forward', group(backArrow(), 'translate(32 0) scale(-1 1)'));
add('verse-previous', straight(-1));
add('verse-next', straight());
add('verse-first', straight(-1, true));
add('verse-last', straight(1, true));
add('app-exit', path('M13 4H5v24h8M17 8v16', c.ink) + path('M12 16h16m-6-6 6 6-6 6', c.red));
add('projects-merge', rect(3, 4, 9, 11, c.blue, c.paleBlue) + rect(20, 4, 9, 11, c.teal, c.paleTeal) + path('M8 18v3l8 5 8-5v-3M16 26v-9m-4 4 4-4 4 4', c.gold));
add('association-memory-rebuild', database() + circle(24, 24, 7, 'none', c.white, 0) + miniTurn(c.teal));
add('sync-theword-verse', group(book(), 'translate(6 5) scale(.63)') + path('M4 12C7 3 18 1 26 8M20 7l6 1-1 6M28 20C25 29 14 31 6 24M12 25l-6-1 1-6', c.gold));
add('tags-show', path('M3 6h13l13 13-10 10L3 13z', c.blue, c.paleBlue) + circle(9, 11, 2, c.blue, c.white) + path('m17 16 6 6M14 19l6 6', c.blue));
add('patch-create', patch() + badge(plus(), c.teal));
add('patch-apply', patch() + badge(check(), c.teal));
add('database-vacuum', database() + circle(24, 24, 7, 'none', c.white, 0) + path('M24 18v4m-3-3 3 3 3-3M24 30v-4m-3 3 3-3 3 3', c.teal));
add('keyboard-shortcuts', rect(3, 7, 26, 18, c.ink, c.white, 3) + path('M7 12h1m4 0h1m4 0h1m4 0h3M7 17h1m4 0h1m4 0h1m4 0h3M10 21h12', c.blue, 'none', 2.2));
add('layout-reset', rect(3, 4, 26, 23, c.blue, c.white, 2) + path('M3 10h26M11 10v17', c.blue) + circle(23, 24, 7, 'none', c.white, 0) + miniTurn());
add('guide-mode', path('M12 22c0-4-5-5-5-10a9 9 0 0 1 18 0c0 5-5 6-5 10z', c.gold, c.paleGold) + path('M12 26h8M14 29h4M12 13l4 4 4-4M16 17v5', c.gold));
add('theword-highlighter-colors', path('m7 21 4-8 10-10 8 8-10 10-8 4z', c.gold, c.paleGold) + path('m11 13 8 8M7 21l4 4', c.gold) + path('M4 29h8', c.blue, 'none', 3) + path('M16 29h5', c.teal, 'none', 3) + path('M25 29h3', c.purple, 'none', 3));
add('llm-options', rect(4, 6, 23, 20, c.purple, c.white, 5) + circle(11, 15, 1.5, c.purple, c.purple) + circle(20, 15, 1.5, c.purple, c.purple) + path('M12 21h7M15 6V3', c.purple) + circle(25, 7, 6, 'none', c.white, 0) + sparkle(25, 7, c.purple));
add('preferences', sliders());
add('file-association-register', document() + badge(path('M22 21h-1a2 2 0 0 0 0 4h2M26 23h1a2 2 0 0 1 0 4h-2M22 24h4', c.teal), c.teal));
add('file-association-unregister', document() + badge(path('M22 21h-1a2 2 0 0 0 0 4M26 23h1a2 2 0 0 1 0 4M23 21l2 6', c.red), c.red));
add('text-load', document() + badge(path('M24 20v8m-3-3 3 3 3-3', c.teal), c.teal));
add('text-clear', document() + badge(cross(), c.red));
add('dictionary-strong-select', dictionary() + path('M15 9l-2 11M21 9l-2 11M12 12h11M11 17h11', c.gold));
add('dictionary-morphology-select', dictionary(c.teal) + path('M17 9v7m-5 4v-4h10v4', c.teal) + circle(17, 10, 2, c.teal, c.paleTeal) + circle(12, 20, 2, c.teal, c.paleTeal) + circle(22, 20, 2, c.teal, c.paleTeal));
add('font-select', path('M4 25 12 5l8 20M7 18h10', c.blue, 'none', 2.3) + path('M22 13h7M25.5 13v12M22 25h7', c.gold));
add('checkbox-unchecked', rect(6, 6, 20, 20, c.ink, c.white, 4));
add('checkbox-checked', rect(6, 6, 20, 20, c.blue, c.paleBlue, 4) + path('M10 16l4 4 8-9', c.blue, 'none', 2.4));
add('checkbox-partial', rect(6, 6, 20, 20, c.blue, c.paleBlue, 4) + path('M11 16h10', c.blue, 'none', 2.4));
add('ai-pending', circle(16, 16, 12, c.gold, c.paleGold) + path('M16 8v8l5 3', c.gold));
add('ai-processing', path('M25 10a11 11 0 1 0 2 10', c.blue, 'none', 2.5) + sparkle(24, 8) + path('M16 12v8M12 16h8', c.blue));
add('ai-success', circle(16, 16, 12, c.teal, c.paleTeal) + path('M10 16l4 4 8-9', c.teal, 'none', 2.4));
add('ai-error', circle(16, 16, 12, c.red, c.paleRed) + cross(16, 16, c.red, 4));
add('tab-source', book() + path('M5 8v15c3 0 5 0 8 1V9c-3-2-5-2-8-1z', 'none', c.paleBlue, 0) + path('M7 12l4 1M7 17l4 1', c.blue));
add('tab-destination', book(c.teal) + path('M19 9v15c3-1 5-1 8-1V8c-3-1-5-1-8 1z', 'none', c.paleTeal, 0) + path('M21 13l4-1M21 18l4-1', c.teal));
add('tab-reference', book() + path('M21 4h5v14l-2.5-2-2.5 2z', c.gold, c.paleGold));
add('comments', path('M5 5h22a2 2 0 0 1 2 2v15a2 2 0 0 1-2 2H13l-7 5v-5H5a2 2 0 0 1-2-2V7a2 2 0 0 1 2-2z', c.blue, c.paleBlue) + path('M8 11h16M8 17h11', c.blue));
add('status-review', document() + circle(23, 22, 5, c.gold, c.paleGold) + path('M27 26l3 3M10 13l2 2 4-5', c.gold));
add('help', circle(16, 16, 12, c.blue, c.paleBlue) + path('M12 11c0-5 9-5 9 0 0 3-5 3-5 7', c.blue) + circle(16, 23, 1, c.blue, c.blue, 1));
add('about', circle(16, 16, 12, c.blue, c.paleBlue) + path('M14 15h2v8M13 23h6', c.blue) + circle(16, 9, 1, c.blue, c.blue, 1));
add('confirm', path('M5 17l7 7L27 8', c.teal, 'none', 3));
add('cancel', path('M7 7l18 18M25 7 7 25', c.ink, 'none', 2.5));
add('add', circle(16, 16, 12, c.blue, c.paleBlue) + plus(16, 16, c.blue, 6));
add('remove', circle(16, 16, 12, c.red, c.paleRed) + path('M10 16h12', c.red));
add('edit', path('m5 21 16-16a3 3 0 0 1 5 5L10 26l-7 3z', c.blue, c.paleBlue) + path('m18 8 6 6M5 21l5 5', c.blue) + path('m3 29 2-8 5 5z', c.gold, c.paleGold));
add('browse', folder() + circle(24, 23, 5, c.blue, c.white) + path('M27 27l3 3', c.blue));
add('refresh', refresh());
add('search', search());
add('print', path('M8 11V3h16v8', c.blue, c.white) + rect(3, 11, 26, 13, c.blue, c.paleBlue, 3) + rect(8, 19, 16, 10, c.blue, c.white, 1) + path('M12 23h8M12 26h5', c.blue) + circle(25, 15, 1, c.teal, c.teal, 1));
add('copy', rect(4, 4, 18, 21, c.ink, c.white) + rect(10, 9, 18, 20, c.blue, c.paleBlue) + path('M15 15h8M15 20h8M15 25h5', c.blue));
add('paste', rect(6, 5, 21, 24, c.blue, c.white, 3) + rect(11, 3, 11, 5, c.gold, c.paleGold, 2) + path('M11 14h11M11 19h11M11 24h7', c.blue));
add('stop', rect(5, 5, 22, 22, c.red, c.paleRed, 5) + rect(11, 11, 10, 10, c.red, c.red, 1));
add('translate', rect(3, 4, 17, 17, c.blue, c.paleBlue, 3) + path('m7 16 4-8 4 8M8 13h6', c.blue) + rect(14, 14, 15, 15, c.teal, c.white, 3) + path('M18 19h7M22 17v2c0 4-2 5-4 6M20 21c1 2 3 4 5 4', c.teal));
add('link', path('m13 11 5-5a6 6 0 0 1 8 8l-5 5M19 21l-5 5a6 6 0 0 1-8-8l5-5', c.blue) + path('M11 21 21 11', c.gold, 'none', 2.5));
add('export-destination-strongs', document(c.teal) + path('M11 14h9M12 11l-1 9M18 11l-1 9', c.teal) + circle(24, 24, 7, 'none', c.white, 0) + exportArrow());
add('export-interlinear', rect(3, 4, 26, 23, c.blue, c.white, 2) + path('M7 9h6m4 0h8M7 13h5m5 0h6', c.blue) + path('M7 18h6m4 0h8M7 22h5', c.teal) + circle(24, 24, 7, 'none', c.white, 0) + exportArrow());

module.exports = {
  id: 'clareza',
  name: 'Clareza',
  subtitle: 'Contornos leves · cor pontual · ritmo aberto',
  description: 'Traços arredondados de espessura constante, espaços abertos e pequenos acentos de cor. O desenho enfatiza leitura rápida e discrição nas barras de ferramentas.',
  palette,
  icons,
};
