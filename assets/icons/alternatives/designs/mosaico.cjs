'use strict';

// Mosaico: original geometric artwork, drawn on a 32-unit grid.
// Flat planes and open counters intentionally replace outlines and shading.
const C = {
  blue: '#2E5FE8', gold: '#E5AF36', teal: '#098D79', ink: '#283650',
  red: '#DA4B43', purple: '#7654B3', paper: '#FFFFFF', pale: '#DCE5FF',
};
const p = (d, fill = C.blue, extra = '') => `<path d="${d}" fill="${fill}" ${extra}/>`;
const r = (x, y, w, h, fill = C.blue, radius = 0.8) => `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${radius}" fill="${fill}"/>`;
const c = (x, y, radius, fill = C.blue) => `<circle cx="${x}" cy="${y}" r="${radius}" fill="${fill}"/>`;
const group = (body, transform) => `<g transform="${transform}">${body}</g>`;
const white = C.paper;
const frame = (color = C.blue) => p('M7 3H25L29 7V25L25 29H7L3 25V7ZM7 7V25H25V7Z', color, 'fill-rule="evenodd"');
const tile = (color = C.blue) => p('M7 3H25L29 7V25L25 29H7L3 25V7Z', color);
const plus = (color = white) => p('M14 7H18V14H25V18H18V25H14V18H7V14H14Z', color);
const check = (color = white) => p('M6 16L10 12L14 17L23 7L27 11L14 26Z', color);
const cross = (color = white) => p('M8 5L16 13L24 5L27 8L19 16L27 24L24 27L16 19L8 27L5 24L13 16L5 8Z', color);
const arrow = (color = C.gold) => p('M2 12H18V5L30 16L18 27V20H2Z', color);
const badge = (body, color = C.teal) => r(17, 17, 13, 13, color, 1.5) + group(body, 'translate(18 18) scale(.36)');
const rows = (color = white, x = 8, y = 12, w = 12) => r(x, y, w, 3, color, 0) + r(x, y + 6, w - 3, 3, color, 0);
const doc = (color = C.blue) => p('M6 2H21L28 9V30H6Z', color) + p('M21 2V9H28Z', C.gold) + rows(white, 10, 14, 13);
const folder = () => p('M2 6H13L17 10H29V27H2Z', C.blue) + p('M2 13H30L26 28H2Z', C.gold);
const disk = () => p('M3 3H24L29 8V29H3Z', C.blue) + r(8, 3, 13, 9, white, 0) + r(17, 3, 4, 7, C.ink, 0) + r(8, 19, 16, 10, C.gold, 0) + r(11, 22, 10, 3, white, 0);
const book = () => p('M2 4H12L16 8V29L12 26H2Z', C.blue) + p('M20 4H30V26H20L16 29V8Z', C.teal) + r(5, 9, 7, 3, white, 0) + r(5, 15, 7, 3, white, 0) + r(20, 9, 7, 3, white, 0) + r(20, 15, 7, 3, white, 0);
const panels = () => r(2, 4, 10, 24, C.blue) + r(20, 4, 10, 24, C.teal) + r(5, 8, 5, 3, white, 0) + r(22, 8, 5, 3, white, 0) + r(5, 21, 5, 3, white, 0) + r(22, 21, 5, 3, white, 0);
const chain = (color = C.gold) => p('M3 11H17V16H13V15H7V21H13V19H17V25H3ZM15 7H29V21H15V16H19V17H25V11H19V13H15Z', color) + r(11, 14, 10, 4, color, 0);
const history = (color = C.ink) => p('M12 3V9H19C26 9 30 13 30 20C30 27 25 30 18 30H15V25H19C23 25 25 24 25 20C25 16 23 14 19 14H12V20L2 11Z', color);
const spark = (color = C.gold) => p('M16 2L20 11L30 15L20 19L16 29L12 19L2 15L12 11Z', color);
const gear = (color = C.ink) => p('M12 2H20V6L24 8L28 6L32 13L28 15V19L31 21L27 28L23 26L20 28V32H12V28L8 26L4 28L0 21L4 19V15L1 13L5 6L9 8L12 6ZM12 12V22H22V12Z', color, 'fill-rule="evenodd"');
const pencil = () => p('M3 21L19 5L27 13L11 29H3Z', C.gold) + p('M19 5L22 2L30 10L27 13Z', C.ink) + p('M3 21L11 29H3Z', C.blue) + p('M8 21L20 9L23 12L11 24Z', white);
const database = () => p('M4 4H26V28H4Z', C.blue) + r(4, 4, 22, 5, C.gold, 0) + r(7, 12, 16, 3, white, 0) + r(7, 20, 16, 3, white, 0);
const dictionary = (color = C.blue) => r(4, 3, 24, 26, color) + r(4, 3, 5, 26, C.ink, 0) + p('M19 3H24V15L21.5 12L19 15Z', C.gold);
const tag = (color = C.gold) => p('M3 4H17L30 17L17 30L3 16ZM7 8V13H12V8Z', color, 'fill-rule="evenodd"');
const magnifier = () => p('M12 2A10 10 0 1 1 12 22A10 10 0 1 1 12 2ZM12 7A5 5 0 1 0 12 17A5 5 0 1 0 12 7Z', C.blue, 'fill-rule="evenodd"') + p('M18 18L22 18L30 26L26 30L18 22Z', C.gold);
const patch = () => p('M6 3H16V7C16 10 22 10 22 7V3H28V13H24C21 13 21 19 24 19H28V29H18V25C18 22 12 22 12 25V29H6Z', C.purple);
const down = (color = C.gold) => group(arrow(color), 'translate(32 0) rotate(90)');
const refresh = (color = C.teal) => p('M27 3V13H17L20 9C14 5 7 9 7 15H2C2 5 14 0 23 5ZM5 29V19H15L12 23C18 27 25 23 25 17H30C30 27 18 32 9 27Z', color);
const icons = {};
const add = (name, body) => { icons[name] = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">${body}</svg>`; };

add('app-ibiblia', book() + group(chain(), 'translate(3 7) scale(.82)'));
add('project-new', doc() + badge(plus(), C.teal));
add('project-open', folder());
add('project-close', folder() + badge(cross(), C.red));
add('project-save', disk());
add('project-save-as', group(disk(), 'scale(.85)') + group(pencil(), 'translate(15 15) scale(.52)'));
add('project-settings', group(doc(), 'translate(0 1) scale(.85)') + group(gear(), 'translate(15 15) scale(.46)'));
add('project-export', group(doc(), 'translate(-1 1) scale(.84)') + group(arrow(C.teal), 'translate(10 12) scale(.64)'));

add('association-auto', panels() + r(10, 14, 12, 4, C.gold, 0) + group(spark(), 'translate(10 2) scale(.38)'));
add('association-clear', panels() + p('M9 14H15L12 18H9ZM20 14H24V18H17Z', C.gold) + p('M17 10H21L14 22H10Z', C.red));
add('association-rollback', group(panels(), 'translate(0 1) scale(1 .7)') + group(history(C.gold), 'translate(5 8) scale(.8 .7)'));
add('history-back', history());
add('history-forward', group(history(), 'translate(32 0) scale(-1 1)'));
const versePage = () => p('M5 2H22L27 7V30H5Z', C.blue) + r(9, 6, 11, 3, white, 0) + r(9, 24, 14, 3, white, 0);
add('verse-previous', versePage() + group(arrow(), 'translate(28 8) scale(-.85 .5)'));
add('verse-next', versePage() + group(arrow(), 'translate(3 8) scale(.85 .5)'));
add('verse-first', versePage() + r(2, 12, 3, 10, C.gold, 0) + group(arrow(), 'translate(29 8) scale(-.69 .5)'));
add('verse-last', versePage() + r(27, 12, 3, 10, C.gold, 0) + group(arrow(), 'translate(2 8) scale(.69 .5)'));
add('app-exit', p('M3 2H19V7H8V25H19V30H3Z', C.ink) + group(arrow(C.red), 'translate(11 6) scale(.66)'));
add('projects-merge', p('M2 3H12V10H2ZM20 3H30V10H20Z', C.blue) + p('M6 11H10V15H22V11H26V19H18V23H22L16 30L10 23H14V19H6Z', C.gold));
add('association-memory-rebuild', group(database(), 'translate(2 0) scale(.8)') + group(refresh(), 'translate(12 12) scale(.56)'));
add('sync-theword-verse', group(book(), 'translate(7 7) scale(.56)') + p('M3 9H7V5H22V2L30 7L22 12V9H7V13H3ZM29 23H25V27H10V30L2 25L10 20V23H25V19H29Z', C.gold));
add('tags-show', tag() + p('M17 11L20 14L15 19L20 24L17 27L9 19Z', C.ink));
add('patch-create', patch() + badge(plus(), C.teal));
add('patch-apply', patch() + badge(check(), C.teal));
add('database-vacuum', group(database(), 'translate(2 4) scale(.8)') + p('M4 2H8V7H11L6 13L2 7H4ZM24 30H28V25H30L26 19L22 25H24Z', C.teal));

let keyRows = '';
for (const y of [10, 16]) for (const x of [6, 12, 18, 24]) keyRows += r(x, y, 3, 3, white, 0);
add('keyboard-shortcuts', r(2, 6, 28, 22, C.ink, 1.5) + keyRows + r(9, 22, 14, 3, C.gold, 0));
add('layout-reset', r(2, 3, 28, 5, C.ink, 0) + r(2, 10, 7, 19, C.blue, 0) + r(11, 10, 19, 8, C.teal, 0) + r(11, 20, 19, 9, C.gold, 0) + group(history(white), 'translate(17 18) scale(.43)'));
add('guide-mode', p('M4 2H25L29 6V26H4Z', C.blue) + r(8, 7, 13, 3, white, 0) + r(8, 13, 8, 3, white, 0) + p('M16 15L29 24L23 26L20 30Z', C.gold));
add('theword-highlighter-colors', p('M4 19L20 3L29 12L13 28H4Z', C.gold) + p('M20 3L22 2L30 10L29 12Z', C.ink) + p('M4 19L13 28H4Z', C.teal) + r(3, 28, 10, 2, C.blue, 0) + r(14, 28, 8, 2, C.red, 0) + r(23, 28, 7, 2, C.purple, 0));
add('llm-options', group(spark(C.purple), 'translate(1 2) scale(.72)') + p('M21 7H24V30H21ZM27 7H30V30H27Z', C.ink) + r(18, 13, 9, 5, C.gold, 0) + r(24, 22, 6, 5, C.teal, 0));
add('preferences', r(4, 3, 4, 26, C.ink, 0) + r(14, 3, 4, 26, C.ink, 0) + r(24, 3, 4, 26, C.ink, 0) + r(2, 7, 9, 6, C.blue, 0) + r(11, 21, 10, 6, C.gold, 0) + r(21, 12, 9, 6, C.teal, 0));
add('file-association-register', group(doc(), 'translate(-1 1) scale(.9)') + group(chain(C.teal), 'translate(10 12) scale(.68)'));
add('file-association-unregister', group(doc(), 'translate(-1 1) scale(.9)') + group(chain(C.red), 'translate(10 12) scale(.68)') + p('M24 18H28L22 28H18Z', white));
add('text-load', doc() + group(down(C.teal), 'translate(17 14) scale(.49)'));
add('text-clear', doc() + p('M17 21L24 14L30 20L23 27H17L14 24Z', C.red) + p('M14 24L17 21L23 27H17Z', C.gold));
add('dictionary-strong-select', dictionary() + p('M12 11H17V16H21V21H17V26H12V21H10V16H12ZM15 16V21H18V16Z', white, 'fill-rule="evenodd"'));
add('dictionary-morphology-select', dictionary(C.teal) + p('M17 10H21V16H26V25H22V20H15V25H11V16H17Z', white));
add('font-select', p('M10 3H17L27 29H21L19 23H8L6 29H2ZM13 9L10 18H17Z', C.blue, 'fill-rule="evenodd"') + r(23, 4, 7, 4, C.gold, 0) + r(25, 8, 4, 11, C.gold, 0));
add('checkbox-unchecked', frame(C.ink));
add('checkbox-checked', tile(C.blue) + check());
add('checkbox-partial', frame(C.blue) + r(10, 13, 12, 6, C.gold, 0));
add('ai-pending', c(16, 16, 14, C.gold) + p('M14 7H18V14H24V18H14Z', C.ink));
add('ai-processing', p('M16 2A14 14 0 1 1 2 16H7A9 9 0 1 0 16 7Z', C.purple) + group(spark(C.gold), 'translate(0 0) scale(.5)'));
add('ai-success', tile(C.teal) + check());
add('ai-error', tile(C.red) + cross());

add('tab-source', doc(C.blue) + p('M3 9H6V27H3Z', C.gold));
add('tab-destination', doc(C.teal) + group(arrow(), 'translate(14 17) scale(.55 .43)'));
add('tab-reference', doc(C.purple) + p('M20 13H27V30L23.5 26L20 30Z', C.gold));
add('comments', p('M2 4H30V24H14L5 31V24H2Z', C.blue) + r(7, 10, 18, 3, white, 0) + r(7, 16, 12, 3, white, 0));
add('status-review', p('M2 3H23V29H2Z', C.ink) + r(6, 8, 4, 4, C.gold, 0) + r(13, 8, 6, 4, white, 0) + r(6, 17, 4, 4, C.teal, 0) + r(13, 17, 6, 4, white, 0) + group(magnifier(), 'translate(16 14) scale(.46)'));
add('help', c(16, 16, 14, C.blue) + p('M9 8H22V18H17V21H13V15H18V12H13V14H9Z', white) + r(13, 24, 4, 4, C.gold, 0));
add('about', c(16, 16, 14, C.gold) + r(13, 6, 6, 5, C.ink, 0) + r(13, 14, 6, 12, C.ink, 0));
add('confirm', check(C.teal));
add('cancel', cross(C.ink));
add('add', plus(C.blue));
add('remove', r(4, 13, 24, 6, C.red, 0));
add('edit', pencil());
add('browse', group(folder(), 'translate(0 -3) scale(.9)') + group(magnifier(), 'translate(13 13) scale(.58)'));
add('refresh', refresh());
add('search', magnifier());
add('print', r(6, 2, 20, 8, C.pale, 0) + p('M2 9H30V25H25V30H7V25H2Z', C.ink) + r(7, 20, 18, 10, C.gold, 0) + r(10, 23, 12, 3, white, 0) + r(24, 12, 3, 3, C.teal, 0));
add('copy', r(2, 2, 19, 24, C.gold) + p('M10 8H25L30 13V30H10Z', C.blue) + r(14, 17, 12, 3, white, 0) + r(14, 23, 8, 3, white, 0));
add('paste', r(4, 4, 24, 26, C.gold) + r(8, 10, 16, 16, white, 0) + r(10, 2, 12, 7, C.ink, 0) + r(11, 14, 10, 3, C.blue, 0) + r(11, 20, 7, 3, C.blue, 0));
add('stop', p('M9 2H23L30 9V23L23 30H9L2 23V9Z', C.red) + r(10, 10, 12, 12, white, 0));
add('translate', p('M2 3H16V20H9L4 25V20H2Z', C.blue) + p('M18 11H30V27H28L23 30V27H18Z', C.teal) + p('M5 8H13V11H11V17H8V11H5Z', white) + p('M21 16H27V19H25V21H27V24H21V21H23V19H21Z', white) + group(arrow(C.gold), 'translate(8 0) scale(.48 .4)'));
add('link', chain());
add('export-destination-strongs', group(doc(C.teal), 'translate(-1 1) scale(.82)') + group(tag(C.gold), 'translate(15 15) scale(.49)') + p('M17 10H24V6L30 12L24 18V14H17Z', C.blue));
add('export-interlinear', r(2, 3, 23, 3, C.blue, 0) + r(2, 8, 15, 3, C.gold, 0) + r(2, 14, 23, 3, C.blue, 0) + r(2, 19, 15, 3, C.gold, 0) + r(2, 25, 13, 3, C.blue, 0) + group(arrow(C.teal), 'translate(14 18) scale(.55 .5)'));

module.exports = {
  id: 'mosaico',
  name: 'Mosaico',
  subtitle: 'Geometria forte · cores sólidas · leitura direta',
  description: 'Planos geométricos de cores sólidas, recortes amplos e proporções robustas. Azul, dourado e verde se encaixam como peças de um mesmo conjunto; os símbolos preservam leitura direta em tamanhos pequenos.',
  palette: C,
  icons,
};
