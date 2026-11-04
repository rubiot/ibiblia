'use strict';

// Original vector studies. These are proposals, never runtime icon resources.
const svg = (body) => `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">${body}</svg>`;

module.exports = [
  {
    id: '01-pergaminho-elo',
    title: 'Pergaminho e elo',
    subtitle: 'Um texto antigo, uma ligação viva',
    description: 'Um pergaminho vertical recebe o elo azul e dourado no centro. As duas bordas enroladas tornam o suporte antigo reconhecível, enquanto o elo mantém a associação de textos como gesto principal.',
    svg: svg(`
      <path fill="#22344D" d="M21 5h26c7 0 12 4 12 11v6h-8v24c0 8-5 13-13 13H16C9 59 5 55 5 48v-7h10V15c0-6 2-10 6-10Z"/>
      <path fill="#D49A28" d="M22 8h25c5 0 9 3 9 8v3h-8v-3c0-3-2-5-5-5H22Z"/>
      <path fill="#FFF4DC" d="M23 8h20c-2 2-3 4-3 8v3h8v27c0 6-4 10-10 10H18V15c0-4 2-7 5-7Z"/>
      <path fill="#F0D598" d="M40 19h8v27c0 6-4 10-10 10h-8c7-2 10-7 10-14Z"/>
      <path fill="#D49A28" d="M8 44h16v4c0 6 4 8 8 8H16c-5 0-8-3-8-8Z"/>
      <path fill="#FFE8AC" d="M8 44h16v4H8Z"/>
      <path fill="none" stroke="#2563EB" stroke-width="4.5" stroke-linecap="round" d="m29 34-2 2a6 6 0 0 1-8.5-8.5l6-6a6 6 0 0 1 8.5 0"/>
      <path fill="none" stroke="#D49A28" stroke-width="4.5" stroke-linecap="round" d="m32 26 2-2a6 6 0 0 1 8.5 8.5l-6 6a6 6 0 0 1-8.5 0"/>
      <path fill="none" stroke="#2563EB" stroke-width="4.5" stroke-linecap="round" d="m27 33 8-8"/>
    `),
  },
  {
    id: '02-pergaminhos-paralelos',
    title: 'Pergaminhos paralelos',
    subtitle: 'Origem e tradução, lado a lado',
    description: 'Dois pergaminhos estreitos se encontram por uma ponte contínua. O azul identifica uma leitura e o dourado a outra; os rolos em extremos opostos sugerem duas tradições textuais que passam a ser consultadas juntas.',
    svg: svg(`
      <path fill="#2563EB" d="M14 5h16v45c0 6-4 10-10 10H6v-9h5V18H5v-4c0-5 4-9 9-9Z"/>
      <path fill="#173F96" d="M6 51h14c4 0 7-2 10-5v4c0 6-4 10-10 10H6Z"/>
      <path fill="#FFF4DC" d="M16 9h10v38c-3 3-6 4-11 4V17c0-3 0-6 1-8Z"/>
      <path fill="#A9C7FF" d="M14 9c-3 0-5 2-5 5h5Z"/>
      <path fill="#D49A28" d="M44 4h14v9h-5v33h6v4c0 5-4 9-9 9H34V14c0-6 4-10 10-10Z"/>
      <path fill="#A87317" d="M34 14c0-6 4-10 10-10h14v9H44c-4 0-7 2-10 5Z"/>
      <path fill="#FFF4DC" d="M38 17c3-3 6-4 11-4v34c0 3 0 6-1 8H38Z"/>
      <path fill="#FFE5A0" d="M50 55c3 0 5-2 5-5h-5Z"/>
      <path fill="none" stroke="#2563EB" stroke-width="3" stroke-linecap="round" d="M19 21h3m-3 6h3m-3 16h3"/>
      <path fill="none" stroke="#D49A28" stroke-width="3" stroke-linecap="round" d="M42 21h3m-3 16h3m-3 6h3"/>
      <path fill="#22344D" d="M22 28h20a6 6 0 0 1 0 12H22a6 6 0 0 1 0-12Z"/>
      <path fill="#FFF4DC" d="M22 31h20a3 3 0 0 1 0 6H22a3 3 0 0 1 0-6Z"/>
      <path fill="#D49A28" d="M28 28h8v12h-8Z"/>
    `),
  },
  {
    id: '03-faixa-leitura',
    title: 'Faixa de leitura',
    subtitle: 'O iBiblia gravado no pergaminho',
    description: 'Um pergaminho horizontal vira uma faixa azul, com a inicial i desenhada por formas simples. As pontas douradas enroladas e a dobra clara dão personalidade ao contorno; a composição reduzida funciona como marca compacta.',
    svg: svg(`
      <path fill="#22344D" d="M12 10h38c6 0 10 4 10 10v26c0 5-4 9-9 9H14C8 55 4 51 4 45V20c0-6 3-10 8-10Z"/>
      <path fill="#D49A28" d="M13 13h36c4 0 8 3 8 7v22H12V20c0-3 0-5 1-7Z"/>
      <path fill="#FFF4DC" d="M13 13h36c4 0 8 3 8 7v4H16v21H7V21c0-5 2-8 6-8Z"/>
      <path fill="#E1B456" d="M7 21c0-5 2-8 6-8h6c-3 2-4 5-4 9v20H7Z"/>
      <path fill="#2563EB" d="M16 20h33v24c0 5 2 8 6 8H15c-5 0-8-3-8-7h9Z"/>
      <path fill="#173F96" d="M16 43H7v2c0 4 3 7 8 7h40c-4 0-6-3-6-8v-2c-4 4-8 6-14 6H16Z"/>
      <path fill="#D49A28" d="M49 20h8v25c0 4-2 7-6 7-4 0-6-3-6-7h4Z"/>
      <path fill="#FFE8AC" d="M49 20h8v5h-8Z"/>
      <circle cx="32" cy="24" r="4" fill="#FFE09B"/>
      <path fill="#FFF4DC" d="M28 32h8v12h-8Z"/>
    `),
  },
];
