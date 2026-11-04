'use strict';
const glyphs=require('./glyphs.json');
const blue='#2563EB',gold='#D49A28';
function beta(x,y,width,height){
  const g=glyphs.beta,s=Math.min(width/g.width,height/g.height);
  const tx=x+(width-g.width*s)/2-g.x*s,ty=y+(height-g.height*s)/2-g.y*s;
  return `<path fill="${gold}" d="${g.d}" transform="translate(${tx} ${ty}) scale(${s})"/>`;
}
const svg=body=>`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">${body}</svg>`;
module.exports=[
  {
    id:'11-vav-beta',title:'Vav e beta',
    description:'A haste do vav sugere o i; o beta minúsculo conserva suas duas curvas e o prolongamento abaixo da linha. Uma assinatura simples em duas cores.',
    svg:svg(`<path fill="${blue}" d="M7.5 17L17.5 14.5Q21 14 21 18V47H13V23L7.5 24Z"/>`+beta(28,6,29,52))
  },
  {
    id:'12-vav-beta-ponto',title:'Inicial pontuada',
    description:'Um ponto sobre a haste inspirada no vav aproxima a leitura de iB. O ponto é uma adaptação gráfica da marca, acrescentada à forma hebraica.',
    svg:svg(`<circle cx="16.5" cy="12.5" r="4.5" fill="${blue}"/><path fill="${blue}" d="M8 25L17.5 22Q21 21.5 21 25V47.5H13V30.5L8 32Z"/>`+beta(28,6,29,52))
  }
];
