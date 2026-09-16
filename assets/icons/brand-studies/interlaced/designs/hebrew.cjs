'use strict';
// Local Segoe UI Bold outlines already used in the earlier brand study.
const glyphs = {
  M: {x:5.125,y:24.25,w:51.0625,h:44.8125,d:'M56.188 69.063L46.219 69.063L46.219 42.25C46.219 39.354 46.344 36.156 46.594 32.656L46.344 32.656C45.823 35.406 45.354 37.385 44.938 38.594L34.438 69.063L26.188 69.063L15.5 38.906C15.208 38.094 14.74 36.01 14.094 32.656L13.813 32.656C14.083 37.073 14.219 40.948 14.219 44.281L14.219 69.063L5.125 69.063L5.125 24.25L19.906 24.25L29.063 50.813C29.792 52.938 30.323 55.073 30.656 57.219L30.844 57.219C31.406 54.74 32 52.583 32.625 50.75L41.781 24.25L56.188 24.25Z'},
  mem: {x:3.40625,y:29.96875,w:37.46875,h:39.6875,d:'M40.875 55C40.875 58.292 40.323 60.964 39.219 63.016C38.115 65.068 36.453 66.688 34.234 67.875C32.016 69.062 29.552 69.656 26.844 69.656C25.427 69.656 24.089 69.542 22.828 69.313C21.568 69.083 20.323 68.615 19.094 67.906L20.844 60.969C22.365 61.49 23.76 61.75 25.031 61.75C28.906 61.75 30.844 59.49 30.844 54.969L30.844 47.25C30.844 44.396 30.104 42.156 28.625 40.531C27.146 38.906 25.052 38.094 22.344 38.094C20.99 38.094 19.693 38.411 18.453 39.047C17.214 39.682 16.24 40.516 15.531 41.547C14.823 42.578 14.469 43.531 14.469 44.406L14.469 69.063L4.375 69.063L4.375 38.813C4.375 37.854 4.328 36.938 4.234 36.063C4.141 35.188 3.865 34.271 3.406 33.313L12.094 29.969C13.094 31.156 13.771 32.677 14.125 34.531C15.833 32.948 17.563 31.818 19.313 31.141C21.063 30.464 22.979 30.125 25.063 30.125C29.792 30.125 33.609 31.646 36.516 34.688C39.422 37.729 40.875 41.833 40.875 47Z'}
};
function glyph(name, box, fill, expand=0) {
  const g=glyphs[name], [x,y,w,h]=box, s=Math.min(w/g.w,h/g.h);
  const tx=x+(w-g.w*s)/2-g.x*s, ty=y+(h-g.h*s)/2-g.y*s;
  return `<path d="${g.d}" fill="${fill}"${expand?` stroke="${fill}" stroke-width="${expand/s}" stroke-linejoin="round"`:''} transform="translate(${tx} ${ty}) scale(${s})"/>`;
}
function woven(blueName,blueBox,goldName,goldBox,overBlue,id) {
  const zone=`<path d="${overBlue}"/>`, full='<rect width="64" height="64" fill="white"/>';
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"><defs>
    <clipPath id="${id}-over-blue">${zone}</clipPath>
    <mask id="${id}-blue-mask" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64">${full}${glyph(goldName,goldBox,'black',3.2)}<g fill="white">${zone}</g></mask>
    <mask id="${id}-gold-mask" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64">${full}<g clip-path="url(#${id}-over-blue)">${glyph(blueName,blueBox,'black',3.2)}</g></mask>
    </defs><g mask="url(#${id}-blue-mask)">${glyph(blueName,blueBox,'#2563EB')}</g><g mask="url(#${id}-gold-mask)">${glyph(goldName,goldBox,'#D49A28')}</g></svg>`;
}
module.exports = [
  {id:'09-mem-trama',title:'Mem e M · trama',description:'As duas letras ocupam o mesmo espaço. A diagonal azul passa à frente de um traço dourado e retorna por trás do outro.',svg:woven('M',[6,15,49,42],'mem',[13,5,32,41],'M28 16H48V54H28Z','mem-trama')},
  {id:'10-mem-diagonal',title:'Mem e M · diagonal',description:'A mem azul envolve parte do M dourado. O deslocamento preserva as duas letras e evidencia a alternância dos traços.',svg:woven('mem',[6,6,37,48],'M',[24,22,35,35],'M19 40H38V58H19Z','mem-diagonal')}
];
