// Original iBiblia vector artwork. Run with Node.js and the sharp package.
// All runtime assets are checked in; this is needed only when changing artwork.
const fs = require('node:fs');
const path = require('node:path');
const cp = require('node:child_process');
const root = path.resolve(__dirname, '..');
const repo = path.resolve(root, '../..');
const args = process.argv.slice(2);
const option = name => { const i = args.indexOf(name); return i < 0 ? undefined : args[i + 1]; };
const sharp = require(option('--sharp-module') || 'sharp');
const sizes = [16, 20, 24, 32, 40, 48, 64];
const icons = [];
const appMarkSvg = fs.readFileSync(path.join(root, 'brand/app-ibiblia.svg'), 'utf8');
const iconSvg = icon => icon.symbol === 'app-ibiblia' ? appMarkSvg : svg(icon.body);
const colors = {blue:'#2563EB', gold:'#D49A28', teal:'#138878', red:'#D94343', slate:'#64748B', purple:'#8056C7'};
const gradients = Object.entries(colors).map(([name, color]) => `<linearGradient id="${name}" x1="0" y1="0" x2="0.8" y2="1"><stop stop-color="${color}"/><stop offset="1" stop-color="${name==='blue'?'#1647BA':name==='gold'?'#B87717':name==='teal'?'#076A63':name==='red'?'#B32937':name==='slate'?'#475569':'#623BA6'}"/></linearGradient>`).join('');
const svg = body => `<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32"><defs>${gradients}</defs>${body}</svg>\n`;
const p = (d,fill,extra='')=>`<path d="${d}" fill="${fill}" ${extra}/>`;
const line=(d,color='#FFFFFF',width=2)=>p(d,'none',`stroke="${color}" stroke-width="${width}" stroke-linecap="round" stroke-linejoin="round"`);
const rect=(x,y,w,h,r,fill,extra='')=>`<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="${r}" fill="${fill}" ${extra}/>`;
const circ=(x,y,r,fill,extra='')=>`<circle cx="${x}" cy="${y}" r="${r}" fill="${fill}" ${extra}/>`;
const grad=name=>`url(#${name})`;
const textLines=(x=9,y=12,color='#7295D5')=>line(`M${x} ${y}h11 M${x} ${y+4}h11 M${x} ${y+8}h7`,color,1.8);
const doc=(color='blue')=>p('M8 3h12l6 6v18a2 2 0 0 1-2 2H8a3 3 0 0 1-3-3V6a3 3 0 0 1 3-3',grad(color))+p('M9 5h10v6h5v15H8V6a1 1 0 0 1 1-1','#F4F8FF')+p('M20 4v6h5','#B9D0FB')+textLines();
const check=(x=0,y=0,color='#FFFFFF')=>`<g transform="translate(${x} ${y})">${line('M-3 0l2 2 4-4',color,2)}</g>`;
const cross=(x=0,y=0,color='#FFFFFF')=>`<g transform="translate(${x} ${y})">${line('M-2.5-2.5l5 5 M2.5-2.5l-5 5',color,2)}</g>`;
const plus=(x=0,y=0,color='#FFFFFF')=>`<g transform="translate(${x} ${y})">${line('M-3 0h6 M0-3v6',color,2)}</g>`;
const badge=(color,body)=>circ(24,24,7,'#FFFFFF')+circ(24,24,6,grad(color))+body;
const spark=(x=24,y=9,color='gold')=>p(`M${x} ${y-7}l2 5 5 2-5 2-2 5-2-5-5-2 5-2z`,grad(color));
const pencil=()=>p('M16 24l2-6 8-8a2.2 2.2 0 0 1 3 3l-8 8z',grad('gold'))+p('M16 24l1-4 3 3z','#475569')+line('M24 12l3 3','#FDECC5',1.5);
const gear=()=>p('M13 3h6l1 4 4-1 3 5-3 3 3 3-3 5-4-1-1 5h-6l-1-5-4 1-3-5 3-3-3-3 3-5 4 1z',grad('slate'))+circ(16,14,5,'#EAF1FF')+circ(16,14,2.3,grad('blue'));
const book=(color='blue')=>p('M2 7c5-2 9-1 14 2 5-3 9-4 14-2v20c-5-2-10-1-14 1-4-2-9-3-14-1z',grad(color))+p('M4 5c4-1 8 0 11 2v17c-4-2-7-2-11-1z','#F7FAFF')+p('M17 7c3-2 7-3 11-2v18c-4-1-7-1-11 1z','#DCE9FF')+line('M7 10l5 1 M7 14l5 1 M20 11l5-1 M20 15l5-1','#7394CA',1.7);
const link=()=>line('M13 20l-2 2a3 3 0 0 0 4 4l3-3 M19 24l2-2a3 3 0 0 0-4-4l-3 3','#B87717',3.4)+line('M14 24l4-4','#F4C860',2.2);
const chips=()=>rect(3,5,11,7,2,grad('blue'))+rect(18,5,11,7,2,grad('teal'))+rect(3,19,11,7,2,'#DCE9FF')+rect(18,19,11,7,2,'#CCEBE5')+line('M6 8.5h5 M21 8.5h5','#FFFFFF',1.7)+line('M6 22.5h5','#7295D5',1.7)+line('M21 22.5h5','#6BA99F',1.7);
const arrow=(direction='right',color='blue')=>`<g ${direction==='left'?'transform="translate(32 0) scale(-1 1)"':''}>${p('M5 12h12V6l12 10-12 10v-6H5z',grad(color))}${line('M7 14h12v-4','#FFFFFF',1.1)}</g>`;
const curved=(direction='left',color='slate')=>`<g ${direction==='right'?'transform="translate(32 0) scale(-1 1)"':''}>${p('M12 4L2 13l10 8v-6h5c5 0 8 4 8 9v3c3-3 5-7 4-11-1-5-5-8-11-8h-6z',grad(color))}${line('M7 12l3-3 M14 11h4c4 0 7 2 8 5','#FFFFFF',1.2)}</g>`;
const db=()=>p('M5 8h22v17c0 5-22 5-22 0z',grad('blue'))+`<ellipse cx="16" cy="8" rx="11" ry="5" fill="#82AAF2"/>`+line('M6 14c5 4 15 4 20 0 M6 20c5 4 15 4 20 0','#BBD3FC',1.4);
const add=(symbol,body,meaning,action=null,legacy=null)=>icons.push({symbol,body,meaning,pascalAction:action,legacyImageIndex:legacy});

add('app-ibiblia',null,'Application mark: Clareza outlined Bible pages joined by a gold link, shared by every command icon family.');
add('project-new',doc()+badge('teal',plus(24,24)),'Create an association or translation project.','ActionNewProject',6);
add('project-open',p('M3 7a3 3 0 0 1 3-3h7l3 4h11a2 2 0 0 1 2 2v15H3z','#B87717')+rect(6,8,17,14,1,'#F3F7FF')+p('M2 12h28l-4 15H5z',grad('gold'))+line('M6 15h19','#FCE4AD',1.5),'Open an existing project.','ActionOpenProject',0);
add('project-close',doc()+badge('red',cross(24,24)),'Close the current project.','ActionCloseProject',1);
const disk=rect(4,3,24,26,3,grad('blue'))+rect(9,3,14,10,1,'#DCE9FF')+rect(18,5,3,6,0,'#7394CA')+rect(8,18,16,11,2,'#F4F8FF')+line('M11 22h10 M11 25h8','#7295D5',1.5);
add('project-save',disk,'Save changes.','ActionSaveProject',2);
add('project-save-as',disk+pencil(),'Save the project under a new name.','ActionSaveProjectAs',3);
add('project-settings',doc()+`<g transform="translate(15 15) scale(.55)">${gear()}</g>`,'Edit current project settings.','ActionProjectProperties');
add('project-export',doc()+`<g transform="translate(12 15) scale(.65)">${arrow()}</g>`,'Open the export workflow.','ActionExportProject');
add('association-auto',chips()+line('M9 12v4h14v3','#D49A28',2.4)+spark(16,16),'Suggest associations between source and destination syntagms.','ActionSuggestAssociation',4);
add('association-clear',chips()+line('M8 12v4h4 M20 16h4v3','#64748B',2.4)+line('M18 13l-4 6','#D94343',3),'Remove associations: disconnected text blocks.','ActionClearAssociations',5);
add('association-rollback',chips()+`<g transform="translate(7 9) scale(.65)">${curved('left','gold')}</g>`,'Restore the saved associations of this verse.','ActionRollbackCurrentVerse',9);
add('history-back',curved('left'),'Go back through visited verses.','ActionHistoryBack',7);
add('history-forward',curved('right'),'Go forward through visited verses.','ActionHistoryForward',8);
add('verse-previous',doc()+`<g transform="translate(1 14) scale(.62)">${arrow('left')}</g>`,'Go to the previous Bible verse.','ActionPreviousVerse',7);
add('verse-next',doc()+`<g transform="translate(11 14) scale(.62)">${arrow()}</g>`,'Go to the next Bible verse.','ActionNextVerse',8);
add('verse-first',arrow('left')+rect(1,6,3,20,1,'#1647BA'),'Go to the first verse.','ActionFirstVerse');
add('verse-last',arrow()+rect(28,6,3,20,1,'#1647BA'),'Go to the last verse.','ActionLastVerse');
add('app-exit',rect(4,3,16,26,2,'#DDE5F0')+p('M5 4l12 3v20L5 30z',grad('slate'))+`<g transform="translate(12 6) scale(.63)">${arrow('right','red')}</g>`,'Exit iBiblia.','ActionExit',10);
add('projects-merge',rect(2,3,10,12,2,'#82AAF2')+rect(20,3,10,12,2,'#79C4B7')+line('M7 17v4h18v-4 M16 21v6','#64748B',2.5)+rect(11,25,10,6,2,grad('blue')),'Merge two projects.','ActionMergeProjects');
add('association-memory-rebuild',db()+badge('teal',line('M21 25a3 3 0 1 0 1-4 M21 20v4h3','#FFFFFF',1.7)),'Rebuild statistical association memory.','ActionRebuildSuggestionDatabase');
add('sync-theword-verse',book()+line('M4 3h20l-3-2 M28 29H8l3 2','#D49A28',2.3),'Synchronize the displayed Bible verse with theWord.','ActionSyncTheWordVerse');
add('tags-show',p('M3 5h13l13 13-11 11L3 14z',grad('teal'))+circ(9,10,2.3,'#F4F8FF')+line('M15 14l6 6 M12 17l6 6','#D0F2E9',1.7),'Show Strong and morphology tags.','ActionShowTags');
const patch=p('M5 6h8a4 4 0 1 1 6 0h8v9a4 4 0 1 0 0 6v7H5v-8a4 4 0 1 0 0-6z',grad('purple'))+line('M8 10h8 M8 24h9','#CBBBEE',1.5);
add('patch-create',patch+badge('blue',plus(24,24)),'Create a patch from selected verses.','ActionExportPatch');
add('patch-apply',patch+badge('teal',check(24,24)),'Apply accepted patch changes.','ActionApplyPatch');
add('database-vacuum',db()+badge('slate',line('M21 22l3 3 3-3 M21 26h6','#FFFFFF',1.8)),'Compact the project database.','ActionVaccumProject');
add('keyboard-shortcuts',rect(2,7,28,20,3,grad('slate'))+[6,12,18,24].map(x=>[11,16].map(y=>rect(x-1,y,3,3,.6,'#E9F0FD')).join('')).join('')+rect(10,21,12,2,1,'#E9F0FD'),'View keyboard shortcuts.','ActionKeyboardShortcuts');
add('layout-reset',rect(3,4,26,24,3,'#DCE9FF')+rect(3,4,26,5,2,grad('blue'))+rect(5,11,7,14,1,'#7295D5')+`<g transform="translate(12 10) scale(.55)">${curved('left')}</g>`,'Restore the default window layout.','ActionResetLayout');
add('guide-mode',circ(16,14,10,grad('gold'))+p('M11 23h10l-2 5h-6z',grad('slate'))+line('M13 11l3-2 3 2-3 4v3','#FFF4D6',2.3),'Show contextual guidance.','ActionGuideMode');
add('theword-highlighter-colors',p('M4 23l5-8 12-12 8 8-12 12-9 4z',grad('gold'))+p('M8 19l5 5-8 4-3-3z','#475569')+line('M12 13l8 8','#F8DD98',2)+rect(16,27,14,3,1,'#70BFAE'),'Configure theWord highlighter colors.','ActionTheWordHighlighterColors');
add('llm-options',rect(5,7,22,21,5,grad('purple'))+rect(9,11,14,11,2,'#E9DFFA')+circ(13,16,1.8,'#8056C7')+circ(20,16,1.8,'#8056C7')+line('M13 20h6','#8056C7',1.5)+spark(23,6),'Configure AI providers and verse flows.','ActionLlmOptions');
add('preferences',line('M4 8h24 M4 16h24 M4 24h24','#91A2B9',3)+rect(9,4,6,8,2,grad('blue'))+rect(21,12,6,8,2,grad('teal'))+rect(7,20,6,8,2,grad('gold')),'Open application preferences.','FActionPreferences');
add('file-association-register',doc()+badge('teal',line('M22 24l2-2a1.5 1.5 0 0 1 2 2l-2 2 M24 23l-2 2a1.5 1.5 0 0 1-2-2l2-2','#FFFFFF',1.7)),'Register iBiblia as handler for project files.','ActionRegisterFileAssociation');
add('file-association-unregister',doc()+badge('slate',line('M21 24h6','#FFFFFF',2)),'Remove the project file registration.','ActionUnregisterFileAssociation');
add('text-load',doc()+badge('blue',line('M24 21v6 M21 24l3 3 3-3','#FFFFFF',1.8)),'Load or replace Bible text.','ActionLoadText');
add('text-clear',doc()+badge('red',line('M21 24h6','#FFFFFF',2)),'Clear the selected Bible text.','ActionClearText');
const dictionary=rect(6,3,22,26,3,grad('blue'))+rect(3,4,5,24,2,'#1647BA')+rect(11,7,13,12,1,'#E9F1FF')+line('M14 11h7 M14 15h5','#7295D5',1.5)+p('M19 22h5v9l-2.5-2-2.5 2z',grad('gold'));
add('dictionary-strong-select',dictionary,'Choose a Strong dictionary.','ActionSelectStrongDictionary');
add('dictionary-morphology-select',dictionary.replaceAll('url(#blue)','url(#teal)').replaceAll('#1647BA','#076A63')+circ(20,13,4,'#D0EDE7')+line('M18 13h4 M20 11v4','#138878',1.4),'Choose a morphology dictionary.','ActionSelectMorphologyDictionary');
add('font-select',p('M4 26l8-22h5l8 22h-5l-2-6H11l-2 6z M12 16h5l-2.5-8z',grad('blue'),'fill-rule="evenodd"')+rect(22,26,8,3,1,grad('gold')),'Choose a text font.','ActionSelectFont');
add('checkbox-unchecked',rect(4,4,24,24,5,'#F8FAFC','stroke="#8EA0B8" stroke-width="2"'),'Unchecked selectable Bible item.');
add('checkbox-checked',rect(3,3,26,26,5,grad('blue'))+line('M9 16l5 5 9-10','#FFFFFF',3),'Checked selectable Bible item.');
add('checkbox-partial',rect(3,3,26,26,5,grad('blue'))+rect(9,14,14,4,1,'#FFFFFF'),'Partially selected Bible parent.');
add('ai-pending',circ(16,17,12,grad('gold'))+circ(16,17,8.5,'#FFF5DD')+line('M16 11v7h5','#B87717',2.6),'Queued AI job: clock.');
add('ai-processing',p('M16 3a13 13 0 1 0 13 13h-5a8 8 0 1 1-8-8z',grad('blue'))+spark(24,7),'Running AI job: progress ring and spark.');
add('ai-success',circ(16,16,13,grad('teal'))+line('M8 16l6 6 10-12','#FFFFFF',3),'Completed AI job: check.');
add('ai-error',circ(16,16,13,grad('red'))+line('M11 11l10 10 M21 11L11 21','#FFFFFF',3),'Failed AI job: cross.');
add('tab-source',doc()+rect(3,3,6,26,2,grad('blue')),'Source Bible text.');
add('tab-destination',doc('teal')+rect(23,3,6,26,2,grad('teal')),'Destination Bible text.');
add('tab-reference',book('slate'),'Reference Bible text.');
add('comments',p('M5 4h22a3 3 0 0 1 3 3v15a3 3 0 0 1-3 3H13l-8 6v-6a3 3 0 0 1-3-3V7a3 3 0 0 1 3-3',grad('gold'))+line('M8 10h16 M8 15h16 M8 20h9','#FFF1CF',2),'Verse comments and notes.');
add('status-review',p('M1 16C8 3 24 3 31 16 24 29 8 29 1 16',grad('blue'))+circ(16,16,7,'#EAF1FF')+circ(16,16,3.5,'#2563EB')+circ(17,14,1.2,'#FFFFFF'),'Review verses or filter the status catalog.');
add('help',circ(16,16,13,grad('blue'))+line('M11 11c0-6 12-6 11 1-.5 3-6 3-6 7','#FFFFFF',2.7)+circ(16,24,1.6,'#FFFFFF'),'Open help.');
add('about',circ(16,16,13,grad('blue'))+rect(14.5,14,3,10,1.5,'#FFFFFF')+circ(16,9,1.9,'#FFFFFF'),'About iBiblia.');
add('confirm',circ(16,16,13,grad('teal'))+line('M8 16l6 6 10-12','#FFFFFF',3),'Accept or apply the current changes.');
add('cancel',circ(16,16,13,grad('slate'))+line('M11 11l10 10 M21 11L11 21','#FFFFFF',3),'Cancel or close without applying.');
add('add',circ(16,16,13,grad('blue'))+line('M9 16h14 M16 9v14','#FFFFFF',3),'Add an item.');
add('remove',circ(16,16,13,grad('red'))+line('M9 16h14','#FFFFFF',3),'Remove an item.');
add('edit',doc()+pencil(),'Edit text or configuration.');
add('browse',rect(3,5,26,23,3,grad('gold'))+rect(7,9,18,14,2,'#FFF4D6')+[11,16,21].map(x=>circ(x,16,1.7,'#B87717')).join(''),'Browse for a file or location.');
add('refresh',p('M25 9l5-5v13H17l5-5a9 9 0 1 0 1 11l4 3A14 14 0 1 1 25 9',grad('teal')),'Reload or refresh.');
add('search',circ(13,13,9,grad('blue'))+circ(13,13,5.6,'#DCE9FF')+line('M20 20l8 8','#64748B',5)+line('M11 10h4','#FFFFFF',2),'Find text or an item.');
add('print',rect(3,11,26,14,3,grad('slate'))+rect(8,3,16,11,1,'#EAF1FF')+rect(8,19,16,11,1,'#F8FAFC')+line('M11 23h10 M11 26h7','#7295D5',1.5)+circ(25,16,1.2,'#85D3BF'),'Print the current content.');
add('copy',rect(3,3,19,23,3,'#82AAF2')+`<g transform="translate(4 2)">${doc()}</g>`,'Copy selected content.');
add('paste',rect(5,6,22,24,3,grad('gold'))+rect(8,10,16,17,1,'#FFF8E8')+rect(11,3,10,9,2,'#A87925')+rect(13,4,6,4,1,'#F8E2AE')+line('M11 17h10 M11 21h8','#BA974C',1.5),'Paste content.');
add('stop',rect(4,4,24,24,5,grad('red'))+rect(10,10,12,12,2,'#FFF4F4'),'Stop or cancel running processing.');
add('translate',rect(2,3,19,21,3,grad('blue'))+rect(12,11,19,19,3,grad('teal'))+line('M6 9h10 M8 14h6 M16 17h11 M17 22h9','#FFFFFF',2),'Translate a verse or configure translation.');
add('link',book()+link(),'Connect source and destination Bible text.');
add('export-destination-strongs',doc('teal')+badge('blue',line('M22 21v6 M26 21v6 M21 23h6 M21 25h6','#FFFFFF',1.3)),'Export destination text with Strong tags.','ActionExportDestinationWithStrongs');
add('export-interlinear',rect(3,4,26,25,3,'#EAF1FF')+line('M7 9h18 M7 19h18','#2563EB',2.7)+line('M7 13h7 M18 13h7 M7 23h7 M18 23h7','#138878',2),'Export interlinear text.','ActionExportInterlinearText');

function enumName(symbol) { return 'ai'+symbol.split('-').map(s=>s==='ibiblia'?'IBiblia':s[0].toUpperCase()+s.slice(1)).join(''); }
async function main() {
  for (const folder of ['source/svg','manifest','app/ico','preview','generated',...sizes.map(s=>'png/'+s),'png/128','png/256']) fs.mkdirSync(path.join(root,folder),{recursive:true});
  for (const icon of icons) {
    fs.writeFileSync(path.join(root,'source/svg',icon.symbol+'.svg'),iconSvg(icon));
    for (const size of icon.symbol==='app-ibiblia'?[...sizes,128,256]:sizes) await sharp(Buffer.from(iconSvg(icon))).resize(size,size).png().toFile(path.join(root,'png',String(size),icon.symbol+'.png'));
  }
  const icoSizes=[16,20,24,32,40,48,64,128,256], header=Buffer.alloc(6+16*icoSizes.length), chunks=[];
  header.writeUInt16LE(1,2); header.writeUInt16LE(icoSizes.length,4);
  let offset=header.length;
  // Lazarus TIcon treats entries below 256px as DIBs. Use uncompressed
  // 32-bit BGRA at every size, with an explicit AND mask for Win32 readers.
  for (const [i, size] of icoSizes.entries()) {
    const rgba = await sharp(path.join(root, 'png', String(size), 'app-ibiblia.png')).ensureAlpha().raw().toBuffer();
    const maskStride = Math.ceil(size / 32) * 4;
    const xorBytes = size * size * 4;
    const image = Buffer.alloc(40 + xorBytes + maskStride * size);
    image.writeUInt32LE(40, 0);
    image.writeInt32LE(size, 4);
    image.writeInt32LE(size * 2, 8);
    image.writeUInt16LE(1, 12);
    image.writeUInt16LE(32, 14);
    image.writeUInt32LE(xorBytes, 20);
    for (let y = 0; y < size; y++) {
      const bottomY = size - 1 - y;
      for (let x = 0; x < size; x++) {
        const source = (y * size + x) * 4;
        const target = 40 + (bottomY * size + x) * 4;
        image[target] = rgba[source + 2];
        image[target + 1] = rgba[source + 1];
        image[target + 2] = rgba[source];
        image[target + 3] = rgba[source + 3];
        if (rgba[source + 3] === 0)
          image[40 + xorBytes + bottomY * maskStride + (x >> 3)] |= 0x80 >> (x & 7);
      }
    }
    const entry = 6 + i * 16;
    header[entry] = size === 256 ? 0 : size;
    header[entry + 1] = size === 256 ? 0 : size;
    header.writeUInt16LE(1, entry + 4);
    header.writeUInt16LE(32, entry + 6);
    header.writeUInt32LE(image.length, entry + 8);
    header.writeUInt32LE(offset, entry + 12);
    offset += image.length;
    chunks.push(image);
  }
  const ico=Buffer.concat([header,...chunks]);
  fs.writeFileSync(path.join(root,'app/ico/iBiblia.ico'),ico); fs.writeFileSync(path.join(repo,'iBiblia.ico'),ico);
  const resourceName=(i,s)=>'IBIBLIA_'+i.symbol.replaceAll('-','_').toUpperCase()+'_'+s;
  const resources=icons.flatMap(i=>(i.symbol==='app-ibiblia'?[...sizes,128,256]:sizes).map(s=>`${resourceName(i,s)} RCDATA "../png/${s}/${i.symbol}.png"`)).join('\n')+'\nIBIBLIA_APP_ICO RCDATA "../app/ico/iBiblia.ico"\n';
  fs.writeFileSync(path.join(root,'generated/ui_icons.rc'),resources);
  fs.writeFileSync(path.join(root,'generated/ui_icon_enum.inc'),icons.map(i=>'    '+enumName(i.symbol)).join(',\n')+'\n');
  fs.writeFileSync(path.join(root,'generated/ui_icon_names.inc'),icons.map(i=>"    '"+i.symbol+"'").join(',\n')+'\n');
  fs.writeFileSync(path.join(root,'manifest/icons.json'),JSON.stringify({schemaVersion:2,project:'iBiblia',provenance:'Original artwork created for iBiblia; no third-party icon artwork imported.',purpose:'Active symbolic icon catalog. Project status IDs are deliberately absent: their labels and colors come from each project.',icons:icons.map(({body,...i})=>({...i,pascalIcon:enumName(i.symbol),resourcePrefix:'IBIBLIA_'+i.symbol.replaceAll('-','_').toUpperCase(),sizes:i.symbol==='app-ibiblia'?[...sizes,128,256]:sizes,source:'source/svg/'+i.symbol+'.svg'}))},null,2)+'\n');
  const cols=6, cellW=176, cellH=132, rows=Math.ceil(icons.length/cols);
  const board=`<svg xmlns="http://www.w3.org/2000/svg" width="${cols*cellW}" height="${110+rows*cellH}"><rect width="100%" height="100%" fill="#F3F5F8"/><text x="28" y="43" font-family="Segoe UI, sans-serif" font-size="28" fill="#1F2937">iBiblia · Textos conectados</text><text x="28" y="74" font-family="Segoe UI, sans-serif" font-size="14" fill="#64748B">Original SVG family · 16 / 24 / 48 px · blue, gold and calm surfaces</text>${icons.map((i,n)=>{const x=n%cols*cellW,y=110+Math.floor(n/cols)*cellH; return `<g transform="translate(${x} ${y})"><rect x="8" y="0" width="160" height="122" rx="10" fill="#FFFFFF"/><image x="20" y="41" width="16" height="16" href="data:image/svg+xml;base64,${Buffer.from(iconSvg(i)).toString('base64')}"/><image x="51" y="37" width="24" height="24" href="data:image/svg+xml;base64,${Buffer.from(iconSvg(i)).toString('base64')}"/><image x="96" y="25" width="48" height="48" href="data:image/svg+xml;base64,${Buffer.from(iconSvg(i)).toString('base64')}"/><text x="88" y="97" text-anchor="middle" font-family="Segoe UI, sans-serif" font-size="${i.symbol.length>23?9:11}" fill="#475569">${i.symbol}</text></g>`;}).join('')}</svg>`;
  fs.writeFileSync(path.join(root,'preview/contact-sheet.svg'),board);
  await sharp(Buffer.from(board)).png().toFile(path.join(root,'preview/contact-sheet.png'));
  if (option('--windres')) cp.execFileSync(option('--windres'),['--preprocessor='+path.join(path.dirname(option('--windres')),'cpp.exe'),'--input','ui_icons.rc','--output','ui_icons.res','--output-format=res'],{cwd:path.join(root,'generated'),stdio:'inherit'});
  console.log(`Generated ${icons.length} original SVG icons, PNG resolutions, Windows ICO, catalog and contact sheet.`);
}
main().catch(e=>{console.error(e); process.exit(1);});
