#!/usr/bin/env node
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const cp = require('node:child_process');

const root = path.resolve(__dirname, '..');
const generated = path.join(root, 'generated');
const families = ['clareza', 'mosaico', 'folio'];
const sizes = [16, 20, 24, 32, 40, 48, 64];
const pngSignature = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]);

function option(name) {
  const index = process.argv.indexOf(name);
  if (index < 0) return null;
  if (!process.argv[index + 1] || process.argv[index + 1].startsWith('--'))
    throw new Error(`Missing value for ${name}`);
  return process.argv[index + 1];
}

function validatePng(file, size) {
  const data = fs.readFileSync(file);
  if (data.length < 33 || !data.subarray(0, 8).equals(pngSignature) ||
      data.toString('ascii', 12, 16) !== 'IHDR')
    throw new Error(`Invalid PNG: ${file}`);
  if (data.readUInt32BE(16) !== size || data.readUInt32BE(20) !== size)
    throw new Error(`Expected ${size} x ${size} PNG: ${file}`);
  if (![4, 6].includes(data[25]))
    throw new Error(`Expected PNG with an alpha channel: ${file}`);
}

function main() {
  const windres = option('--windres');
  const catalog = JSON.parse(fs.readFileSync(path.join(root, 'manifest/icons.json'), 'utf8'));
  if (!Array.isArray(catalog.icons)) throw new Error('Missing icon catalog');
  const symbols = catalog.icons.map(icon => icon.symbol);
  if (new Set(symbols).size !== symbols.length ||
      symbols.some(symbol => !/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/.test(symbol)))
    throw new Error('Icon symbols must be unique lowercase identifiers');
  if (!symbols.includes('app-ibiblia')) throw new Error('Missing unique application mark');
  const commandSymbols = symbols.filter(symbol => symbol !== 'app-ibiblia');
  if (commandSymbols.length === 0) throw new Error('Missing command icons');

  // The application mark is always loaded from ui_icons.res. Alternative
  // marks remain design studies and must never become selectable resources.
  const resources = [];
  for (const family of families) {
    for (const symbol of commandSymbols) {
      for (const size of sizes) {
        const relative = `../alternatives/${family}/png/${size}/${symbol}.png`;
        validatePng(path.resolve(generated, relative), size);
        const resourceName = `IBIBLIA_${family}_${symbol}_${size}`.replaceAll('-', '_').toUpperCase();
        resources.push(`${resourceName} RCDATA "${relative}"`);
      }
    }
  }

  // Complete validation before changing the generated resource declaration.
  fs.mkdirSync(generated, { recursive: true });
  fs.writeFileSync(path.join(generated, 'ui_icon_families.rc'), resources.join('\n') + '\n');
  if (windres) {
    const compiler = path.resolve(windres);
    const staging = path.join(generated, 'ui_icon_families.res.tmp');
    try {
      cp.execFileSync(compiler, [
        '--preprocessor=' + path.join(path.dirname(compiler), 'cpp.exe'),
        '--input', 'ui_icon_families.rc',
        '--output', staging,
        '--output-format=res'
      ], { cwd: generated, stdio: 'inherit' });
      fs.renameSync(staging, path.join(generated, 'ui_icon_families.res'));
    } finally {
      if (fs.existsSync(staging)) fs.unlinkSync(staging);
    }
  }

  console.log(`Validated ${families.length} families, ${commandSymbols.length} commands and ${sizes.length} PNG sizes: ${resources.length} resources.`);
  console.log(windres ? 'Generated ui_icon_families.rc and ui_icon_families.res.' :
    'Generated ui_icon_families.rc. Pass --windres to compile ui_icon_families.res.');
  console.log('The application mark and the original icon resources are unchanged.');
}

try {
  main();
} catch (error) {
  console.error(error.message);
  process.exitCode = 1;
}
