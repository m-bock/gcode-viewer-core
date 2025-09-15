#!/usr/bin/env node
/* check-exports.js
   Usage: node scripts/check-exports.js [rootDir]
   Exits with code 1 if any .d.ts declares a value export that index.js does not export.
*/
const fs = require("fs");
const path = require("path");

const root = process.argv[2] ? path.resolve(process.argv[2]) : process.cwd();

function walk(dir, out = []) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const e of entries) {
    if (e.name === "node_modules" || e.name.startsWith(".")) continue;
    const p = path.join(dir, e.name);
    if (e.isDirectory()) walk(p, out);
    else if (e.isFile() && e.name === "index.d.ts") out.push(p);
  }
  return out;
}

function stripJsComments(src) {
  // remove /* ... */ and // ... comments (not perfect but fine for export blocks)
  return src.replace(/\/\*[\s\S]*?\*\//g, "").replace(/\/\/[^\n\r]*/g, "");
}

function parseDeclaredConstExports(dtsContent) {
  const names = new Set();
  const re = /\bexport\s+(?:declare\s+)?const\s+([A-Za-z_$][\w$]*)\b/g;
  let m;
  while ((m = re.exec(dtsContent))) {
    names.add(m[1]);
  }
  return names;
}

function parseJsNamedExports(jsContent) {
  const names = new Set();
  const src = stripJsComments(jsContent);

  // match: export { A, B, C as D, ... };
  const blockRe = /\bexport\s*{\s*([\s\S]*?)\s*}\s*;?/g;
  let block;
  while ((block = blockRe.exec(src))) {
    const inside = block[1];
    // split by commas that separate specifiers
    for (const raw of inside.split(",")) {
      const part = raw.trim();
      if (!part) continue;
      // handle "Foo as Bar" (we care about the exported name on the right)
      const asMatch = part.match(
        /^([A-Za-z_$][\w$]*)(?:\s+as\s+([A-Za-z_$][\w$]*))?$/
      );
      if (asMatch) {
        const exportedName = asMatch[2] || asMatch[1];
        names.add(exportedName);
      }
    }
  }
  return names;
}

function checkOne(dtsPath) {
  const jsPath = path.join(path.dirname(dtsPath), "index.js");
  const relDts = path.relative(root, dtsPath);
  const relJs = path.relative(root, jsPath);

  if (!fs.existsSync(jsPath)) {
    return {
      file: relDts,
      missing: [],
      errors: [],
    };
  }

  const dts = fs.readFileSync(dtsPath, "utf8");
  const js = fs.readFileSync(jsPath, "utf8");

  const declared = parseDeclaredConstExports(dts);
  const exported = parseJsNamedExports(js);

  const missing = [];
  for (const name of declared) {
    if (!exported.has(name)) missing.push(name);
  }

  return { file: relDts, missing, errors: [] };
}

function main() {
  const dtsFiles = walk(root);
  if (dtsFiles.length === 0) {
    console.log("No index.d.ts files found under", root);
    process.exit(0);
  }

  let hasIssues = false;
  for (const f of dtsFiles) {
    const result = checkOne(f);
    if (result.errors.length) {
      hasIssues = true;
      console.error(`\n[ERROR] ${result.file}`);
      for (const e of result.errors) console.error("  -", e);
      continue;
    }
    if (result.missing.length) {
      hasIssues = true;
      console.error(`\n[MISMATCH] ${result.file}`);
      console.error("  Declared in .d.ts but NOT exported by index.js:");
      for (const name of result.missing) console.error("   •", name);
    }
  }

  if (!hasIssues) {
    console.log(
      "✅ All index.d.ts value exports are present in their index.js exports."
    );
  } else {
    process.exit(1);
  }
}

main();
