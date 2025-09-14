import fs from "node:fs";
import path from "node:path";

const SRC_DIR = "output"; // your PureScript output folder

const importRe = /import\((['"])([^'"]+)\1\)\.([A-Za-z_$][\w$]*)/g;

function processFile(filePath) {
  let code = fs.readFileSync(filePath, "utf8");
  const seen = new Map(); // path -> alias
  let impCount = 0;

  // Replace inline imports with aliases
  const newCode = code.replace(importRe, (_m, _q, importPath, name) => {
    let alias = seen.get(importPath);
    if (!alias) {
      alias = `__imp${++impCount}`;
      seen.set(importPath, alias);
    }
    return `${alias}.${name}`;
  });

  if (seen.size === 0) {
    return; // no inline imports -> skip writing file
  }

  // Prepend imports
  const imports =
    [...seen.entries()]
      .map(([p, alias]) => `import type * as ${alias} from "${p}";`)
      .join("\n") + "\n\n";

  const fixedCode = imports + newCode;

  fs.writeFileSync(filePath, fixedCode, "utf8");
  console.log(`✔ Fixed ${filePath} (${seen.size} imports)`);
}

function walk(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(p);
    } else if (entry.isFile() && p.endsWith(".d.ts")) {
      processFile(p);
    }
  }
}

walk(SRC_DIR);
console.log("✅ All inline imports fixed directly in output/");
