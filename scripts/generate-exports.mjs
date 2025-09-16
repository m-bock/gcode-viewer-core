#!/usr/bin/env node

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const projectRoot = path.resolve(__dirname, "..");

// Configuration
const CONFIG = {
  outputDir: "output",
  packageJsonFile: "package.json",
  requiredFiles: ["index.d.ts", "index.js"],
};

/**
 * Scans the output directory for PureScript modules that have both index.js and index.d.ts files
 * @param {Object} options Command line options
 * @returns {Array<{moduleName: string, exportPath: string}>} Array of module info
 */
function findPureScriptModules(options = {}) {
  const outputDir = path.join(projectRoot, CONFIG.outputDir);
  const modules = [];

  if (!fs.existsSync(outputDir)) {
    console.warn(
      `Output directory not found at ${outputDir}. Run "spago build" first.`
    );
    return modules;
  }

  const entries = fs.readdirSync(outputDir, { withFileTypes: true });

  for (const entry of entries) {
    if (entry.isDirectory()) {
      // Check if the module name starts with any of the configured prefixes

      const moduleDir = path.join(outputDir, entry.name);

      // Check if all required files exist
      const hasAllFiles = CONFIG.requiredFiles.every((file) =>
        fs.existsSync(path.join(moduleDir, file))
      );

      if (hasAllFiles) {
        // Convert module name to export path
        // GCodeViewer.StateMachines.App -> StateMachines/App
        // Data.Maybe -> Data/Maybe
        // Effect.Console -> Effect/Console
        const moduleName = entry.name;
        const exportPath = moduleName.replace(/\./g, "/");

        modules.push({
          moduleName,
          exportPath,
          jsPath: `./${CONFIG.outputDir}/${moduleName}/index.js`,
          dtsPath: `./${CONFIG.outputDir}/${moduleName}/index.d.ts`,
        });
      } else if (!options.quiet) {
      }
    }
  }

  return modules.sort((a, b) => a.exportPath.localeCompare(b.exportPath));
}

/**
 * Generates the exports object for package.json
 * @param {Array} modules Array of module info
 * @returns {Object} Exports object
 */
function generateExports(modules) {
  const exports = {};

  for (const module of modules) {
    exports[`./${module.exportPath}`] = {
      types: module.dtsPath,
      import: module.jsPath,
      default: module.jsPath,
    };
  }

  return exports;
}

/**
 * Updates package.json with new exports
 * @param {Object} newExports New exports object
 */
function updatePackageJson(newExports) {
  const packageJsonPath = path.join(projectRoot, CONFIG.packageJsonFile);

  if (!fs.existsSync(packageJsonPath)) {
    throw new Error(`${CONFIG.packageJsonFile} not found`);
  }

  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));

  // Preserve only lowercase exports (like "./react-utils") and overwrite all others
  const preservedExports = {};
  if (packageJson.exports) {
    for (const [key, value] of Object.entries(packageJson.exports)) {
      // Keep exports that start with "./" followed by lowercase letters
      if (key.match(/^\.\/[a-z][a-z0-9-.]*$/)) {
        preservedExports[key] = value;
        console.log(`📝 Preserving lowercase export: ${key}`);
      }
    }
  }

  // Always overwrite with new exports, preserving only lowercase ones
  packageJson.exports = { ...preservedExports, ...newExports };

  // Write back with proper formatting
  const updatedContent = JSON.stringify(packageJson, null, 2) + "\n";
  fs.writeFileSync(packageJsonPath, updatedContent);

  console.log("✅ Updated package.json with new exports");
}

/**
 * Shows help information
 */
function showHelp() {
  console.log(`
📦 Package.json Exports Generator

Usage: npm run generate-exports [options]

Options:
  --help, -h     Show this help message
  --dry-run, -d  Show what would be generated without updating package.json
  --verbose, -v  Show detailed output
  --quiet, -q    Suppress warning messages about skipped modules

Examples:
  npm run generate-exports
  npm run generate-exports -- --dry-run
  npm run generate-exports -- --verbose

This script automatically scans the output directory for PureScript modules
that have both index.js and index.d.ts files, then generates the appropriate
exports entries for package.json.

Supported module prefixes:
  - GCodeViewer.* (your custom modules)
  - Data.* (PureScript standard library)
  - Control.* (PureScript control modules)
  - Effect.* (PureScript effect modules)
`);
}

/**
 * Main function
 */
function main() {
  const args = process.argv.slice(2);

  // Parse command line arguments
  const options = {
    help: args.includes("--help") || args.includes("-h"),
    dryRun: args.includes("--dry-run") || args.includes("-d"),
    verbose: args.includes("--verbose") || args.includes("-v"),
    quiet: args.includes("--quiet") || args.includes("-q"),
  };

  if (options.help) {
    showHelp();
    return;
  }

  try {
    if (options.verbose) {
      console.log("🔍 Scanning for PureScript modules...");
      console.log(`   Looking in: ${path.join(projectRoot, CONFIG.outputDir)}`);
      console.log(`   Required files: ${CONFIG.requiredFiles.join(", ")}`);
    } else {
      console.log("🔍 Scanning for PureScript modules...");
    }

    const modules = findPureScriptModules(options);

    if (modules.length === 0) {
      console.log(
        "❌ No PureScript modules found with both index.js and index.d.ts files"
      );
      console.log('   Make sure to run "spago build" first');
      process.exit(1);
    }

    console.log(`📦 Found ${modules.length} modules:`);
    modules.forEach((module) => {
      console.log(`   - ${module.exportPath} (${module.moduleName})`);
    });

    console.log("\n🔧 Generating exports...");
    const exports = generateExports(modules);

    if (options.dryRun) {
      console.log("\n🔍 Dry run - would generate these exports:");
      console.log(JSON.stringify(exports, null, 2));
      console.log("\n✨ Dry run complete. No files were modified.");
      return;
    }

    console.log("\n📝 Updating package.json...");
    updatePackageJson(exports);

    console.log("\n✨ Done! Package.json exports have been updated.");
  } catch (error) {
    console.error("❌ Error:", error.message);
    if (options.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

// Run if this script is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
