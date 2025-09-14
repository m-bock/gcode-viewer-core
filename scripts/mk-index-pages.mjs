#!/usr/bin/env node

import fs from "fs";
import path from "path";
import { execSync } from "child_process";

const docsDir = "docs";
const apiDir = path.join(docsDir, "api");
const releasesDir = path.join(docsDir, "releases");

// Ensure docs directory exists
if (!fs.existsSync(docsDir)) {
  fs.mkdirSync(docsDir, { recursive: true });
}

// Get all git tags (versions)
function getGitTags() {
  try {
    const tags = execSync("git tag --sort=-version:refname", {
      encoding: "utf8",
    })
      .trim()
      .split("\n")
      .filter((tag) => tag && tag.startsWith("v"))
      .map((tag) => tag.substring(1)); // Remove 'v' prefix
    return tags;
  } catch (error) {
    console.warn("Could not get git tags:", error.message);
    return [];
  }
}

// Get existing API versions
function getApiVersions() {
  if (!fs.existsSync(apiDir)) {
    return [];
  }
  return fs
    .readdirSync(apiDir, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => dirent.name)
    .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
}

// Get existing release files
function getReleaseFiles() {
  if (!fs.existsSync(releasesDir)) {
    return [];
  }
  return fs
    .readdirSync(releasesDir)
    .filter((file) => file.endsWith(".tgz"))
    .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
}

// Generate HTML content
function generateIndexHtml(apiVersions, releaseFiles) {
  const currentDate = new Date().toISOString().split("T")[0];

  // Create a map of versions with their available resources
  const versionMap = new Map();

  // Add API versions
  apiVersions.forEach((version) => {
    if (!versionMap.has(version)) {
      versionMap.set(version, {});
    }
    versionMap.get(version).hasApi = true;
  });

  // Add release versions
  releaseFiles.forEach((file) => {
    const version =
      file.match(/gcode-viewer-core-(.+?)\.tgz/)?.[1] || "unknown";
    if (!versionMap.has(version)) {
      versionMap.set(version, {});
    }
    versionMap.get(version).hasRelease = true;
    versionMap.get(version).releaseFile = file;
  });

  // Sort versions (newest first)
  const sortedVersions = Array.from(versionMap.entries()).sort(([a], [b]) =>
    b.localeCompare(a, undefined, { numeric: true })
  );

  return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>GCode Viewer Core - Documentation & Releases</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>GCode Viewer Core</h1>
            <p>PureScript library for GCode visualization and processing</p>
        </div>
        
        <div class="content">
            <div class="version-list">
                ${
                  sortedVersions.length > 0
                    ? sortedVersions
                        .map(
                          ([version, resources]) => `
                            <div class="version-item">
                                <div class="version-info">
                                    <div class="version-badge">v${version}</div>
                                </div>
                                <div class="version-links">
                                    ${
                                      resources.hasApi
                                        ? `<a href="api/${version}/index.html">📚 API Docs</a>`
                                        : `<a class="disabled">📚 API Docs</a>`
                                    }
                                    ${
                                      resources.hasRelease
                                        ? `<a href="releases/${resources.releaseFile}" download>📦 Download</a>`
                                        : `<a class="disabled">📦 Download</a>`
                                    }
                                </div>
                            </div>
                        `
                        )
                        .join("")
                    : `
                    <div class="empty-state">No versions available yet.</div>
                `
                }
            </div>
        </div>
        
        <div class="footer">
            <p>Generated on ${currentDate} | <a href="https://github.com/m-bock/gcode-viewer-core">GitHub Repository</a></p>
        </div>
    </div>
</body>
</html>`;
}

// Main execution
function main() {
  console.log("🔍 Scanning for versions...");

  const gitTags = getGitTags();
  const apiVersions = getApiVersions();
  const releaseFiles = getReleaseFiles();

  console.log(`📚 Found ${apiVersions.length} API versions:`, apiVersions);
  console.log(
    `📦 Found ${releaseFiles.length} release files:`,
    releaseFiles.map((f) => f.replace(".tgz", ""))
  );

  const htmlContent = generateIndexHtml(apiVersions, releaseFiles);
  const indexPath = path.join(docsDir, "index.html");

  // Copy CSS file to docs directory
  const cssSourcePath = "styles/index.css";
  const cssDestPath = path.join(docsDir, "styles.css");

  if (fs.existsSync(cssSourcePath)) {
    fs.copyFileSync(cssSourcePath, cssDestPath);
    console.log(`📄 Copied CSS file: ${cssDestPath}`);
  } else {
    console.warn(`⚠️  CSS file not found: ${cssSourcePath}`);
  }

  fs.writeFileSync(indexPath, htmlContent);
  console.log(`✅ Generated index page: ${indexPath}`);
}

main();
