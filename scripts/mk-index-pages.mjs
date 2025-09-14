#!/usr/bin/env node

import fs from "fs";
import path from "path";
import { execSync } from "child_process";

const docsDir = "docs";

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

// Generate HTML content
function generateIndexHtml(gitTags) {
  const currentDate = new Date().toISOString().split("T")[0];

  // Use git tags as the source of truth, already sorted newest first
  const sortedVersions = gitTags;

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
        </div>
        
        <div class="content">
            <div class="version-list">
                ${
                  sortedVersions.length > 0
                    ? sortedVersions
                        .map(
                          (version) => `
                            <div class="version-item">
                                <div class="version-info">
                                    <div class="version-badge">v${version}</div>
                                </div>
                                <div class="version-links">
                                    <a href="api/${version}/index.html">📚 API Docs</a>
                                    <a href="releases/m-bock-gcode-viewer-core-${version}.tgz" download>📦 Download</a>
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
  console.log("🔍 Scanning for git tags...");

  const gitTags = getGitTags();

  console.log(`📚 Found ${gitTags.length} git tags:`, gitTags);

  const htmlContent = generateIndexHtml(gitTags);
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
