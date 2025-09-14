gen:
    rm -rf output/*/index.d.ts
    npx spago run
    node scripts/fix-inline-imports.mjs
    npx prettier --write --ignore-path "" 'output/*/index.d.ts'
    npx typedoc "output/*/index.d.ts" --out docs --readme none --tsconfig tsconfig.typedoc.json
    node scripts/generate-exports.mjs


publish:
    #!/usr/bin/env bash
    source .env
    npm config set //npm.pkg.github.com/:_authToken=$GITHUB_TOKEN
    npm publish --access public --registry=https://npm.pkg.github.com/