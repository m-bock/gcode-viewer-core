
VERSION := `node -p "require('./package.json').version"`

clean:
    rm -rf output
    rm -rf docs

check-git-clean:
    git diff-index --quiet HEAD -- || { echo "❌ Git tree is not clean!"; exit 1; }

gen-types:
    rm -rf output/*/index.d.ts
    npx spago run
    node scripts/fix-inline-imports.mjs
    npx prettier --write --ignore-path "" 'output/*/index.d.ts'

gen-type-docs:
    npx typedoc "output/*/index.d.ts" --out docs/api/{{VERSION}} --readme none --tsconfig tsconfig.typedoc.json

gen-foreign-types:
    npx tsc  ./output/Stadium.React/foreign.js --allowJs --declaration --emitDeclarationOnly 

check-exports:
    node scripts/check-exports.js

pack-release:
    mkdir -p docs/releases
    npm pack --quiet --pack-destination docs/releases

link-release:
    mkdir -p docs/v
    ln -s ../releases/m-bock-gcode-viewer-core-{{VERSION}}.tgz docs/v/{{VERSION}}

patch-package-json:
    node scripts/generate-exports.mjs
    
generate-doc-page:
    node scripts/mk-index-pages.mjs

publish:
    git push
    npx gh-pages -d docs --add

run-publish: check-git-clean clean gen-types gen-foreign-types check-exports patch-package-json gen-type-docs pack-release generate-doc-page publish

run-dev: gen-types gen-foreign-types check-exports patch-package-json