
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

check-exports:
    node scripts/check-exports.js

pack-release:
    mkdir -p docs/releases
    npm pack --quiet --pack-destination docs/releases

patch-package-json:
    node scripts/generate-exports.mjs
    
generate-doc-page:
    node scripts/mk-index-pages.mjs

git-tag:
    git tag v{{VERSION}}
    git push origin v{{VERSION}}

publish:
    git push
    npx gh-pages -d docs --add

run-publish: check-git-clean clean git-tag gen-types check-exports patch-package-json gen-type-docs pack-release generate-doc-page publish

run-dev: gen-types check-exports patch-package-json