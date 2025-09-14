
VERSION := `node -p "require('./package.json').version"`

clean:
    rm -rf output

check-git-clean:
    git diff-index --quiet HEAD -- || { echo "❌ Git tree is not clean!"; exit 1; }

gen:
    rm -rf docs
    mkdir -p docs/releases
    mkdir -p docs/api
    rm -rf output/*/index.d.ts
    npx spago run
    node scripts/fix-inline-imports.mjs
    npx prettier --write --ignore-path "" 'output/*/index.d.ts'
    npx typedoc "output/*/index.d.ts" --out docs/api/{{VERSION}} --readme none --tsconfig tsconfig.typedoc.json
    node scripts/generate-exports.mjs
    npm pack --pack-destination docs/releases

publish:
    git tag v{{VERSION}}
    git push origin v{{VERSION}}
    npx gh-pages -d docs --add

run-publish: check-git-clean clean gen publish