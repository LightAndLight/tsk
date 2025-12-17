format:
    fd -e hs | xargs -n 1 -P $(nproc) fourmolu -i -q

gen-docs:
    rm -rf docs.old
    mv docs docs.old
    mkdir -p docs

    fd '(CNAME|html|css)$' docs-src | xargs -i cp '{}' docs/

    cabal build tsk-binary-format
    haskell-mustache \
        docs-src/version.html.template \
        <(jq -sR '{title: "tsk binary format version 0", body: .}' <(cabal run tsk-binary-format -- version 0)) \
        > docs/v0.html
    haskell-mustache \
        docs-templates/binary-format.html.template \
        <(jq -sR '{grammar: .}' <(cabal run tsk-binary-format -- grammar)) \
        > docs/binary-format.html

    rm -r docs.old

cabal2nix:
    cabal2nix tsk > tsk/tsk.nix
