format:
    fd -e hs | xargs -n 1 -P $(nproc) fourmolu -i -q

gen-docs:
    cabal build tsk-binary-format
    haskell-mustache \
        docs-templates/version.html \
        <(jq -sR '{title: "tsk binary format version 0", body: .}' <(cabal run tsk-binary-format -- version 0)) \
        > docs/v0.html
    haskell-mustache \
        docs-templates/binary-format.html \
        <(jq -sR '{grammar: .}' <(cabal run tsk-binary-format -- grammar)) \
        > docs/binary-format.html
