FROM gitpod/workspace-full

FROM ocaml/opam2:4.10
        
RUN opam install utop dune merlin ocaml-lsp-server 