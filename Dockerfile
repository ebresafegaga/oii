FROM gitpod/workspace-full

USER root

FROM ocaml/opam2:4.10
        
# RUN opam install utop dune merlin 