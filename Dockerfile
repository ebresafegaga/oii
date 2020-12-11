# FROM gitpod/workspace-full

# FROM ocaml/opam2:4.10
FROM jdan/ocaml-docker

RUN opam update 

RUN opam switch create 4.10.0 && \
    eval $(opam env) && \
    opam switch 4.10.0

RUN opam install utop dune merlin
        
# RUN opam install utop dune merlin 