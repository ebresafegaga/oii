FROM gitpod/workspace-full

FROM ocaml/opam2:4.10
        
RUN opam utop install dune merlin ocamllsp 