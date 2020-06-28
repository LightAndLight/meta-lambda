let
  pkgs = import <nixpkgs> {};
in
  pkgs.stdenv.mkDerivation {
    name = "meta-lambda-docs";
    src = ./.;
    buildInputs = [ 
      (with pkgs; texlive.combine { inherit (texlive) scheme-full; })
    ];
    buildPhase = ''
      pdflatex pl.tex
    '';
    installPhase = ''
      mkdir -p $out
      cp pl.pdf $out/
    '';
  }
