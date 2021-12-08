let project = import compiler/default.nix;
in
with project.pkgs;
project.shellFor {
  exactDeps = true;
  withHoogle = false;
  buildInputs = with pkgs; [ cargo rustc ];
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
