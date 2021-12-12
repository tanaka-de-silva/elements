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
    brittany = "0.13.1.2";
    haskell-language-server = "latest";
  };
}
