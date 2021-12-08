let
  sources = {
    haskellNix = builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/b2479523c0bafc9a6ce63cafefd38a110dd01331.tar.gz";
      sha256 = "0i77m8348klzqh2hry7gmykmkk3ngdnabnfjja2mwpynazpgvvzh";
    };
  };
  haskellNix = import sources.haskellNix { };
  pkgs = import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "elements-compiler";
    src = ./.;
  };
}
