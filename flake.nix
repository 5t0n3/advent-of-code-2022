{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs.haskellPackages; [
        (ghcWithPackages (nixpkgs.lib.attrVals ["split" "formatting" "unordered-containers"]))
        ormolu
      ];
    };

    formatter.${system} = pkgs.alejandra;
  };
}
