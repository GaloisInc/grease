# TODO(lb): x86_64 compiler
{
  description = "grease examples";
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  outputs = { self, nixpkgs }:
    let
      crossPkgs = import nixpkgs {
        system = "x86_64-linux";
        crossSystem = {
          config = "armv7l-linux-musleabi";
        };
      };
      shell = crossPkgs.mkShell {
        hardeningDisable = ["all"];
        buildInputs = [
        ];
      };
    in {
      defaultPackage.x86_64-linux = shell;
      packages.x86_64-linux = {
        inherit shell;
      };
    };
}
