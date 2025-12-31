{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakelight.url = "github:nix-community/flakelight";
  };
  
  outputs = { flakelight, ... }@inputs:
    flakelight ./. {
      devShell = pkgs: {
        packages = with pkgs; [
          emacs-pgtk
        ];
      };
    };
}
