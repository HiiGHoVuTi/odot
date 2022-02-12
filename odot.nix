{ mkDerivation, base, lib }:
mkDerivation {
  pname = "odot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Odot is a todo-app made for its creator, and you too if your brain works the same way";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
