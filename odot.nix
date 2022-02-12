{ mkDerivation, base, brick, directory, generic-lens, lib
, microlens, optparse-applicative, serialise, text, time, vty
}:
mkDerivation {
  pname = "odot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brick directory generic-lens microlens optparse-applicative
    serialise text time vty
  ];
  description = "Odot is a todo-app made for its creator, and you too if your brain works the same way";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
