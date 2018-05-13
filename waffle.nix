{ mkDerivation, aeson, base, bytestring, http-client, servant
, servant-client, stdenv, text
}:
mkDerivation {
  pname = "waffle";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client servant servant-client text
  ];
  homepage = "https://github.com/urbint/waffle";
  description = "Haskell bindings to the Waffle API";
  license = stdenv.lib.licenses.bsd3;
}
