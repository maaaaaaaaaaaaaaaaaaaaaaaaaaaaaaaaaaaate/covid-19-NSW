{ mkDerivation, aeson, base, bytestring, cassava, containers
, geojson, http-types, lens, lens-aeson, stdenv, time, wai, warp
, wreq
}:
mkDerivation {
  pname = "CovidNSW";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cassava containers geojson http-types lens
    lens-aeson time wai warp wreq
  ];
  homepage = "gnarly.dog";
  description = "Covid-19 NSW map";
  license = stdenv.lib.licenses.mit;
}
