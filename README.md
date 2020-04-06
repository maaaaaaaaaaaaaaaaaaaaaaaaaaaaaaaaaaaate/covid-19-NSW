Map website to track Covid-19 cases in NSW.

Best compiled with nix. nix-build release.nix & nix-shell --run "cd js; npm run build"
Backend providing GeoJSON from data sources is built with Haskell, can install with cabal.
Frontend in js directory, built using open street maps, uses npm.
