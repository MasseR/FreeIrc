{ mkDerivation, acid-state, aeson, async, base, bytestring
, case-insensitive, classy-prelude, conduit, conduit-combinators
, conduit-extra, containers, lens, lens-aeson, lifted-async
, lifted-base, monad-control, monad-logger, mtl, mueval, network
, network-simple, process, regex-tdfa, safecopy, stdenv, stm
, tagsoup, text, time, transformers, wreq, yaml
}:
mkDerivation {
  pname = "wtfboths";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state aeson async base bytestring case-insensitive
    classy-prelude conduit conduit-combinators conduit-extra containers
    lens lens-aeson lifted-async lifted-base monad-control monad-logger
    mtl mueval network network-simple process regex-tdfa safecopy stm
    tagsoup text time transformers wreq yaml
  ];
  homepage = "https://github.com/githubuser/wtfboths#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
