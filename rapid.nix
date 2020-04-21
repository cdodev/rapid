{ mkDerivation, aeson, aeson-pretty, aws-lambda-haskell-runtime
, base, binary, bytestring, case-insensitive, hpack, http-api-data
, http-client, http-types, insert-ordered-containers, iproute
, katip, lens, lens-aeson, neat-interpolation, network, process
, servant, servant-server, servant-swagger, servant-swagger-ui
, servant-swagger-ui-core, servant-swagger-ui-redoc, stdenv
, swagger2, template-haskell, text, th-lift-instances, transformers
, unliftio, unordered-containers, vault, wai, wai-extra, warp
}:
mkDerivation {
  pname = "rapid";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty aws-lambda-haskell-runtime base binary
    bytestring case-insensitive http-api-data http-client http-types
    insert-ordered-containers iproute katip lens lens-aeson
    neat-interpolation network process servant servant-server
    servant-swagger servant-swagger-ui servant-swagger-ui-core
    servant-swagger-ui-redoc swagger2 template-haskell text
    th-lift-instances transformers unliftio unordered-containers vault
    wai wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-pretty aws-lambda-haskell-runtime base binary
    bytestring case-insensitive http-api-data http-client http-types
    insert-ordered-containers iproute katip lens lens-aeson
    neat-interpolation network process servant servant-server
    servant-swagger servant-swagger-ui servant-swagger-ui-core
    servant-swagger-ui-redoc swagger2 template-haskell text
    th-lift-instances transformers unliftio unordered-containers vault
    wai wai-extra warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cdodev/rapid#readme";
  license = stdenv.lib.licenses.bsd3;
}
