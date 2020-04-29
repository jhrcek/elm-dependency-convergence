{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson          (FromJSON, eitherDecodeFileStrict,
                                      parseJSON, withObject, (.:))
import           Data.Foldable       (for_)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text, pack)
import           System.Exit         (die, exitFailure)
import           System.IO           (stderr)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text.IO        as Text

main :: IO ()
main = do
    versions <- traverse loadDirectDeps elmJsons
    let pkgs = findPackagesWithMoreVersions versions
    reportVersionDifferences pkgs versions

elmJsons :: [FilePath]
elmJsons =
    [ -- TODO read list of elm.jsons from STDIN or search current directory recursively
    ]

type Package = Text -- "elm/json"

type Version = Text -- "1.0.0"

data Versions =
    Versions
        { _versionMap :: HashMap Package Version
        , _elmJson    :: FilePath
        }

instance FromJSON Versions where
    parseJSON =
        withObject "elm.json" $ \o -> do
            deps <- o .: "dependencies"
            depMap <- deps .: "direct"
            pure $ Versions depMap ""

loadDirectDeps :: FilePath -> IO Versions
loadDirectDeps elmJson =
    eitherDecodeFileStrict elmJson >>=
    either
        (\parseError ->
             die $
             "Failed to load dependency versions from " <>
             elmJson <> " : " <> parseError)
        (\(Versions versionMap _) -> return $ Versions versionMap elmJson)

findPackagesWithMoreVersions :: [Versions] -> [Package]
findPackagesWithMoreVersions =
    Map.keys .
    Map.filter ((> 1) . Set.size) .
    foldl (Map.unionWith Set.union) Map.empty .
    fmap (fmap Set.singleton . _versionMap)

reportVersionDifferences :: [Package] -> [Versions] -> IO ()
reportVersionDifferences [] _ = putErr "All versions are aligned"
reportVersionDifferences packages versions = do
    for_ packages $ \pkg -> do
        putErr $ "Different versions of " <> pkg <> ":"
        for_ versions $ \(Versions versionMap elmJson) ->
            for_ (Map.lookup pkg versionMap) $ \version ->
                putErr $ "    " <> pack elmJson <> " has " <> version
    exitFailure

putErr :: Text -> IO ()
putErr = Text.hPutStrLn stderr
