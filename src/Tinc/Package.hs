module Tinc.Package (
  Package(..)
, setGitRevision
, Version(..)
, showPackage
, parsePackage
, parseInstallPlan
) where

data Package
  = Package {
    packageName :: String,
    packageVersion :: Version
  }
  deriving (Eq, Ord, Show)

setGitRevision :: String -> Package -> Package
setGitRevision revision (Package name (Version number _)) =
  Package name (Version number (Just revision))

data Version = Version {
  versionNumber :: String
, versionGitRevision :: Maybe String
} deriving (Eq, Ord, Show)

showPackage :: Package -> String
showPackage (Package name version) = name ++ "-" ++ showVersion version

showVersion :: Version -> String
showVersion (Version v _) = v

parsePackage :: String -> Package
parsePackage s = case break (== '-') (reverse s) of
  (v, '-' : p) -> Package (reverse p) (Version (reverse v) Nothing)
  _ -> Package s (Version "" Nothing)

parseInstallPlan :: String -> [Package]
parseInstallPlan = map parsePackage . concatMap (take 1 . words) . drop 2 . lines
