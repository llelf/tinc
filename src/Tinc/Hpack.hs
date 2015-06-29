module Tinc.Hpack (
  GitDependency(..) -- FIXME
, extractGitDependencies
) where

import           Hpack.Config
import           System.Directory

import           Tinc.Fail
import           Tinc.Git

-- TODO: merge with Tinc.Git ?
extractGitDependencies :: IO [GitDependency]
extractGitDependencies = do
  exists <- doesFileExist packageConfig
  if exists
    then do
      (_, package) <- readPackageConfig packageConfig >>= either die return
      return [GitDependency name url ref | Dependency name (Just (GitRef url ref)) <- packageDependencies package]
    else return []
