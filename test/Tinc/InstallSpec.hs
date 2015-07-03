{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.InstallSpec (spec) where

import           Helper
import           MockedEnv
import           Test.Mockery.Action

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Mockery.Directory

import           Tinc.Cache
import           Tinc.Git
import           Tinc.Install
import           Tinc.Package
import           Tinc.Process
import           Tinc.Types

data Env = Env {
  envReadProcess :: FilePath -> [String] -> String -> IO String
, envCallProcess :: FilePath -> [String] -> IO ()
}

env :: Env
env = Env readProcess callProcess

instance Process (WithEnv Env) where
  readProcess command args input = WithEnv $ asks envReadProcess >>= liftIO . ($ input) . ($ args) . ($ command)
  callProcess command args = WithEnv $ asks envCallProcess >>= liftIO . ($ args) . ($ command)

spec :: Spec
spec = do
  let cabalSandboxInit = ("cabal", ["sandbox", "init"], touch ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/package.cache")
  describe "cabalInstallPlan" $ do
    it "returns install plan" $ do
      inTempDirectory $ do
        writeFile "foo.cabal" $ unlines [
            "name:             foo"
          , "version:          0.0.0"
          , "build-type:       Simple"
          , "cabal-version:    >= 1.10"
          , "library"
          , "  build-depends:"
          , "      base == 4.*"
          , "    , setenv == 0.1.1.3"
          ]

        sandbox <- getCurrentDirectory

        let mockedReadProcess = mock ("cabal", args, "", return cabalInstallOutput)
              where
                args = ["install", "--only-dependencies", "--enable-tests", "--dry-run", sandbox]
            mockedCallProcess = mock cabalSandboxInit

            cabalInstallOutput :: String
            cabalInstallOutput = unlines [
                "Resolving dependencies..."
              , "In order, the following would be installed (use -v for more details):"
              , "setenv-0.1.1.3"
              ]
        
            mockedEnv :: Env
            mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}

        withEnv mockedEnv (cabalInstallPlan undefined []) `shouldReturn` [Package "setenv" "0.1.1.3"]

    it "" $ do
      inTempDirectory $ do
        writeFile "foo.cabal" $ unlines [
            "name:             foo"
          , "version:          0.0.0"
          , "build-type:       Simple"
          , "cabal-version:    >= 1.10"
          , "library"
          , "  build-depends:"
          , "      base == 4.*"
          , "    , setenv"
          ]

        sandbox <- getCurrentDirectory

        let rev = "fc2b9dbb754edcc14b0d9fa21201d67bc00794ec"

            gitCache = Path (sandbox </> "git-cache")
            gitDependency = CachedGitDependency "setenv" rev
            gitDependencyPath = path gitCache </> "setenv" </> rev

        createDirectoryIfMissing True gitDependencyPath

        writeFile (gitDependencyPath </> "setenv.cabal") $ unlines [
            "name:             setenv"
          , "version:          0.1.1.2"
          , "build-type:       Simple"
          , "cabal-version:    >= 1.10"
          , "library"
          ]

        let mockedReadProcess = mock ("cabal", ["install", "--only-dependencies", "--enable-tests", "--dry-run", sandbox], "", readFile "cabal-output")
            mockedCallProcess = mockMany [
                cabalSandboxInit
              , ("cabal", ["sandbox", "add-source", gitDependencyPath], baz)
              ]
            
            baz = writeFile "cabal-output" $ unlines [
                "Resolving dependencies..."
              , "In order, the following would be installed (use -v for more details):"
              , "setenv-0.1.1.2"
              ]
        
            mockedEnv :: Env
            mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}

        withEnv mockedEnv (cabalInstallPlan gitCache [gitDependency]) `shouldReturn` [Package "setenv" "0.1.1.2" {versionGitRevision = Just rev}]

  describe "populateCache" $ do
    let mockedReadProcess = mockMany ([] :: [(String, [String], String, IO String)])
    it "uses git dependencies" $
      inTempDirectory $
      withSystemTempDirectory "tinc" $ \ (Path -> cache) ->
      withSystemTempDirectory "tinc" $ \ (Path -> gitCache) -> do
        let mockedCallProcess = mockMany $
              cabalSandboxInit :
              ("cabal", ["sandbox", "add-source", path gitCache </> "foo" </> "abc"], writeFile "add-source" "foo") :
              ("cabal", ["install", "foo-0.1.0"], (readFile "add-source" `shouldReturn` "foo") >> writeFile "install" "bar") :
              []
            mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}
        _ <- withEnv mockedEnv $
          populateCache cache gitCache [Package "foo" "0.1.0"{versionGitRevision = Just "abc"}] []
        [sandbox] <- lookupSandboxes cache
        readFile (path sandbox </> "install") `shouldReturn` "bar"

    it "stores revisions of git dependencies in the cache" $
      inTempDirectory $
      withSystemTempDirectory "tinc" $ \ (Path -> cache) ->
      withSystemTempDirectory "tinc" $ \ (Path -> gitCache) -> do
        let mockedCallProcess = mockMany $
              cabalSandboxInit :
              ("cabal", ["sandbox", "add-source", path gitCache </> "foo" </> "abc"], return ()) :
              ("cabal", ["install", "foo-0.1.0"], return ()) :
              []
            mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}
        _ <- withEnv mockedEnv $
          populateCache cache gitCache [Package "foo" "0.1.0"{versionGitRevision = Just "abc"}] []
        [sandbox] <- lookupSandboxes cache
        packageDb <- findPackageDb sandbox
        readGitRevisions packageDb `shouldReturn` [GitRevision "foo" "abc"]
