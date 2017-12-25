{-# LANGUAGE TemplateHaskell #-}
-- :set -XOverloadedStrings

module Comonads (
        buildProject
        , github
        , travis
        , benchs
        , ProjectBuilder
        , ProjectSettings (..)
        , Project (..)
        , Tree (..)
        ) where

import           Control.Comonad              (Comonad (..), extract)
import           Control.Comonad.Trans.Traced (Traced, runTraced, traced)
import           Control.Lens                 (makeLenses, (&), (.~), (^.))
import           Data.Text                    (Text)

data ProjectSettings = ProjectSettings
    { _settingsBenchs :: Bool  -- ^ enable benchmarks for project?
    , _settingsGithub :: Bool  -- ^ set up github     for project?
    , _settingsTravis :: Bool  -- ^ set up Travis CI  for project?
    } deriving (Show)

makeLenses ''ProjectSettings

instance Monoid ProjectSettings where
    mempty = defaultSettings
    mappend ps1 ps2 = let sBenchs =  _settingsBenchs ps1 || _settingsBenchs ps2 in
                      let sGithub =  _settingsGithub ps1 || _settingsGithub ps2 in
                      let sTravis =  _settingsTravis ps1 || _settingsTravis ps2 in
                      ProjectSettings sBenchs sGithub sTravis


data Project = Project
    { projectName :: Text
    , hasBenchs   :: Bool
    , hasGithub   :: Bool
    , hasTravis   :: Bool
    } deriving (Show)

type ProjectBuilder = Traced ProjectSettings Project

defaultSettings :: ProjectSettings
defaultSettings = ProjectSettings False False False

github :: ProjectBuilder -> Project
github builder = runTraced builder (defaultSettings & settingsGithub .~ True)

benchs :: ProjectBuilder -> Project
benchs builder = runTraced builder (defaultSettings & settingsBenchs .~ True)

travis :: ProjectBuilder -> Project
travis builder = if hasGithub $ runTraced builder defaultSettings
                 then runTraced builder (defaultSettings & settingsTravis .~ True)
                 else runTraced builder defaultSettings

addName :: Text -> ProjectSettings -> Project
addName name' settings = Project name' (settings ^. settingsBenchs) (settings ^. settingsGithub) (settings ^. settingsTravis)

buildProject :: Text -> ProjectBuilder
buildProject text = traced (addName text)


-- Tree --

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node a subForest) = Node (f a) (map (fmap f) subForest)

instance Comonad Tree where
    extract (Node a _) = a

    duplicate n@(Node _ subForest) = Node n (map duplicate subForest)
