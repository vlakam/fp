{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module FS (
    retrieveFS
    , cd
    , ls
    , file
    , changeExtension
    , removeDirectory
    , lsDeep
) where

import           Control.Lens     (Traversal', each, filtered, makeLenses,
                                   makePrisms, traversed, (%~), (&), (.~), (^.),
                                   (^..))
import           System.Directory (doesDirectoryExist, doesFileExist,
                                   listDirectory)
import           System.FilePath  (replaceExtension, splitPath, takeFileName,
                                   (</>))

data FS
    = Dir { _fname    :: FilePath
          , _contents :: [FS]}
    | File { _fname :: FilePath}
    deriving (Eq)

instance Show FS where
    show (File n) = n
    show o@(Dir _ _) = show' 0 o
        where
        spaces' nn = replicate nn ' '
        show' nn (File a) = spaces' nn ++ a
        show' nn (Dir a c) =
            concat [spaces' nn, a, " ->", concatMap (("\n" ++) . show' (nn + 2)) c]

makeLenses ''FS
makePrisms ''FS

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir = not . isFile

retrieveFS ::  FilePath -> IO FS
retrieveFS fs =
    ((,) <$> doesFileExist fs <*> doesDirectoryExist fs) >>= \case
        (True,False) -> pure $ File $ takeFileName fs
        (False,True) -> do
            content <- listDirectory fs
            Dir (last $ splitPath fs) <$> mapM (retrieveFS . (</>) fs) content
        _ -> undefined

{-
*Th Comonads FS Lens Th Control.Lens> retrieveFS "./test"
test ->
  Spec.hs
-}

cd :: FilePath -> Traversal' FS FS
cd dirname = contents . traversed . filtered (\x -> isDir x && x ^. fname == dirname)

ls :: Traversal' FS FS
ls = contents . each

{-
*Th Comonads FS Lens Th> fs <- retrieveFS "./src"
*Th Comonads FS Lens Th> import Control.Lens
*Th Comonads FS Lens Th Control.Lens> fs ^.. ls
[FS.hs,Th.hs,Comonads.hs,Lens.hs]
-}

file :: FilePath -> Traversal' FS FS
file filename = contents . traversed . filtered (\x -> isFile x && x ^. fname == filename)

{-
*Th Comonads FS Lens Th Control.Lens> fs ^.. file "FS.hs"
[FS.hs]
-}

files,dirs :: Traversal' FS FS
files = contents . traversed . filtered isFile
dirs = contents . traversed . filtered isDir

changeExtension :: String -> FS -> FS
changeExtension newExt = files . fname %~ flip replaceExtension newExt

{-
*Th Comonads FS Lens Th Control.Lens> fs <- retrieveFS "."
*Th Comonads FS Lens Th Control.Lens> changeExtension "petrovich" $ fs ^?! cd "src"
src ->
  FS.petrovich
  Th.petrovich
  Comonads.petrovich
  Lens.petrovich
-}

removeDirectory :: FilePath -> FS -> FS
removeDirectory fName fs = fs & contents .~ newContent
    where newContent = fs ^.. contents . traversed . filtered (\x -> not $ x ^. fname == fName && null (x ^. contents))

{-
*Th Comonads FS Lens Th Control.Lens> fs <- retrieveFS "."
*Th Comonads FS Lens Th Control.Lens> fs ^.. ls
[hw4.cabal,README.md,emptyFolder ->, src -> ...
*Th Comonads FS Lens Th Control.Lens> removeDirectory "emptyFolder" fs ^.. ls
[hw4.cabal,README.md,src -> ...
*Th Comonads FS Lens Th Control.Lens> removeDirectory "src" fs ^.. ls
[hw4.cabal,README.md,emptyFolder ->,src -> ...
-}

lsDeep :: FS -> [FilePath]
lsDeep fs = let fileNames = fs ^.. files . fname
            in (fileNames ++ (fs ^.. dirs >>= lsDeep))

{-
*Th Comonads FS Lens Th Control.Lens> fs <- retrieveFS "."
*Th Comonads FS Lens Th Control.Lens> fs & lsDeep
["hw4.cabal","README.md","LICENSE","FS.hs"...
-}
