#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, FontyFruity, JuicyPixels, linear, optparse-generic, pretty-simple, rasterific-svg, svg-tree
default-extensions: DeriveAnyClass, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards
ghc-options: -Wall -Wno-incomplete-patterns
-}

{- TODO
use lenses (since svg-tree is clearly set up that way)
remove all incomplete pattern matches etc. one way or another
may not work correctly if height attribute not equal to viewbox height (ditto width)
auto-scale
dpi setting doesn't seem to make a difference to the output
what does 'extra' field in sporcle table even do?
better way of allowing user to enter name, hint, extra
empty names, hints, extra seem to mess things up (actually likely a bug in sporcle's parser)
-}

import Codec.Picture
import Control.Monad
import Data.List.Extra
import Data.Maybe
import Graphics.Rasterific.Svg
import Graphics.Svg
import Graphics.Text.TrueType
import Linear.V2
import Options.Generic
import Text.Pretty.Simple

data Args = Args
    { inSvg :: FilePath
    , outPng :: FilePath
    , outSporcle :: FilePath
    , dpi :: Int
    , debug :: Bool
    }
    deriving (Generic, ParseRecord)

data Entry = Entry
    { hint :: String
    , answer :: String
    , extra :: String
    , shape :: [V2 Int]
    , answerPos :: V2 Int
    }

data MetaData = MetaData
    { hint' :: Maybe String
    , answer' :: Maybe String
    }

main :: IO ()
main = do
    Args{..} <- getRecord "Sporcle picture click SVG helper"
    Just doc <- loadSvgFile inSvg
    when debug $ pPrint doc
    writePng outPng =<< fst <$> renderSvgDocument emptyFontCache Nothing dpi doc
    writeFile outSporcle $ unlines $ map render $ convertDoc doc

convertDoc :: Document -> [Entry]
convertDoc Document{..} = map (makeEntry . convertElem (V2 x y)) _elements
  where
    Just (x,y,_,_) = _viewBox

makeEntry :: (MetaData, [V2 Double]) -> Entry
makeEntry (MetaData{..}, vs) = Entry{..}
  where
    answer = fromMaybe "answer" answer'
    hint = fromMaybe "hint" hint'
    extra = "extra"
    shape = round <<$>> vs
    answerPos = round <$> mean vs

convertElem :: V2 Double -> Tree -> (MetaData, [V2 Double])
convertElem v = \case
    GroupTree Group{..} -> case _groupChildren of
        [ PathTree Path{..} ] ->
            (parseMetaData _attrId, map (subtract v) $ convertPath _pathDefinition)
          where
            DrawAttributes{..} = _pathDrawAttributes

-- read from a path's id tag
-- sticking to the sporcle convention, we separate by tab
--TODO this could probably be more concise
parseMetaData :: Maybe String -> MetaData
parseMetaData = (\(hint',answer') -> MetaData{..}) . \case
    Just s -> case splitOn "\t" s of
        [x1,x2] -> (Just x1, Just x2)
        [x] -> (Just x, Just x)
        [] -> (Nothing, Nothing)
    Nothing -> (Nothing, Nothing)

-- expects a MoveTo, several LineTo, then an EndPath
convertPath :: [PathCommand] -> [V2 Double]
convertPath = \case
    MoveTo OriginAbsolute [ v ] : cs -> v : f cs
  where
    f = \case
        LineTo OriginAbsolute [ v ] : cs -> v : f cs
        [EndPath] -> []

render :: Entry -> String
render Entry{..} = intercalate "\t"
    [ hint
    , answer
    , extra
    , intercalate "; " $ map vec shape
    , vec answerPos
    ]
  where
    vec (V2 x y) = show x <> "," <> show y


{- Util -}

--TODO centroid would be preferable - although in practice we'll aften manually adjust anyway
    -- besides, this point is used for top-left of text box rather than centre
mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
