{-# LANGUAGE NoMonomorphismRestriction #-}
--TODO workaround until the day 'RecordDotSyntax' lands
-- latter preferred, but not working with HLS
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

{- TODO
remove all incomplete pattern matches etc. one way or another
may not work correctly if height attribute not equal to viewbox height (ditto width)
auto-scale
dpi setting doesn't seem to make a difference to the output
what does 'extra' field in sporcle table even do?
empty names, hints, extra seem to mess things up (actually likely a bug in sporcle's parser)
-}

import Codec.Picture
import Control.Monad
import Data.List.Extra
import Data.Maybe
import DotHacks ()
import Graphics.Rasterific.Svg
import Graphics.Svg
import Graphics.Text.TrueType
import Linear.V2
import Options.Generic
import Text.Pretty.Simple

data Args = Args
    { inSvg :: FilePath,
      outPng :: FilePath,
      outSporcle :: FilePath,
      dpi :: Int,
      debug :: Bool
    }
    deriving (Generic, ParseRecord)

data Entry = Entry
    { hint :: String,
      answer :: String,
      extra :: String,
      shape :: [V2 Int],
      answerPos :: V2 Int
    }
    deriving (Generic)

data MetaData = MetaData
    { hint' :: Maybe String,
      answer' :: Maybe String
    }
    deriving (Generic)

main :: IO ()
main = do
    (args :: Args) <- getRecord "Sporcle picture click SVG helper"
    Just doc <- loadSvgFile args.inSvg
    when args.debug $ pPrint doc
    writePng args.outPng =<< fst <$> renderSvgDocument emptyFontCache Nothing args.dpi doc
    writeFile args.outSporcle $ unlines $ map render $ convertDoc doc

convertDoc :: Document -> [Entry]
convertDoc doc = map (uncurry makeEntry . convertElem (V2 x y)) doc._elements
    where
        Just (x, y, _, _) = doc._viewBox

makeEntry :: MetaData -> [V2 Double] -> Entry
makeEntry m vs =
    Entry
        { answer = fromMaybe "answer" m.answer',
          hint = fromMaybe "hint" m.hint',
          extra = "extra",
          shape = round <<$>> vs,
          answerPos = round <$> mean vs
        }

convertElem :: V2 Double -> Tree -> (MetaData, [V2 Double])
convertElem v = \case
    GroupTree g -> case g._groupChildren of
        [PathTree p] ->
            ( maybe (MetaData Nothing Nothing) parseMetaData p._pathDrawAttributes._attrId,
              map (subtract v) $ convertPath p._pathDefinition
            )

-- read from a path's id tag
-- sticking to the sporcle convention, we separate by tab
parseMetaData :: String -> MetaData
parseMetaData s = MetaData {hint', answer'}
    where
        (hint', answer') =
            case splitOn "\t" s of
                [x1, x2] -> (Just x1, Just x2)
                [x] -> (Just x, Just x)
                [] -> (Nothing, Nothing)

-- expects a MoveTo, several LineTo, then an EndPath
convertPath :: [PathCommand] -> [V2 Double]
convertPath = \case
    MoveTo OriginAbsolute [v] : cs -> v : f cs
    where
        f = \case
            LineTo OriginAbsolute [v] : cs -> v : f cs
            [EndPath] -> []

render :: Entry -> String
render e =
    intercalate
        "\t"
        [ e.hint,
          e.answer,
          e.extra,
          intercalate "; " $ map vec e.shape,
          vec e.answerPos
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
