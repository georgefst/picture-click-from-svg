--TODO workaround until the day 'RecordDotSyntax' lands
-- latter preferred, but not working with HLS
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

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
import Data.Bool
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Debug.Pretty.Simple (pTraceShow)
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
convertDoc doc = concat [uncurry makeEntry . trans <$> treePaths e | e <- doc._elements]
    where
        trans = second $ map $ subtract $ case doc._viewBox of
            Just (x, y, _, _) -> V2 x y
            Nothing -> trace "SVG has no viewbox" doc $ V2 1920 1080

makeEntry :: MetaData -> [V2 Double] -> Entry
makeEntry m vs =
    Entry
        { answer = fromMaybe "answer" m.answer',
          hint = fromMaybe "hint" m.hint',
          extra = "extra",
          shape = round <<$>> vs,
          answerPos = round <$> mean vs
        }

treePaths :: Tree -> [(MetaData, [V2 Double])]
treePaths = \case
    GroupTree g -> treePaths =<< g._groupChildren
    PathTree p ->
        either (\s -> trace s p._pathDefinition []) pure $
            (maybe (MetaData Nothing Nothing) parseMetaData p._pathDrawAttributes._attrId,)
                <$> convertPath p._pathDefinition
    SymbolTree (Symbol g) -> treePaths =<< g._groupChildren
    UseTree _ (Just t) -> treePaths t
    _ -> []

-- read from a path's id tag
-- sticking to the sporcle convention, we separate by tab
parseMetaData :: String -> MetaData
parseMetaData s = MetaData {hint', answer'}
    where
        (hint', answer') =
            case splitOn "\t" s of
                [] -> (Nothing, Nothing)
                [x] -> (Just x, Just x)
                x1 : x2 : xs ->
                    applyUnless
                        (null xs)
                        (trace "Failed to parse metadata (more than one tab)" s)
                        (Just x1, Just x2)

-- expects a MoveTo, several LineTo, then an EndPath
convertPath :: [PathCommand] -> Either String [V2 Double]
convertPath = \case
    MoveTo OriginAbsolute [v] : cs -> (v :) <$> f cs
    _ -> Left "Illegal start of path"
    where
        f = \case
            LineTo OriginAbsolute [v] : cs -> (v :) <$> f cs
            [EndPath] -> pure []
            _ -> Left "Malformed path"

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

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

applyUnless :: Bool -> (a -> a) -> a -> a
applyUnless = applyWhen . not

--TODO using a pair is a bit of hack - might be time to make that indentation PR...
trace :: Show a => String -> a -> b -> b
trace s x r = pTraceShow (s, x) r
