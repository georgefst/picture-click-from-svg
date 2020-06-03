--TODO workaround until the day 'RecordDotSyntax' lands
-- latter preferred, but not working with HLS
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

{- TODO
auto-scale
dpi setting doesn't seem to make a difference to the output
what does 'extra' field in sporcle table even do?
empty names, hints, extra seem to mess things up (actually likely a bug in sporcle's parser)
-}

import Codec.Picture
import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Data.Composition
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import DotHacks ()
import Graphics.Rasterific.Svg
import Graphics.Svg
import Graphics.Text.TrueType
import Linear.V2
import Options.Generic
import System.Console.ANSI
import System.Exit
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
    { meta :: MetaData,
      shape :: [V2 Int],
      answerPos :: V2 Int
    }
    deriving (Generic)

data MetaData = MetaData
    { hint :: String,
      answer :: String,
      extra :: String
    }
    deriving (Generic)

--TODO there would be benefits to making this more abstract
type M a = Writer [Warning] a

data Warning where
    Warning :: Show a => String -> a -> Warning

warn :: Show a => String -> a -> M ()
warn = tell . pure .: Warning

main :: IO ()
main = handle (\(e :: IOError) -> printError (show e) >> exitFailure) $ do
    (args :: Args) <- getRecord "Sporcle picture click SVG helper"
    loadSvgFile args.inSvg >>= \case
        Nothing -> printError "couldn't parse input file - are you sure it's an SVG?"
        Just doc -> do
            when args.debug $ pPrint doc
            writePng args.outPng =<< fst <$> renderSvgDocument emptyFontCache Nothing args.dpi doc
            let (entries, warnings) = runWriter $ convertDoc doc
            forM_ warnings $ \(Warning s x) -> do
                printWarning s
                pPrint x --TODO PR for total indentation
            writeFile args.outSporcle $ unlines $ map render entries
            putStrCol Green "Success!\n"

convertDoc :: Document -> M [Entry]
convertDoc doc = do
    trans <- second . map . subtract <$> case doc._viewBox of
        Just (x, y, w, h) -> do
            --TODO we ought to actually be able to adjust to these not being equal
            when (doc._width /= Just (Num w)) $ warn "width attribute not equal to viewbox width" (w, doc._width)
            when (doc._height /= Just (Num h)) $ warn "height attribute not equal to viewbox height" (h, doc._height)
            return $ V2 x y
        Nothing -> warn "SVG has no viewbox" () >> return (V2 1920 1080)
    concat <$> sequence [uncurry makeEntry . trans <<$>> treePaths e | e <- doc._elements]

makeEntry :: MetaData -> [V2 Double] -> Entry
makeEntry meta vs =
    Entry
        { meta,
          shape = round <<$>> vs,
          answerPos = round <$> mean vs
        }

treePaths :: Tree -> M [(MetaData, [V2 Double])]
treePaths = \case
    GroupTree g -> concat <$> mapM treePaths g._groupChildren
    PathTree p -> case convertPath p._pathDefinition of
        Left s -> warn s p._pathDefinition >> return []
        Right vs -> do
            x <- maybe (return def) parseMetaData p._pathDrawAttributes._attrId
            return [(x, vs)]
    SymbolTree (Symbol g) -> concat <$> mapM treePaths g._groupChildren
    UseTree _ (Just t) -> treePaths t
    _ -> return []
    where
        def =
            MetaData
                { hint = "hint",
                  answer = "answer",
                  extra = "extra"
                }

--TODO implementation is quite odd - I'm sure it could be simpler
-- read from a path's id tag
-- sticking to the sporcle convention, we separate by tab
parseMetaData :: String -> M MetaData
parseMetaData s = do
    (hint, (answer, extra)) <- case uncons' $ splitOn "\t" s of
        (x0, xs0) -> (fromMaybe "hint" x0,) <$> case uncons' xs0 of
            (x1, xs1) -> (fromMaybe "answer" x1,) <$> case uncons' xs1 of
                (x2, xs2) -> do
                    unless (null xs2) $ warn "Failed to fully parse metadata (more than one tab character)" s
                    return $ fromMaybe "extra" x2
    return $ MetaData {hint, answer, extra}
    where
        uncons' = maybe (Nothing, []) (first Just) . uncons

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
        [ e.meta.hint,
          e.meta.answer,
          e.meta.extra,
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

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

putStrCol :: Color -> String -> IO ()
putStrCol c s = setSGR [SetColor Foreground Dull c, SetConsoleIntensity BoldIntensity] >> putStr s >> setSGR []

printWarning :: String -> IO ()
printWarning s = putStrCol Yellow "Warning: " >> putStrLn s

printError :: String -> IO ()
printError s = putStrCol Red "Error: " >> putStrLn s
