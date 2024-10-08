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
import Graphics.Rasterific.Svg
import Graphics.Svg
import Graphics.Text.TrueType
import Linear.V2
import Options.Generic
import System.Console.ANSI qualified as ANSI
import System.Exit
import Text.Pretty.Simple

main :: IO ()
main = handle (\(e :: IOError) -> printError (show e) >> exitFailure) $ do
    (args :: Args) <- getRecord "Sporcle picture click SVG helper"
    loadSvgFile args.inSvg >>= \case
        Nothing -> printError "couldn't parse input file - are you sure it's an SVG?"
        Just doc -> do
            when args.debug $ pPrint doc
            writePng args.outPng . fst =<< renderSvgDocument emptyFontCache Nothing (fromMaybe 100 args.dpi) doc
            let (entries, warnings) = runWriter $ allShapes doc
            forM_ warnings $ \(Warning s x) -> do
                printWarning s
                pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} x
            writeFile args.outSporcle $ unlines $ map render entries
            putStrCol ANSI.Green "Success!\n"

allShapes :: Document -> M [Shape]
allShapes doc = do
    trans <-
        second . map . subtract <$> case doc._viewBox of
            Just (x, y, w, h) -> do
                -- TODO we ought to actually be able to adjust to these not being equal
                when (doc._width /= Just (Num w)) $ warn "width attribute not equal to viewbox width" (w, doc._width)
                when (doc._height /= Just (Num h)) $ warn "height attribute not equal to viewbox height" (h, doc._height)
                return $ V2 x y
            Nothing -> warn "SVG has no viewbox" () >> return (V2 1920 1080)
    concat <$> sequence [uncurry makeShape . trans <<$>> treePaths e | e <- doc._elements]

makeShape :: MetaData -> [V2 Double] -> Shape
makeShape meta vs =
    Shape
        { meta
        , shape = round <<$>> vs
        , answerPos = round <$> mean vs
        }

treePaths :: Tree -> M [(MetaData, [V2 Double])]
treePaths = \case
    GroupTree g -> concat <$> mapM treePaths g._groupChildren
    PathTree p -> case extractPath p._pathDefinition of
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
            { hint = "hint"
            , answer = "answer"
            , extra = "extra"
            }

-- TODO implementation is quite odd - I'm sure it could be simpler
-- read from a path's 'id' tag
-- sticking to the sporcle convention, we separate by tab
parseMetaData :: String -> M MetaData
parseMetaData s = do
    (hint, (answer, extra)) <- case uncons' $ splitOn "\t" s of
        (x0, xs0) ->
            (fromMaybe "hint" x0,) <$> case uncons' xs0 of
                (x1, xs1) ->
                    (fromMaybe "answer" x1,) <$> case uncons' xs1 of
                        (x2, xs2) -> do
                            unless (null xs2) $ warn "Failed to fully parse metadata (more than one tab character)" s
                            return $ fromMaybe "extra" x2
    return $ MetaData{hint, answer, extra}
  where
    uncons' = maybe (Nothing, []) (first Just) . uncons

-- expects a 'MoveTo', several 'LineTo', then an 'EndPath'
extractPath :: [PathCommand] -> Either String [V2 Double]
extractPath = \case
    MoveTo OriginAbsolute [v] : cs -> (v :) <$> f cs
    _ -> Left "Illegal start of path"
  where
    f = \case
        LineTo OriginAbsolute [v] : cs -> (v :) <$> f cs
        [EndPath] -> pure []
        _ -> Left "Malformed path"

render :: Shape -> String
render e =
    intercalate
        "\t"
        [ e.meta.hint
        , e.meta.answer
        , e.meta.extra
        , intercalate "; " $ map vec e.shape
        , vec e.answerPos
        ]
  where
    vec (V2 x y) = show x <> "," <> show y

data Args = Args
    { inSvg :: FilePath
    , outPng :: FilePath
    , outSporcle :: FilePath
    , dpi :: Maybe Int
    , debug :: Bool
    }
    deriving (Generic, ParseRecord)

data Shape = Shape
    { meta :: MetaData
    , shape :: [V2 Int]
    , answerPos :: V2 Int
    }
    deriving (Generic)

data MetaData = MetaData
    { hint :: String
    , answer :: String
    , extra :: String
    }
    deriving (Generic)

{- Our main monad -}
-- TODO there would be benefits to making this more abstract

type M a = Writer [Warning] a

data Warning where
    Warning :: (Show a) => String -> a -> Warning

warn :: (Show a) => String -> a -> M ()
warn = tell . pure .: Warning

{- Util -}

-- TODO centroid would be preferable - although in practice we'll aften manually adjust anyway
-- besides, this point is used for top-left of text box rather than centre
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

putStrCol :: ANSI.Color -> String -> IO ()
putStrCol c s = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull c, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
    putStr s
    ANSI.setSGR []

printWarning :: String -> IO ()
printWarning s = putStrCol ANSI.Yellow "Warning: " >> putStrLn s

printError :: String -> IO ()
printError s = putStrCol ANSI.Red "Error: " >> putStrLn s
