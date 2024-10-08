module Lib (
    makePng,
    generateSporcle,
    Warning (..),
)
where

{- TODO
auto-scale
dpi setting doesn't seem to make a difference to the output
what does 'extra' field in sporcle table even do?
empty names, hints, extra seem to mess things up (actually likely a bug in sporcle's parser)
-}

import Codec.Picture
import Control.Monad
import Control.Monad.Writer
import Data.Composition
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import GHC.Generics (Generic)
import Graphics.Rasterific.Svg
import Graphics.Svg hiding (Dpi, Image)
import Graphics.Text.TrueType
import Linear.V2

makePng :: Dpi -> Document -> IO (Image PixelRGBA8, LoadedElements)
makePng = renderSvgDocument emptyFontCache Nothing

generateSporcle :: Document -> (String, [Warning])
generateSporcle = first (unlines . map render) . runWriter . allShapes

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

-- read from a path's 'id' tag
-- sticking to the sporcle convention, we separate by tab
parseMetaData :: String -> M MetaData
parseMetaData s =
    splitOn "\t" s
        & ( munch (flip id . fromMaybe "hint")
                . munch (flip id . fromMaybe "answer")
                $ munch (flip \() c b a -> MetaData a b $ fromMaybe "extra" c) \remainder ->
                    unless (null remainder) $ warn "Failed to fully parse metadata (more than one tab character)" s
          )
  where
    -- TODO this implementation is a little strange
    -- it feels like we're half way to accidentally building some nice parser abstraction
    -- where parsers take subparsers as input, rather than returning the unused input portion
    -- but it's unclear how to make it any more elegant than this
    -- or whether `Parser f s a = [s] -> f a` can be given a nice `Applicative` instance
    munch f r = \case
        [] -> f Nothing <$> r [] -- if we have an empty list, run r on it
        y : ys -> f (Just y) <$> r ys -- if we have a non-empty list, use y for the current parser and run r on remainder

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

type M = Writer [Warning]

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
