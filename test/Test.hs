module Main (main) where

import Codec.Picture.Png
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Graphics.Svg hiding (Dpi)
import Graphics.Text.TrueType
import Lib
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
    examples <- fmap (map dropExtension) . filterM (doesFileExist . (exampleDir </>)) =<< listDirectory exampleDir
    defaultMain . testGroup "tests" $
        examples <&> \name ->
            withResource
                (fromMaybe (error "failed to load SVG") <$> loadSvgFile (exampleDir </> name <.> "svg"))
                mempty
                $ \doc ->
                    testGroup
                        name
                        [ goldenVsString "sporcle" (outputDir </> name <.> "sporcle") $
                            TL.encodeUtf8 . TL.pack . fst . generateSporcle <$> doc
                        , goldenVsString "png" (outputDir </> name <.> "png") $
                            pure . encodePng . fst =<< makePng dpi =<< doc
                        ]

dpi :: Dpi
dpi = 100

exampleDir :: FilePath
exampleDir = "examples"

outputDir :: FilePath
outputDir = exampleDir </> "output"
