module Main (main) where

import Codec.Picture
import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import Graphics.Rasterific.Svg
import Graphics.Svg
import Graphics.Text.TrueType
import Lib
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

data Args = Args
    { inSvg :: FilePath
    , outPng :: FilePath
    , outSporcle :: FilePath
    , dpi :: Maybe Int
    , debug :: Bool
    }
    deriving (Generic, ParseRecord)

putStrCol :: ANSI.Color -> String -> IO ()
putStrCol c s = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull c, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
    putStr s
    ANSI.setSGR []

printWarning :: String -> IO ()
printWarning s = putStrCol ANSI.Yellow "Warning: " >> putStrLn s

printError :: String -> IO ()
printError s = putStrCol ANSI.Red "Error: " >> putStrLn s
