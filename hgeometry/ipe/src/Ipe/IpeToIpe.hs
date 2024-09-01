{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.IpeToIpe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Use 'ipetoipe' to generate pdf files.
--
-- Note that all functions in this module require that 'ipetoipe' is
-- installed (it is bundled with ipe) and available on the path.

--------------------------------------------------------------------------------
module Ipe.IpeToIpe where

import           HGeometry.Interval
import           Ipe.Types (IpeFile)
import           Ipe.Writer (writeIpeFile, IpeWriteText)
import           System.Directory.OsPath (getTemporaryDirectory, removeFile)
import           System.OsPath
import qualified System.Process.Typed as Process
import           System.Random

--------------------------------------------------------------------------------

-- | Call 'ipetoipe' to produce an image of the specified type.
ipeToIpeWith                          :: Options -- ^ the options to use
                                      -> FileType -- ^ output file type
                                      -> OsPath -- ^ input file path
                                      -> OsPath -- ^ output file path
                                      -> IO ()
ipeToIpeWith options fType inFp outFp =
    do
      inFp'  <- decodeFS inFp
      outFp' <- decodeFS outFp
      Process.withProcessWait (Process.proc "ipetoipe" $ args inFp' outFp') $ \_iperenderProc ->
        pure ()
  where
    args inFp' outFp' =
           [ "-" <> show fType ] <>
           concat [ [ "-pages", pageRange r ] | PageRange r <- [pages options] ] <>
           concat [ [ "-view", pageView p v ] | Just (p,v) <- [singleView options] ] <>
           [ "-export" | Export == export options ] <>
           [ "-runlatex" | runLatex options] <>
           [ "-nozip"    | NoZip == nozip options ] <>
           [ inFp'
           , outFp'
           ]
    pageRange (ClosedInterval l u) = show l <> "-" <> show u
    pageView p v = show p <> "-" <> show v


-- | Call 'ipetoipe' with the default options.
ipeToIpe :: FileType -> OsPath -> OsPath -> IO ()
ipeToIpe = ipeToIpeWith defaultOptions

-- | Write an ipe file to disk as an ipe readable pdf file.
--
-- Note that like all other functions in this module, this requires
-- 'ipetoipe' to be installed (it is bundled with ipe) and available
-- on the path.
--
-- Note this will write soem intermediate file to your system temp dir.
writeIpeFileAsPdf      :: IpeWriteText r => OsPath -> IpeFile r -> IO ()
writeIpeFileAsPdf fp f = do num    <- randomIO @Int
                            tempDir <- getTemporaryDirectory
                            num' <- encodeUtf $ show num
                            let xmlFp = tempDir </> [osp|writeipepdf|] <> num' <.> [osp|ipe|]
                            writeIpeFile xmlFp f
                            ipeToIpe PDF xmlFp fp
                            removeFile xmlFp


data FileType = PDF | XML
  deriving (Eq,Enum)

instance Show FileType where
  show = \case
    PDF -> "pdf"
    XML -> "xml"

data Export = RetainIpeInfo | Export
  deriving (Show,Eq,Enum)

type PageNumber = Int
type ViewNumber = Int

data MarkedView = All | OnlyMarkedViews
  deriving (Show,Eq,Enum)

data NoZip = NoZip | Zip
  deriving (Show,Eq,Enum)

data PageRange = EntireFile
               | PageRange (ClosedInterval PageNumber)
               deriving (Show,Eq)

data Options = Options { export     :: Export
                       , pages      :: PageRange
                       , singleView :: Maybe (PageNumber,ViewNumber)
                       , markedView :: MarkedView
                       , runLatex   :: Bool
                       , nozip      :: NoZip
                       } deriving (Show,Eq)

defaultOptions :: Options
defaultOptions = Options RetainIpeInfo
                         EntireFile
                         Nothing -- default : entire file
                         All
                         False
                         Zip