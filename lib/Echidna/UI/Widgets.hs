module Echidna.UI.Widgets where

import Brick
import Brick.AttrMap qualified as A
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Reader (MonadReader, asks)
import Data.List (nub, intersperse, sortBy)
import Data.Text qualified as T
import Data.Version (showVersion)
import Graphics.Vty qualified as V
import Paths_echidna qualified (version)
import Text.Printf (printf)
import Text.Wrap

import Echidna.ABI
import Echidna.Campaign (isDone)
import Echidna.Events (Events)
import Echidna.Types.Campaign
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxResult(..))
import Echidna.UI.Report
import Echidna.Types.Config
import Data.Map (Map)
import EVM.Types (Addr, W256)
import EVM (Contract)
import qualified Brick.Widgets.Dialog as B
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data UIStateStatus = Uninitialized | Running | Timedout
data UIState = UIState
  { status :: UIStateStatus
  , campaign :: Campaign
  , fetchedContracts :: Map Addr Contract
  , fetchedSlots :: Map Addr (Map W256 W256)
  , fetchedDialog :: B.Dialog ()
  , displayFetchedDialog :: Bool
  }

attrs :: A.AttrMap
attrs = A.attrMap (V.white `on` V.black)
  [ (attrName "failure", fg V.brightRed)
  , (attrName "maximum", fg V.brightBlue)
  , (attrName "bold", fg V.white `V.withStyle` V.bold)
  , (attrName "tx", fg V.brightWhite)
  , (attrName "working", fg V.brightBlue)
  , (attrName "success", fg V.brightGreen)
  ]

bold :: Widget n -> Widget n
bold = withAttr (attrName "bold")

data Name =
  TestsViewPort
  | SBClick ClickableScrollbarElement Name
  deriving (Ord, Show, Eq)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: MonadReader EConfig m => UIState -> m (Widget Name)
campaignStatus uiState = do
  done <- isDone uiState.campaign
  case (uiState.status, done) of
    (Uninitialized, _) -> pure $ mainbox (padLeft (Pad 1) $ str "Starting up, please wait...") emptyWidget
    (Timedout, _) ->
      mainbox <$> testsWidget uiState.campaign._tests
              <*> pure (finalStatus "Timed out, C-c or esc to exit")
    (_, True) ->
      mainbox <$> testsWidget uiState.campaign._tests
              <*> pure (finalStatus "Campaign complete, C-c or esc to exit")
    _ ->
      mainbox <$> testsWidget uiState.campaign._tests
              <*> pure emptyWidget
  where
    mainbox :: Widget Name -> Widget Name -> Widget Name
    mainbox inner underneath =
      hCenter $ hLimit 120 $
        wrapInner inner underneath
    wrapInner inner underneath =
      joinBorders $ borderWithLabel (bold $ str title) $
        vLimit 5 (hLimitPercent 50 (summaryWidget uiState.campaign) <+> vBorder <+> hLimitPercent 50 (fetchCacheWidget uiState.fetchedContracts uiState.fetchedSlots))
        <=>
        hBorderWithLabel (str "Tests")
        <=>
        inner
        <=>
        underneath
    title = "Echidna " ++ showVersion Paths_echidna.version
    finalStatus s = hBorder <=> hCenter (bold $ str s)

summaryWidget :: Campaign -> Widget Name
summaryWidget c =
  padLeft (Pad 1) $
    vLimit 1 (str "Tests found: " <+> str (show (length c._tests)) <+> fill ' ')
    <=>
    str ("Seed: " ++ show c._genDict._defSeed)
    <=>
    str (ppCoverage c._coverage)
    <=>
    str (ppCorpus c._corpus)

fetchCacheWidget :: Map Addr Contract -> Map Addr (Map W256 W256) -> Widget Name
fetchCacheWidget contracts slots =
  padLeft (Pad 1) $
    str ("Fetched contracts: " <> show (length contracts))
    <=>
    str ("Fetched slots: " <> show (sum $ length <$> slots))

fetchedDialogWidget :: UIState -> Widget n
fetchedDialogWidget uiState =
  B.renderDialog uiState.fetchedDialog $ padLeftRight 1 $
    foldl (<=>) emptyWidget (Map.mapWithKey renderContract uiState.fetchedContracts)
  where
  renderContract addr _code =
    bold (str (show addr))
    <=>
    renderSlots addr
  renderSlots addr =
    foldl (<=>) emptyWidget $
      Map.mapWithKey renderSlot (fromMaybe mempty $ Map.lookup addr uiState.fetchedSlots)
  renderSlot slot value =
    padLeft (Pad 1) $ str (show slot <> " => " <> show value)


failedFirst :: EchidnaTest -> EchidnaTest -> Ordering
failedFirst t1 _ | didFailed t1 = LT
                 | otherwise   = GT

testsWidget :: MonadReader EConfig m => [EchidnaTest] -> m (Widget Name)
testsWidget tests' =
  withClickableVScrollBars SBClick .
  withVScrollBars OnRight .
  withVScrollBarHandles .
  viewport TestsViewPort Vertical .
  foldl (<=>) emptyWidget . intersperse hBorder <$>
    traverse testWidget (sortBy failedFirst tests')

testWidget :: MonadReader EConfig m => EchidnaTest -> m (Widget Name)
testWidget etest =
 case etest.testType of
      Exploration           -> widget tsWidget "exploration" ""
      PropertyTest n _      -> widget tsWidget n ""
      OptimizationTest n _  -> widget optWidget n "optimizing "
      AssertionTest _ s _   -> widget tsWidget (encodeSig s) "assertion in "
      CallTest n _          -> widget tsWidget n ""

  where
  widget f n infront = do
    (status, details) <- f (etest.testState) etest
    pure $ padLeft (Pad 1) $
      str infront <+> name n <+> str ": " <+> status
      <=> padTop (Pad 1) details
  name n = bold $ str (T.unpack n)

tsWidget :: MonadReader EConfig m
         => TestState -> EchidnaTest -> m (Widget Name, Widget Name)
tsWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
tsWidget Solved     t = failWidget Nothing t.testReproducer t.testEvents t.testValue t.testResult
tsWidget Passed     _ = pure (withAttr (attrName "success") $ str "PASSED!", emptyWidget)
tsWidget (Open i)   t = do
  n <- asks (._cConf._testLimit)
  if i >= n then
    tsWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "fuzzing " ++ progress i n, emptyWidget)
tsWidget (Large n)  t = do
  m <- asks (._cConf._shrinkLimit)
  failWidget (if n < m then Just (n,m) else Nothing) t.testReproducer t.testEvents t.testValue t.testResult

titleWidget :: Widget n
titleWidget = str "Call sequence" <+> str ":"

eventWidget :: Events -> Widget n
eventWidget es =
  if null es then str ""
  else str "Event sequence" <+> str ":"
       <=> strWrapWith wrapSettings (T.unpack $ T.intercalate "\n" es)

failWidget :: MonadReader EConfig m
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> TxResult -> m (Widget Name, Widget Name)
failWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
failWidget b xs es _ r = do
  s <- seqWidget xs
  pure (failureBadge  <+> str (" with " ++ show r), status <=> titleWidget <=> s <=> eventWidget es)
  where
  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr (attrName "working") (str ("shrinking " ++ progress n m))

optWidget :: MonadReader EConfig m
          => TestState -> EchidnaTest -> m (Widget Name, Widget Name)
optWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
optWidget Solved     _ = error "optimization tests cannot be solved"
optWidget Passed     t = pure (str $ "max value found: " ++ show t.testValue, emptyWidget)
optWidget (Open i)   t = do
  n <- asks (._cConf._testLimit)
  if i >= n then
    optWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "optimizing " ++ progress i n ++ ", current max value: " ++ show t.testValue, emptyWidget)
optWidget (Large n)  t = do
  m <- asks (._cConf._shrinkLimit)
  maxWidget (if n < m then Just (n,m) else Nothing) t.testReproducer t.testEvents t.testValue

maxWidget :: MonadReader EConfig m
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> m (Widget Name, Widget Name)
maxWidget _ [] _  _ = pure (failureBadge, str "*no transactions made*")
maxWidget b xs es v = do
  s <- seqWidget xs
  pure (maximumBadge  <+> str (" max value: " ++ show v), status <=> titleWidget <=> s <=> eventWidget es)
  where
  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr (attrName "working") (str ("shrinking " ++ progress n m))

seqWidget :: MonadReader EConfig m => [Tx] -> m (Widget Name)
seqWidget xs = do
    ppTxs <- mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs
    let ordinals = str . printf "%d." <$> [1 :: Int ..]
    pure $
      foldl (<=>) emptyWidget $
        zipWith (<+>) ordinals (withAttr (attrName "tx") . strWrapWith wrapSettings <$> ppTxs)

failureBadge :: Widget Name
failureBadge = withAttr (attrName "failure") $ str "FAILED!"

maximumBadge :: Widget Name
maximumBadge = withAttr (attrName "maximum") $ str "OPTIMIZED!"

wrapSettings :: WrapSettings
wrapSettings = defaultWrapSettings { breakLongWords = True }
