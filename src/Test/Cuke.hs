{-# LANGUAGE OverloadedStrings #-}

module Test.Cuke where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.Async   as Async
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Aeson                 as Ae
import qualified Data.Aeson.Types           as Ae
import qualified Data.Attoparsec            as Atto
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as Sl
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes
import qualified Pipes.Attoparsec           as Pa
import           Pipes.Network.TCP
import qualified Pipes.Prelude              as P
import qualified Pipes.Prelude              as P
import           Data.String

fastMain :: Int -> IO ()
fastMain n = void $ Async.race (Conc.threadDelay (n * 1000000)) main

main :: IO ()
main = serve (Host "localhost") "3901" $ \(sock, addr) -> do
  runEffect (fromSocket sock 4096 >-> trans >-> toSocket sock)

trans :: Pipe S.ByteString S.ByteString IO ()
trans = do
  err <- parsePipe Ae.json'
         >-> decodePipe
         >-> dropLeft
         >-> handle
         >-> P.map (Sl.toStrict . Ae.encode)
  liftIO (print err)

-- instance IsString Ae.Value where
--   fromString s = Ae.String (fromString s)

data WireMessage
  = StepMatches T.Text
  | Invoke T.Text [Ae.Value]
  | BeginScenario [T.Text]
  | EndScenario [T.Text]
  | SnippetText { stepKeyword :: T.Text
                , multilineArgClass :: T.Text
                , stepName :: T.Text
                }
  | DiffFailed
  | DiffOk
  deriving ( Eq, Show )

instance Ae.FromJSON WireMessage where
  parseJSON v = do
    packet <- Ae.parseJSON v
    case packet of
      [] -> fail "empty wire message tuple"
      ["step_matches", obj] ->
        flip (Ae.withObject "step_matches params") obj $ \o ->
          StepMatches <$> o Ae..: "name_to_match"

      ["invoke", obj] ->
        flip (Ae.withObject "invoke params") obj $ \o ->
          Invoke <$> o Ae..: "id"
                 <*> o Ae..: "args"

      ["begin_scenario"] -> pure (BeginScenario [])
      ["begin_scenario", obj] ->
        flip (Ae.withObject "begin_scenario params") obj $ \o ->
          BeginScenario <$> o Ae..: "tags"
      ["end_scenario"]   -> pure (EndScenario   [])
      ["end_scenario", obj] ->
        flip (Ae.withObject "end_scenario params") obj $ \o ->
          BeginScenario <$> o Ae..: "tags"

      ["snippet", obj] -> 
        flip (Ae.withObject "snippet params") obj $ \o ->
          SnippetText <$> o Ae..: "step_keyword"
                      <*> o Ae..: "multiline_arg_class"
                      <*> o Ae..: "step_name"

      ["diff_failed"] -> pure DiffFailed
      ["diff_ok"]     -> pure DiffOk

data WireResponse
  = Pending
  | Failure FailureMsg
  | Yikes
  | Success (Maybe [SuccessArg])
  | Diff DiffTable
  | ImmediateDiff DiffTable
  deriving ( Eq, Show )

data DiffTable = DiffTable
  deriving ( Eq, Show )

data SuccessArg
  = SuccessArg { sargId     :: T.Text
               , sargArgs   :: [Ae.Value]
               , sargSource :: T.Text
               , sargRegexp :: T.Text
               }
  deriving ( Eq, Show )

data FailureMsg
  = FailureMsg { fmsgMessage   :: T.Text
               , fmsgException :: Maybe T.Text
               , fmsgBacktrace :: Maybe T.Text
               }
  deriving ( Eq, Show )
        
handle :: Monad m => Pipe Ae.Value Ae.Value m r
handle = cat

parsePipe :: Monad m => Atto.Parser a -> Pipe S.ByteString a m String
parsePipe pa = go (Atto.parse pa) where
  go     f                         = await >>= handle . f
  handle (Atto.Done lf a)          = yield a >> handle (Atto.parse pa lf)
  handle (Atto.Partial f)          = go f
  handle (Atto.Fail _lf _ctxs err) = return err

decodePipe
  :: (Monad m, Ae.FromJSON a)
  => Pipe Ae.Value (Either (String, Ae.Value) a) m r
decodePipe = forever $ await >>= \v ->
  case Ae.parse Ae.parseJSON v of
    Ae.Success a -> yield (Right a)
    Ae.Error   e -> yield (Left (e, v))

dropLeft :: Monad m => Pipe (Either a b) b m r
dropLeft = forever $ await >>= \eit ->
  case eit of
    Left  _ -> return ()
    Right a -> yield a
