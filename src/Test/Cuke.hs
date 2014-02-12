{-# LANGUAGE OverloadedStrings #-}

module Test.Cuke where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.Async   as Async
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Morph
import           Control.Monad.State.Strict
import qualified Data.Aeson                 as Ae
import qualified Data.Aeson.Types           as Ae
import qualified Data.Attoparsec            as Atto
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as Sl
import qualified Data.Text                  as T
import           Lens.Family
import           Pipes
import qualified Pipes.ByteString           as Ps
import           Pipes.Core
import           Pipes.Group
import           Pipes.Network.TCP
import qualified Pipes.Prelude              as P
import           Test.Cuke.Protocol

fastMain :: Int -> IO ()
fastMain n = void $ Async.race (Conc.threadDelay (n * 1000000)) main

parseGroups :: Monad m => Atto.Parser a -> FreeT (Producer S.ByteString m) m r -> Producer (Either String a) m r
parseGroups p = folds go (Atto.parse p S.empty) end where
  go  f@Atto.Fail{} _      = f
  go  d@Atto.Done{} _      = d
  go  (Atto.Partial f) a   = f a
  end (Atto.Fail lf ctx e) = Left e
  end Atto.Partial{}       = Left "incomplete"
  end (Atto.Done _ a)      = Right a

wireIn :: MonadIO m => Socket -> Producer (Either String WireMessage) m ()
wireIn s = parseGroups Ae.json' (fromSocket s 4096 ^. Ps.lines) >-> P.map decoder where
  decoder eit = do
    val <- eit
    Ae.parseEither Ae.parseJSON val

wireOut :: MonadIO m => Socket -> Consumer WireResponse m ()
wireOut s = printer >-> toSocket s where
  printer = forever $ do
    resp <- await
    Ps.fromLazy (Ae.encode resp)
    yield "\n"

midIO :: Pipe WireMessage WireResponse IO r
midIO = hoist (return . runIdentity) mid

mid :: Pipe WireMessage WireResponse Identity r
mid = P.map go where
  go :: WireMessage -> WireResponse
  go = undefined
  -- go (StepQuery step)    = Success (Just [SuccessArg "1" [] Nothing Nothing])
  -- go (Invoke id args)    = Failure (Just $ FailureMsg (Just "1") ["2", "3", "4"] ["line 1", "line 2"])
  -- go (BeginScenario _)   = Success Nothing
  -- go (EndScenario _)     = Success Nothing
  -- go (SnippetText _ _ _) = SuccessSnippet "whatever"

middle :: Pipe (Either String WireMessage) WireResponse IO ()
middle = dropLeft >-> midIO where
  dropLeft = forever $ do
    ei <- await
    case ei of
      Left  _ -> return ()
      Right a -> yield a

main :: IO ()
main = serve (Host "localhost") "3901" $ \(sock, addr) ->
  runEffect ( wireIn sock >-> middle >-> wireOut sock )
