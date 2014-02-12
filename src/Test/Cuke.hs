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
import           Pipes.Lift
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
  go (StepQuery step)    = MatchResponse      (StepMatches [StepDetail step [StepArg "hello" 0] Nothing Nothing])
  go (Invoke id args)    = InvocationResponse InvocationSucceeded 
  go (BeginScenario _)   = StructuralResponse ConfirmScenario
  go (EndScenario _)     = StructuralResponse ConfirmScenarioEnd
  go (SnippetText _ _ _) = SnippetResponse    (ProvideSnippet "whatever")

middle :: Pipe (Either String WireMessage) WireResponse IO ()
middle = dieLeft >-> midIO >-> P.chain (\a -> putStr "< " >> print (Ae.encode a)) where
  dieLeft = do
    ei <- await
    case ei of
      Left  e -> lift (putStrLn e) >> return ()
      Right a -> lift (putStr "> " >> print a) >> yield a >> dieLeft

main :: IO ()
main = serve (Host "localhost") "3901" $ \(sock, addr) ->
  runEffect ( wireIn sock >-> middle >-> wireOut sock )
