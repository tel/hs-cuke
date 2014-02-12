{-# LANGUAGE OverloadedStrings #-}

module Test.Cuke.Protocol where

import           Control.Applicative
import           Data.Aeson          ((.:), (.=))
import qualified Data.Aeson          as Ae
import           Data.Maybe
import qualified Data.Text           as T

data WireMessage
  = StepQuery T.Text
  | Invoke T.Text [T.Text]
  | BeginScenario [T.Text]
  | EndScenario [T.Text]
  | DiffFailed
  | DiffOk
  | SnippetText { stepKeyword       :: T.Text
                , multilineArgClass :: T.Text
                , stepName          :: T.Text
                }
  deriving ( Eq, Show )

instance Ae.FromJSON WireMessage where
  parseJSON v = do
    packet <- Ae.parseJSON v
    case packet of
      [] -> fail "empty wire message tuple"
      ["step_matches", obj] ->
        flip (Ae.withObject "step_matches params") obj $ \o ->
          StepQuery <$> o .: "name_to_match"

      ["invoke", obj] ->
        flip (Ae.withObject "invoke params") obj $ \o ->
          Invoke <$> o .: "id"
                 <*> o .: "args"

      ["begin_scenario"] -> pure (BeginScenario [])
      ["begin_scenario", obj] ->
        flip (Ae.withObject "begin_scenario params") obj $ \o ->
          BeginScenario <$> o .: "tags"
      ["end_scenario"]   -> pure (EndScenario   [])
      ["end_scenario", obj] ->
        flip (Ae.withObject "end_scenario params") obj $ \o ->
          BeginScenario <$> o .: "tags"

      ["snippet", obj] ->
        flip (Ae.withObject "snippet params") obj $ \o ->
          SnippetText <$> o .: "step_keyword"
                      <*> o .: "multiline_arg_class"
                      <*> o .: "step_name"

      ["diff_failed"] -> pure DiffFailed
      ["diff_ok"]     -> pure DiffOk

data WireResponse
  = MatchResponse      MatchResponse
  | InvocationResponse InvocationResponse
  | StructuralResponse StructuralResponse
  | SnippetResponse    ProvideSnippet
  deriving ( Eq, Show )

data MatchResponse
  = StepMatches [StepDetail]
  | StepDoesNotMatch
  deriving ( Eq, Show )

data StepDetail
  = StepDetail { identifier :: T.Text
               , arguments  :: [StepArg]
               , stepSource :: Maybe T.Text
               , stepRegexp :: Maybe T.Text
               }
  deriving ( Eq, Show )

data StepArg
  = StepArg { value    :: T.Text
            , position :: Int
            }
  deriving ( Eq, Show )

data InvocationResponse
  = InvocationPending   { reason :: T.Text }
  | InvocationSucceeded
  | DiffRequested       { recoverable :: Bool
                        -- ^ If a diff is recoverable then we'll expect the
                        -- interaction to continue. Otherwise, a failed
                        -- diff will terminate the entire interaction.
                        , diffTable   :: DiffTable
                        }
  | InvocationFailed FailureMsg
  deriving ( Eq, Show )

data StructuralResponse
  = ConfirmScenario
  | ConfirmScenarioEnd
  deriving ( Eq, Show )

data ProvideSnippet =
  ProvideSnippet { snippet :: T.Text }
  deriving ( Eq, Show )

data DiffTable = DiffTable
  deriving ( Eq, Show )

data FailureMsg
  = FailureMsg { failMessage :: Maybe T.Text
               -- ^ This should be a short description of the failure.
               , exception   :: [T.Text]
               -- ^ This ought to be a taxonomic path describing the
               -- failure. When reported the segments will be intercalated
               -- with separating dots.
               , backtrace   :: [T.Text]
               -- ^ This should report an organized list of failure
               -- conditions. Normally it'd be a backtrace, but that's not
               -- exactly what we get in Haskell.
               }
  deriving ( Eq, Show )

(.=?) :: Ae.ToJSON a => T.Text -> Maybe a -> Maybe (T.Text, Ae.Value)
_ .=? Nothing = Nothing
t .=? Just a  = Just (t .= a)

objectM :: [Maybe (T.Text, Ae.Value)] -> Ae.Value
objectM = Ae.object . catMaybes

success_ = Ae.String "success"
fail_    = Ae.String "fail"
pending_ = Ae.String "pending"
diff_    = Ae.String "diff"
diffi_   = Ae.String "diff!"

instance Ae.ToJSON StepArg where
  toJSON sa = Ae.object
    [ "val" .= value    sa
    , "pos" .= position sa
    ]

instance Ae.ToJSON StepDetail where
  toJSON sd = objectM
    [ Just ("id"     .=  identifier sd)
    , Just ("args"   .=  arguments  sd)
    ,       "source" .=? stepSource sd
    ,       "regexp" .=? stepRegexp sd
    ]

instance Ae.ToJSON MatchResponse where
  toJSON StepDoesNotMatch   = Ae.toJSON [fail_]
  toJSON (StepMatches args) = Ae.toJSON [success_, Ae.toJSON args]

instance Ae.ToJSON InvocationResponse where
  toJSON (InvocationPending reason) = Ae.toJSON [pending_, Ae.toJSON reason]
  toJSON InvocationSucceeded        = Ae.toJSON [success_]
  toJSON (InvocationFailed fmsg)    = Ae.toJSON [fail_, Ae.toJSON fmsg]
  toJSON d@DiffRequested{} 
    | recoverable d = Ae.toJSON [ diff_,  Ae.toJSON (diffTable d) ]
    | otherwise     = Ae.toJSON [ diffi_, Ae.toJSON (diffTable d) ]

instance Ae.ToJSON StructuralResponse where
  toJSON ConfirmScenario    = Ae.toJSON [success_]
  toJSON ConfirmScenarioEnd = Ae.toJSON [success_]

instance Ae.ToJSON ProvideSnippet where
  toJSON ps = Ae.toJSON [success_, Ae.toJSON (snippet ps)]

instance Ae.ToJSON FailureMsg where
  toJSON fm = objectM
    [ "message"   .=? failMessage fm
    , "exception" .=? fmap (T.intercalate ".") (mayEmpty $ exception fm)
    , "backtrace" .=? fmap T.unlines           (mayEmpty $ backtrace fm)
    ]
    where
      mayEmpty [] = Nothing
      mayEmpty xs = Just xs

instance Ae.ToJSON DiffTable where
  toJSON = undefined

instance Ae.ToJSON WireResponse where
  toJSON (MatchResponse mr)      = Ae.toJSON mr
  toJSON (InvocationResponse ir) = Ae.toJSON ir
  toJSON (StructuralResponse sr) = Ae.toJSON sr
  toJSON (SnippetResponse ps)    = Ae.toJSON ps
