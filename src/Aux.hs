{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Aux
  ( RunTermError (RunTermError),
    RunTermErrorKind
      ( BadTerm,
        EvalFailure
      ),
    runTerm,
    appBulitinFun,
  )
where

import Control.Monad.Error.Class (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Coerce (Coercible, coerce)
import Data.Default.Class (def)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Text (Text)
import PlutusCore
  ( DefaultFun,
    DefaultUni,
    Error (FreeVariableErrorE),
    Name,
    Term,
    TyName,
    TypeCheckConfig (TypeCheckConfig),
    builtinMeaningsToTypes,
    defKindCheckConfig,
    inferType,
    runQuoteT,
  )
import PlutusCore.Compiler.Erase (eraseTerm)
import PlutusCore.Evaluation.Machine.ExBudget
  ( ExBudget,
    ExRestrictingBudget
      ( ExRestrictingBudget
      ),
  )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as UPLC
import PlutusCore.MkPlc (TermLike (builtin), mkIterAppNoAnn)
import PlutusCore.Pretty (prettyPlcClassicSimple)
import System.FilePath ((<.>), (</>))
import UntypedPlutusCore qualified as UPlc
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPlc

-- | Represent errors that can occur in the 'runTerm' function.
data RunTermError
  = RunTermError
      -- | The name of the testcase
      String
      -- | The kind of the error
      RunTermErrorKind
  deriving stock (Show, Eq)

data RunTermErrorKind
  = -- | The term doesn't type-check, or has free variables
    BadTerm (Error DefaultUni DefaultFun ())
  | -- | An error occurs while evaluating the term in the CEK machine
    EvalFailure
      (UPlc.CekEvaluationException UPlc.NamedDeBruijn DefaultUni DefaultFun)
  deriving stock (Show, Eq)

-- | 'runTerm name outputDir term' checks the validity of the given 'term' and
-- | runs it in the CEK machine. The type-erased term, evaluation result and
-- | budget are written to the 'outputDir' directory, using the following filenames:
-- | 'name'.uplc, 'name'.uplc.budget.result, 'name'.uplc.result respectively.
runTerm ::
  forall (m :: Type -> Type).
  (MonadError RunTermError m, MonadIO m) =>
  -- | Name of the test case
  String ->
  -- | Path of a directory, where all the artifacts will be written to
  FilePath ->
  -- | Term to be run
  Term TyName Name DefaultUni DefaultFun () ->
  m ()
runTerm name outputDir term = do
  case withTypeCheckedUPlcTerm term $ (,) <$> id <*> evalUPlcTerm Nothing of
    Left typeErr -> throwError . RunTermError name . BadTerm $ typeErr
    Right (checkedTerm, (result, evalBudget, _)) -> do
      writeUPlcTerm (outputDir </> (name <> ".uplc")) checkedTerm
      let budgetFile = outputDir </> name <.> "uplc" <.> "budget" <.> "expected"
      let outputFile = outputDir </> name <.> "uplc" <.> "expected"
      case result of
        Left _ -> do
          let failureText = "evaluation failure"
          liftIO . writeFile budgetFile $ failureText
          liftIO . writeFile outputFile $ failureText
        Right finalTerm -> do
          liftIO . Aeson.encodeFile budgetFile $ evalBudget
          writeUPlcTerm outputFile finalTerm

-- Helpers

-- | Apply arguments to a builtin function.
appBulitinFun ::
  DefaultFun ->
  [Term TyName Name DefaultUni DefaultFun ()] ->
  Term TyName Name DefaultUni DefaultFun ()
appBulitinFun builtinFn = mkIterAppNoAnn (builtin () builtinFn)

withTypeCheckedUPlcTerm ::
  forall (a :: Type).
  Term TyName Name DefaultUni DefaultFun () ->
  (UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () -> a) ->
  Either (Error DefaultUni DefaultFun ()) a
withTypeCheckedUPlcTerm term f = do
  runQuoteT $ do
    tcConfig <- TypeCheckConfig defKindCheckConfig <$> builtinMeaningsToTypes def ()
    void $ inferType tcConfig term
  term' <- liftEither . first FreeVariableErrorE . UPlc.deBruijnTerm . eraseTerm $ term
  pure . f $ term'

evalUPlcTerm ::
  Maybe ExBudget ->
  UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () ->
  ( Either
      (UPlc.CekEvaluationException UPlc.NamedDeBruijn DefaultUni DefaultFun)
      (UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun ()),
    ExBudget,
    [Text]
  )
evalUPlcTerm budget =
  let exBudgetMode :: UPlc.ExBudgetMode ExBudget DefaultUni DefaultFun
      exBudgetMode =
        maybe
          (coerceMode UPlc.counting)
          (coerceMode . UPlc.restricting . ExRestrictingBudget)
          budget
   in UPlc.runCekDeBruijn
        UPLC.defaultCekParametersForTesting
        exBudgetMode
        UPlc.logEmitter
  where
    coerceMode ::
      forall (cost :: Type).
      (Coercible cost ExBudget) =>
      UPlc.ExBudgetMode cost DefaultUni DefaultFun ->
      UPlc.ExBudgetMode ExBudget DefaultUni DefaultFun
    coerceMode = coerce

renderUPlcTerm ::
  UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () ->
  String
renderUPlcTerm = show . prettyPlcClassicSimple

writeUPlcTerm ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  FilePath ->
  UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () ->
  m ()
writeUPlcTerm f = liftIO . writeFile f . renderUPlcTerm
