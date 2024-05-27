{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Aux (runBuiltin) where

import Control.Monad.Error.Class (liftEither)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (Coercible, coerce)
import Data.Default.Class (Default (def))
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
    deBruijnTerm,
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
import PlutusCore.Pretty (prettyPlcClassicDebug)
import System.Exit (die)
import System.FilePath ((</>))
import UntypedPlutusCore qualified as UPlc
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPlc

runBuiltin ::
  String -> -- Prefix
  FilePath -> -- Path of a directory, where all the artifacts will be written to
  Maybe ExBudget ->
  DefaultFun -> -- A builtin function
  [Term TyName Name DefaultUni DefaultFun ()] -> -- Arguments to apply to the builtin function
  IO ()
runBuiltin name outputDir budget builtinFn args = do
  let term = appBulitinFun builtinFn args

  (checkedTerm, (evalResult, evalBudget, _evalLog)) <-
    either
      (die . (\err -> "(" <> name <> ") " <> "Failed to type check: " <> err) . show)
      pure
      $ withTypeCheckedUPlcTerm term
      $ (,) <$> id <*> evalUPlcTerm budget

  writeUPlcTerm (outputDir </> (name <> ".uplc")) checkedTerm
  Aeson.encodeFile (outputDir </> (name <> ".uplc.budget.result")) evalBudget

  case evalResult of
    Right finalTerm -> writeUPlcTerm (outputDir </> (name <> ".uplc.result")) finalTerm
    Left err -> die $ "(" <> name <> ") " <> "Failed to evaluate: " <> show err

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
  term' <- liftEither $ first FreeVariableErrorE $ deBruijnTerm term
  pure $ f $ eraseTerm term'

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
        UPLC.defaultCekParameters
        exBudgetMode
        UPlc.logEmitter
  where
    coerceMode ::
      (Coercible cost ExBudget) =>
      UPlc.ExBudgetMode cost DefaultUni DefaultFun ->
      UPlc.ExBudgetMode ExBudget DefaultUni DefaultFun
    coerceMode = coerce

renderUPlcTerm ::
  UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () ->
  String
renderUPlcTerm = show . prettyPlcClassicDebug

writeUPlcTerm ::
  FilePath ->
  UPlc.Term UPlc.NamedDeBruijn DefaultUni DefaultFun () ->
  IO ()
writeUPlcTerm f = writeFile f . renderUPlcTerm
