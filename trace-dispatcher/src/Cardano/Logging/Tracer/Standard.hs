{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as TIO
import           System.IO (hFlush, stdout)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (uncurry3)

import qualified Control.Tracer as T

standardTracer :: forall m. MonadIO m => Trace m FormattedMessage
standardTracer =
    Trace $ T.arrow $ T.emit $ uncurry3 output
  where
    output ::
         LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output LoggingContext {} Nothing (FormattedHuman _c msg) = liftIO $
      TIO.putStrLn msg >> hFlush stdout
    output LoggingContext {} Nothing (FormattedMachine msg) = liftIO $
      TIO.putStrLn msg >> hFlush stdout
    output LoggingContext {} (Just Reset) _ = pure ()
    output lk (Just c@Document {}) (FormattedHuman co msg) =
       docIt
        (Stdout (if co then HumanFormatColoured else HumanFormatUncoloured))
        (FormattedHuman co "")
        (lk, Just c, msg)
    output lk (Just c@Document {}) (FormattedMachine msg) =
       docIt
        (Stdout MachineFormat)
        (FormattedMachine "")
        (lk, Just c, msg)
    output LoggingContext {} _ _a = pure ()
