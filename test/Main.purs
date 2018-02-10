module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.DateTime as DT
import Data.DateTime.ISO (ISO(..), unwrapISO)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Maybe (fromJust)

import Partial.Unsafe (unsafePartial)

import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    describe "purescript-datetime-iso" do

        describe "decoding" do

            it "decodes standard js ISO strings (ala Date.prototype.toISOString)" do
                case decodeISO "2018-01-09T13:16:43.772Z" of
                    Left err ->
                        fail $ "decoding failed: " <> err
                    Right iso ->
                        unwrapISO iso `shouldEqual`
                            mkDateTime 2018 DT.January 9 13 16 43 772

            describe "optional characters" do

                it "doesn't need hyphens in the date" do
                    case decodeISO "20180109T13:16:43.772Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            unwrapISO iso `shouldEqual`
                                mkDateTime 2018 DT.January 9 13 16 43 772

                it "doesn't need colons in the time" do
                    case decodeISO "20180109T131643.772Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            unwrapISO iso `shouldEqual`
                                mkDateTime 2018 DT.January 9 13 16 43 772

            describe "milliseconds" do

                it "handles zero milliseconds" do
                    case decodeISO "2018-01-09T13:16:43.0Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            unwrapISO iso `shouldEqual`
                                mkDateTime 2018 DT.January 9 13 16 43 0

                it "handles empty milliseconds" do
                    case decodeISO "2018-01-09T13:16:43Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            unwrapISO iso `shouldEqual`
                                mkDateTime 2018 DT.January 9 13 16 43 0

                it "handles milliseconds 0-999" do
                    case decodeISO "2018-01-09T13:16:43.999Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            unwrapISO iso `shouldEqual`
                                mkDateTime 2018 DT.January 9 13 16 43 999

                it "handles more than 3 digits second fraction" do
                    case decodeISO "2018-01-09T13:16:43.1234Z" of
                        Left err ->
                            fail $ "decoding failed: " <> err
                        Right iso ->
                            show iso `shouldEqual`
                                "2018-01-09T13:16:43.123Z"

            describe "malformed input" do  -- malformed as far as we're concerned...

                it "fails if not YYYY MM DD" do
                    case decodeISO "2018-1-9T13:16:43.1Z" of
                        Left err -> do
                            pure unit
                        Right _ ->
                            fail "shouldn't have parsed"

                it "requires a terminating 'Z' (UTC)" do
                    case decodeISO "2018-1-9T13:16:43.0" of
                        Left err -> do
                            pure unit
                        Right _ ->
                            fail "shouldn't have parsed"

        describe "printing" do

            it "prints like an ISO string" do
                let dt = mkDateTime 2018 DT.January 9 13 16 43 772
                show (ISO dt) `shouldEqual` "2018-01-09T13:16:43.772Z"

            it "explicitly prints zero milliseconds" do
                let dt = mkDateTime 2018 DT.January 9 13 16 43 0
                show (ISO dt) `shouldEqual` "2018-01-09T13:16:43.0Z"

decodeISO :: String -> Either String ISO
decodeISO = fromString >>> decodeJson

-- Helper function for constructing DateTimes.
mkDateTime
    :: Int
    -> DT.Month
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> DT.DateTime
mkDateTime year month day hh mm ss ms =
    let
        date =
            DT.canonicalDate
                (toEnum' year)
                month
                (toEnum' day)
        time =
            DT.Time
                (toEnum' hh)
                (toEnum' mm)
                (toEnum' ss)
                (toEnum' ms)
    in
        DT.DateTime date time
    where
        toEnum' :: forall a. BoundedEnum a => Int -> a
        toEnum' = toEnum >>> unsafePartial fromJust
