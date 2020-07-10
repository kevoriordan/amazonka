{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.DialogState (
  DialogState (
    ..
    , ConfirmIntent
    , ElicitIntent
    , ElicitSlot
    , Failed
    , Fulfilled
    , ReadyForFulfillment
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DialogState = DialogState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ConfirmIntent :: DialogState
pattern ConfirmIntent = DialogState' "ConfirmIntent"

pattern ElicitIntent :: DialogState
pattern ElicitIntent = DialogState' "ElicitIntent"

pattern ElicitSlot :: DialogState
pattern ElicitSlot = DialogState' "ElicitSlot"

pattern Failed :: DialogState
pattern Failed = DialogState' "Failed"

pattern Fulfilled :: DialogState
pattern Fulfilled = DialogState' "Fulfilled"

pattern ReadyForFulfillment :: DialogState
pattern ReadyForFulfillment = DialogState' "ReadyForFulfillment"

{-# COMPLETE
  ConfirmIntent,
  ElicitIntent,
  ElicitSlot,
  Failed,
  Fulfilled,
  ReadyForFulfillment,
  DialogState' #-}

instance FromText DialogState where
    parser = (DialogState' . mk) <$> takeText

instance ToText DialogState where
    toText (DialogState' ci) = original ci

-- | Represents an enum of /known/ $DialogState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DialogState where
    toEnum i = case i of
        0 -> ConfirmIntent
        1 -> ElicitIntent
        2 -> ElicitSlot
        3 -> Failed
        4 -> Fulfilled
        5 -> ReadyForFulfillment
        _ -> (error . showText) $ "Unknown index for DialogState: " <> toText i
    fromEnum x = case x of
        ConfirmIntent -> 0
        ElicitIntent -> 1
        ElicitSlot -> 2
        Failed -> 3
        Fulfilled -> 4
        ReadyForFulfillment -> 5
        DialogState' name -> (error . showText) $ "Unknown DialogState: " <> original name

-- | Represents the bounds of /known/ $DialogState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DialogState where
    minBound = ConfirmIntent
    maxBound = ReadyForFulfillment

instance Hashable     DialogState
instance NFData       DialogState
instance ToByteString DialogState
instance ToQuery      DialogState
instance ToHeader     DialogState

instance FromJSON DialogState where
    parseJSON = parseJSONText "DialogState"
