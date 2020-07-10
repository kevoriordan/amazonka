{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.DialogActionType (
  DialogActionType (
    ..
    , DATClose
    , DATConfirmIntent
    , DATDelegate
    , DATElicitIntent
    , DATElicitSlot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DialogActionType = DialogActionType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DATClose :: DialogActionType
pattern DATClose = DialogActionType' "Close"

pattern DATConfirmIntent :: DialogActionType
pattern DATConfirmIntent = DialogActionType' "ConfirmIntent"

pattern DATDelegate :: DialogActionType
pattern DATDelegate = DialogActionType' "Delegate"

pattern DATElicitIntent :: DialogActionType
pattern DATElicitIntent = DialogActionType' "ElicitIntent"

pattern DATElicitSlot :: DialogActionType
pattern DATElicitSlot = DialogActionType' "ElicitSlot"

{-# COMPLETE
  DATClose,
  DATConfirmIntent,
  DATDelegate,
  DATElicitIntent,
  DATElicitSlot,
  DialogActionType' #-}

instance FromText DialogActionType where
    parser = (DialogActionType' . mk) <$> takeText

instance ToText DialogActionType where
    toText (DialogActionType' ci) = original ci

-- | Represents an enum of /known/ $DialogActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DialogActionType where
    toEnum i = case i of
        0 -> DATClose
        1 -> DATConfirmIntent
        2 -> DATDelegate
        3 -> DATElicitIntent
        4 -> DATElicitSlot
        _ -> (error . showText) $ "Unknown index for DialogActionType: " <> toText i
    fromEnum x = case x of
        DATClose -> 0
        DATConfirmIntent -> 1
        DATDelegate -> 2
        DATElicitIntent -> 3
        DATElicitSlot -> 4
        DialogActionType' name -> (error . showText) $ "Unknown DialogActionType: " <> original name

-- | Represents the bounds of /known/ $DialogActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DialogActionType where
    minBound = DATClose
    maxBound = DATElicitSlot

instance Hashable     DialogActionType
instance NFData       DialogActionType
instance ToByteString DialogActionType
instance ToQuery      DialogActionType
instance ToHeader     DialogActionType

instance ToJSON DialogActionType where
    toJSON = toJSONText

instance FromJSON DialogActionType where
    parseJSON = parseJSONText "DialogActionType"
