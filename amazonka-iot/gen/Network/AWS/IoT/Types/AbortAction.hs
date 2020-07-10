{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AbortAction (
  AbortAction (
    ..
    , Cancel
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AbortAction = AbortAction' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Cancel :: AbortAction
pattern Cancel = AbortAction' "CANCEL"

{-# COMPLETE
  Cancel,
  AbortAction' #-}

instance FromText AbortAction where
    parser = (AbortAction' . mk) <$> takeText

instance ToText AbortAction where
    toText (AbortAction' ci) = original ci

-- | Represents an enum of /known/ $AbortAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AbortAction where
    toEnum i = case i of
        0 -> Cancel
        _ -> (error . showText) $ "Unknown index for AbortAction: " <> toText i
    fromEnum x = case x of
        Cancel -> 0
        AbortAction' name -> (error . showText) $ "Unknown AbortAction: " <> original name

-- | Represents the bounds of /known/ $AbortAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AbortAction where
    minBound = Cancel
    maxBound = Cancel

instance Hashable     AbortAction
instance NFData       AbortAction
instance ToByteString AbortAction
instance ToQuery      AbortAction
instance ToHeader     AbortAction

instance ToJSON AbortAction where
    toJSON = toJSONText

instance FromJSON AbortAction where
    parseJSON = parseJSONText "AbortAction"
