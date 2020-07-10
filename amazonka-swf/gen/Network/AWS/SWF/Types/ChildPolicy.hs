{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ChildPolicy (
  ChildPolicy (
    ..
    , Abandon
    , RequestCancel
    , Terminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChildPolicy = ChildPolicy' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Abandon :: ChildPolicy
pattern Abandon = ChildPolicy' "ABANDON"

pattern RequestCancel :: ChildPolicy
pattern RequestCancel = ChildPolicy' "REQUEST_CANCEL"

pattern Terminate :: ChildPolicy
pattern Terminate = ChildPolicy' "TERMINATE"

{-# COMPLETE
  Abandon,
  RequestCancel,
  Terminate,
  ChildPolicy' #-}

instance FromText ChildPolicy where
    parser = (ChildPolicy' . mk) <$> takeText

instance ToText ChildPolicy where
    toText (ChildPolicy' ci) = original ci

-- | Represents an enum of /known/ $ChildPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChildPolicy where
    toEnum i = case i of
        0 -> Abandon
        1 -> RequestCancel
        2 -> Terminate
        _ -> (error . showText) $ "Unknown index for ChildPolicy: " <> toText i
    fromEnum x = case x of
        Abandon -> 0
        RequestCancel -> 1
        Terminate -> 2
        ChildPolicy' name -> (error . showText) $ "Unknown ChildPolicy: " <> original name

-- | Represents the bounds of /known/ $ChildPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChildPolicy where
    minBound = Abandon
    maxBound = Terminate

instance Hashable     ChildPolicy
instance NFData       ChildPolicy
instance ToByteString ChildPolicy
instance ToQuery      ChildPolicy
instance ToHeader     ChildPolicy

instance ToJSON ChildPolicy where
    toJSON = toJSONText

instance FromJSON ChildPolicy where
    parseJSON = parseJSONText "ChildPolicy"
