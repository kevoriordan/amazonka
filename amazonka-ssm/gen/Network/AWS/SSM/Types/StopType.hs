{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StopType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.StopType (
  StopType (
    ..
    , STCancel
    , STComplete
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StopType = StopType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern STCancel :: StopType
pattern STCancel = StopType' "Cancel"

pattern STComplete :: StopType
pattern STComplete = StopType' "Complete"

{-# COMPLETE
  STCancel,
  STComplete,
  StopType' #-}

instance FromText StopType where
    parser = (StopType' . mk) <$> takeText

instance ToText StopType where
    toText (StopType' ci) = original ci

-- | Represents an enum of /known/ $StopType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StopType where
    toEnum i = case i of
        0 -> STCancel
        1 -> STComplete
        _ -> (error . showText) $ "Unknown index for StopType: " <> toText i
    fromEnum x = case x of
        STCancel -> 0
        STComplete -> 1
        StopType' name -> (error . showText) $ "Unknown StopType: " <> original name

-- | Represents the bounds of /known/ $StopType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StopType where
    minBound = STCancel
    maxBound = STComplete

instance Hashable     StopType
instance NFData       StopType
instance ToByteString StopType
instance ToQuery      StopType
instance ToHeader     StopType

instance ToJSON StopType where
    toJSON = toJSONText
