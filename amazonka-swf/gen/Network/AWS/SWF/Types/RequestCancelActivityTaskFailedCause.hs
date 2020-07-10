{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelActivityTaskFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RequestCancelActivityTaskFailedCause (
  RequestCancelActivityTaskFailedCause (
    ..
    , RCATFCActivityIdUnknown
    , RCATFCOperationNotPermitted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RequestCancelActivityTaskFailedCause = RequestCancelActivityTaskFailedCause' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern RCATFCActivityIdUnknown :: RequestCancelActivityTaskFailedCause
pattern RCATFCActivityIdUnknown = RequestCancelActivityTaskFailedCause' "ACTIVITY_ID_UNKNOWN"

pattern RCATFCOperationNotPermitted :: RequestCancelActivityTaskFailedCause
pattern RCATFCOperationNotPermitted = RequestCancelActivityTaskFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  RCATFCActivityIdUnknown,
  RCATFCOperationNotPermitted,
  RequestCancelActivityTaskFailedCause' #-}

instance FromText RequestCancelActivityTaskFailedCause where
    parser = (RequestCancelActivityTaskFailedCause' . mk) <$> takeText

instance ToText RequestCancelActivityTaskFailedCause where
    toText (RequestCancelActivityTaskFailedCause' ci) = original ci

-- | Represents an enum of /known/ $RequestCancelActivityTaskFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RequestCancelActivityTaskFailedCause where
    toEnum i = case i of
        0 -> RCATFCActivityIdUnknown
        1 -> RCATFCOperationNotPermitted
        _ -> (error . showText) $ "Unknown index for RequestCancelActivityTaskFailedCause: " <> toText i
    fromEnum x = case x of
        RCATFCActivityIdUnknown -> 0
        RCATFCOperationNotPermitted -> 1
        RequestCancelActivityTaskFailedCause' name -> (error . showText) $ "Unknown RequestCancelActivityTaskFailedCause: " <> original name

-- | Represents the bounds of /known/ $RequestCancelActivityTaskFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RequestCancelActivityTaskFailedCause where
    minBound = RCATFCActivityIdUnknown
    maxBound = RCATFCOperationNotPermitted

instance Hashable     RequestCancelActivityTaskFailedCause
instance NFData       RequestCancelActivityTaskFailedCause
instance ToByteString RequestCancelActivityTaskFailedCause
instance ToQuery      RequestCancelActivityTaskFailedCause
instance ToHeader     RequestCancelActivityTaskFailedCause

instance FromJSON RequestCancelActivityTaskFailedCause where
    parseJSON = parseJSONText "RequestCancelActivityTaskFailedCause"
