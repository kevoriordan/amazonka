{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.OutputLocationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.OutputLocationType (
  OutputLocationType (
    ..
    , CustomerBucket
    , ServiceBucket
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OutputLocationType = OutputLocationType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern CustomerBucket :: OutputLocationType
pattern CustomerBucket = OutputLocationType' "CUSTOMER_BUCKET"

pattern ServiceBucket :: OutputLocationType
pattern ServiceBucket = OutputLocationType' "SERVICE_BUCKET"

{-# COMPLETE
  CustomerBucket,
  ServiceBucket,
  OutputLocationType' #-}

instance FromText OutputLocationType where
    parser = (OutputLocationType' . mk) <$> takeText

instance ToText OutputLocationType where
    toText (OutputLocationType' ci) = original ci

-- | Represents an enum of /known/ $OutputLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OutputLocationType where
    toEnum i = case i of
        0 -> CustomerBucket
        1 -> ServiceBucket
        _ -> (error . showText) $ "Unknown index for OutputLocationType: " <> toText i
    fromEnum x = case x of
        CustomerBucket -> 0
        ServiceBucket -> 1
        OutputLocationType' name -> (error . showText) $ "Unknown OutputLocationType: " <> original name

-- | Represents the bounds of /known/ $OutputLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OutputLocationType where
    minBound = CustomerBucket
    maxBound = ServiceBucket

instance Hashable     OutputLocationType
instance NFData       OutputLocationType
instance ToByteString OutputLocationType
instance ToQuery      OutputLocationType
instance ToHeader     OutputLocationType

instance FromJSON OutputLocationType where
    parseJSON = parseJSONText "OutputLocationType"
