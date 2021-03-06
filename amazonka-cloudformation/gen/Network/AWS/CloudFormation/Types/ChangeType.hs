{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ChangeType (
  ChangeType (
    ..
    , CTResource
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeType = ChangeType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CTResource :: ChangeType
pattern CTResource = ChangeType' "Resource"

{-# COMPLETE
  CTResource,
  ChangeType' #-}

instance FromText ChangeType where
    parser = (ChangeType' . mk) <$> takeText

instance ToText ChangeType where
    toText (ChangeType' ci) = original ci

-- | Represents an enum of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeType where
    toEnum i = case i of
        0 -> CTResource
        _ -> (error . showText) $ "Unknown index for ChangeType: " <> toText i
    fromEnum x = case x of
        CTResource -> 0
        ChangeType' name -> (error . showText) $ "Unknown ChangeType: " <> original name

-- | Represents the bounds of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeType where
    minBound = CTResource
    maxBound = CTResource

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromXML ChangeType where
    parseXML = parseXMLText "ChangeType"
