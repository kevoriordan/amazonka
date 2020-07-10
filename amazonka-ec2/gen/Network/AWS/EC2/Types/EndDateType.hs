{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EndDateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EndDateType (
  EndDateType (
    ..
    , Limited
    , Unlimited
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EndDateType = EndDateType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Limited :: EndDateType
pattern Limited = EndDateType' "limited"

pattern Unlimited :: EndDateType
pattern Unlimited = EndDateType' "unlimited"

{-# COMPLETE
  Limited,
  Unlimited,
  EndDateType' #-}

instance FromText EndDateType where
    parser = (EndDateType' . mk) <$> takeText

instance ToText EndDateType where
    toText (EndDateType' ci) = original ci

-- | Represents an enum of /known/ $EndDateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndDateType where
    toEnum i = case i of
        0 -> Limited
        1 -> Unlimited
        _ -> (error . showText) $ "Unknown index for EndDateType: " <> toText i
    fromEnum x = case x of
        Limited -> 0
        Unlimited -> 1
        EndDateType' name -> (error . showText) $ "Unknown EndDateType: " <> original name

-- | Represents the bounds of /known/ $EndDateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndDateType where
    minBound = Limited
    maxBound = Unlimited

instance Hashable     EndDateType
instance NFData       EndDateType
instance ToByteString EndDateType
instance ToQuery      EndDateType
instance ToHeader     EndDateType

instance FromXML EndDateType where
    parseXML = parseXMLText "EndDateType"
