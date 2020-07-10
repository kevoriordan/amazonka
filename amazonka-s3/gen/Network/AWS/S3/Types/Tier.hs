{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Tier (
  Tier (
    ..
    , TBulk
    , TExpedited
    , TStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Tier = Tier' (CI Text)
              deriving (Eq, Ord, Read, Show, Data, Typeable,
                        Generic)

pattern TBulk :: Tier
pattern TBulk = Tier' "Bulk"

pattern TExpedited :: Tier
pattern TExpedited = Tier' "Expedited"

pattern TStandard :: Tier
pattern TStandard = Tier' "Standard"

{-# COMPLETE
  TBulk,
  TExpedited,
  TStandard,
  Tier' #-}

instance FromText Tier where
    parser = (Tier' . mk) <$> takeText

instance ToText Tier where
    toText (Tier' ci) = original ci

-- | Represents an enum of /known/ $Tier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Tier where
    toEnum i = case i of
        0 -> TBulk
        1 -> TExpedited
        2 -> TStandard
        _ -> (error . showText) $ "Unknown index for Tier: " <> toText i
    fromEnum x = case x of
        TBulk -> 0
        TExpedited -> 1
        TStandard -> 2
        Tier' name -> (error . showText) $ "Unknown Tier: " <> original name

-- | Represents the bounds of /known/ $Tier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Tier where
    minBound = TBulk
    maxBound = TStandard

instance Hashable     Tier
instance NFData       Tier
instance ToByteString Tier
instance ToQuery      Tier
instance ToHeader     Tier

instance ToXML Tier where
    toXML = toXMLText
