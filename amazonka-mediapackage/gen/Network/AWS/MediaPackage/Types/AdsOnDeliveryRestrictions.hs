{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions (
  AdsOnDeliveryRestrictions (
    ..
    , Both
    , None
    , Restricted
    , Unrestricted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | This setting allows the delivery restriction flags on SCTE-35 segmentation descriptors to
--
-- determine whether a message signals an ad.  Choosing "NONE" means no SCTE-35 messages become
-- ads.  Choosing "RESTRICTED" means SCTE-35 messages of the types specified in AdTriggers that
-- contain delivery restrictions will be treated as ads.  Choosing "UNRESTRICTED" means SCTE-35
-- messages of the types specified in AdTriggers that do not contain delivery restrictions will
-- be treated as ads.  Choosing "BOTH" means all SCTE-35 messages of the types specified in
-- AdTriggers will be treated as ads.  Note that Splice Insert messages do not have these flags
-- and are always treated as ads if specified in AdTriggers.
data AdsOnDeliveryRestrictions = AdsOnDeliveryRestrictions' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Both :: AdsOnDeliveryRestrictions
pattern Both = AdsOnDeliveryRestrictions' "BOTH"

pattern None :: AdsOnDeliveryRestrictions
pattern None = AdsOnDeliveryRestrictions' "NONE"

pattern Restricted :: AdsOnDeliveryRestrictions
pattern Restricted = AdsOnDeliveryRestrictions' "RESTRICTED"

pattern Unrestricted :: AdsOnDeliveryRestrictions
pattern Unrestricted = AdsOnDeliveryRestrictions' "UNRESTRICTED"

{-# COMPLETE
  Both,
  None,
  Restricted,
  Unrestricted,
  AdsOnDeliveryRestrictions' #-}

instance FromText AdsOnDeliveryRestrictions where
    parser = (AdsOnDeliveryRestrictions' . mk) <$> takeText

instance ToText AdsOnDeliveryRestrictions where
    toText (AdsOnDeliveryRestrictions' ci) = original ci

-- | Represents an enum of /known/ $AdsOnDeliveryRestrictions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdsOnDeliveryRestrictions where
    toEnum i = case i of
        0 -> Both
        1 -> None
        2 -> Restricted
        3 -> Unrestricted
        _ -> (error . showText) $ "Unknown index for AdsOnDeliveryRestrictions: " <> toText i
    fromEnum x = case x of
        Both -> 0
        None -> 1
        Restricted -> 2
        Unrestricted -> 3
        AdsOnDeliveryRestrictions' name -> (error . showText) $ "Unknown AdsOnDeliveryRestrictions: " <> original name

-- | Represents the bounds of /known/ $AdsOnDeliveryRestrictions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdsOnDeliveryRestrictions where
    minBound = Both
    maxBound = Unrestricted

instance Hashable     AdsOnDeliveryRestrictions
instance NFData       AdsOnDeliveryRestrictions
instance ToByteString AdsOnDeliveryRestrictions
instance ToQuery      AdsOnDeliveryRestrictions
instance ToHeader     AdsOnDeliveryRestrictions

instance ToJSON AdsOnDeliveryRestrictions where
    toJSON = toJSONText

instance FromJSON AdsOnDeliveryRestrictions where
    parseJSON = parseJSONText "AdsOnDeliveryRestrictions"
