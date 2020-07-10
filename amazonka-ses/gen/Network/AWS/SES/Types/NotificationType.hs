{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.NotificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.NotificationType (
  NotificationType (
    ..
    , Bounce
    , Complaint
    , Delivery
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotificationType = NotificationType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Bounce :: NotificationType
pattern Bounce = NotificationType' "Bounce"

pattern Complaint :: NotificationType
pattern Complaint = NotificationType' "Complaint"

pattern Delivery :: NotificationType
pattern Delivery = NotificationType' "Delivery"

{-# COMPLETE
  Bounce,
  Complaint,
  Delivery,
  NotificationType' #-}

instance FromText NotificationType where
    parser = (NotificationType' . mk) <$> takeText

instance ToText NotificationType where
    toText (NotificationType' ci) = original ci

-- | Represents an enum of /known/ $NotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotificationType where
    toEnum i = case i of
        0 -> Bounce
        1 -> Complaint
        2 -> Delivery
        _ -> (error . showText) $ "Unknown index for NotificationType: " <> toText i
    fromEnum x = case x of
        Bounce -> 0
        Complaint -> 1
        Delivery -> 2
        NotificationType' name -> (error . showText) $ "Unknown NotificationType: " <> original name

-- | Represents the bounds of /known/ $NotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotificationType where
    minBound = Bounce
    maxBound = Delivery

instance Hashable     NotificationType
instance NFData       NotificationType
instance ToByteString NotificationType
instance ToQuery      NotificationType
instance ToHeader     NotificationType
