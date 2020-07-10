{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditNotificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditNotificationType (
  AuditNotificationType (
    ..
    , ANTSNS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditNotificationType = AuditNotificationType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ANTSNS :: AuditNotificationType
pattern ANTSNS = AuditNotificationType' "SNS"

{-# COMPLETE
  ANTSNS,
  AuditNotificationType' #-}

instance FromText AuditNotificationType where
    parser = (AuditNotificationType' . mk) <$> takeText

instance ToText AuditNotificationType where
    toText (AuditNotificationType' ci) = original ci

-- | Represents an enum of /known/ $AuditNotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditNotificationType where
    toEnum i = case i of
        0 -> ANTSNS
        _ -> (error . showText) $ "Unknown index for AuditNotificationType: " <> toText i
    fromEnum x = case x of
        ANTSNS -> 0
        AuditNotificationType' name -> (error . showText) $ "Unknown AuditNotificationType: " <> original name

-- | Represents the bounds of /known/ $AuditNotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditNotificationType where
    minBound = ANTSNS
    maxBound = ANTSNS

instance Hashable     AuditNotificationType
instance NFData       AuditNotificationType
instance ToByteString AuditNotificationType
instance ToQuery      AuditNotificationType
instance ToHeader     AuditNotificationType

instance ToJSON AuditNotificationType where
    toJSON = toJSONText

instance FromJSON AuditNotificationType where
    parseJSON = parseJSONText "AuditNotificationType"
