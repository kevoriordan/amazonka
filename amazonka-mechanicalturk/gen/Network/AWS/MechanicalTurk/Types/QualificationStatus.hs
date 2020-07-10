{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.QualificationStatus (
  QualificationStatus (
    ..
    , Granted
    , Revoked
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QualificationStatus = QualificationStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Granted :: QualificationStatus
pattern Granted = QualificationStatus' "Granted"

pattern Revoked :: QualificationStatus
pattern Revoked = QualificationStatus' "Revoked"

{-# COMPLETE
  Granted,
  Revoked,
  QualificationStatus' #-}

instance FromText QualificationStatus where
    parser = (QualificationStatus' . mk) <$> takeText

instance ToText QualificationStatus where
    toText (QualificationStatus' ci) = original ci

-- | Represents an enum of /known/ $QualificationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QualificationStatus where
    toEnum i = case i of
        0 -> Granted
        1 -> Revoked
        _ -> (error . showText) $ "Unknown index for QualificationStatus: " <> toText i
    fromEnum x = case x of
        Granted -> 0
        Revoked -> 1
        QualificationStatus' name -> (error . showText) $ "Unknown QualificationStatus: " <> original name

-- | Represents the bounds of /known/ $QualificationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QualificationStatus where
    minBound = Granted
    maxBound = Revoked

instance Hashable     QualificationStatus
instance NFData       QualificationStatus
instance ToByteString QualificationStatus
instance ToQuery      QualificationStatus
instance ToHeader     QualificationStatus

instance ToJSON QualificationStatus where
    toJSON = toJSONText

instance FromJSON QualificationStatus where
    parseJSON = parseJSONText "QualificationStatus"
