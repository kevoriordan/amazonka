{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatusName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationStatusName (
  AssociationStatusName (
    ..
    , ASNFailed
    , ASNPending
    , ASNSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationStatusName = AssociationStatusName' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ASNFailed :: AssociationStatusName
pattern ASNFailed = AssociationStatusName' "Failed"

pattern ASNPending :: AssociationStatusName
pattern ASNPending = AssociationStatusName' "Pending"

pattern ASNSuccess :: AssociationStatusName
pattern ASNSuccess = AssociationStatusName' "Success"

{-# COMPLETE
  ASNFailed,
  ASNPending,
  ASNSuccess,
  AssociationStatusName' #-}

instance FromText AssociationStatusName where
    parser = (AssociationStatusName' . mk) <$> takeText

instance ToText AssociationStatusName where
    toText (AssociationStatusName' ci) = original ci

-- | Represents an enum of /known/ $AssociationStatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationStatusName where
    toEnum i = case i of
        0 -> ASNFailed
        1 -> ASNPending
        2 -> ASNSuccess
        _ -> (error . showText) $ "Unknown index for AssociationStatusName: " <> toText i
    fromEnum x = case x of
        ASNFailed -> 0
        ASNPending -> 1
        ASNSuccess -> 2
        AssociationStatusName' name -> (error . showText) $ "Unknown AssociationStatusName: " <> original name

-- | Represents the bounds of /known/ $AssociationStatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationStatusName where
    minBound = ASNFailed
    maxBound = ASNSuccess

instance Hashable     AssociationStatusName
instance NFData       AssociationStatusName
instance ToByteString AssociationStatusName
instance ToQuery      AssociationStatusName
instance ToHeader     AssociationStatusName

instance ToJSON AssociationStatusName where
    toJSON = toJSONText

instance FromJSON AssociationStatusName where
    parseJSON = parseJSONText "AssociationStatusName"
