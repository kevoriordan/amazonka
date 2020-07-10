{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PrincipalType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.PrincipalType (
  PrincipalType (
    ..
    , IAM
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PrincipalType = PrincipalType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern IAM :: PrincipalType
pattern IAM = PrincipalType' "IAM"

{-# COMPLETE
  IAM,
  PrincipalType' #-}

instance FromText PrincipalType where
    parser = (PrincipalType' . mk) <$> takeText

instance ToText PrincipalType where
    toText (PrincipalType' ci) = original ci

-- | Represents an enum of /known/ $PrincipalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PrincipalType where
    toEnum i = case i of
        0 -> IAM
        _ -> (error . showText) $ "Unknown index for PrincipalType: " <> toText i
    fromEnum x = case x of
        IAM -> 0
        PrincipalType' name -> (error . showText) $ "Unknown PrincipalType: " <> original name

-- | Represents the bounds of /known/ $PrincipalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PrincipalType where
    minBound = IAM
    maxBound = IAM

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance ToJSON PrincipalType where
    toJSON = toJSONText

instance FromJSON PrincipalType where
    parseJSON = parseJSONText "PrincipalType"
