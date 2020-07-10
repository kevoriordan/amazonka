{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CRType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.CRType (
  CRType (
    ..
    , EC2
    , Spot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CRType = CRType' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern EC2 :: CRType
pattern EC2 = CRType' "EC2"

pattern Spot :: CRType
pattern Spot = CRType' "SPOT"

{-# COMPLETE
  EC2,
  Spot,
  CRType' #-}

instance FromText CRType where
    parser = (CRType' . mk) <$> takeText

instance ToText CRType where
    toText (CRType' ci) = original ci

-- | Represents an enum of /known/ $CRType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CRType where
    toEnum i = case i of
        0 -> EC2
        1 -> Spot
        _ -> (error . showText) $ "Unknown index for CRType: " <> toText i
    fromEnum x = case x of
        EC2 -> 0
        Spot -> 1
        CRType' name -> (error . showText) $ "Unknown CRType: " <> original name

-- | Represents the bounds of /known/ $CRType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CRType where
    minBound = EC2
    maxBound = Spot

instance Hashable     CRType
instance NFData       CRType
instance ToByteString CRType
instance ToQuery      CRType
instance ToHeader     CRType

instance ToJSON CRType where
    toJSON = toJSONText

instance FromJSON CRType where
    parseJSON = parseJSONText "CRType"
