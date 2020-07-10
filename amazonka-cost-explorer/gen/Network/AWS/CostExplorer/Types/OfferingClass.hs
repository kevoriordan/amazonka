{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.OfferingClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.OfferingClass (
  OfferingClass (
    ..
    , Convertible
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OfferingClass = OfferingClass' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Convertible :: OfferingClass
pattern Convertible = OfferingClass' "CONVERTIBLE"

pattern Standard :: OfferingClass
pattern Standard = OfferingClass' "STANDARD"

{-# COMPLETE
  Convertible,
  Standard,
  OfferingClass' #-}

instance FromText OfferingClass where
    parser = (OfferingClass' . mk) <$> takeText

instance ToText OfferingClass where
    toText (OfferingClass' ci) = original ci

-- | Represents an enum of /known/ $OfferingClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingClass where
    toEnum i = case i of
        0 -> Convertible
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for OfferingClass: " <> toText i
    fromEnum x = case x of
        Convertible -> 0
        Standard -> 1
        OfferingClass' name -> (error . showText) $ "Unknown OfferingClass: " <> original name

-- | Represents the bounds of /known/ $OfferingClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingClass where
    minBound = Convertible
    maxBound = Standard

instance Hashable     OfferingClass
instance NFData       OfferingClass
instance ToByteString OfferingClass
instance ToQuery      OfferingClass
instance ToHeader     OfferingClass

instance ToJSON OfferingClass where
    toJSON = toJSONText

instance FromJSON OfferingClass where
    parseJSON = parseJSONText "OfferingClass"
