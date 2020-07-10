{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily (
  UnlimitedSupportedInstanceFamily (
    ..
    , T2
    , T3
    , T3a
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data UnlimitedSupportedInstanceFamily = UnlimitedSupportedInstanceFamily' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern T2 :: UnlimitedSupportedInstanceFamily
pattern T2 = UnlimitedSupportedInstanceFamily' "t2"

pattern T3 :: UnlimitedSupportedInstanceFamily
pattern T3 = UnlimitedSupportedInstanceFamily' "t3"

pattern T3a :: UnlimitedSupportedInstanceFamily
pattern T3a = UnlimitedSupportedInstanceFamily' "t3a"

{-# COMPLETE
  T2,
  T3,
  T3a,
  UnlimitedSupportedInstanceFamily' #-}

instance FromText UnlimitedSupportedInstanceFamily where
    parser = (UnlimitedSupportedInstanceFamily' . mk) <$> takeText

instance ToText UnlimitedSupportedInstanceFamily where
    toText (UnlimitedSupportedInstanceFamily' ci) = original ci

-- | Represents an enum of /known/ $UnlimitedSupportedInstanceFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UnlimitedSupportedInstanceFamily where
    toEnum i = case i of
        0 -> T2
        1 -> T3
        2 -> T3a
        _ -> (error . showText) $ "Unknown index for UnlimitedSupportedInstanceFamily: " <> toText i
    fromEnum x = case x of
        T2 -> 0
        T3 -> 1
        T3a -> 2
        UnlimitedSupportedInstanceFamily' name -> (error . showText) $ "Unknown UnlimitedSupportedInstanceFamily: " <> original name

-- | Represents the bounds of /known/ $UnlimitedSupportedInstanceFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UnlimitedSupportedInstanceFamily where
    minBound = T2
    maxBound = T3a

instance Hashable     UnlimitedSupportedInstanceFamily
instance NFData       UnlimitedSupportedInstanceFamily
instance ToByteString UnlimitedSupportedInstanceFamily
instance ToQuery      UnlimitedSupportedInstanceFamily
instance ToHeader     UnlimitedSupportedInstanceFamily

instance FromXML UnlimitedSupportedInstanceFamily where
    parseXML = parseXMLText "UnlimitedSupportedInstanceFamily"
