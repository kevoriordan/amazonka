{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReleaseStatusValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReleaseStatusValues (
  ReleaseStatusValues (
    ..
    , Beta
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReleaseStatusValues = ReleaseStatusValues' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Beta :: ReleaseStatusValues
pattern Beta = ReleaseStatusValues' "beta"

{-# COMPLETE
  Beta,
  ReleaseStatusValues' #-}

instance FromText ReleaseStatusValues where
    parser = (ReleaseStatusValues' . mk) <$> takeText

instance ToText ReleaseStatusValues where
    toText (ReleaseStatusValues' ci) = original ci

-- | Represents an enum of /known/ $ReleaseStatusValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReleaseStatusValues where
    toEnum i = case i of
        0 -> Beta
        _ -> (error . showText) $ "Unknown index for ReleaseStatusValues: " <> toText i
    fromEnum x = case x of
        Beta -> 0
        ReleaseStatusValues' name -> (error . showText) $ "Unknown ReleaseStatusValues: " <> original name

-- | Represents the bounds of /known/ $ReleaseStatusValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReleaseStatusValues where
    minBound = Beta
    maxBound = Beta

instance Hashable     ReleaseStatusValues
instance NFData       ReleaseStatusValues
instance ToByteString ReleaseStatusValues
instance ToQuery      ReleaseStatusValues
instance ToHeader     ReleaseStatusValues

instance FromJSON ReleaseStatusValues where
    parseJSON = parseJSONText "ReleaseStatusValues"
