{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.RightsizingType (
  RightsizingType (
    ..
    , Modify
    , Terminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RightsizingType = RightsizingType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Modify :: RightsizingType
pattern Modify = RightsizingType' "MODIFY"

pattern Terminate :: RightsizingType
pattern Terminate = RightsizingType' "TERMINATE"

{-# COMPLETE
  Modify,
  Terminate,
  RightsizingType' #-}

instance FromText RightsizingType where
    parser = (RightsizingType' . mk) <$> takeText

instance ToText RightsizingType where
    toText (RightsizingType' ci) = original ci

-- | Represents an enum of /known/ $RightsizingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RightsizingType where
    toEnum i = case i of
        0 -> Modify
        1 -> Terminate
        _ -> (error . showText) $ "Unknown index for RightsizingType: " <> toText i
    fromEnum x = case x of
        Modify -> 0
        Terminate -> 1
        RightsizingType' name -> (error . showText) $ "Unknown RightsizingType: " <> original name

-- | Represents the bounds of /known/ $RightsizingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RightsizingType where
    minBound = Modify
    maxBound = Terminate

instance Hashable     RightsizingType
instance NFData       RightsizingType
instance ToByteString RightsizingType
instance ToQuery      RightsizingType
instance ToHeader     RightsizingType

instance FromJSON RightsizingType where
    parseJSON = parseJSONText "RightsizingType"
