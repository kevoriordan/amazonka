{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion (
  RelationalDatabasePasswordVersion (
    ..
    , RDPVCurrent
    , RDPVPending
    , RDPVPrevious
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RelationalDatabasePasswordVersion = RelationalDatabasePasswordVersion' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern RDPVCurrent :: RelationalDatabasePasswordVersion
pattern RDPVCurrent = RelationalDatabasePasswordVersion' "CURRENT"

pattern RDPVPending :: RelationalDatabasePasswordVersion
pattern RDPVPending = RelationalDatabasePasswordVersion' "PENDING"

pattern RDPVPrevious :: RelationalDatabasePasswordVersion
pattern RDPVPrevious = RelationalDatabasePasswordVersion' "PREVIOUS"

{-# COMPLETE
  RDPVCurrent,
  RDPVPending,
  RDPVPrevious,
  RelationalDatabasePasswordVersion' #-}

instance FromText RelationalDatabasePasswordVersion where
    parser = (RelationalDatabasePasswordVersion' . mk) <$> takeText

instance ToText RelationalDatabasePasswordVersion where
    toText (RelationalDatabasePasswordVersion' ci) = original ci

-- | Represents an enum of /known/ $RelationalDatabasePasswordVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RelationalDatabasePasswordVersion where
    toEnum i = case i of
        0 -> RDPVCurrent
        1 -> RDPVPending
        2 -> RDPVPrevious
        _ -> (error . showText) $ "Unknown index for RelationalDatabasePasswordVersion: " <> toText i
    fromEnum x = case x of
        RDPVCurrent -> 0
        RDPVPending -> 1
        RDPVPrevious -> 2
        RelationalDatabasePasswordVersion' name -> (error . showText) $ "Unknown RelationalDatabasePasswordVersion: " <> original name

-- | Represents the bounds of /known/ $RelationalDatabasePasswordVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RelationalDatabasePasswordVersion where
    minBound = RDPVCurrent
    maxBound = RDPVPrevious

instance Hashable     RelationalDatabasePasswordVersion
instance NFData       RelationalDatabasePasswordVersion
instance ToByteString RelationalDatabasePasswordVersion
instance ToQuery      RelationalDatabasePasswordVersion
instance ToHeader     RelationalDatabasePasswordVersion

instance ToJSON RelationalDatabasePasswordVersion where
    toJSON = toJSONText
