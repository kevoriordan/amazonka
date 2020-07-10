{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.StopScope
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.StopScope (
  StopScope (
    ..
    , RuleSet
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StopScope = StopScope' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern RuleSet :: StopScope
pattern RuleSet = StopScope' "RuleSet"

{-# COMPLETE
  RuleSet,
  StopScope' #-}

instance FromText StopScope where
    parser = (StopScope' . mk) <$> takeText

instance ToText StopScope where
    toText (StopScope' ci) = original ci

-- | Represents an enum of /known/ $StopScope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StopScope where
    toEnum i = case i of
        0 -> RuleSet
        _ -> (error . showText) $ "Unknown index for StopScope: " <> toText i
    fromEnum x = case x of
        RuleSet -> 0
        StopScope' name -> (error . showText) $ "Unknown StopScope: " <> original name

-- | Represents the bounds of /known/ $StopScope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StopScope where
    minBound = RuleSet
    maxBound = RuleSet

instance Hashable     StopScope
instance NFData       StopScope
instance ToByteString StopScope
instance ToQuery      StopScope
instance ToHeader     StopScope

instance FromXML StopScope where
    parseXML = parseXMLText "StopScope"
