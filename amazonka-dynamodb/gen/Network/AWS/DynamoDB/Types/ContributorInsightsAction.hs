{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ContributorInsightsAction (
  ContributorInsightsAction (
    ..
    , Disable
    , Enable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContributorInsightsAction = ContributorInsightsAction' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Disable :: ContributorInsightsAction
pattern Disable = ContributorInsightsAction' "DISABLE"

pattern Enable :: ContributorInsightsAction
pattern Enable = ContributorInsightsAction' "ENABLE"

{-# COMPLETE
  Disable,
  Enable,
  ContributorInsightsAction' #-}

instance FromText ContributorInsightsAction where
    parser = (ContributorInsightsAction' . mk) <$> takeText

instance ToText ContributorInsightsAction where
    toText (ContributorInsightsAction' ci) = original ci

-- | Represents an enum of /known/ $ContributorInsightsAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContributorInsightsAction where
    toEnum i = case i of
        0 -> Disable
        1 -> Enable
        _ -> (error . showText) $ "Unknown index for ContributorInsightsAction: " <> toText i
    fromEnum x = case x of
        Disable -> 0
        Enable -> 1
        ContributorInsightsAction' name -> (error . showText) $ "Unknown ContributorInsightsAction: " <> original name

-- | Represents the bounds of /known/ $ContributorInsightsAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContributorInsightsAction where
    minBound = Disable
    maxBound = Enable

instance Hashable     ContributorInsightsAction
instance NFData       ContributorInsightsAction
instance ToByteString ContributorInsightsAction
instance ToQuery      ContributorInsightsAction
instance ToHeader     ContributorInsightsAction

instance ToJSON ContributorInsightsAction where
    toJSON = toJSONText
