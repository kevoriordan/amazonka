{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionOwner
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionOwner (
  ActionOwner (
    ..
    , AWS
    , Custom
    , ThirdParty
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionOwner = ActionOwner' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AWS :: ActionOwner
pattern AWS = ActionOwner' "AWS"

pattern Custom :: ActionOwner
pattern Custom = ActionOwner' "Custom"

pattern ThirdParty :: ActionOwner
pattern ThirdParty = ActionOwner' "ThirdParty"

{-# COMPLETE
  AWS,
  Custom,
  ThirdParty,
  ActionOwner' #-}

instance FromText ActionOwner where
    parser = (ActionOwner' . mk) <$> takeText

instance ToText ActionOwner where
    toText (ActionOwner' ci) = original ci

-- | Represents an enum of /known/ $ActionOwner.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionOwner where
    toEnum i = case i of
        0 -> AWS
        1 -> Custom
        2 -> ThirdParty
        _ -> (error . showText) $ "Unknown index for ActionOwner: " <> toText i
    fromEnum x = case x of
        AWS -> 0
        Custom -> 1
        ThirdParty -> 2
        ActionOwner' name -> (error . showText) $ "Unknown ActionOwner: " <> original name

-- | Represents the bounds of /known/ $ActionOwner.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionOwner where
    minBound = AWS
    maxBound = ThirdParty

instance Hashable     ActionOwner
instance NFData       ActionOwner
instance ToByteString ActionOwner
instance ToQuery      ActionOwner
instance ToHeader     ActionOwner

instance ToJSON ActionOwner where
    toJSON = toJSONText

instance FromJSON ActionOwner where
    parseJSON = parseJSONText "ActionOwner"
