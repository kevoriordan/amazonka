{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchAction (
  PatchAction (
    ..
    , AllowAsDependency
    , Block
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchAction = PatchAction' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AllowAsDependency :: PatchAction
pattern AllowAsDependency = PatchAction' "ALLOW_AS_DEPENDENCY"

pattern Block :: PatchAction
pattern Block = PatchAction' "BLOCK"

{-# COMPLETE
  AllowAsDependency,
  Block,
  PatchAction' #-}

instance FromText PatchAction where
    parser = (PatchAction' . mk) <$> takeText

instance ToText PatchAction where
    toText (PatchAction' ci) = original ci

-- | Represents an enum of /known/ $PatchAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchAction where
    toEnum i = case i of
        0 -> AllowAsDependency
        1 -> Block
        _ -> (error . showText) $ "Unknown index for PatchAction: " <> toText i
    fromEnum x = case x of
        AllowAsDependency -> 0
        Block -> 1
        PatchAction' name -> (error . showText) $ "Unknown PatchAction: " <> original name

-- | Represents the bounds of /known/ $PatchAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchAction where
    minBound = AllowAsDependency
    maxBound = Block

instance Hashable     PatchAction
instance NFData       PatchAction
instance ToByteString PatchAction
instance ToQuery      PatchAction
instance ToHeader     PatchAction

instance ToJSON PatchAction where
    toJSON = toJSONText

instance FromJSON PatchAction where
    parseJSON = parseJSONText "PatchAction"
