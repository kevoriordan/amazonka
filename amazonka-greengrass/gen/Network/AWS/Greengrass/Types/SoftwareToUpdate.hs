{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.SoftwareToUpdate (
  SoftwareToUpdate (
    ..
    , Core
    , OtaAgent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The piece of software on the Greengrass core that will be updated.
data SoftwareToUpdate = SoftwareToUpdate' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Core :: SoftwareToUpdate
pattern Core = SoftwareToUpdate' "core"

pattern OtaAgent :: SoftwareToUpdate
pattern OtaAgent = SoftwareToUpdate' "ota_agent"

{-# COMPLETE
  Core,
  OtaAgent,
  SoftwareToUpdate' #-}

instance FromText SoftwareToUpdate where
    parser = (SoftwareToUpdate' . mk) <$> takeText

instance ToText SoftwareToUpdate where
    toText (SoftwareToUpdate' ci) = original ci

-- | Represents an enum of /known/ $SoftwareToUpdate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SoftwareToUpdate where
    toEnum i = case i of
        0 -> Core
        1 -> OtaAgent
        _ -> (error . showText) $ "Unknown index for SoftwareToUpdate: " <> toText i
    fromEnum x = case x of
        Core -> 0
        OtaAgent -> 1
        SoftwareToUpdate' name -> (error . showText) $ "Unknown SoftwareToUpdate: " <> original name

-- | Represents the bounds of /known/ $SoftwareToUpdate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SoftwareToUpdate where
    minBound = Core
    maxBound = OtaAgent

instance Hashable     SoftwareToUpdate
instance NFData       SoftwareToUpdate
instance ToByteString SoftwareToUpdate
instance ToQuery      SoftwareToUpdate
instance ToHeader     SoftwareToUpdate

instance ToJSON SoftwareToUpdate where
    toJSON = toJSONText
