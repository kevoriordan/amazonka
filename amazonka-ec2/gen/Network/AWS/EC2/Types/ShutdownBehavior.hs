{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ShutdownBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ShutdownBehavior (
  ShutdownBehavior (
    ..
    , SBStop
    , SBTerminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ShutdownBehavior = ShutdownBehavior' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern SBStop :: ShutdownBehavior
pattern SBStop = ShutdownBehavior' "stop"

pattern SBTerminate :: ShutdownBehavior
pattern SBTerminate = ShutdownBehavior' "terminate"

{-# COMPLETE
  SBStop,
  SBTerminate,
  ShutdownBehavior' #-}

instance FromText ShutdownBehavior where
    parser = (ShutdownBehavior' . mk) <$> takeText

instance ToText ShutdownBehavior where
    toText (ShutdownBehavior' ci) = original ci

-- | Represents an enum of /known/ $ShutdownBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShutdownBehavior where
    toEnum i = case i of
        0 -> SBStop
        1 -> SBTerminate
        _ -> (error . showText) $ "Unknown index for ShutdownBehavior: " <> toText i
    fromEnum x = case x of
        SBStop -> 0
        SBTerminate -> 1
        ShutdownBehavior' name -> (error . showText) $ "Unknown ShutdownBehavior: " <> original name

-- | Represents the bounds of /known/ $ShutdownBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShutdownBehavior where
    minBound = SBStop
    maxBound = SBTerminate

instance Hashable     ShutdownBehavior
instance NFData       ShutdownBehavior
instance ToByteString ShutdownBehavior
instance ToQuery      ShutdownBehavior
instance ToHeader     ShutdownBehavior

instance FromXML ShutdownBehavior where
    parseXML = parseXMLText "ShutdownBehavior"
