{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetTypeEnum (
  TargetTypeEnum (
    ..
    , IP
    , Instance
    , Lambda
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetTypeEnum = TargetTypeEnum' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern IP :: TargetTypeEnum
pattern IP = TargetTypeEnum' "ip"

pattern Instance :: TargetTypeEnum
pattern Instance = TargetTypeEnum' "instance"

pattern Lambda :: TargetTypeEnum
pattern Lambda = TargetTypeEnum' "lambda"

{-# COMPLETE
  IP,
  Instance,
  Lambda,
  TargetTypeEnum' #-}

instance FromText TargetTypeEnum where
    parser = (TargetTypeEnum' . mk) <$> takeText

instance ToText TargetTypeEnum where
    toText (TargetTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $TargetTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetTypeEnum where
    toEnum i = case i of
        0 -> IP
        1 -> Instance
        2 -> Lambda
        _ -> (error . showText) $ "Unknown index for TargetTypeEnum: " <> toText i
    fromEnum x = case x of
        IP -> 0
        Instance -> 1
        Lambda -> 2
        TargetTypeEnum' name -> (error . showText) $ "Unknown TargetTypeEnum: " <> original name

-- | Represents the bounds of /known/ $TargetTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetTypeEnum where
    minBound = IP
    maxBound = Lambda

instance Hashable     TargetTypeEnum
instance NFData       TargetTypeEnum
instance ToByteString TargetTypeEnum
instance ToQuery      TargetTypeEnum
instance ToHeader     TargetTypeEnum

instance FromXML TargetTypeEnum where
    parseXML = parseXMLText "TargetTypeEnum"
