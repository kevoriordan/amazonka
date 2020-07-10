{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ComputePlatform
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.ComputePlatform (
  ComputePlatform (
    ..
    , Ecs
    , Lambda
    , Server
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComputePlatform = ComputePlatform' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Ecs :: ComputePlatform
pattern Ecs = ComputePlatform' "ECS"

pattern Lambda :: ComputePlatform
pattern Lambda = ComputePlatform' "Lambda"

pattern Server :: ComputePlatform
pattern Server = ComputePlatform' "Server"

{-# COMPLETE
  Ecs,
  Lambda,
  Server,
  ComputePlatform' #-}

instance FromText ComputePlatform where
    parser = (ComputePlatform' . mk) <$> takeText

instance ToText ComputePlatform where
    toText (ComputePlatform' ci) = original ci

-- | Represents an enum of /known/ $ComputePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComputePlatform where
    toEnum i = case i of
        0 -> Ecs
        1 -> Lambda
        2 -> Server
        _ -> (error . showText) $ "Unknown index for ComputePlatform: " <> toText i
    fromEnum x = case x of
        Ecs -> 0
        Lambda -> 1
        Server -> 2
        ComputePlatform' name -> (error . showText) $ "Unknown ComputePlatform: " <> original name

-- | Represents the bounds of /known/ $ComputePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComputePlatform where
    minBound = Ecs
    maxBound = Server

instance Hashable     ComputePlatform
instance NFData       ComputePlatform
instance ToByteString ComputePlatform
instance ToQuery      ComputePlatform
instance ToHeader     ComputePlatform

instance ToJSON ComputePlatform where
    toJSON = toJSONText

instance FromJSON ComputePlatform where
    parseJSON = parseJSONText "ComputePlatform"
