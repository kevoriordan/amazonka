{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Compute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Compute (
  Compute (
    ..
    , Graphics
    , Graphicspro
    , Performance
    , Power
    , Powerpro
    , Standard
    , Value
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Compute = Compute' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Graphics :: Compute
pattern Graphics = Compute' "GRAPHICS"

pattern Graphicspro :: Compute
pattern Graphicspro = Compute' "GRAPHICSPRO"

pattern Performance :: Compute
pattern Performance = Compute' "PERFORMANCE"

pattern Power :: Compute
pattern Power = Compute' "POWER"

pattern Powerpro :: Compute
pattern Powerpro = Compute' "POWERPRO"

pattern Standard :: Compute
pattern Standard = Compute' "STANDARD"

pattern Value :: Compute
pattern Value = Compute' "VALUE"

{-# COMPLETE
  Graphics,
  Graphicspro,
  Performance,
  Power,
  Powerpro,
  Standard,
  Value,
  Compute' #-}

instance FromText Compute where
    parser = (Compute' . mk) <$> takeText

instance ToText Compute where
    toText (Compute' ci) = original ci

-- | Represents an enum of /known/ $Compute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Compute where
    toEnum i = case i of
        0 -> Graphics
        1 -> Graphicspro
        2 -> Performance
        3 -> Power
        4 -> Powerpro
        5 -> Standard
        6 -> Value
        _ -> (error . showText) $ "Unknown index for Compute: " <> toText i
    fromEnum x = case x of
        Graphics -> 0
        Graphicspro -> 1
        Performance -> 2
        Power -> 3
        Powerpro -> 4
        Standard -> 5
        Value -> 6
        Compute' name -> (error . showText) $ "Unknown Compute: " <> original name

-- | Represents the bounds of /known/ $Compute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Compute where
    minBound = Graphics
    maxBound = Value

instance Hashable     Compute
instance NFData       Compute
instance ToByteString Compute
instance ToQuery      Compute
instance ToHeader     Compute

instance ToJSON Compute where
    toJSON = toJSONText

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"
