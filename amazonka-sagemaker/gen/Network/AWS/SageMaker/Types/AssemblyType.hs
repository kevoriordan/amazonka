{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AssemblyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AssemblyType (
  AssemblyType (
    ..
    , ATLine
    , ATNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssemblyType = AssemblyType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ATLine :: AssemblyType
pattern ATLine = AssemblyType' "Line"

pattern ATNone :: AssemblyType
pattern ATNone = AssemblyType' "None"

{-# COMPLETE
  ATLine,
  ATNone,
  AssemblyType' #-}

instance FromText AssemblyType where
    parser = (AssemblyType' . mk) <$> takeText

instance ToText AssemblyType where
    toText (AssemblyType' ci) = original ci

-- | Represents an enum of /known/ $AssemblyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssemblyType where
    toEnum i = case i of
        0 -> ATLine
        1 -> ATNone
        _ -> (error . showText) $ "Unknown index for AssemblyType: " <> toText i
    fromEnum x = case x of
        ATLine -> 0
        ATNone -> 1
        AssemblyType' name -> (error . showText) $ "Unknown AssemblyType: " <> original name

-- | Represents the bounds of /known/ $AssemblyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssemblyType where
    minBound = ATLine
    maxBound = ATNone

instance Hashable     AssemblyType
instance NFData       AssemblyType
instance ToByteString AssemblyType
instance ToQuery      AssemblyType
instance ToHeader     AssemblyType

instance ToJSON AssemblyType where
    toJSON = toJSONText

instance FromJSON AssemblyType where
    parseJSON = parseJSONText "AssemblyType"
