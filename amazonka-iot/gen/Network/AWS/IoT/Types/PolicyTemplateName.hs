{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyTemplateName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.PolicyTemplateName (
  PolicyTemplateName (
    ..
    , BlankPolicy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyTemplateName = PolicyTemplateName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern BlankPolicy :: PolicyTemplateName
pattern BlankPolicy = PolicyTemplateName' "BLANK_POLICY"

{-# COMPLETE
  BlankPolicy,
  PolicyTemplateName' #-}

instance FromText PolicyTemplateName where
    parser = (PolicyTemplateName' . mk) <$> takeText

instance ToText PolicyTemplateName where
    toText (PolicyTemplateName' ci) = original ci

-- | Represents an enum of /known/ $PolicyTemplateName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyTemplateName where
    toEnum i = case i of
        0 -> BlankPolicy
        _ -> (error . showText) $ "Unknown index for PolicyTemplateName: " <> toText i
    fromEnum x = case x of
        BlankPolicy -> 0
        PolicyTemplateName' name -> (error . showText) $ "Unknown PolicyTemplateName: " <> original name

-- | Represents the bounds of /known/ $PolicyTemplateName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyTemplateName where
    minBound = BlankPolicy
    maxBound = BlankPolicy

instance Hashable     PolicyTemplateName
instance NFData       PolicyTemplateName
instance ToByteString PolicyTemplateName
instance ToQuery      PolicyTemplateName
instance ToHeader     PolicyTemplateName

instance ToJSON PolicyTemplateName where
    toJSON = toJSONText

instance FromJSON PolicyTemplateName where
    parseJSON = parseJSONText "PolicyTemplateName"
