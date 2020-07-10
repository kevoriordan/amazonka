{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RequiresRecreation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.RequiresRecreation (
  RequiresRecreation (
    ..
    , Always
    , Conditionally
    , Never
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RequiresRecreation = RequiresRecreation' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Always :: RequiresRecreation
pattern Always = RequiresRecreation' "ALWAYS"

pattern Conditionally :: RequiresRecreation
pattern Conditionally = RequiresRecreation' "CONDITIONALLY"

pattern Never :: RequiresRecreation
pattern Never = RequiresRecreation' "NEVER"

{-# COMPLETE
  Always,
  Conditionally,
  Never,
  RequiresRecreation' #-}

instance FromText RequiresRecreation where
    parser = (RequiresRecreation' . mk) <$> takeText

instance ToText RequiresRecreation where
    toText (RequiresRecreation' ci) = original ci

-- | Represents an enum of /known/ $RequiresRecreation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RequiresRecreation where
    toEnum i = case i of
        0 -> Always
        1 -> Conditionally
        2 -> Never
        _ -> (error . showText) $ "Unknown index for RequiresRecreation: " <> toText i
    fromEnum x = case x of
        Always -> 0
        Conditionally -> 1
        Never -> 2
        RequiresRecreation' name -> (error . showText) $ "Unknown RequiresRecreation: " <> original name

-- | Represents the bounds of /known/ $RequiresRecreation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RequiresRecreation where
    minBound = Always
    maxBound = Never

instance Hashable     RequiresRecreation
instance NFData       RequiresRecreation
instance ToByteString RequiresRecreation
instance ToQuery      RequiresRecreation
instance ToHeader     RequiresRecreation

instance FromJSON RequiresRecreation where
    parseJSON = parseJSONText "RequiresRecreation"
