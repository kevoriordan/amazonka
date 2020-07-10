{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.AdditionalArtifact (
  AdditionalArtifact (
    ..
    , Athena
    , Quicksight
    , Redshift
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The types of manifest that you want AWS to create for this report.
--
--
data AdditionalArtifact = AdditionalArtifact' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Athena :: AdditionalArtifact
pattern Athena = AdditionalArtifact' "ATHENA"

pattern Quicksight :: AdditionalArtifact
pattern Quicksight = AdditionalArtifact' "QUICKSIGHT"

pattern Redshift :: AdditionalArtifact
pattern Redshift = AdditionalArtifact' "REDSHIFT"

{-# COMPLETE
  Athena,
  Quicksight,
  Redshift,
  AdditionalArtifact' #-}

instance FromText AdditionalArtifact where
    parser = (AdditionalArtifact' . mk) <$> takeText

instance ToText AdditionalArtifact where
    toText (AdditionalArtifact' ci) = original ci

-- | Represents an enum of /known/ $AdditionalArtifact.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdditionalArtifact where
    toEnum i = case i of
        0 -> Athena
        1 -> Quicksight
        2 -> Redshift
        _ -> (error . showText) $ "Unknown index for AdditionalArtifact: " <> toText i
    fromEnum x = case x of
        Athena -> 0
        Quicksight -> 1
        Redshift -> 2
        AdditionalArtifact' name -> (error . showText) $ "Unknown AdditionalArtifact: " <> original name

-- | Represents the bounds of /known/ $AdditionalArtifact.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdditionalArtifact where
    minBound = Athena
    maxBound = Redshift

instance Hashable     AdditionalArtifact
instance NFData       AdditionalArtifact
instance ToByteString AdditionalArtifact
instance ToQuery      AdditionalArtifact
instance ToHeader     AdditionalArtifact

instance ToJSON AdditionalArtifact where
    toJSON = toJSONText

instance FromJSON AdditionalArtifact where
    parseJSON = parseJSONText "AdditionalArtifact"
