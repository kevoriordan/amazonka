{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.EvaluationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.EvaluationType (
  EvaluationType (
    ..
    , Dynamic
    , Static
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EvaluationType = EvaluationType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Dynamic :: EvaluationType
pattern Dynamic = EvaluationType' "DYNAMIC"

pattern Static :: EvaluationType
pattern Static = EvaluationType' "STATIC"

{-# COMPLETE
  Dynamic,
  Static,
  EvaluationType' #-}

instance FromText EvaluationType where
    parser = (EvaluationType' . mk) <$> takeText

instance ToText EvaluationType where
    toText (EvaluationType' ci) = original ci

-- | Represents an enum of /known/ $EvaluationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EvaluationType where
    toEnum i = case i of
        0 -> Dynamic
        1 -> Static
        _ -> (error . showText) $ "Unknown index for EvaluationType: " <> toText i
    fromEnum x = case x of
        Dynamic -> 0
        Static -> 1
        EvaluationType' name -> (error . showText) $ "Unknown EvaluationType: " <> original name

-- | Represents the bounds of /known/ $EvaluationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EvaluationType where
    minBound = Dynamic
    maxBound = Static

instance Hashable     EvaluationType
instance NFData       EvaluationType
instance ToByteString EvaluationType
instance ToQuery      EvaluationType
instance ToHeader     EvaluationType

instance FromJSON EvaluationType where
    parseJSON = parseJSONText "EvaluationType"
