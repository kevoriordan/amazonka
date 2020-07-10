{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContainerMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ContainerMode (
  ContainerMode (
    ..
    , MultiModel
    , SingleModel
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContainerMode = ContainerMode' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MultiModel :: ContainerMode
pattern MultiModel = ContainerMode' "MultiModel"

pattern SingleModel :: ContainerMode
pattern SingleModel = ContainerMode' "SingleModel"

{-# COMPLETE
  MultiModel,
  SingleModel,
  ContainerMode' #-}

instance FromText ContainerMode where
    parser = (ContainerMode' . mk) <$> takeText

instance ToText ContainerMode where
    toText (ContainerMode' ci) = original ci

-- | Represents an enum of /known/ $ContainerMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerMode where
    toEnum i = case i of
        0 -> MultiModel
        1 -> SingleModel
        _ -> (error . showText) $ "Unknown index for ContainerMode: " <> toText i
    fromEnum x = case x of
        MultiModel -> 0
        SingleModel -> 1
        ContainerMode' name -> (error . showText) $ "Unknown ContainerMode: " <> original name

-- | Represents the bounds of /known/ $ContainerMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerMode where
    minBound = MultiModel
    maxBound = SingleModel

instance Hashable     ContainerMode
instance NFData       ContainerMode
instance ToByteString ContainerMode
instance ToQuery      ContainerMode
instance ToHeader     ContainerMode

instance ToJSON ContainerMode where
    toJSON = toJSONText

instance FromJSON ContainerMode where
    parseJSON = parseJSONText "ContainerMode"
