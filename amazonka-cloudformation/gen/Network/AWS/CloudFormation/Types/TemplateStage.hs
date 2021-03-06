{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TemplateStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.TemplateStage (
  TemplateStage (
    ..
    , Original
    , Processed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TemplateStage = TemplateStage' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Original :: TemplateStage
pattern Original = TemplateStage' "Original"

pattern Processed :: TemplateStage
pattern Processed = TemplateStage' "Processed"

{-# COMPLETE
  Original,
  Processed,
  TemplateStage' #-}

instance FromText TemplateStage where
    parser = (TemplateStage' . mk) <$> takeText

instance ToText TemplateStage where
    toText (TemplateStage' ci) = original ci

-- | Represents an enum of /known/ $TemplateStage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TemplateStage where
    toEnum i = case i of
        0 -> Original
        1 -> Processed
        _ -> (error . showText) $ "Unknown index for TemplateStage: " <> toText i
    fromEnum x = case x of
        Original -> 0
        Processed -> 1
        TemplateStage' name -> (error . showText) $ "Unknown TemplateStage: " <> original name

-- | Represents the bounds of /known/ $TemplateStage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TemplateStage where
    minBound = Original
    maxBound = Processed

instance Hashable     TemplateStage
instance NFData       TemplateStage
instance ToByteString TemplateStage
instance ToQuery      TemplateStage
instance ToHeader     TemplateStage

instance FromXML TemplateStage where
    parseXML = parseXMLText "TemplateStage"
