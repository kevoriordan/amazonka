{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ValidationSeverity (
  ValidationSeverity (
    ..
    , Error'
    , Warning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ValidationSeverity = ValidationSeverity' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Error' :: ValidationSeverity
pattern Error' = ValidationSeverity' "error"

pattern Warning :: ValidationSeverity
pattern Warning = ValidationSeverity' "warning"

{-# COMPLETE
  Error',
  Warning,
  ValidationSeverity' #-}

instance FromText ValidationSeverity where
    parser = (ValidationSeverity' . mk) <$> takeText

instance ToText ValidationSeverity where
    toText (ValidationSeverity' ci) = original ci

-- | Represents an enum of /known/ $ValidationSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ValidationSeverity where
    toEnum i = case i of
        0 -> Error'
        1 -> Warning
        _ -> (error . showText) $ "Unknown index for ValidationSeverity: " <> toText i
    fromEnum x = case x of
        Error' -> 0
        Warning -> 1
        ValidationSeverity' name -> (error . showText) $ "Unknown ValidationSeverity: " <> original name

-- | Represents the bounds of /known/ $ValidationSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ValidationSeverity where
    minBound = Error'
    maxBound = Warning

instance Hashable     ValidationSeverity
instance NFData       ValidationSeverity
instance ToByteString ValidationSeverity
instance ToQuery      ValidationSeverity
instance ToHeader     ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
