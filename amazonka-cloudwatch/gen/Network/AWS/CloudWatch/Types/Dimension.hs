{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Dimension
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Dimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Expands the identity of a metric.
--
--
--
-- /See:/ 'dimension' smart constructor.
data Dimension = Dimension'{_dName :: !Text,
                            _dValue :: !Text}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the dimension.
--
-- * 'dValue' - The value representing the dimension measurement.
dimension
    :: Text -- ^ 'dName'
    -> Text -- ^ 'dValue'
    -> Dimension
dimension pName_ pValue_
  = Dimension'{_dName = pName_, _dValue = pValue_}

-- | The name of the dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\ s a -> s{_dName = a})

-- | The value representing the dimension measurement.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\ s a -> s{_dValue = a})

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable Dimension where

instance NFData Dimension where

instance ToQuery Dimension where
        toQuery Dimension'{..}
          = mconcat ["Name" =: _dName, "Value" =: _dValue]
