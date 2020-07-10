{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data set whose latest contents are used as input to the notebook or application.
--
--
--
-- /See:/ 'datasetContentVersionValue' smart constructor.
newtype DatasetContentVersionValue = DatasetContentVersionValue'{_dcvvDatasetName
                                                                 :: Text}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'DatasetContentVersionValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvvDatasetName' - The name of the data set whose latest contents are used as input to the notebook or application.
datasetContentVersionValue
    :: Text -- ^ 'dcvvDatasetName'
    -> DatasetContentVersionValue
datasetContentVersionValue pDatasetName_
  = DatasetContentVersionValue'{_dcvvDatasetName =
                                  pDatasetName_}

-- | The name of the data set whose latest contents are used as input to the notebook or application.
dcvvDatasetName :: Lens' DatasetContentVersionValue Text
dcvvDatasetName = lens _dcvvDatasetName (\ s a -> s{_dcvvDatasetName = a})

instance FromJSON DatasetContentVersionValue where
        parseJSON
          = withObject "DatasetContentVersionValue"
              (\ x ->
                 DatasetContentVersionValue' <$> (x .: "datasetName"))

instance Hashable DatasetContentVersionValue where

instance NFData DatasetContentVersionValue where

instance ToJSON DatasetContentVersionValue where
        toJSON DatasetContentVersionValue'{..}
          = object
              (catMaybes
                 [Just ("datasetName" .= _dcvvDatasetName)])
