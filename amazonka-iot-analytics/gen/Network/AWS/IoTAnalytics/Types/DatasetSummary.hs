{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetSummary where

import Network.AWS.IoTAnalytics.Types.DatasetActionSummary
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a data set.
--
--
--
-- /See:/ 'datasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'{_dtstsmmryCreationTime
                                      :: !(Maybe POSIX),
                                      _dtstsmmryStatus ::
                                      !(Maybe DatasetStatus),
                                      _dtstsmmryActions ::
                                      !(Maybe (List1 DatasetActionSummary)),
                                      _dtstsmmryTriggers ::
                                      !(Maybe [DatasetTrigger]),
                                      _dtstsmmryDatasetName :: !(Maybe Text),
                                      _dtstsmmryLastUpdateTime ::
                                      !(Maybe POSIX)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtstsmmryCreationTime' - The time the data set was created.
--
-- * 'dtstsmmryStatus' - The status of the data set.
--
-- * 'dtstsmmryActions' - A list of "DataActionSummary" objects.
--
-- * 'dtstsmmryTriggers' - A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five DataSetTrigger objects
--
-- * 'dtstsmmryDatasetName' - The name of the data set.
--
-- * 'dtstsmmryLastUpdateTime' - The last time the data set was updated.
datasetSummary
    :: DatasetSummary
datasetSummary
  = DatasetSummary'{_dtstsmmryCreationTime = Nothing,
                    _dtstsmmryStatus = Nothing,
                    _dtstsmmryActions = Nothing,
                    _dtstsmmryTriggers = Nothing,
                    _dtstsmmryDatasetName = Nothing,
                    _dtstsmmryLastUpdateTime = Nothing}

-- | The time the data set was created.
dtstsmmryCreationTime :: Lens' DatasetSummary (Maybe UTCTime)
dtstsmmryCreationTime = lens _dtstsmmryCreationTime (\ s a -> s{_dtstsmmryCreationTime = a}) . mapping _Time

-- | The status of the data set.
dtstsmmryStatus :: Lens' DatasetSummary (Maybe DatasetStatus)
dtstsmmryStatus = lens _dtstsmmryStatus (\ s a -> s{_dtstsmmryStatus = a})

-- | A list of "DataActionSummary" objects.
dtstsmmryActions :: Lens' DatasetSummary (Maybe (NonEmpty DatasetActionSummary))
dtstsmmryActions = lens _dtstsmmryActions (\ s a -> s{_dtstsmmryActions = a}) . mapping _List1

-- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five DataSetTrigger objects
dtstsmmryTriggers :: Lens' DatasetSummary [DatasetTrigger]
dtstsmmryTriggers = lens _dtstsmmryTriggers (\ s a -> s{_dtstsmmryTriggers = a}) . _Default . _Coerce

-- | The name of the data set.
dtstsmmryDatasetName :: Lens' DatasetSummary (Maybe Text)
dtstsmmryDatasetName = lens _dtstsmmryDatasetName (\ s a -> s{_dtstsmmryDatasetName = a})

-- | The last time the data set was updated.
dtstsmmryLastUpdateTime :: Lens' DatasetSummary (Maybe UTCTime)
dtstsmmryLastUpdateTime = lens _dtstsmmryLastUpdateTime (\ s a -> s{_dtstsmmryLastUpdateTime = a}) . mapping _Time

instance FromJSON DatasetSummary where
        parseJSON
          = withObject "DatasetSummary"
              (\ x ->
                 DatasetSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "actions")
                     <*> (x .:? "triggers" .!= mempty)
                     <*> (x .:? "datasetName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatasetSummary where

instance NFData DatasetSummary where
