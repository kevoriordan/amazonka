{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterSummary where

import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary description of the cluster.
--
--
--
-- /See:/ 'clusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'{_csStatus ::
                                      !(Maybe ClusterStatus),
                                      _csClusterARN :: !(Maybe Text),
                                      _csOutpostARN :: !(Maybe Text),
                                      _csNormalizedInstanceHours ::
                                      !(Maybe Int),
                                      _csName :: !(Maybe Text),
                                      _csId :: !(Maybe Text)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStatus' - The details about the current status of the cluster.
--
-- * 'csClusterARN' - The Amazon Resource Name of the cluster.
--
-- * 'csOutpostARN' - The Amazon Resource Name (ARN) of the Outpost where the cluster is launched. 
--
-- * 'csNormalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'csName' - The name of the cluster.
--
-- * 'csId' - The unique identifier for the cluster.
clusterSummary
    :: ClusterSummary
clusterSummary
  = ClusterSummary'{_csStatus = Nothing,
                    _csClusterARN = Nothing, _csOutpostARN = Nothing,
                    _csNormalizedInstanceHours = Nothing,
                    _csName = Nothing, _csId = Nothing}

-- | The details about the current status of the cluster.
csStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | The Amazon Resource Name of the cluster.
csClusterARN :: Lens' ClusterSummary (Maybe Text)
csClusterARN = lens _csClusterARN (\ s a -> s{_csClusterARN = a})

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched. 
csOutpostARN :: Lens' ClusterSummary (Maybe Text)
csOutpostARN = lens _csOutpostARN (\ s a -> s{_csOutpostARN = a})

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
csNormalizedInstanceHours :: Lens' ClusterSummary (Maybe Int)
csNormalizedInstanceHours = lens _csNormalizedInstanceHours (\ s a -> s{_csNormalizedInstanceHours = a})

-- | The name of the cluster.
csName :: Lens' ClusterSummary (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a})

-- | The unique identifier for the cluster.
csId :: Lens' ClusterSummary (Maybe Text)
csId = lens _csId (\ s a -> s{_csId = a})

instance FromJSON ClusterSummary where
        parseJSON
          = withObject "ClusterSummary"
              (\ x ->
                 ClusterSummary' <$>
                   (x .:? "Status") <*> (x .:? "ClusterArn") <*>
                     (x .:? "OutpostArn")
                     <*> (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable ClusterSummary where

instance NFData ClusterSummary where
