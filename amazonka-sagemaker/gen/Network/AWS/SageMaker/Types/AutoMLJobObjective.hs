{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobObjective
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobObjective where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLMetricEnum

-- | Applies a metric to minimize or maximize for the job's objective.
--
--
--
-- /See:/ 'autoMLJobObjective' smart constructor.
newtype AutoMLJobObjective = AutoMLJobObjective'{_amljoMetricName
                                                 :: AutoMLMetricEnum}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'AutoMLJobObjective' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amljoMetricName' - The name of the metric.
autoMLJobObjective
    :: AutoMLMetricEnum -- ^ 'amljoMetricName'
    -> AutoMLJobObjective
autoMLJobObjective pMetricName_
  = AutoMLJobObjective'{_amljoMetricName =
                          pMetricName_}

-- | The name of the metric.
amljoMetricName :: Lens' AutoMLJobObjective AutoMLMetricEnum
amljoMetricName = lens _amljoMetricName (\ s a -> s{_amljoMetricName = a})

instance FromJSON AutoMLJobObjective where
        parseJSON
          = withObject "AutoMLJobObjective"
              (\ x -> AutoMLJobObjective' <$> (x .: "MetricName"))

instance Hashable AutoMLJobObjective where

instance NFData AutoMLJobObjective where

instance ToJSON AutoMLJobObjective where
        toJSON AutoMLJobObjective'{..}
          = object
              (catMaybes [Just ("MetricName" .= _amljoMetricName)])
