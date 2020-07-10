{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricTransformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.MetricTransformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates how to transform ingested log events to metric data in a CloudWatch metric.
--
--
--
-- /See:/ 'metricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'{_mtDefaultValue
                                                  :: !(Maybe Double),
                                                  _mtMetricName :: !Text,
                                                  _mtMetricNamespace :: !Text,
                                                  _mtMetricValue :: !Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtDefaultValue' - (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
--
-- * 'mtMetricName' - The name of the CloudWatch metric.
--
-- * 'mtMetricNamespace' - The namespace of the CloudWatch metric.
--
-- * 'mtMetricValue' - The value to publish to the CloudWatch metric when a filter pattern matches a log event.
metricTransformation
    :: Text -- ^ 'mtMetricName'
    -> Text -- ^ 'mtMetricNamespace'
    -> Text -- ^ 'mtMetricValue'
    -> MetricTransformation
metricTransformation pMetricName_ pMetricNamespace_
  pMetricValue_
  = MetricTransformation'{_mtDefaultValue = Nothing,
                          _mtMetricName = pMetricName_,
                          _mtMetricNamespace = pMetricNamespace_,
                          _mtMetricValue = pMetricValue_}

-- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
mtDefaultValue :: Lens' MetricTransformation (Maybe Double)
mtDefaultValue = lens _mtDefaultValue (\ s a -> s{_mtDefaultValue = a})

-- | The name of the CloudWatch metric.
mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\ s a -> s{_mtMetricName = a})

-- | The namespace of the CloudWatch metric.
mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace = lens _mtMetricNamespace (\ s a -> s{_mtMetricNamespace = a})

-- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\ s a -> s{_mtMetricValue = a})

instance FromJSON MetricTransformation where
        parseJSON
          = withObject "MetricTransformation"
              (\ x ->
                 MetricTransformation' <$>
                   (x .:? "defaultValue") <*> (x .: "metricName") <*>
                     (x .: "metricNamespace")
                     <*> (x .: "metricValue"))

instance Hashable MetricTransformation where

instance NFData MetricTransformation where

instance ToJSON MetricTransformation where
        toJSON MetricTransformation'{..}
          = object
              (catMaybes
                 [("defaultValue" .=) <$> _mtDefaultValue,
                  Just ("metricName" .= _mtMetricName),
                  Just ("metricNamespace" .= _mtMetricNamespace),
                  Just ("metricValue" .= _mtMetricValue)])
