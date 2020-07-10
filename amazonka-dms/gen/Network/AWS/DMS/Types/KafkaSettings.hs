{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.KafkaSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes an Apache Kafka endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
--
--
-- /See:/ 'kafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'{_ksTopic ::
                                    !(Maybe Text),
                                    _ksBroker :: !(Maybe Text)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KafkaSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksTopic' - The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
--
-- * 'ksBroker' - The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
kafkaSettings
    :: KafkaSettings
kafkaSettings
  = KafkaSettings'{_ksTopic = Nothing,
                   _ksBroker = Nothing}

-- | The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
ksTopic :: Lens' KafkaSettings (Maybe Text)
ksTopic = lens _ksTopic (\ s a -> s{_ksTopic = a})

-- | The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
ksBroker :: Lens' KafkaSettings (Maybe Text)
ksBroker = lens _ksBroker (\ s a -> s{_ksBroker = a})

instance FromJSON KafkaSettings where
        parseJSON
          = withObject "KafkaSettings"
              (\ x ->
                 KafkaSettings' <$>
                   (x .:? "Topic") <*> (x .:? "Broker"))

instance Hashable KafkaSettings where

instance NFData KafkaSettings where

instance ToJSON KafkaSettings where
        toJSON KafkaSettings'{..}
          = object
              (catMaybes
                 [("Topic" .=) <$> _ksTopic,
                  ("Broker" .=) <$> _ksBroker])
