{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for the rollout of OTA updates.
--
--
--
-- /See:/ 'awsJobExecutionsRolloutConfig' smart constructor.
newtype AWSJobExecutionsRolloutConfig = AWSJobExecutionsRolloutConfig'{_ajercMaximumPerMinute
                                                                       ::
                                                                       Maybe
                                                                         Nat}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'AWSJobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajercMaximumPerMinute' - The maximum number of OTA update job executions started per minute.
awsJobExecutionsRolloutConfig
    :: AWSJobExecutionsRolloutConfig
awsJobExecutionsRolloutConfig
  = AWSJobExecutionsRolloutConfig'{_ajercMaximumPerMinute
                                     = Nothing}

-- | The maximum number of OTA update job executions started per minute.
ajercMaximumPerMinute :: Lens' AWSJobExecutionsRolloutConfig (Maybe Natural)
ajercMaximumPerMinute = lens _ajercMaximumPerMinute (\ s a -> s{_ajercMaximumPerMinute = a}) . mapping _Nat

instance FromJSON AWSJobExecutionsRolloutConfig where
        parseJSON
          = withObject "AWSJobExecutionsRolloutConfig"
              (\ x ->
                 AWSJobExecutionsRolloutConfig' <$>
                   (x .:? "maximumPerMinute"))

instance Hashable AWSJobExecutionsRolloutConfig where

instance NFData AWSJobExecutionsRolloutConfig where

instance ToJSON AWSJobExecutionsRolloutConfig where
        toJSON AWSJobExecutionsRolloutConfig'{..}
          = object
              (catMaybes
                 [("maximumPerMinute" .=) <$> _ajercMaximumPerMinute])
