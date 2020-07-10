{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationId
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.ConfigurationId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of information about the configuration.
--
-- /See:/ 'configurationId' smart constructor.
data ConfigurationId = ConfigurationId'{_ciId ::
                                        !(Maybe Text),
                                        _ciRevision :: !(Maybe Int)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'ciRevision' - The revision number of the configuration.
configurationId
    :: ConfigurationId
configurationId
  = ConfigurationId'{_ciId = Nothing,
                     _ciRevision = Nothing}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
ciId :: Lens' ConfigurationId (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a})

-- | The revision number of the configuration.
ciRevision :: Lens' ConfigurationId (Maybe Int)
ciRevision = lens _ciRevision (\ s a -> s{_ciRevision = a})

instance FromJSON ConfigurationId where
        parseJSON
          = withObject "ConfigurationId"
              (\ x ->
                 ConfigurationId' <$>
                   (x .:? "id") <*> (x .:? "revision"))

instance Hashable ConfigurationId where

instance NFData ConfigurationId where

instance ToJSON ConfigurationId where
        toJSON ConfigurationId'{..}
          = object
              (catMaybes
                 [("id" .=) <$> _ciId,
                  ("revision" .=) <$> _ciRevision])
