{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled.
--
--
--
-- /See:/ 'advancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'{_asoEnabled
                                                        :: !(Maybe Bool),
                                                        _asoInternalUserDatabaseEnabled
                                                        :: !(Maybe Bool)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'AdvancedSecurityOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asoEnabled' - True if advanced security is enabled.
--
-- * 'asoInternalUserDatabaseEnabled' - True if the internal user database is enabled.
advancedSecurityOptions
    :: AdvancedSecurityOptions
advancedSecurityOptions
  = AdvancedSecurityOptions'{_asoEnabled = Nothing,
                             _asoInternalUserDatabaseEnabled = Nothing}

-- | True if advanced security is enabled.
asoEnabled :: Lens' AdvancedSecurityOptions (Maybe Bool)
asoEnabled = lens _asoEnabled (\ s a -> s{_asoEnabled = a})

-- | True if the internal user database is enabled.
asoInternalUserDatabaseEnabled :: Lens' AdvancedSecurityOptions (Maybe Bool)
asoInternalUserDatabaseEnabled = lens _asoInternalUserDatabaseEnabled (\ s a -> s{_asoInternalUserDatabaseEnabled = a})

instance FromJSON AdvancedSecurityOptions where
        parseJSON
          = withObject "AdvancedSecurityOptions"
              (\ x ->
                 AdvancedSecurityOptions' <$>
                   (x .:? "Enabled") <*>
                     (x .:? "InternalUserDatabaseEnabled"))

instance Hashable AdvancedSecurityOptions where

instance NFData AdvancedSecurityOptions where
