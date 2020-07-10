{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.PauseClusterMessage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.PauseClusterMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | /See:/ 'pauseClusterMessage' smart constructor.
newtype PauseClusterMessage = PauseClusterMessage'{_pcmClusterIdentifier
                                                   :: Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'PauseClusterMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcmClusterIdentifier' - The identifier of the cluster to be paused.
pauseClusterMessage
    :: Text -- ^ 'pcmClusterIdentifier'
    -> PauseClusterMessage
pauseClusterMessage pClusterIdentifier_
  = PauseClusterMessage'{_pcmClusterIdentifier =
                           pClusterIdentifier_}

-- | The identifier of the cluster to be paused.
pcmClusterIdentifier :: Lens' PauseClusterMessage Text
pcmClusterIdentifier = lens _pcmClusterIdentifier (\ s a -> s{_pcmClusterIdentifier = a})

instance FromXML PauseClusterMessage where
        parseXML x
          = PauseClusterMessage' <$> (x .@ "ClusterIdentifier")

instance Hashable PauseClusterMessage where

instance NFData PauseClusterMessage where

instance ToQuery PauseClusterMessage where
        toQuery PauseClusterMessage'{..}
          = mconcat
              ["ClusterIdentifier" =: _pcmClusterIdentifier]
