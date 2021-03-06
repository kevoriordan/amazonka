{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.DeleteMarkerReplication where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplicationStatus

-- | Specifies whether Amazon S3 replicates the delete markers. If you specify a @Filter@ , you must specify this element. However, in the latest version of replication configuration (when @Filter@ is specified), Amazon S3 doesn't replicate delete markers. Therefore, the @DeleteMarkerReplication@ element can contain only <Status>Disabled</Status>. For an example configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-config-min-rule-config Basic Rule Configuration> . 
--
--
--
-- /See:/ 'deleteMarkerReplication' smart constructor.
newtype DeleteMarkerReplication = DeleteMarkerReplication'{_dmrStatus
                                                           ::
                                                           Maybe
                                                             DeleteMarkerReplicationStatus}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteMarkerReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrStatus' - Indicates whether to replicate delete markers.
deleteMarkerReplication
    :: DeleteMarkerReplication
deleteMarkerReplication
  = DeleteMarkerReplication'{_dmrStatus = Nothing}

-- | Indicates whether to replicate delete markers.
dmrStatus :: Lens' DeleteMarkerReplication (Maybe DeleteMarkerReplicationStatus)
dmrStatus = lens _dmrStatus (\ s a -> s{_dmrStatus = a})

instance FromXML DeleteMarkerReplication where
        parseXML x
          = DeleteMarkerReplication' <$> (x .@? "Status")

instance Hashable DeleteMarkerReplication where

instance NFData DeleteMarkerReplication where

instance ToXML DeleteMarkerReplication where
        toXML DeleteMarkerReplication'{..}
          = mconcat ["Status" @= _dmrStatus]
