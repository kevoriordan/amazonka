{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveRoleFromDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from an Amazon Aurora DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Integrating.Authorizing.html Authorizing Amazon Aurora MySQL to Access Other AWS Services on Your Behalf > in the /Amazon Aurora User Guide/ .
--
--
module Network.AWS.RDS.RemoveRoleFromDBCluster
    (
    -- * Creating a Request
      removeRoleFromDBCluster
    , RemoveRoleFromDBCluster
    -- * Request Lenses
    , rrfdcFeatureName
    , rrfdcDBClusterIdentifier
    , rrfdcRoleARN

    -- * Destructuring the Response
    , removeRoleFromDBClusterResponse
    , RemoveRoleFromDBClusterResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeRoleFromDBCluster' smart constructor.
data RemoveRoleFromDBCluster = RemoveRoleFromDBCluster'{_rrfdcFeatureName
                                                        :: !(Maybe Text),
                                                        _rrfdcDBClusterIdentifier
                                                        :: !Text,
                                                        _rrfdcRoleARN :: !Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'RemoveRoleFromDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrfdcFeatureName' - The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
--
-- * 'rrfdcDBClusterIdentifier' - The name of the DB cluster to disassociate the IAM role from.
--
-- * 'rrfdcRoleARN' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
removeRoleFromDBCluster
    :: Text -- ^ 'rrfdcDBClusterIdentifier'
    -> Text -- ^ 'rrfdcRoleARN'
    -> RemoveRoleFromDBCluster
removeRoleFromDBCluster pDBClusterIdentifier_
  pRoleARN_
  = RemoveRoleFromDBCluster'{_rrfdcFeatureName =
                               Nothing,
                             _rrfdcDBClusterIdentifier = pDBClusterIdentifier_,
                             _rrfdcRoleARN = pRoleARN_}

-- | The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
rrfdcFeatureName :: Lens' RemoveRoleFromDBCluster (Maybe Text)
rrfdcFeatureName = lens _rrfdcFeatureName (\ s a -> s{_rrfdcFeatureName = a})

-- | The name of the DB cluster to disassociate the IAM role from.
rrfdcDBClusterIdentifier :: Lens' RemoveRoleFromDBCluster Text
rrfdcDBClusterIdentifier = lens _rrfdcDBClusterIdentifier (\ s a -> s{_rrfdcDBClusterIdentifier = a})

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
rrfdcRoleARN :: Lens' RemoveRoleFromDBCluster Text
rrfdcRoleARN = lens _rrfdcRoleARN (\ s a -> s{_rrfdcRoleARN = a})

instance AWSRequest RemoveRoleFromDBCluster where
        type Rs RemoveRoleFromDBCluster =
             RemoveRoleFromDBClusterResponse
        request = postQuery rds
        response
          = receiveNull RemoveRoleFromDBClusterResponse'

instance Hashable RemoveRoleFromDBCluster where

instance NFData RemoveRoleFromDBCluster where

instance ToHeaders RemoveRoleFromDBCluster where
        toHeaders = const mempty

instance ToPath RemoveRoleFromDBCluster where
        toPath = const "/"

instance ToQuery RemoveRoleFromDBCluster where
        toQuery RemoveRoleFromDBCluster'{..}
          = mconcat
              ["Action" =:
                 ("RemoveRoleFromDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "FeatureName" =: _rrfdcFeatureName,
               "DBClusterIdentifier" =: _rrfdcDBClusterIdentifier,
               "RoleArn" =: _rrfdcRoleARN]

-- | /See:/ 'removeRoleFromDBClusterResponse' smart constructor.
data RemoveRoleFromDBClusterResponse = RemoveRoleFromDBClusterResponse'
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'RemoveRoleFromDBClusterResponse' with the minimum fields required to make a request.
--
removeRoleFromDBClusterResponse
    :: RemoveRoleFromDBClusterResponse
removeRoleFromDBClusterResponse
  = RemoveRoleFromDBClusterResponse'

instance NFData RemoveRoleFromDBClusterResponse where
