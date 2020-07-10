{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.RepositoryCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.RepositoryCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The repository credentials for private registry authentication.
--
--
--
-- /See:/ 'repositoryCredentials' smart constructor.
newtype RepositoryCredentials = RepositoryCredentials'{_rcCredentialsParameter
                                                       :: Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'RepositoryCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCredentialsParameter' - The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
repositoryCredentials
    :: Text -- ^ 'rcCredentialsParameter'
    -> RepositoryCredentials
repositoryCredentials pCredentialsParameter_
  = RepositoryCredentials'{_rcCredentialsParameter =
                             pCredentialsParameter_}

-- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
rcCredentialsParameter :: Lens' RepositoryCredentials Text
rcCredentialsParameter = lens _rcCredentialsParameter (\ s a -> s{_rcCredentialsParameter = a})

instance FromJSON RepositoryCredentials where
        parseJSON
          = withObject "RepositoryCredentials"
              (\ x ->
                 RepositoryCredentials' <$>
                   (x .: "credentialsParameter"))

instance Hashable RepositoryCredentials where

instance NFData RepositoryCredentials where

instance ToJSON RepositoryCredentials where
        toJSON RepositoryCredentials'{..}
          = object
              (catMaybes
                 [Just
                    ("credentialsParameter" .= _rcCredentialsParameter)])
