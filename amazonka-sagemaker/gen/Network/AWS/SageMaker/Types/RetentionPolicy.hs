{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.RetentionPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.RetentionType

-- | The retention policy.
--
--
--
-- /See:/ 'retentionPolicy' smart constructor.
newtype RetentionPolicy = RetentionPolicy'{_rpHomeEfsFileSystem
                                           :: Maybe RetentionType}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpHomeEfsFileSystem' - The home Amazon Elastic File System (EFS).
retentionPolicy
    :: RetentionPolicy
retentionPolicy
  = RetentionPolicy'{_rpHomeEfsFileSystem = Nothing}

-- | The home Amazon Elastic File System (EFS).
rpHomeEfsFileSystem :: Lens' RetentionPolicy (Maybe RetentionType)
rpHomeEfsFileSystem = lens _rpHomeEfsFileSystem (\ s a -> s{_rpHomeEfsFileSystem = a})

instance Hashable RetentionPolicy where

instance NFData RetentionPolicy where

instance ToJSON RetentionPolicy where
        toJSON RetentionPolicy'{..}
          = object
              (catMaybes
                 [("HomeEfsFileSystem" .=) <$> _rpHomeEfsFileSystem])
