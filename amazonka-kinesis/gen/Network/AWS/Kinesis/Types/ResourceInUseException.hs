{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ResourceInUseException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ResourceInUseException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The resource is not available for this operation. For successful operation, the resource must be in the @ACTIVE@ state.
--
--
--
-- /See:/ 'resourceInUseException' smart constructor.
newtype ResourceInUseException = ResourceInUseException'{_riueMessage
                                                         :: Maybe Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'ResourceInUseException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riueMessage' - A message that provides information about the error.
resourceInUseException
    :: ResourceInUseException
resourceInUseException
  = ResourceInUseException'{_riueMessage = Nothing}

-- | A message that provides information about the error.
riueMessage :: Lens' ResourceInUseException (Maybe Text)
riueMessage = lens _riueMessage (\ s a -> s{_riueMessage = a})

instance FromJSON ResourceInUseException where
        parseJSON
          = withObject "ResourceInUseException"
              (\ x ->
                 ResourceInUseException' <$> (x .:? "message"))

instance Hashable ResourceInUseException where

instance NFData ResourceInUseException where
