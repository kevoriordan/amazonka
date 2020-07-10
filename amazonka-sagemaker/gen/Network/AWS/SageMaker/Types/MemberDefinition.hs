{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MemberDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MemberDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CognitoMemberDefinition

-- | Defines the Amazon Cognito user group that is part of a work team.
--
--
--
-- /See:/ 'memberDefinition' smart constructor.
newtype MemberDefinition = MemberDefinition'{_mdCognitoMemberDefinition
                                             :: Maybe CognitoMemberDefinition}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdCognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
memberDefinition
    :: MemberDefinition
memberDefinition
  = MemberDefinition'{_mdCognitoMemberDefinition =
                        Nothing}

-- | The Amazon Cognito user group that is part of the work team.
mdCognitoMemberDefinition :: Lens' MemberDefinition (Maybe CognitoMemberDefinition)
mdCognitoMemberDefinition = lens _mdCognitoMemberDefinition (\ s a -> s{_mdCognitoMemberDefinition = a})

instance FromJSON MemberDefinition where
        parseJSON
          = withObject "MemberDefinition"
              (\ x ->
                 MemberDefinition' <$>
                   (x .:? "CognitoMemberDefinition"))

instance Hashable MemberDefinition where

instance NFData MemberDefinition where

instance ToJSON MemberDefinition where
        toJSON MemberDefinition'{..}
          = object
              (catMaybes
                 [("CognitoMemberDefinition" .=) <$>
                    _mdCognitoMemberDefinition])
