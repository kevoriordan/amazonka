{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.GroupSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.GroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for a group without metadata.
--
--
--
-- /See:/ 'groupSummary' smart constructor.
data GroupSummary = GroupSummary'{_gsFilterExpression
                                  :: !(Maybe Text),
                                  _gsGroupARN :: !(Maybe Text),
                                  _gsGroupName :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsFilterExpression' - The filter expression defining the parameters to include traces.
--
-- * 'gsGroupARN' - The ARN of the group generated based on the GroupName.
--
-- * 'gsGroupName' - The unique case-sensitive name of the group.
groupSummary
    :: GroupSummary
groupSummary
  = GroupSummary'{_gsFilterExpression = Nothing,
                  _gsGroupARN = Nothing, _gsGroupName = Nothing}

-- | The filter expression defining the parameters to include traces.
gsFilterExpression :: Lens' GroupSummary (Maybe Text)
gsFilterExpression = lens _gsFilterExpression (\ s a -> s{_gsFilterExpression = a})

-- | The ARN of the group generated based on the GroupName.
gsGroupARN :: Lens' GroupSummary (Maybe Text)
gsGroupARN = lens _gsGroupARN (\ s a -> s{_gsGroupARN = a})

-- | The unique case-sensitive name of the group.
gsGroupName :: Lens' GroupSummary (Maybe Text)
gsGroupName = lens _gsGroupName (\ s a -> s{_gsGroupName = a})

instance FromJSON GroupSummary where
        parseJSON
          = withObject "GroupSummary"
              (\ x ->
                 GroupSummary' <$>
                   (x .:? "FilterExpression") <*> (x .:? "GroupARN") <*>
                     (x .:? "GroupName"))

instance Hashable GroupSummary where

instance NFData GroupSummary where
