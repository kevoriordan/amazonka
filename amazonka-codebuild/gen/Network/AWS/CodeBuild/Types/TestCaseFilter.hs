{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.TestCaseFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter used to return specific types of test cases. 
--
--
--
-- /See:/ 'testCaseFilter' smart constructor.
newtype TestCaseFilter = TestCaseFilter'{_tcfStatus
                                         :: Maybe Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestCaseFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcfStatus' - The status used to filter test cases. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ . A @TestCaseFilter@ can have one status. 
testCaseFilter
    :: TestCaseFilter
testCaseFilter
  = TestCaseFilter'{_tcfStatus = Nothing}

-- | The status used to filter test cases. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ . A @TestCaseFilter@ can have one status. 
tcfStatus :: Lens' TestCaseFilter (Maybe Text)
tcfStatus = lens _tcfStatus (\ s a -> s{_tcfStatus = a})

instance Hashable TestCaseFilter where

instance NFData TestCaseFilter where

instance ToJSON TestCaseFilter where
        toJSON TestCaseFilter'{..}
          = object (catMaybes [("status" .=) <$> _tcfStatus])
