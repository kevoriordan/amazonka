{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EBSInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EBSEncryptionSupport
import Network.AWS.EC2.Types.EBSOptimizedSupport
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon EBS features supported by the instance type.
--
--
--
-- /See:/ 'ebsInfo' smart constructor.
data EBSInfo = EBSInfo'{_eiEncryptionSupport ::
                        !(Maybe EBSEncryptionSupport),
                        _eiEBSOptimizedSupport ::
                        !(Maybe EBSOptimizedSupport)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiEncryptionSupport' - Indicates whether Amazon EBS encryption is supported.
--
-- * 'eiEBSOptimizedSupport' - Indicates that the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
ebsInfo
    :: EBSInfo
ebsInfo
  = EBSInfo'{_eiEncryptionSupport = Nothing,
             _eiEBSOptimizedSupport = Nothing}

-- | Indicates whether Amazon EBS encryption is supported.
eiEncryptionSupport :: Lens' EBSInfo (Maybe EBSEncryptionSupport)
eiEncryptionSupport = lens _eiEncryptionSupport (\ s a -> s{_eiEncryptionSupport = a})

-- | Indicates that the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
eiEBSOptimizedSupport :: Lens' EBSInfo (Maybe EBSOptimizedSupport)
eiEBSOptimizedSupport = lens _eiEBSOptimizedSupport (\ s a -> s{_eiEBSOptimizedSupport = a})

instance FromXML EBSInfo where
        parseXML x
          = EBSInfo' <$>
              (x .@? "encryptionSupport") <*>
                (x .@? "ebsOptimizedSupport")

instance Hashable EBSInfo where

instance NFData EBSInfo where
