-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell and Christoffer Holmstedt
-- Date: 2014-XX-XX
-- Summary:
-- This package implements the VN protocol.

with Interfaces;
with GNAT.IO;

package VN is

   package Text_IO renames GNAT.IO;

   type VN_CUUID is Array(1..16) of Interfaces.Unsigned_8;
   for VN_CUUID'Alignment use 4;

   type VN_Logical_Address is mod 2 ** 32;
   for VN_Logical_Address'Size use 32;
   LOGICAL_ADDRES_UNKNOWN : VN_Logical_Address := 2;

   type Send_Status is (OK,
                        ERROR_UNKNOWN,
                        ERROR_BUFFER_FULL,
                        ERROR_NO_ADDRESS_RECEIVED);

   type Receive_Status is (NO_MSG_RECEIVED,
                           MSG_RECEIVED_NO_MORE_AVAILABLE,
                           MSG_RECEIVED_MORE_AVAILABLE,
                           ERROR_UNKNOWN);
end VN;
