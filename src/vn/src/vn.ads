with Interfaces;

--  with Ada.Text_IO;
--with System.Text_IO;

with GNAT.IO;

package VN is

   --package Text_IO renames Ada.Text_IO;

 package Text_IO renames GNAT.IO;

   type VN_CUUID is Array(1..16) of Interfaces.Unsigned_8;

   subtype VN_Logical_Address is Interfaces.Unsigned_32;

   type Send_Status is (OK,
                        ERROR_UNKNOWN,
                        ERROR_BUFFER_FULL,
                        ERROR_NO_ADDRESS_RECEIVED);

   type Receive_Status is (NO_MSG_RECEIVED,
                           MSG_RECEIVED_NO_MORE_AVAILABLE,
                           MSG_RECEIVED_MORE_AVAILABLE,
                           ERROR_UNKNOWN);
end VN;
