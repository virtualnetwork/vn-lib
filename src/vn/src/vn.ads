with Interfaces;


package VN is

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
