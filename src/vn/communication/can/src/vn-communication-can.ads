
with Interfaces;
with Buffers;

package VN.Communication.CAN is


   type CAN_Message_Prio is mod 2 ** 6;
   for CAN_Message_Prio'size use 6;

   type CAN_Message_Type is mod 2 ** 7;
   for CAN_Message_Type'size use 7;

--     type CAN_Address_Sender is new VN.Message.CAN_Address_Sender;
--     type CAN_Address_Receiver is new VN.Message.CAN_Address_Receiver;

   type CAN_Address_Sender is mod 2 ** 7;
   for CAN_Address_Sender'size use 7;

   type CAN_Address_Receiver is mod 2 ** 8;
   for CAN_Address_Receiver'size use 8;

   function "=" (Left : CAN_Address_Sender; Right : CAN_Address_Receiver) return Boolean;

   function "=" (Left : CAN_Address_Receiver; Right : CAN_Address_Sender) return Boolean;

   function Convert (x : CAN_Address_Sender) return CAN_Address_Receiver;

   type DLC_Type is range 0..8;
   for DLC_Type'size use 4;

   type CAN_message_ID is mod 2 ** 29;
   for CAN_message_ID'size use 29;

   subtype UCID is Interfaces.Unsigned_32 range 0..268435455;
   type Byte8 is array (DLC_Type range 1..8) of Interfaces.Unsigned_8;

   type CAN_Message_Logical is
      record
         isNormal 	: boolean; --Normal or Request CAN address
         SenderUCID  	: UCID;    		--relevant only if isNormal=false
         msgPrio  	: CAN_Message_Prio;	--relevant only if isNormal=true
         msgType 	: CAN_Message_Type;	--relevant only if isNormal=true
         Receiver 	: CAN_Address_Receiver;	--relevant only if isNormal=true
         Sender   	: CAN_Address_Sender;	--relevant only if isNormal=true
         Length   	: DLC_Type;
         Data     	: Byte8;
      end record;


   package CAN_Message_Buffers is new Buffers(VN.Communication.CAN.CAN_Message_Logical);

end VN.Communication.CAN;
