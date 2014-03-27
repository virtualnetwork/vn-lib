
with Interfaces;

package VN.Communication.CAN is

   type CAN_Message_Prio is mod 2 ** 6;
   for CAN_Message_Prio'size use 6;

   type CAN_Message_Type is mod 2 ** 7;
   for CAN_Message_Type'size use 7;

   type CAN_Address_Sender is mod 2 ** 7;
   for CAN_Address_Sender'size use 7;

   type CAN_Address_Receiver is mod 2 ** 8;
   for CAN_Address_Receiver'size use 8;

   function "=" (Left : CAN_Address_Sender; Right : CAN_Address_Receiver) return Boolean;

   function "=" (Left : CAN_Address_Receiver; Right : CAN_Address_Sender) return Boolean;


end VN.Communication.CAN;
