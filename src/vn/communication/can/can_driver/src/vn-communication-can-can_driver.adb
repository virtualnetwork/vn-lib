-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains procedures for sending and receiving CAN messages
-- as well as for conversion between logical and physical representations
-- of CAN messages.
-- This code interfaces with driver code written in C.

-- Please note: This code will not compile unless it is compiled with the correct makefile,
-- the BAP repository needs to be included.


with Interfaces;
use Interfaces;

with GNAT.IO;
use GNAT.IO;

package body VN.Communication.CAN.CAN_Driver is

   procedure Send(message : VN.Communication.CAN.CAN_Message_Logical;
                  status : out VN.Send_Status) is

      physicalMessage : aliased Physical_Logical.CAN_Message_Physical;

      use Interfaces.C;
   begin



      -- ToDo: This is just for testing, right now the CAN drivers don't work so we'll have to pretend we sent the message without doing so:
--        status := VN.OK;
--        return;

      Physical_Logical.LogicalToPhysical(message, physicalMessage);

      if message.isNormal and message.msgType = VN.Communication.CAN.CAN_Message_Type(1) then
         GNAT.IO.Put_Line("Sent Assign_CAN_Address, address= " & physicalMessage.Data(4)'Img);
         GNAT.IO.Put_Line("ID= " & physicalMessage.ID'Img);
         GNAT.IO.Put_Line("Length= " & physicalMessage.Length'Img);

         GNAT.IO.Put_Line("Data= " & physicalMessage.Data(0)'Img & physicalMessage.Data(1)'Img & physicalMessage.Data(2)'Img
                          & physicalMessage.Data(3)'Img& physicalMessage.Data(4)'Img & physicalMessage.Data(5)'Img
                          & physicalMessage.Data(6)'Img & physicalMessage.Data(7)'Img);

      end if;


      if SendPhysical(physicalMessage'Unchecked_Access) = 1 then
         status := VN.OK;
      else
         status := VN.ERROR_UNKNOWN;
      end if;

      return;

      if CAN_Message_Buffers.Full(SendBuffer) then
         status := VN.ERROR_BUFFER_FULL;
      else
         status := VN.OK;
         CAN_Message_Buffers.Insert(physicalMessage, SendBuffer);
      end if;
   end Send;

   function Send_Buffer_Full return Boolean is
   begin
      return CAN_Message_Buffers.Full(SendBuffer);
   end Send_Buffer_Full;

   procedure Receive(message : out VN.Communication.CAN.CAN_Message_Logical;
                     status : out VN.Receive_Status) is

      physicalMessage : aliased Physical_Logical.CAN_Message_Physical;
      use Interfaces.C;
   begin


      -- ToDo: This is just for testing, right now the CAN drivers don't work so we'll have to pretend we just didn't receive a message
--        status := VN.NO_MSG_RECEIVED;
--        return;

      if ReceivePhysical(physicalMessage'Unchecked_Access) = 1 then
         status := VN.MSG_RECEIVED_NO_MORE_AVAILABLE;
         Physical_Logical.PhysicalToLogical(physicalMessage, message);

         if message.isNormal and message.msgType = VN.Communication.CAN.CAN_Message_Type(1) then
            GNAT.IO.Put_Line("Received Assign_CAN_Address, address= " & physicalMessage.Data(4)'Img);
            GNAT.IO.Put_Line("ID= " & physicalMessage.ID'Img);
            GNAT.IO.Put_Line("Length= " & physicalMessage.Length'Img);

            GNAT.IO.Put_Line("Data= " & physicalMessage.Data(0)'Img & physicalMessage.Data(1)'Img & physicalMessage.Data(2)'Img
                             & physicalMessage.Data(3)'Img& physicalMessage.Data(4)'Img & physicalMessage.Data(5)'Img
                             & physicalMessage.Data(6)'Img & physicalMessage.Data(7)'Img);

         end if;
      else
         status := VN.NO_MSG_RECEIVED;
      end if;

      return;

      if CAN_Message_Buffers.Empty(ReceiveBuffer) then
         status := VN.NO_MSG_RECEIVED;
      else
         CAN_Message_Buffers.Remove(Physical_Logical.CAN_Message_Physical(physicalMessage), ReceiveBuffer);
         Physical_Logical.PhysicalToLogical(physicalMessage, message);

         if CAN_Message_Buffers.Empty(ReceiveBuffer) then
            status := VN.MSG_RECEIVED_NO_MORE_AVAILABLE;
         else
            status := VN.MSG_RECEIVED_MORE_AVAILABLE;
         end if;
      end if;
   end Receive;

   procedure Update_Filters(filterAccess : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is
      mask_C, template_C : Interfaces.C.unsigned;
      mask, template : VN.Communication.CAN.CAN_message_ID;
      isUsed, hasChanged : Boolean;

      ret : Interfaces.C.int;

      use Interfaces.C;
   begin

      for i in VN.Communication.CAN.CAN_Filtering.Filter_ID_Type'Range loop
         filterAccess.Get_Filter(i, mask, template, isUsed, hasChanged);

         if isUsed and hasChanged then
            ret := Set_CAN_Filter(Interfaces.C.unsigned_char(i), Interfaces.C.unsigned(mask), Interfaces.C.unsigned(template));

            if ret /= 1 then
               GNAT.IO.Put_Line("Update of CAN filter failed");
            end if;
         end if;
      end loop;
   end Update_Filters;




-- Remove this when compiling for PC, keep when compiling for SmartFusion2:
--     procedure CANHandler(ID : System.BB.Interrupts.Interrupt_ID) is
--     begin
--        null; --ToDo: Implement this...
--     end CANHandler;

   procedure Init is
      ret : Interfaces.Integer_32;
   begin
      ret := Interfaces.Integer_32(CAN_Init); -- Remove this when compiling for PC, keep when compiling for SmartFusion2

      GNAT.IO.New_Line(2);

      if ret = 0 then
         GNAT.IO.Put_Line("CAN initiated successfully");
      else

         GNAT.IO.Put_Line("CAN initiated with error code= " & ret'Img);
      end if;
    --  System.BB.Interrupts.Attach_Handler(CANHandler'Access, System.BB.Interrupts.Interrupt_ID(32));
   end Init;


   -- ToDo: This is just for testing, to make sure the C-code does not interfere:
--     function SendPhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int is
--     begin
--        return 1;
--     end SendPhysical;
--
--     function ReceivePhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int is
--     begin
--        return 0;
--     end ReceivePhysical;
--
--     procedure Test_CAN_Send is
--     begin
--        null;
--     end Test_CAN_Send;
--
--     function Test return Interfaces.C.int is
--     begin
--        null;
--     end Test;

begin
   Init;
end VN.Communication.CAN.CAN_Driver;
