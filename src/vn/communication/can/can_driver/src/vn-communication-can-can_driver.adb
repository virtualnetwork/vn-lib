------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- This package contains procedures for sending and receiving CAN messages
-- as well as for conversion between logical and physical representations
-- of CAN messages.
-- This code interfaces with driver code written in C.

-- Please note: This code will not compile unless it is compiled with the correct makefile,
-- the BAP repository needs to be included.

-- ToDo: The imported C-drivers are (at the time of writing) not very good or reliable.
-- ToDo: The imported C-drivers do not support the CAN message filtering.


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
      Physical_Logical.LogicalToPhysical(message, physicalMessage);

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

      if ReceivePhysical(physicalMessage'Unchecked_Access) = 1 then
         status := VN.MSG_RECEIVED_NO_MORE_AVAILABLE;
         Physical_Logical.PhysicalToLogical(physicalMessage, message);
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

   procedure Init is
      ret : Interfaces.Integer_32;
   begin
      ret := Interfaces.Integer_32(CAN_Init); 

      GNAT.IO.New_Line(2);

      if ret = 0 then
         GNAT.IO.Put_Line("CAN initiated successfully");
      else
         GNAT.IO.Put_Line("CAN initiated with error code= " & ret'Img);
      end if;
   end Init;

begin
   Init;
end VN.Communication.CAN.CAN_Driver;
