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

-- Summary: Logical_Address_Handler keeps track of which logical addresses 
-- that exist above the CAN subnet (i.e. messages sent to these logical
-- addresses can be routed via this unit).

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Logical_Address_Handler is

   overriding procedure Update(this : in out Logical_Address_Handler; 
                               msgIn : VN.Communication.CAN.CAN_Message_Logical;      bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
      
      address : VN.VN_Logical_Address;

      use Ada.Real_Time;
   begin

      case this.currentState is 
         when Unactivated => 
            bWillSend := false;
            
         when Activated => 
           
            if not Address_Buffers.Empty(this.sendBuffer) then            
               
               Address_Buffers.Remove(address, this.sendBuffer);               
               VN.Communication.CAN.Logic.Message_Utils.AddressAnswerToMessage(msgOut, 255, this.myCANAddress, 
                                                                               this.myCANAddress, address, 0);

               VN.Communication.CAN.Logic.DebugOutput("CAN address " & this.myCANAddress'Img & " sent AddressAnswer about log addr " & address'Img, 1);
               
               bWillSend := true;
               return;
            else
               
               if Ada.Real_Time.Clock - this.timer > ADDRESS_DISTRIBUTION_PERIOD and this.numAddresses > 0 then

                  this.timer := Ada.Real_Time.Clock;
                  
                  VN.Communication.CAN.Logic.Message_Utils.AddressAnswerToMessage(msgOut, 255, this.myCANAddress, this.myCANAddress, 
                                                                                  this.list(this.list'First), 0); 
                  
                  VN.Communication.CAN.Logic.DebugOutput("CAN address " & this.myCANAddress'Img & " REsent AddressAnswer about log addr " & this.list(this.list'First)'Img, 1);
                  
                  bWillSend := true;

                  if this.numAddresses > 1 then
                     this.currentState := Distributing;
                     this.distIndex := this.distIndex + 1;
                  end if;
               else 
                     bWillSend := false;
               end if;
            end if;

         when Distributing => 
            if this.distIndex < this.numAddresses then
               VN.Communication.CAN.Logic.Message_Utils.AddressAnswerToMessage(msgOut, 255, this.myCANAddress, this.myCANAddress,  
                                                                               this.list(this.distIndex), 0);
               
               VN.Communication.CAN.Logic.DebugOutput("CAN address " & this.myCANAddress'Img & " distributed AddressAnswer about log addr " & this.list(this.distIndex)'Img, 1);
               
               this.distIndex := this.distIndex + 1;
               bWillSend := true;
            else 
               this.distIndex := this.list'First;            
               this.currentState := Activated;
               bWillSend := false;
            end if;
      end case;
   end Update;
      

   procedure Received_From_Address(this : in out Logical_Address_Handler; Address : VN.VN_Logical_Address) is
   begin
      Bool_Routing.Insert(this.hasReceivedFrom, Address, true);
   end Received_From_Address;


   procedure Sent_From_Address(this : in out Logical_Address_Handler; Address : VN.VN_Logical_Address) is
      found : Boolean;
      temp : Boolean;
   begin

      if Address = VN.LOGICAL_ADDRES_UNKNOWN then
         return;
      end if;
        
      Bool_Routing.Search(this.hasReceivedFrom, Address, temp, found);
      
      if not found then
         Bool_Routing.Search(this.hasSentTo, Address, temp, found);

         if not found then
            Bool_Routing.Insert(this.hasSentTo, Address, true);
            
            -- Add the address to be sent in a AddressAswer message periodically
            if this.numAddresses < this.list'Length then
               this.list(this.list'First + this.numAddresses) := Address;
               this.numAddresses := this.numAddresses + 1;
            end if;

            -- ToDo: An error occurs here, therefore this is commented out
            -- Add the address to be sent in a AddressAswer message as soon as possible
--              if not Address_Buffers.Full(this.sendBuffer) then 
--                 Address_Buffers.Insert(Address, this.sendBuffer);
--              end if;
         end if;
      end if;
   end Sent_From_Address; 

   procedure Activate(this : in out Logical_Address_Handler; 
                      theCANAddress : VN.Communication.CAN.CAN_Address_Sender) is
   begin 
      if this.currentState = Unactivated then
         this.myCANAddress := theCANAddress;
         this.currentState := Activated;
      
         this.timer := Ada.Real_Time.Clock;
      end if;
   end Activate;
 
end VN.Communication.CAN.Logic.Logical_Address_Handler;
