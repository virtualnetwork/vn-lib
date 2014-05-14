-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
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
               bWillSend := true;
               return;
            else
               
               if this.timer - Ada.Real_Time.Clock > ADDRESS_DISTRIBUTION_PERIOD then

                  this.timer := Ada.Real_Time.Clock;
                  
                  VN.Communication.CAN.Logic.Message_Utils.AddressAnswerToMessage(msgOut, 255, this.myCANAddress, this.myCANAddress, 
                                                                                  this.list(this.list'First), 0); 
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


   procedure Sent_To_Address(this : in out Logical_Address_Handler; Address : VN.VN_Logical_Address) is
      found : Boolean;
      temp : Boolean;
   begin
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

            -- Add the address to be sent in a AddressAswer message as soon as possible
            if not Address_Buffers.Full(this.sendBuffer) then 
               Address_Buffers.Insert(Address, this.sendBuffer);
            end if;
         end if;
      end if;
   end Sent_To_Address; 

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

