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
--  Copyright 2014 Christoffer Holmstedt (christoffer.holmstedt@gmail.com)
------------------------------------------------------------------------------

with VN.Message;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Distribute_Route;
with VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;

package body VN.Communication.Routing is

   -- Router Send procedure
   procedure Send(This: in out Router;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status)
   is

      procedure Handle_Distribute_Route(Message: in VN.Message.VN_Message_Basic;
                                        source : Protocol_Address_Type) is

         msgDistribute : VN.Message.Distribute_Route.VN_Message_Distribute_Route;
      begin
         VN.Message.Distribute_Route.To_Distribute_Route(Message, msgDistribute);
         Protocol_Router.Insert(This.myTable, msgDistribute.Component_Address, source);
      end Handle_Distribute_Route;

      found : Boolean;
      address : Protocol_Address_Type;

      msgAssignAddr : VN.Message.Assign_Address.VN_Message_Assign_Address;
      msgAssignAddrBlock : VN.Message.Assign_Address_Block.VN_Message_Assign_Address_Block;

      use VN.Message;
   begin

--      if not This.Initiated then
--         This.Init;
--      end if;

      -- ASSIGN_ADDR and ASSIGN_ADDR_BLOCK are routed on their receiver's
      -- CUUID since the receiver does not have a logical address yet
      if Message.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
         VN.Message.Assign_Address.To_Assign_Address(Message, msgAssignAddr);
         CUUID_Protocol_Routing.Search(msgAssignAddr.CUUID, address, found);

      elsif Message.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK then

         VN.Message.Assign_Address_Block.To_Assign_Address_Block(Message, msgAssignAddrBlock);
         CUUID_Protocol_Routing.Search(msgAssignAddrBlock.CUUID, address, found);
      else
         --Protocol_Address_Type(0) means that the message shall be returned to the application layer
         Protocol_Router.Insert(This.myTable, Message.Header.Source, Protocol_Address_Type(0));
         Protocol_Router.Search(This.myTable, Message.Header.Destination, address, found);

         --Get routing info from Distribute Route messages:
         if Message.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
            Handle_Distribute_Route(Message, address);
         end if;
      end if;

      if found then
         if address = 0 then -- the case when the message is to be sent back to the application layer
            Status := ERROR_UNKNOWN; -- ToDo, what do we do if This happens!!???
         else
            This.Array_Of_Routes(Integer(address)).Send(Message, Status);
         end if;
      else
         Status := ERROR_NO_ADDRESS_RECEIVED; --should not really happen?
      end if;
   end Send;

   -- Router Receive procedure
   procedure Receive(This: in out Router;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status)
   is

      procedure HandleCUUIDRouting(Message : VN.Message.VN_Message_Basic;
                                   source : Protocol_Address_Type) is

         msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
      begin
         VN.Message.Local_Hello.To_Local_Hello(Message, msgLocalHello);
         CUUID_Protocol_Routing.Insert(msgLocalHello.CUUID, source);
      end HandleCUUIDRouting;

      tempMsg : VN.Message.VN_Message_Basic;
      tempStatus : VN.Receive_Status;
      stop : boolean := false;
      firstLoop : boolean := true;
      wasNextInTurn : Protocol_Address_Type := This.nextProtocolInTurn;

      found : Boolean;
      address : Protocol_Address_Type;
      sendStatus : VN.Send_Status;

      use VN.Message;
   begin

      while firstLoop or (not stop and wasNextInTurn /= This.nextProtocolInTurn) loop

         firstLoop := false;
         This.Array_Of_Routes(This.nextProtocolInTurn).Receive(tempMsg, tempStatus);

         --TODO, This will need to be updated if more options for VN.Receive_Status are added:
         if tempStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
           tempStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

            --A special case of retreiving routing info:
            if tempMsg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               HandleCUUIDRouting(tempMsg, This.nextProtocolInTurn);
            else
               Protocol_Router.Insert(This.myTable, tempMsg.Header.Source,
                                      Protocol_Address_Type(This.nextProtocolInTurn));
            end if;

            --Check if the message shall be re-routed onto a subnet, or returned to the application layer:
            if tempMsg.Header.Opcode /= VN.Message.OPCODE_LOCAL_HELLO and --LocalHello and LocalAck shall always be sent to the application layer
              tempMsg.Header.Opcode /= VN.Message.OPCODE_LOCAL_ACK then

               Protocol_Router.Search(This.myTable, tempMsg.Header.Destination, address, found);

               if found and address /= 0 then -- address = 0 means send to Application layer
                  This.Array_Of_Routes(address).Send(tempMsg, sendStatus); --Pass the message on to another subnet
                  tempStatus := VN.NO_MSG_RECEIVED;

                  stop := false;
               else
                  stop := true;
               end if;
            else
               stop := true;
            end if;

            if stop then
               Status := tempStatus;
               Message := tempMsg;
            end if;
         end if;

         This.nextProtocolInTurn := This.nextProtocolInTurn rem This.Number_Of_Routes;
         This.nextProtocolInTurn := This.nextProtocolInTurn + 1;
      end loop;

   end Receive;

   -- Router Add procedure
   procedure Add_Com(This : in out Router;
               Com_Ptr: VN.Communication.Com_Access)
   is
   begin
      if This.Number_Of_Routes >= MAX_NUMBER_OF_SUBNETS then
         return;
      end if;

      for i in Com_Access_Array'First .. Com_Access_Array'First + This.Number_Of_Routes - 1 loop
         if This.Array_Of_Routes(i) = Com_Ptr then
            return;
         end if;
      end loop;

      This.Array_Of_Routes(Com_Access_Array'First + This.Number_Of_Routes) := Com_Ptr;
      This.Number_Of_Routes := This.Number_Of_Routes + 1;
   end Add_Com;

end VN.Communication.Routing;
