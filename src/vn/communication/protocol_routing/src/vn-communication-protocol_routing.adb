
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing is a middle layer between the subnets and the
-- application layer. The Protocol_Routing package will decide on
-- which subnet to send a VN message sent by the application layer
-- and whether a VN message received via a subnet shall be
-- delivered to the application layer or sent to another
-- subnet, and if so, which one.

-- ToDo: Protocol_Routing_Type might not need to be protected?
-- ToDo: Only CAN subnet added so far.
-- ToDo: Some special cases not handled.

with VN;
with VN.Message;
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Distribute_Route;
with VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;

package body VN.Communication.Protocol_Routing is

   overriding procedure Send(this : in out Protocol_Routing_Type;
                             Message: in VN.Message.VN_Message_Basic;
                             Status: out VN.Send_Status) is

      procedure Handle_Distribute_Route(Message: in VN.Message.VN_Message_Basic;
                                        source : Protocol_Address_Type) is

         msgDistribute : VN.Message.Distribute_Route.VN_Message_Distribute_Route;
      begin
         VN.Message.Distribute_Route.To_Distribute_Route(Message, msgDistribute);
         Protocol_Router.Insert(this.myTable, msgDistribute.Component_Address, source);
      end Handle_Distribute_Route;


      found : Boolean;
      address : Protocol_Address_Type;

      msgAssignAddr 	    : VN.Message.Assign_Address.VN_Message_Assign_Address;
      msgAssignAddrBlock : VN.Message.Assign_Address_Block.VN_Message_Assign_Address_Block;
   begin

      if not this.Initiated then
         this.Init;
      end if;

      -- ASSIGN_ADDR and ASSIGN_ADDR_BLOCK are routed on their receiver's
      -- CUUID since the receiver does not have a logical address yet
      if Message.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
         VN.Message.Assign_Address.To_Assign_Address(Message, msgAssignAddr);
         CUUID_Protocol_Routing.Search(msgAssignAddr.CUUID, address, found);

      elsif Message.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK then

         VN.Message.Assign_Address_Block.To_Assign_Address_Block(Message, msgAssignAddrBlock);
         CUUID_Protocol_Routing.Search(msgAssignAddrBlock.CUUID, address, found);
      else
         Protocol_Router.Insert(this.myTable, Message.Header.Source, Application_Layer);
         Protocol_Router.Search(this.myTable, Message.Header.Destination, address, found);

         --Get routing info from Distribute Route messages:
         if Message.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
            Handle_Distribute_Route(Message, address);
         end if;
      end if;

      if found then
         case address is
            when CAN_Subnet =>
               this.myCANInterface.Send(Message, Status);

            when Application_Layer =>
               null; -- ToDo, what do we do if this happens!!???
         end case;
      else
         Status := ERROR_NO_ADDRESS_RECEIVED; --should not really happen?
      end if;
   end Send;


   overriding procedure Receive(this : in out Protocol_Routing_Type;
                                Message : out VN.Message.VN_Message_Basic;
                                Status: out VN.Receive_Status) is

      procedure HandleCUUIDRouting(Message : VN.Message.VN_Message_Basic;
                                   source : Protocol_Address_Type) is

         msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
      begin
         VN.Message.Local_Hello.To_Local_Hello(Message, msgLocalHello);
         CUUID_Protocol_Routing.Insert(msgLocalHello.CUUID, source);
      end HandleCUUIDRouting;


      tempMsg    : VN.Message.VN_Message_Basic;
      tempStatus : VN.Receive_Status;

      stop : boolean := false;
      firstLoop : boolean := true;
      wasNextInTurn : Protocol_Address_Type := this.nextProtocolInTurn;

   begin

      while firstLoop or (not stop and wasNextInTurn /= this.nextProtocolInTurn) loop
         firstLoop := false;

         case this.nextProtocolInTurn is
            when CAN_Subnet => -- TODO, update this line when more Subnets are added

               this.nextProtocolInTurn := CAN_Subnet; -- TODO, update this line when more Subnets are added

               this.myCANInterface.Receive(tempMsg, tempStatus);

               --TODO, this will need to be updated if more options for VN.Receive_Status are added:
               if tempStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
                 tempStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

                  --Some special cases of retreiving routing info:
                  if tempMsg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
                     HandleCUUIDRouting(tempMsg, CAN_Subnet);

                  else
                     Protocol_Router.Insert(this.myTable, tempMsg.Header.Source, CAN_Subnet);
                  end if;

                  Status := tempStatus;
                  Message := tempMsg;

                  stop := true;
               end if;

            when Application_Layer =>  -- TODO, add more cases as more Subnets are added
               null;
         end case;
      end loop;

   end Receive;

   procedure Init(this : in out Protocol_Routing_Type) is -- ToDo: For testing only!!!!!!!!!!
      testCUUID : VN.VN_CUUID := (others => 42);
   begin
      this.Initiated := true;
      Protocol_Router.Insert(this.myTable, 1337, CAN_Subnet);
      CUUID_Protocol_Routing.Insert(testCUUID, CAN_Subnet);

      GNAT.IO.Put_Line("Protocol_Routing initiated");  --ToDo, for testing
   end Init;

end VN.Communication.Protocol_Routing;
