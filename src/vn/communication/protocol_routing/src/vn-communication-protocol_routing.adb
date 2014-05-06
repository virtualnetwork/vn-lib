
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

with VN;
with VN.Message;
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Distribute_Route;
with VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;

package body VN.Communication.Protocol_Routing is

   procedure Send(this : in out Protocol_Routing_Type;
                             Message: in VN.Message.VN_Message_Basic;
                             Status: out VN.Send_Status) is

      procedure Handle_Distribute_Route(Message: in VN.Message.VN_Message_Basic;
                                        source : Protocol_Address_Type) is

         msgDistribute : VN.Message.Distribute_Route.VN_Message_Distribute_Route;
      begin
         VN.Message.Distribute_Route.To_Distribute_Route(Message, msgDistribute);
         Protocol_Router.Insert(this.myTable, msgDistribute.Component_Address, source);
      end Handle_Distribute_Route;

      found   : Boolean;
      address : Protocol_Address_Type;

      msgAssignAddr 	 : VN.Message.Assign_Address.VN_Message_Assign_Address;
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
         --Protocol_Address_Type(0) means that the message shall be returned to the application layer
         Protocol_Router.Insert(this.myTable, Message.Header.Source, Protocol_Address_Type(0));
         Protocol_Router.Search(this.myTable, Message.Header.Destination, address, found);

         --Get routing info from Distribute Route messages:
         if Message.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
            Handle_Distribute_Route(Message, address);
         end if;
      end if;

      if found then
         if address = 0 then -- the case when the message is to be sent back to the application layer
            Status := ERROR_UNKNOWN; -- ToDo, what do we do if this happens!!???
         else
            this.myInterfaces(Integer(address)).Send(Message, Status);
         end if;
      else
         Status := ERROR_NO_ADDRESS_RECEIVED; --should not really happen?
      end if;
   end Send;


   procedure Receive(this : in out Protocol_Routing_Type;
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
      stop          : boolean := false;
      firstLoop     : boolean := true;
      wasNextInTurn : Protocol_Address_Type := this.nextProtocolInTurn;

      found      : Boolean;
      address    : Protocol_Address_Type;
      sendStatus : VN.Send_Status;
   begin

      if this.numberOfInterfaces > 0 then
         while firstLoop or (not stop and wasNextInTurn /= this.nextProtocolInTurn) loop

            firstLoop := false;
            this.myInterfaces(this.nextProtocolInTurn).Receive(tempMsg, tempStatus);

            --TODO, this will need to be updated if more options for VN.Receive_Status are added:
            if tempStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
              tempStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

               --A special case of retreiving routing info:
               if tempMsg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
                  HandleCUUIDRouting(tempMsg, this.nextProtocolInTurn);
               else
                  Protocol_Router.Insert(this.myTable, tempMsg.Header.Source,
                                         Protocol_Address_Type(this.nextProtocolInTurn));
               end if;

               --Check if the message shall be re-routed onto a subnet, or returned to the application layer:
               if tempMsg.Header.Opcode /= VN.Message.OPCODE_LOCAL_HELLO and --LocalHello and LocalAck shall always be sent to the application layer
                 tempMsg.Header.Opcode /= VN.Message.OPCODE_LOCAL_ACK then

                  Protocol_Router.Search(this.myTable, tempMsg.Header.Destination, address, found);

                  if found and address /= 0 then --  address = 0 means send to Application layer
                     this.myInterfaces(address).Send(tempMsg, sendStatus); --Pass the message on to another subnet
                     tempStatus := VN.NO_MSG_RECEIVED;

                     stop := false;
                  else
                     stop := true;
                  end if;
               else
                  stop := true;
               end if;

               if stop then
                  Status  := tempStatus;
                  Message := tempMsg;
               end if;
            end if;

            this.nextProtocolInTurn := this.nextProtocolInTurn rem this.numberOfInterfaces;
            this.nextProtocolInTurn := this.nextProtocolInTurn + 1;
         end loop;

      else
         Status := VN.NO_MSG_RECEIVED;
      end if;
   end Receive;

   procedure Add_Interface(this : in out Protocol_Routing_Type;
                           theInterface : VN.Communication.Com_Access) is
      TOO_MANY_INTERFACES : exception;
   begin
      if this.numberOfInterfaces >= MAX_NUMBER_OF_SUBNETS then
         raise TOO_MANY_INTERFACES;
      end if;

      for i in Interface_Array'First .. Interface_Array'First + this.numberOfInterfaces - 1 loop
         if this.myInterfaces(i) = theInterface then
            return;
         end if;
      end loop;

      this.myInterfaces(Interface_Array'First + this.numberOfInterfaces) := theInterface;
      this.numberOfInterfaces := this.numberOfInterfaces + 1;
   end Add_Interface;

   procedure Init(this : in out Protocol_Routing_Type) is -- ToDo: For testing only!!!!!!!!!!
      testCUUID : VN.VN_CUUID := (others => 42);
   begin
      this.Initiated := true;
      Protocol_Router.Insert(this.myTable, 1337, Protocol_Address_Type(1));
      CUUID_Protocol_Routing.Insert(testCUUID, Protocol_Address_Type(1));

      GNAT.IO.Put_Line("Protocol_Routing initiated");
   end Init;

end VN.Communication.Protocol_Routing;
