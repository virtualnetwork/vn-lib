
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

package body VN.Communication.Protocol_Routing is

   protected body Protocol_Routing_Type is

      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status) is
         found : Boolean;
         address : Protocol_Address_Type;
      begin
         Protocol_Router.Insert(myTable, Message.Get_Source, Application_Layer);

         Protocol_Router.Search(myTable, Message.Get_Destination, address, found);

         if found then
            case address is
               when CAN_Subnet =>
                  myCANInterface.Send(Message, Status);

               when Application_Layer =>
                  null; -- ToDo, what do we do if this happens!!???
            end case;
         else
            Status := ERROR_NO_ADDRESS_RECEIVED; --should not really happen?
         end if;
      end Send;


      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status) is

         tempMsg    : VN.Message.VN_Message_Basic;
         tempStatus : VN.Receive_Status;
      begin

         case nextProtocolInTurn is
            when CAN_Subnet => -- TODO, update this line when more Subnets are added

               nextProtocolInTurn := CAN_Subnet; -- TODO, update this line when more Subnets are added

               myCANInterface.Receive(tempMsg, tempStatus);

                 --TODO, this will need to be updated if more options for VN.Receive_Status are added:
               if tempStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
                 tempStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

                  Protocol_Router.Insert(myTable, tempMsg.Get_Source, CAN_Subnet);
                  Status := tempStatus;
                  VN.Message.Assignment(Message, tempMsg);
               end if;

            when Application_Layer =>  -- TODO, add more cases as more Subnets are added
               null;
         end case;

         Status := tempStatus;
      end Receive;

   end Protocol_Routing_Type;
end VN.Communication.Protocol_Routing;
