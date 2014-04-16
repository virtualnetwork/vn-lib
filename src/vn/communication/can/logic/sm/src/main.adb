-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Test implementation of the whole VN.Communication.CAN.Logic package.
-- Several instances of VN.Communication.CAN.Logic.SM.SM_Duty are 
-- are used and the communication between them is simulated.

pragma Profile (Ravenscar);

with VN;
use VN;

with VN.Communication.CAN.Logic.SM;

with Ada.Real_Time;
use Ada.Real_Time;

with VN.Communication.CAN.Logic;
use VN.Communication.CAN.Logic;

with VN.Communication.CAN.Logic.SM;
with VN.Message;
use VN.Message;

with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Local_Ack;
with VN.Message.Assign_Address;

with Interfaces;


procedure main is

   use VN.Communication.CAN.CAN_Message_Buffers;
   use VN.Communication.CAN.Logic.SM.Unit_Buffers;

   Next_Period : Ada.Real_Time.Time;
   Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(400);

   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);
   U2 : aliased VN.Communication.CAN.UCID := 2;
   C2 : aliased VN.VN_CUUID := (2, others => 5);
   U3 : aliased VN.Communication.CAN.UCID := 3;
   C3 : aliased VN.VN_CUUID := (3, others => 5);


--     DutyArray : Array(1..3) of VN.Communication.CAN.Logic.SM.SM_Duty_ptr :=
--       (new VN.Communication.CAN.Logic.SM.SM_Duty(U1'Unchecked_Access, C1'Unchecked_Access),
--        new VN.Communication.CAN.Logic.SM.SM_Duty(U2'Unchecked_Access, C2'Unchecked_Access),
--        new VN.Communication.CAN.Logic.SM.SM_Duty(U3'Unchecked_Access, C3'Unchecked_Access));

   DutyArray : Array(1..2) of VN.Communication.CAN.Logic.SM.SM_Duty_ptr :=
     (new VN.Communication.CAN.Logic.SM.SM_Duty(U1'Unchecked_Access, C1'Unchecked_Access),
      new VN.Communication.CAN.Logic.SM.SM_Duty(U2'Unchecked_Access, C2'Unchecked_Access));
   
   type BufferArray is array(DutyArray'Range) of VN.Communication.CAN.CAN_Message_Buffers.Buffer(100);
   messagesIn : BufferArray;
   messagesOut : BufferArray;


   tempTest : boolean := false;--for testing

   msg : VN.Message.VN_Message_Basic;
   msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
   msgLocalAck : VN.Message.Local_Ack.VN_Message_Local_Ack;

   msgAssignAddr 	 : VN.Message.Assign_Address.VN_Message_Assign_Address;

   receiveStatus : VN.Receive_Status;
   sendStatus : VN.Send_Status;
begin

   VN.Text_IO.Put_Line("VN.Message.VN_Message_Basic'Size= " & VN.Message.VN_Message_Basic'Size'Img);
   VN.Text_IO.Put_Line("VN.Message.Local_Hello.VN_Message_Local_Hello'Size= " & VN.Message.Local_Hello.VN_Message_Local_Hello'Size'Img);
   VN.Text_IO.Put_Line("VN.Message.VN_Message_Byte_Array'Size= " & VN.Message.VN_Message_Byte_Array'Size'Img);

   VN.Text_IO.New_Line;

   Next_Period := Ada.Real_Time.Clock;

   loop
      Next_Period := Next_Period + Period;
      delay until Next_Period;

      for i in DutyArray'Range loop
         VN.Communication.CAN.CAN_Message_Buffers.Clear(messagesOut(i));
         VN.Communication.CAN.Logic.SM.Update(DutyArray(i).all, messagesIn(i), messagesOut(i));

         VN.Communication.CAN.CAN_Message_Buffers.Clear(messagesIn(i));
      end loop;

      for i in messagesOut'Range loop
         declare
            element : VN.Communication.CAN.CAN_Message_Logical;
         begin
            while not VN.Communication.CAN.CAN_Message_Buffers.Empty(messagesOut(i)) loop

               VN.Communication.CAN.CAN_Message_Buffers.Remove(element, messagesOut(i));

               for j in messagesIn'Range loop
                  if i /= j then
                     VN.Communication.CAN.CAN_Message_Buffers.Insert(element, messagesIn(j));
                  end if;
               end loop;
            end loop;
         end;
      end loop;

      for i in DutyArray'Range loop
         VN.Communication.CAN.Logic.SM.Receive(DutyArray(i).all, msg, receiveStatus);
         
         if receiveStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or receiveStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then
            VN.Text_IO.New_Line;
            
            VN.Text_IO.Put_Line("VN message received by duty no " & i'Img & " type= " & msg.Header.Message_Type'img & " Opcode= " & msg.Header.Opcode'img);

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
               VN.Text_IO.Put_Line("LocalHello, type= " & msgLocalHello.Header.Message_Type'Img & 
                                     " CUUID(1)= " & msgLocalHello.CUUID(1)'img & " component type = " & msgLocalHello.Component_Type'img);

               -- Simulate SM-CAN master assigning addresses or address blocks:
               if i = DutyArray'First then
                  
                  msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
                  VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);
                  
                  msgAssignAddr.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
                  msgAssignAddr.Header.Source := 10;
                  msgAssignAddr.CUUID := msgLocalHello.CUUID;
                  msgAssignAddr.Assigned_Address := 20;
                  
                  VN.Message.Assign_Address.To_Basic(msgAssignAddr, msg);
                  VN.Communication.CAN.Logic.SM.Send(DutyArray(i).all, msg, sendStatus);    

                  VN.Text_IO.Put_Line("Assinged Logical address");
                  

               end if;
            end if;

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_ACK then
               VN.Message.Local_Ack.To_Local_Ack(msg, msgLocalAck);
               VN.Text_IO.Put_Line("LocalAck, type= " & msgLocalAck.Header.Message_Type'Img & 
                                     " status = " & msgLocalAck.Status'img);
            end if;

            if msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then 
               VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);

               if msgAssignAddr.CUUID = DutyArray(i).theCUUID.all then
                  VN.Text_IO.Put_Line("Was assigned logical address= " & msgAssignAddr.Assigned_Address'Img & 
                                        " by SM with logical address = " & msgAssignAddr.Header.Source'img);
               end if;
            end if;
            
            VN.Text_IO.New_Line;
         end if;
            
      end loop;
   end loop;
end main;
