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

with VN.Message.Local_Hello;

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
   status : VN.Receive_Status;
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
         VN.Communication.CAN.Logic.SM.Receive(DutyArray(i).all, msg, status);
         
         if status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or status = VN.MSG_RECEIVED_MORE_AVAILABLE then
            VN.Text_IO.New_Line;
            
            VN.Text_IO.Put_Line("VN message received by duty no " & i'Img & " type= " & msg.Header.Message_Type'img & " Opcode= " & msg.Header.Opcode'img);

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
               VN.Text_IO.Put_Line("LocalHello type= " & msgLocalHello.Header.Message_Type'Img & 
                                     " CUUID(1)= " & msgLocalHello.CUUID(1)'img & " component type = " & msgLocalHello.Component_Type'img);
            end if;
            
            VN.Text_IO.New_Line;
         end if;
            
      end loop;
   end loop;
end main;
