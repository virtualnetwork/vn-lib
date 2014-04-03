
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:

With VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with Ada.Real_Time;
with System;

--  with My_Memcpy;
--  with My_Secondary_Stack;
--  with My_Gnat_Malloc;

package Task_Test is

--     type VN_Message_Basic is tagged limited private;
--
--     function VN_Message_To_Int(i : Integer) return VN_Message_Basic;
--     function Int_To_VN_Message(m : VN_Message_Basic) return Integer;
--
--     pragma Stream_Convert(VN_Message_Basic, VN_Message_To_Int, Int_To_VN_Message);

   thePeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);
   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);

   myInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (myInterface'Access, System.Priority'Last, thePeriod'Access);

--  private
--     type VN_Message_Basic is tagged limited
--        record
--           temp : Integer;
--        end record;

   procedure Nothing;

end Task_Test;
