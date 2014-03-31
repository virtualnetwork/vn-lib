
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:

With VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with Ada.Real_Time;
with System;

package Task_Test is


   thePeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);

   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);

   myInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (myInterface'Access, System.Priority'Last, thePeriod'Access);


end Task_Test;
