
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: CAN_Task is a task that sends and receives CAN messages.
--  that are to be sent, or have been received, over the CAN network.
-- Main_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.

with System;
with Ada.Real_Time;
with VN.Communication.CAN.CAN_Interface;

--with BBB_CAN;

package VN.Communication.CAN.Can_Task is

   task type CAN_Task_Type(myAccess : VN.Communication.CAN.CAN_Interface.CAN_Interface_Access;
                           Pri : System.Priority;
                           thePeriod : access Ada.Real_Time.Time_Span) is --;
                         --  port : access String  -- SHALL NOT INCLUDE "/dev/"
                         --  ) is
      pragma Priority(Pri);
   end CAN_Task_Type;
end VN.Communication.CAN.Can_Task;
