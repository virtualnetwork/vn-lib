
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Task is the lowlevel task that accesses the CAN_Interface object.
-- It reads CAN messages from the lowlevel read buffer, runs the Update
-- function of CAN_Interface and writes CAN messages to the lowlevel send buffer.
-- Each task that accesses an instance of CAN_Interface will do so using an
-- access variable (pointer).

with System;
with Ada.Real_Time;
with VN.Communication.CAN.CAN_Interface;


with CAN_Driver;

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
