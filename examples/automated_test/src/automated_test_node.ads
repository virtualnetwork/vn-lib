-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Automated_Test_Node is a test for the VN protocol.

with System;
with Ada.Real_Time;

with Interfaces;
use Interfaces;

with VN;

with VN.Communication;
with VN.Communication.CAN;
use VN.Communication.CAN;
with VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.CAN.CAN_Filtering;

with VN.Communication.Protocol_Routing;

package Automated_Test_Node is

   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;

   CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);
   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);

   myInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access,
      theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.Node);

   myCANTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (myInterface'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

end Automated_Test_Node;
