-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Task is the lowlevel task that accesses the CAN_SM_Type object.
-- It reads CAN messages from the lowlevel read buffer, runs the Update
-- function of CAN_SM_Type and writes CAN messages to the lowlevel send buffer.

-- Each task that accesses an instance of CAN_SM_Type will do so using an
-- access variable (pointer).

with Ada.Real_Time;
with Ada.Text_IO;

--with UartWrapper;
--  with Physical_Logical;
with VN.Communication.CAN;
with VN.Communication.CAN.Logic.SM;


package body VN.Communication.CAN.Can_Task is

   package buf renames VN.Communication.CAN.CAN_Message_Buffers;

   task body CAN_Task_Type is
      use Ada.Real_Time;

      myPeriod : Ada.Real_Time.Time_Span;

      Next_Period : Ada.Real_Time.Time;

      BUFFER_SIZE : constant integer := 20; --ToDO: Put this in a config file of some sort
      msgsIn, msgsOut : buf.Buffer(BUFFER_SIZE);

      procedure Input is
         hasReceived : boolean;
         msgLog  : VN.Communication.CAN.CAN_Message_Logical;
         --     msgPhys : CAN_Defs.CAN_Message;
         b : boolean;
      begin
         --  BBB_CAN.Get(msgPhys, hasReceived, b);

         while not buf.Full(msgsIn) and hasReceived loop

            --   Physical_Logical.PhysicalToLogical(msgPhys, msgLog);
            buf.Insert(msgLog, msgsIn);

            --   BBB_CAN.Get(msgPhys, hasReceived, b);
         end loop;
      end Input;

      procedure Output is
         msgLog  : VN.Communication.CAN.CAN_Message_Logical;
         --   msgPhys : CAN_Defs.CAN_Message;
      begin

         while not buf.Empty(msgsOut) loop
            buf.Remove(msgLog, msgsOut);
            --  Physical_Logical.LogicalToPhysical(msgLog, msgPhys);
            --       BBB_CAN.Send(msgPhys);
         end loop;
      end Output;


   begin

      Ada.Text_IO.Put_Line("CAN_Task started");

      myPeriod := thePeriod.all;
      Next_Period := Ada.Real_Time.Clock;

      --  BBB_CAN.Init(port.all, UartWrapper.B115200);

      loop
         Next_Period := Next_Period + myPeriod;
         delay until Next_Period;

         Input;
         myAccess.Update(msgsIn, msgsOut);
         Output;
      end loop;

   end CAN_Task_Type;
end VN.Communication.CAN.Can_Task;
