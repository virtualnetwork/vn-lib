-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_SM_Type is a protected object that holds a Lowlevel.Main.Main_Duty.
-- CAN_SM_Type is to be used by two tasks: One higher level task and
-- CAN_task, a lower level task that handles CAN communication.

-- Each task that accesses an instance of CAN_SM_Type will do so using an
-- access variable (pointer).

-- Please note: If the Ravenscar profile had not been used, the CAN_Task
-- would have been put inside CAN_SM_Type which would have simplyfied the
-- interface of CAN_SM_Type. This would however violate the NO_TASK_HIERARCY
-- restriction that the Ravenscar profile imposes.

with Ada.Text_IO;

package body VN.Communication.CAN.CAN_Interface is

   protected body CAN_Interface_Type is

      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status) is
      begin

         case data.unitType is
            when SM_CAN =>
               if isInitialized then
                  data.SMDuty.Send(Message, Status);
               else
                  isInitialized := true;
                  Init;
                  data.SMDuty.Send(Message, Status);
               end if;

            when Node =>
               if isInitialized then
                  data.nodeDuty.Send(Message, Status);
               else
                  isInitialized := true;
                  Init;
                  data.nodeDuty.Send(Message, Status);
               end if;
         end case;
      end Send;

      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status) is
      begin
         case data.unitType is
            when SM_CAN =>
               if isInitialized then
                  data.SMDuty.Receive(Message, Status);
               else
                  isInitialized := true;
                  Init;
                  data.SMDuty.Receive(Message, Status);
               end if;

            when Node =>
               if isInitialized then
                  data.nodeDuty.Receive(Message, Status);
               else
                  isInitialized := true;
                  Init;
                  data.nodeDuty.Receive(Message, Status);
               end if;
         end case;
      end Receive;


      ----------- FUNCTIONS FOR CAN_task ---------------------
      procedure Update(msgsBuffer : in out VN.Communication.CAN.Logic.SM.CAN_Message_Buffers.Buffer;
                       ret : out VN.Communication.CAN.Logic.SM.CAN_Message_Buffers.Buffer) is

      begin
         case data.unitType is
            when SM_CAN =>
               if isInitialized then
                  data.SMDuty.Update(msgsBuffer, ret);
               else
                  isInitialized := true;
                  Init;
                  data.SMDuty.Update(msgsBuffer, ret);
               end if;

            when Node =>
               if isInitialized then
                  data.nodeDuty.Update(msgsBuffer, ret);
               else
                  isInitialized := true;
                  Init;
                  data.nodeDuty.Update(msgsBuffer, ret);
               end if;
         end case;
      end Update;

      --------------------------------------------------------

      procedure Init is
      begin
         case data.unitType is
            when SM_CAN =>
               if isInitialized then
                  data.SMDuty.Init;
               else
                  isInitialized := true;
                  Init;
                  data.SMDuty.Init;
               end if;

            when Node =>
               if isInitialized then
                  data.nodeDuty.Init;
               else
                  isInitialized := true;
                  Init;
                  data.nodeDuty.Init;
               end if;
         end case;
      end Init;

   end CAN_Interface_Type;

end VN.Communication.CAN.CAN_Interface;
