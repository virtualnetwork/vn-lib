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

--  with VN.Text_IO;

package body VN.Communication.CAN.CAN_Interface is

   protected body CAN_Interface_Type is

      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status) is
      begin

         case data.unitType is
            when SM_CAN =>
               if not isInitialized then
                  Init;
               end if;
               VN.Communication.CAN.Logic.SM.Send(data.SMDuty, Message, Status);
                --  data.SMDuty.Send(Message, Status);

            when Node =>
               if not isInitialized then
                  Init;
               end if;
             --  VN.Communication.CAN.Logic.Node.Send(data.nodeDuty, Message, Status);
--                 data.nodeDuty.Send(Message, Status);
         end case;
      end Send;

      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status) is
      begin
         case data.unitType is
            when SM_CAN =>
               if not isInitialized then
                  Init;
               end if;
               VN.Communication.CAN.Logic.SM.Receive(data.SMDuty, Message, Status);
               --  data.SMDuty.Receive(Message, Status);

            when Node =>
               if not isInitialized then
                  Init;
               end if;
              -- VN.Communication.CAN.Logic.Node.Receive(data.nodeDuty, Message, Status);
               --     data.nodeDuty.Receive(Message, Status);
         end case;
      end Receive;


      ----------- FUNCTIONS FOR CAN_task ---------------------
      procedure Update(msgsBuffer : in out VN.Communication.CAN.CAN_Message_Buffers.Buffer;
                       ret : out VN.Communication.CAN.CAN_Message_Buffers.Buffer) is

      begin
         case data.unitType is
            when SM_CAN =>
               if not isInitialized then
                  Init;
               end if;
               VN.Communication.CAN.Logic.SM.Update(data.SMDuty, msgsBuffer, ret);
--                 data.SMDuty.Update(msgsBuffer, ret);

            when Node =>
               if not isInitialized then
                  Init;
               end if;
--                 VN.Communication.CAN.Logic.Node.Update(data.nodeDuty, msgsBuffer, ret);
--       data.nodeDuty.Update(msgsBuffer, ret);
         end case;
      end Update;

      --------------------------------------------------------

      procedure Init is
      begin
         case data.unitType is
            when SM_CAN =>
               if not isInitialized then
                  isInitialized := true;
                  VN.Communication.CAN.Logic.SM.Init(data.SMDuty);
--                    data.SMDuty.Init;
               end if;

            when Node =>
               if not isInitialized then
                  isInitialized := true;
--                    VN.Communication.CAN.Logic.Node.Init(data.SMDuty)
--                    data.nodeDuty.Init;
               end if;
         end case;
      end Init;

   end CAN_Interface_Type;

end VN.Communication.CAN.CAN_Interface;
