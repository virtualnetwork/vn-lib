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

with VN;
with VN.Communication.CAN;
with VN.Communication.CAN.Logic.SM;

package VN.Communication.CAN.CAN_Interface is

 type Unit_Type is (SM_CAN, Node);

   type Private_Data(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID; unitType : Unit_Type) is limited private;

   protected type CAN_Interface_Type(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID; unitType : Unit_Type) is
        new VN.Communication.Com with

      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status);

      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status);


      ----------- FUNCTIONS FOR CAN_task ---------------------
      procedure Update(msgsBuffer : in out VN.Communication.CAN.CAN_Message_Buffers.Buffer;
                       ret : out VN.Communication.CAN.CAN_Message_Buffers.Buffer);
      --------------------------------------------------------

   private

      procedure Init;

      isInitialized : Boolean := false;
      data : Private_Data(theUCID, theCUUID, unitType);
   end CAN_Interface_Type;

   type CAN_Interface_Access is access all CAN_Interface_Type;

private

   type Private_Data(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID; unitType : Unit_Type) is  --will be a variant record soon enough
      record
         case unitType is
            when SM_CAN =>
               SMDuty : VN.Communication.CAN.Logic.SM.SM_Duty(theUCID, theCUUID);
            when Node =>
               nodeDuty : VN.Communication.CAN.Logic.SM.SM_Duty(theUCID, theCUUID); -- VN.Communication.CAN.Logic.Node.Node_Duty(theUCID, theCUUID);
         end case;
      end record;

end VN.Communication.CAN.CAN_Interface;
