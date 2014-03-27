
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
      procedure Update(msgsBuffer : in out VN.Communication.CAN.Logic.SM.CAN_Message_Buffers.Buffer;
                       ret : out VN.Communication.CAN.Logic.SM.CAN_Message_Buffers.Buffer);
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
