
with VN.Communication.CAN;
with VN.Communication.CAN.CAN_Interface;

package Test_No_Tasks is

   procedure Update;

private

   myUCID :   aliased VN.Communication.CAN.UCID := 5;
   theCUUID : aliased VN.VN_CUUID := (others => 1);
   unitType : VN.Communication.CAN.CAN_Interface.Unit_Type := VN.Communication.CAN.CAN_Interface.SM_CAN;

   inter : VN.Communication.CAN.CAN_Interface.CAN_Interface_Type(myUCID'Access,
                                                                 theCUUID'Access,
                                                                 unitType);

   bufIn, bufOut : VN.Communication.CAN.CAN_Message_Buffers.Buffer(2);
end Test_No_Tasks;
