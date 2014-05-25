with Ada.Real_Time;
with Ada.Text_IO;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Request_Address_Block;
with VN.Message.Assign_Address_Block;

package body Central_Addressing_Service is

   task body CAS is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Request_Address_Block;
      use VN.Message.Assign_Address_Block;

      i: Integer := 1;


      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      CAS_Info.Logical_Address := 1;
      CAS_Info.Component_Type := VN.Message.Other;

      Global_Settings.Start_Time.Get(Next_Period);
      Ada.Text_IO.Put_Line("CAS  STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_CAS.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("CAS  RECV: Empty.");
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            Ada.Text_IO.Put("CAS  RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_REQUEST_ADDR_BLOCK then
               To_Request_Address_Block(Basic_Msg, Request_Address_Block_Msg);
               Unsigned_8_Buffer.Insert(Request_Address_Block_Msg.CUUID(1), Assign_Address_Block_Buffer);
            end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
        if not Unsigned_8_Buffer.Empty(Assign_Address_Block_Buffer) then
           Unsigned_8_Buffer.Remove(Temp_Uint8, Assign_Address_Block_Buffer);

           Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address_Block);
           Basic_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
           Basic_Msg.Header.Source := 0;

           To_Assign_Address_Block(Basic_Msg, Assign_Address_Block_Msg);
           Assign_Address_Block_Msg.CUUID := (others => Temp_Uint8);
           Assign_Address_Block_Msg.Assigned_Base_Address := Assigned_Address_Block;
           -- Assign_Address_Block_Msg.Response_Type := Assigned_Address_Block;
           To_Basic(Assign_Address_Block_Msg, Basic_Msg);

           Assigned_Address_Block := Assigned_Address_Block + 65535;

           Ada.Text_IO.Put("CAS  SEND: ");
           Global_Settings.Logger.Log(Basic_Msg);
           Global_Settings.Com_CAS.Send(Basic_Msg, Send_Status);

        end if;


         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 6;
      end loop;
      ----------------------------

      Ada.Text_IO.Put_Line("CAS  STAT: Stop. Logical Address: " &
                                 CAS_Info.Logical_Address'Img);

   end CAS;

   CAS1: CAS(20, 5000000, 10, 3);

end Central_Addressing_Service;
