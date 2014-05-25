with Ada.Real_Time;
with Global_Settings;
with Buffers;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_LS_Probe;
with VN.Message.Probe_Request;
with VN.Message.Probe_Reply;

package body Lookup_Service is

   task body LS is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Assign_Address;
      use VN.Message.Request_LS_Probe;
      use VN.Message.Probe_Request;
      use VN.Message.Probe_Reply;

      i: Integer := 1;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      App_Info.Component_Type    := VN.Message.Other;
      App_Info.Logical_Address   := VN.LOGICAL_ADDRES_UNKNOWN;

      Global_Settings.Start_Time.Get(Next_Period);
      VN.Text_IO.Put_Line("LS   STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_LS.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            VN.Text_IO.Put_Line("LS   RECV: Empty.");

         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            VN.Text_IO.Put("LS   RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               To_Assign_Address(Basic_Msg, Assign_Address_Msg);
               App_Info.Logical_Address := Assign_Address_Msg.Assigned_Address;

            elsif Basic_Msg.Header.Opcode = VN.Message.OPCODE_REQUEST_LS_PROBE then
               To_Request_LS_Probe(Basic_Msg, Request_LS_Probe_Msg);

               VN_Logical_Address_Buffer.Insert(Request_LS_Probe_Msg.Component_Address, Probe_Request_Buffer);

            elsif Basic_Msg.Header.Opcode = VN.Message.OPCODE_PROBE_REPLY then
               -- TODO: Leave it at a print out (above) for now.
               null;
            end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
        if not VN_Logical_Address_Buffer.Empty(Probe_Request_Buffer) then
            VN_Logical_Address_Buffer.Remove(Temp_Logical_Address, Probe_Request_Buffer);

            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Probe_Request);
            Basic_Msg.Header.Source := App_Info.Logical_Address;
            Basic_Msg.Header.Destination := Temp_Logical_Address;

            -- TODO: Fix log output for Probe_Request
            VN.Text_IO.Put("LS    SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_LS.Send(Basic_Msg, Send_Status);

        end if;


         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 10;
      end loop;
      ----------------------------

      VN.Text_IO.Put_Line("LS   STAT: Stop. Logical Address: " &
                                 App_Info.Logical_Address'Img);

   end LS;

   LS1: LS(30, Global_Settings.Cycle_Time_Applications, 30, 3);

end Lookup_Service;
