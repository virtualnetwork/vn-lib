with Ada.Real_Time;
with Global_Settings;
with Buffers;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Probe_Request;
with VN.Message.Probe_Reply;

package body App2 is

   task body VN_Application is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Assign_Address;
      use VN.Message.Probe_Request;
      use VN.Message.Probe_Reply;

      i: Integer := 1;

      App_Info: VN.Application_Information.VN_Application_Information;

      package VN_Logical_Address_Buffer is
         new Buffers(VN.VN_Logical_Address);

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Assign_Address_Msg: VN.Message.Assign_Address.VN_Message_Assign_Address;
      Probe_Request_Msg: VN.Message.Probe_Request.VN_Message_Probe_Request;
      Probe_Reply_Msg: VN.Message.Probe_Reply.VN_Message_Probe_Reply;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Temp_Logical_Address: VN.VN_Logical_Address := VN.LOGICAL_ADDRES_UNKNOWN;

      -- TODO: Change this buffer to some kind of data store.
      Probe_Reply_Buffer: VN_Logical_Address_Buffer.Buffer(10);

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      App_Info.Component_Type := VN.Message.Other;
      App_Info.Logical_Address := VN.LOGICAL_ADDRES_UNKNOWN;

      Global_Settings.Start_Time.Get(Next_Period);
      VN.Text_IO.Put_Line("APP2 STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_App2.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            --              VN.Text_IO.Put_Line("APP2 RECV: Empty.");
            null;
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            VN.Text_IO.Put("APP2 RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               To_Assign_Address(Basic_Msg, Assign_Address_Msg);
               App_Info.Logical_Address := Assign_Address_Msg.Assigned_Address;

            elsif Basic_Msg.Header.Opcode = VN.Message.OPCODE_PROBE_REQUEST then
               To_Probe_Request(Basic_Msg, Probe_Request_Msg);

               VN_Logical_Address_Buffer.Insert(Probe_Request_Msg.Header.Source, Probe_Reply_Buffer);

            end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
        if not VN_Logical_Address_Buffer.Empty(Probe_Reply_Buffer) then
            VN_Logical_Address_Buffer.Remove(Temp_Logical_Address, Probe_Reply_Buffer);

            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Probe_Reply);
            Basic_Msg.Header.Source := App_Info.Logical_Address;
            Basic_Msg.Header.Destination := Temp_Logical_Address;

            VN.Text_IO.Put("APP2 SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_App2.Send(Basic_Msg, Send_Status);

        end if;


         Next_Period := Next_Period + Period;
--           i := i + 1;
--           exit when i = 15;
      end loop;
      ----------------------------

      VN.Text_IO.Put_Line("APP2 STAT: Stop. Logical Address: " &
                                 App_Info.Logical_Address'Img);

   end VN_Application;

   App: VN_Application(30, Global_Settings.Cycle_Time_Applications, 30, 3);

end App2;
