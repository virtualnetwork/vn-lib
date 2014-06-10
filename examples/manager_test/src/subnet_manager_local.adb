with Ada.Real_Time;
with Buffers;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;
with VN.Message.Request_Address_Block;
with VN.Message.Distribute_Route;
with Interfaces;

package body Subnet_Manager_Local is

   task body SM_L is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Assign_Address;
      use VN.Message.Assign_Address_Block;
      use VN.Message.Request_Address_Block;
      use VN.Message.Request_LS_Probe;
      use VN.Message.Distribute_Route;
      use Interfaces;
      Counter_For_Testing: Integer := 1;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Microseconds(Cycle_Time);

   begin
      SM_L_Info.Component_Type := VN.Message.SM_L;
      SM_L_Info.Logical_Address := VN.LOGICAL_ADDRES_UNKNOWN;

      Global_Settings.Start_Time.Get(Next_Period);
      VN.Text_IO.Put_Line("SM-L STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_SM_L.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            --              VN.Text_IO.Put_Line("SM-L RECV: Empty.");
            null;
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
           Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            -- Print debug text.
            VN.Text_IO.Put("SM-L RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            -- Process incoming message.
            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               To_Local_Hello(Basic_Msg, Local_Hello_Msg);

               if (Local_Hello_Msg.Component_Type = VN.Message.Other or
                     Local_Hello_Msg.Component_Type = VN.Message.LS) then

                  Unsigned_8_Buffer.Insert(Local_Hello_Msg.CUUID(1), Assign_Address_Buffer);

                  if Local_Hello_Msg.Component_Type = VN.Message.LS then
                     LS_CUUID := Local_Hello_Msg.CUUID(1);
                  end if;

               elsif (Local_Hello_Msg.Component_Type = VN.Message.SM_L or
                        Local_Hello_Msg.Component_Type = VN.Message.SM_x) then
                  Unsigned_8_Buffer.Insert(Local_Hello_Msg.CUUID(1), Request_Address_Block_Buffer);

               elsif (Local_Hello_Msg.Component_Type = VN.Message.CAS) then
                  CAS_CUUID := Local_Hello_Msg.CUUID(1);
               end if;

            elsif Basic_Msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK then
               To_Assign_Address_Block(Basic_Msg, Assign_Address_Block_Msg);

               if Assign_Address_Block_Msg.Response_Type = VN.Message.Valid and
                 Assign_Address_Block_Msg.CUUID = Global_Settings.CUUID_SM then

                  Received_Address_Block := Assign_Address_Block_Msg.Assigned_Base_Address;
                  SM_L_Info.Logical_Address := Received_Address_Block;
                  -- Assigned_Address := Received_Address_Block; -- This is correct
                  Assigned_Address := Received_Address_Block - 1; -- This is for debugging

                  if Assign_Address_Block_Msg.Header.Source = VN.CAS_LOGICAL_ADDRESS then
                     CAS_Logical_Address := Assign_Address_Block_Msg.Header.Source;
                  end if;

               elsif Assign_Address_Block_Msg.Response_Type = VN.Message.Valid and
                 Assign_Address_Block_Msg.CUUID /= Global_Settings.CUUID_SM then

                  -- TODO: Remove this send so it's not coupled with
                  -- receive.
                  To_Basic(Assign_Address_Block_Msg, Basic_Msg);
                  Basic_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
                  Basic_Msg.Header.Source := SM_L_Info.Logical_Address;

                  VN.Text_IO.Put("SM-L SEND: ");
                  Global_Settings.Logger.Log(Basic_Msg);
                  Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

                  Unsigned_8_Buffer.Insert(Assign_Address_Block_Msg.CUUID(1), Distribute_Route_Buffer);

                  -- TODO: Temporary fix for only one SM-x
                  -- Need to map CUUIDs to assigned addresses to be
                  -- able to send distribute route messages.
                  -- SM_x_Logical_Address := Assign_Address_Block_Msg.Assigned_Base_Address;
                  VN_Logical_Address_Buffer.Insert(Assign_Address_Block_Msg.Assigned_Base_Address, Distribute_Route_Buffer_Addresses);
               end if;
            elsif Basic_Msg.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
               To_Distribute_Route(Basic_Msg, Distribute_Route_Msg);

               if Distribute_Route_Msg.Component_Type = VN.Message.CAS then
                  CAS_Logical_Address := Distribute_Route_Msg.Component_Address;
                  CAS_CUUID := Distribute_Route_Msg.CUUID(1);

               elsif Distribute_Route_Msg.Component_Type = VN.Message.LS then
                  LS_Logical_Address := Distribute_Route_Msg.Component_Address;
                  LS_CUUID := Distribute_Route_Msg.CUUID(1);
               end if;

            end if;
         end if;

         ----------------------------
         -- Send loop
         ----------------------------
         -- Assign Address
         if not Unsigned_8_Buffer.Empty(Request_Address_Block_Buffer) and
           Has_Received_Address_Block and
           CAS_Logical_Address /= VN.LOGICAL_ADDRES_UNKNOWN then

            Unsigned_8_Buffer.Remove(Temp_Uint8, Request_Address_Block_Buffer);

            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
            Basic_Msg.Header.Source := SM_L_Info.Logical_Address;
            Basic_Msg.Header.Destination := CAS_Logical_Address;

            To_Request_Address_Block(Basic_Msg, Request_Address_Block_Msg);
            Request_Address_Block_Msg.CUUID := (others => Temp_Uint8);
            To_Basic(Request_Address_Block_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

            -- Distribute route, assumes that the LS and CAS are co-located.
         elsif not Unsigned_8_Buffer.Empty(Distribute_Route_Buffer) and
           CAS_Logical_Address /= VN.LOGICAL_ADDRES_UNKNOWN and
           LS_Logical_Address /= VN.LOGICAL_ADDRES_UNKNOWN then

            Unsigned_8_Buffer.Remove(Temp_Uint8, Distribute_Route_Buffer);
            VN_Logical_Address_Buffer.Remove(Temp_Logical_Address, Distribute_Route_Buffer_Addresses);

            -- CAS
            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Distribute_Route);
            Basic_Msg.Header.Source := SM_L_Info.Logical_Address;
            Basic_Msg.Header.Destination := Temp_Logical_Address;
            To_Distribute_Route(Basic_Msg, Distribute_Route_Msg);
            Distribute_Route_Msg.CUUID := (others => CAS_CUUID);
            Distribute_Route_Msg.Component_Address := CAS_Logical_Address;
            Distribute_Route_Msg.Component_Type:= VN.Message.CAS;

            To_Basic(Distribute_Route_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

            -- LS
            Distribute_Route_Msg.CUUID := (others => LS_CUUID);
            Distribute_Route_Msg.Component_Address := LS_Logical_Address;
            Distribute_Route_Msg.Component_Type:= VN.Message.LS;

            To_Basic(Distribute_Route_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

         elsif not Unsigned_8_Buffer.Empty(Assign_Address_Buffer) and
           Has_Received_Address_Block then

            Unsigned_8_Buffer.Remove(Temp_Uint8, Assign_Address_Buffer);
            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
            Basic_Msg.Header.Source := SM_L_Info.Logical_Address;
            Basic_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
            To_Assign_Address(Basic_Msg, Assign_Address_Msg);
            Assign_Address_Msg.CUUID := (others => Temp_Uint8);

            --ToDo: Could have been changed:
            Assign_Address_Msg.Assigned_Address := Get_Address_To_Assign(Temp_Uint8);

            To_Basic(Assign_Address_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

            -- TODO: Fix proper lookup table to keep track of LS, CAS and
            -- other SM-x
            if Temp_Uint8 = LS_CUUID then
               LS_Logical_Address := Assign_Address_Msg.Assigned_Address;
            else
               VN_Logical_Address_Buffer.Insert(Assign_Address_Msg.Assigned_Address, Request_LS_Probe_Buffer);
            end if;

            if Sent_CAS_Request_LS_Probe = false and
              CAS_Logical_Address /= VN.LOGICAL_ADDRES_UNKNOWN then

               VN_Logical_Address_Buffer.Insert(CAS_Logical_Address, Request_LS_Probe_Buffer);
               Sent_CAS_Request_LS_Probe := true;
            end if;

         elsif not VN_Logical_Address_Buffer.Empty(Request_LS_Probe_Buffer) and
           LS_Logical_Address /= VN.LOGICAL_ADDRES_UNKNOWN then

            VN_Logical_Address_Buffer.Remove(Temp_Logical_Address, Request_LS_Probe_Buffer);

            -- Send Distribute route message to LS so that LS can route its LS Probe Request message to
            -- the application:
            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Distribute_Route);
            Basic_Msg.Header.Source := SM_L_Info.Logical_Address;
            Basic_Msg.Header.Destination := LS_Logical_Address;
            To_Distribute_Route(Basic_Msg, Distribute_Route_Msg);

            Distribute_Route_Msg.CUUID := (others => 42); -- ToDo: This is not correct, however this is probably ok anyways
            Distribute_Route_Msg.Component_Address := Temp_Logical_Address;
            Distribute_Route_Msg.Component_Type:= VN.Message.Other;

            To_Basic(Distribute_Route_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);
            -- **************

            Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Request_LS_Probe);
            Basic_Msg.Header.Source := SM_L_Info.Logical_Address;
            Basic_Msg.Header.Destination := LS_Logical_Address;
            To_Request_LS_Probe(Basic_Msg, Request_LS_Probe_Msg);
            Request_LS_Probe_Msg.Component_Address := Temp_Logical_Address;

            To_Basic(Request_LS_Probe_Msg, Basic_Msg);

            VN.Text_IO.Put("SM-L SEND: ");
            Global_Settings.Logger.Log(Basic_Msg);
            Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

         end if;

         Next_Period := Next_Period + Period;
         --           Counter_For_Testing := Counter_For_Testing + 1;
         --           exit when Counter_For_Testing = 60;
      end loop;
      ----------------------------

      VN.Text_IO.Put_Line("SM-L STAT: Stop. Logical Address: " &
                            SM_L_Info.Logical_Address'Img);

   end SM_L;

   -- Start one instance of the SM-L
   SM_L1: SM_L(20, Global_Settings.Cycle_Time_SM_L, 80, 3);

   ----------------------------
   -- Helper functions below
   ----------------------------
   function Get_Address_To_Assign(CUUID_Uint8: in Interfaces.Unsigned_8)
                                  return VN.VN_Logical_Address is
      use VN;
   begin
      -- TODO: This function doesn't take into account the CUUID, which it
      -- should.
      Assigned_Address := Assigned_Address + 1;
      return Assigned_Address + 1;
   end Get_Address_To_Assign;

   function Has_Received_Address_Block return Boolean is
      use VN;
   begin
      if Received_Address_Block /= VN.LOGICAL_ADDRES_UNKNOWN then
         return true;
      else
         return false;
      end if;
   end Has_Received_Address_Block;

end Subnet_Manager_Local;
