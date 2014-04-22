/*******************************************************************************
 * (c) Copyright 2012-2013 Microsemi SoC Products Group.  All rights reserved
 *
 * SmartFusion2 MSSCAN example demonstrating the Data transmission and Reception
 * using MSSCAN (FullCAN).
 * For Transmission: Get data from Hyperterminal using MSSUART0 --> Form as CAN
 * packets --> Send to CAN Analyzer.
 * For Reception: Send the CAN Message from CAN Analyzer --> read the Message-->
 * Send to hyperterminal using MSSUART0.
 * Board settings and test procedure clearly mentioned in ReadMe.txt file.
 *
 * SVN $Revision: 5439 $
 * SVN $Date: 2013-03-29 08:58:40 +0530 (Fri, 29 Mar 2013) $
 */

 /*----------------------------------------------------------------------------
 * include files
 */
//#include "drivers/mss_uart/mss_uart.h"
#include "drivers/mss_can/mss_can.h"
#include<stdio.h>
#include<stdlib.h>


/*------------------------------------------------------------------------------
  Static Variables.
 */
CAN_FILTEROBJECT pFilter;
CAN_MSGOBJECT pMsg;
CAN_MSGOBJECT rx_buf;
CAN_RXMSGOBJECT rx_msg;

/*------------------------------------------------------------------------------
  Macros.
 */
#define   SYSTEM_CLOCK        32000000
#define   ENTER               0x0D


typedef struct C_CAN_Msg_T {
   uint32_t ID;
   uint32_t data_length;
   int8_t DATA[8];

} C_CAN_Message_Type;


int test() {return 42;}

int Send_CAN_Message(C_CAN_Message_Type *msg) {
    int i;

    pMsg.ID  = msg->ID;
    pMsg.DLC = msg->data_length;

    for(i=0; i < msg->data_length-1; i++) {
          pMsg.DATA[i] = msg->DATA[i];
    }

    pMsg.NA0 = 1; // ToDo. ???????????? "[0..15] Message Valid Bit, 0 == Not valid."
    pMsg.IDE = 1; //use extended message IDs
    pMsg.RTR = 1; //regular message (not remote frame)
    pMsg.NA1 = 0; //padding?

    return MSS_CAN_send_message_n(&g_can0, 0, &pMsg);
}

int Receive_CAN_Message(C_CAN_Message_Type *msg) { //returns 1 if message was received, 0 otherwise

    if(CAN_VALID_MSG == MSS_CAN_get_message_n(&g_can0, 0, &rx_buf)) {
        int i;

        msg->ID = pMsg.ID;
        msg->data_length = pMsg.DLC;

        for(i=0; i < msg->data_length-1; i++) {
            msg->DATA[i] = pMsg.DATA[i];
        }

        return 1;
    } else {
        return 0;
    }
}

void Init_CAN() {

    MSS_CAN_init(&g_can0,
                 CAN_SPEED_32M_500K,
                 (PCAN_CONFIG_REG)0,
                 6,
                 6);

    MSS_CAN_set_mode(&g_can0, CANOP_MODE_NORMAL);

    MSS_CAN_start(&g_can0);

    /* Configure for receive */
    /* Initialize the rx mailbox */
    rx_msg.ID = 0x200;
    rx_msg.DATAHIGH = 0u;
    rx_msg.DATALOW = 0u;
    rx_msg.AMR.L = 0x00000000; //0xFFFFFFFF;
    rx_msg.ACR.L = 0x00000000;
    rx_msg.AMR_D = 0xFFFFFFFF;
    rx_msg.ACR_D = 0x00000000;
    rx_msg.RXB.DLC = 8u;
    rx_msg.RXB.IDE = 0u;

    MSS_CAN_config_buffer_n(&g_can0, 0, &rx_msg);
}

