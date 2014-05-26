/*******************************************************************************
 *
 * This code is based on code from:
 * SmartFusion2 MSSCAN example demonstrating the Data transmission and Reception
 * using MSSCAN (FullCAN) by Microsemi SoC Products Group.
 *
 * No rights reserved regarding this code file.
 */

 /*----------------------------------------------------------------------------
 * include files
 */
#include "mss_can.h"
//#include "drivers/mss_can/mss_can.h"
//#include <stdio.h>
//#include <stdlib.h>
//#include <stdint.h>

CAN_MSGOBJECT tx_msg;


typedef struct C_CAN_Msg_T {
   uint32_t ID;
   uint32_t data_length;
   //int8_t DATA[8];
   uint32_t DATAHIGH;
   uint32_t DATALOW;
} C_CAN_Message_Type;



int Init_CAN() {

    int32_t err;
    CAN_CONFIG_REG CANReg;
    CAN_FILTEROBJECT pFilter;
    int ret;

    CANReg.CFG_BITRATE      = 219;
    CANReg.CFG_TSEG1        = 12;
    CANReg.CFG_TSEG2        = 1;
    CANReg.AUTO_RESTART     = 0;
    CANReg.CFG_SJW          = 0;
    CANReg.SAMPLING_MODE    = 0;
    CANReg.EDGE_MODE        = 0;
    CANReg.ENDIAN           = 1; 

    err = MSS_CAN_init(
        &g_can0,
        //CAN_SPEED_32M_10K,
        CAN_SET_BITRATE(24) | CAN_SET_TSEG1(12) | CAN_SET_TSEG2(1), // | CAN_SET_TSEG1(12) | CAN_SET_TSEG2(1), // 100kbit/s ???
        (PCAN_CONFIG_REG)0, 
        6,
        6
    );

    if (err == CAN_OK) {
 //       printf("CAN-Controller initialized!\r\n");
        ret = CAN_OK;
    }
    else {
    //    printf("Failed initializing CAN-Controller! Error: %ld\r\n\n", err);
        ret = CAN_OK - 1; //ToDo
    }

    MSS_CAN_set_mode(&g_can0, CANOP_MODE_NORMAL);
    MSS_CAN_start(&g_can0);


    tx_msg.ID       = 0x120;
    tx_msg.DATALOW  = 0x00000000;
    tx_msg.DATAHIGH = 0x00000000;
    tx_msg.NA0      = 1;
    tx_msg.DLC      = 8;
    tx_msg.IDE      = 1;
    tx_msg.RTR      = 0;

   // printf("\r\n Initializing CAN-Controller... ");
 //   fflush(stdout);

    pFilter.ACR.L   = 0x00000000 ;
    pFilter.AMR.L   = 0xFFFFFFFF;
    pFilter.AMCR_D.MASK = 0xFFFF;
    pFilter.AMCR_D.CODE = 0x00;

    err = MSS_CAN_config_buffer(&g_can0, &pFilter);
  /*  if (err != CAN_OK) {
        printf("\n\r Message Buffer configuration Error\r\n");
    } */


    //MSS_CAN_set_int_ebl(&g_can0, CAN_INT_RX_MSG | CAN_INT_TX_MSG);
 //   MSS_CAN_set_int_ebl(&g_can0, CAN_INT_RX_MSG | CAN_INT_TX_MSG | CAN_INT_GLOBAL | CAN_INT_BIT_ERR | CAN_INT_ACK_ERR | CAN_INT_CRC_ERR
  //     | CAN_INT_RX_MSG_LOST);

    return ret;
}

int test() {return 42;}

int Send_CAN_Message(C_CAN_Message_Type *msg) {

   // printf("main.c: Send_CAN_Message run!\r\n");
  //  int i;

    tx_msg.ID  = msg->ID;
    tx_msg.DLC = msg->data_length;

   /* for(i=0; i < msg->data_length; i++) {
          tx_msg.DATA[i] = msg->DATA[i];
    }*/
    tx_msg.DATAHIGH =  msg->DATAHIGH;
    tx_msg.DATALOW  =  msg->DATALOW;

    while( MSS_CAN_send_message_ready(&g_can0) != CAN_OK); //wait until ready

    //return MSS_CAN_send_message_n(&g_can0, 6, &tx_msg);
    return MSS_CAN_send_message(&g_can0, &tx_msg);
}

int Receive_CAN_Message(C_CAN_Message_Type *msg) { //returns 1 if message was received, 0 otherwise

    CAN_MSGOBJECT rx_msg;
   // int i;
   // printf("main.c: Receive_CAN_Message run!\r\n");


    if(CAN_VALID_MSG == MSS_CAN_get_message(&g_can0, &rx_msg)) {

        msg->ID = rx_msg.ID;  //??
        msg->data_length = rx_msg.DLC;//??

     /*   for(i=0; i < msg->data_length; i++) {
            msg->DATA[i] = 0xFF & rx_msg.DATA[i];
        }*/
    msg->DATAHIGH = rx_msg.DATAHIGH;
    msg->DATALOW = rx_msg.DATALOW;

        return 1;
    } else {
        return 0;
    }
}

void Test_Send() {
    ;
 /*   CAN_MSGOBJECT pMsg;
    pMsg.ID=0x20;
    pMsg.DATALOW = 0x11111111;
    pMsg.DATAHIGH = 0x22222222;
    pMsg.NA0 = 1;
    pMsg.DLC = 4;
    pMsg.IDE = 1;
    pMsg.RTR = 0;
    pMsg.NA1 = 0; //???

    MSS_CAN_send_message_n(&g_can0, 0, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 1, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 2, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 3, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 4, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 5, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 6, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 7, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 8, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 9, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 10, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 11, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 12, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 13, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 14, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 15, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 16, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 17, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 18, &pMsg);
    MSS_CAN_send_message_n(&g_can0, 19, &pMsg); */
}


// Will return 1 on success
int Set_Filter(uint8_t mailbox_number, uint32_t mask, uint32_t template) {
    uint16_t temp = 0xFFFF;
    if (MSS_CAN_set_mask_n(&g_can0, mailbox_number, mask, template, temp, temp) == CAN_OK)
        return 1;
    else
	return 0;
}


