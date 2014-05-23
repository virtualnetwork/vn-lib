VN CAN Subnet Protocol
==========================

## High level

### Identifiers and addresses
**There exists two forms of identifiers in this protocol:** 
  * **The Component Universally Unique Identifier (CUUID)**  is a high level identifier that is to be unique worldwide for the particular component. The length of the CUUID is 128 bits (16 bytes). Any component on a VN network needs a CUUID.
  * **The Universally Unique CAN Identifier (UCID)** is a low level identifier that is to be unique on the VN-CAN network for the particular component. The length of the UCID is 28 bits. Any component on the VN-CAN network will need a UCID.

**There exists two forms of addresses in this protocol:**
  * **The logical addresses.**  Defined in the VN protocol.
  * **CAN addresses.** The CAN address. Any component on the VN-CAN network will be assigned a CAN address. The CAN addresses are only used on the CAN bus on the low level part of the VN-CAN protocol. The CAN addresses are managed by the SM-CAN master.

### Stored data
**The following data is stored in this protocol:**
  * *Send buffer*, for VN-messages.
  * *Receive buffer*, for VN-messages.
  * A *primary routing* table, connecting a given logical address to a low level
    address.
  * A *CUUID routing* table, connecting a given CUUID to a subnet address. This
    table is used in some special cases of VN messages (e.g. **AssignAddr**)
    that cannot rely on logical addresses for routing.


### Interface
**There exists two public functions stored in this protocol:**
  * *Send*. Will take a VN message as argument, an out-variable will tell the
    outcome of the call, i.e. “Transmission OK” or “Buffer full”. The message
    is written to the send buffer if it is not full.
  * *Receive*. Will give a VN message as an out-variable, another out-variable
    will tell the outcome of the call, i.e. “No message received”, “Message
    received, buffer empty” and “Message received, more available”.

### Unit discovery process
Section *Unit discovery process* in *VN generic subnet protocol* shall be followed. Section *Low level* will give further specifications for how this is to be done.

### Route discovery process
#### For routes to overlying units
See Section *For routes to overlying units* in *VN generic subnet protocol*.

#### For routes to underlying units
See Section *For routes to underlying units* in *VN generic subnet protocol*.

#### Transmission of VN messages
Section *Transmission of VN messages* in *VN generic subnet protocol* applies. Transmission of VN messages is further specified in Section *Low level*.

#### Reception of VN messages
When a VN message is received on the subnet it shall be pushed to the receive buffer. 
Actions according to sections *Route discovery process* and *Unit discovery process* shall also be performed.

#### Logical addresses
Assignment of logical addresses is handled by higher level protocols and is not described in this protocol.


## Low level

#### Division of message IDs
Bits are numbered 28-0 where bit 28 is most significant and is sent first.
There are two groups of message types: *RequestCANAddress* message (one message type) and *Normal* CAN Messages. These are defined by the first bit of the message ID.
The message ID is divided according to:

1. **msgID[28] = 1: RequestCANAddress message**<br/>
	msgID[27 .. 0] = *Universally Unique CAN Identifier* (*UCID*) for the node
2. **msgID[28] = 0: Normal CAN messages** <br/>
	msgID[27 .. 22] = Message priority <br/>
	msgID[21 .. 15] = Message type <br/>
	msgID[14 .. 7] = Receiver address <br/>
	msgID[6 .. 0] = Sender address 

*Please note:* Even though CAN addresses are 8 bit, addresses over 127 will never be used as a sender address since the maximum number of allowed CAN nodes on a CAN network is 128 one only needs addresses 0 through 127. This means that only 7 bits are needed for the Sender address.

#### Message priority
To be determined, set by higher protocols?

#### Message type
The types of messages present are listed below. Please note that in the case of two messages having equal priory fields the message type field will determine priority. A lower Message type number will give a higher priority.


| **Message type number** | **Meaning** | **Comment**  |
| ----------------------- | ----------- | ------------ |
| Not in this list | **RequestCANAddress** | Sent at regular intervals by any unit on the CAN network that does not have an CAN address yet. Contains information telling whether its a normal node or an SM-CAN. Nodes shall wait to send this message until they have received a **Normal CAN message** (*This shows that a SM-CAN master has been assigned.*) such as the **CANMasterAssigned** message, since nobody will care about the **RequestCANAddress** except the SM-CAN master. |
| 0 | **AssignCANAddress** | Assignment of CAN-address from SM-CAN master. Is sent to address 255 (Broadcast address). Contains the  Universally Unique CAN Identifier (UCID) for the node and its assigned CAN address |
| 1 | **CANMasterAssigned** | Is sent as a part of the CAN master negotiation protocol. Shall be ignored by ordinary nodes. |
| 3 | **Ping** | Sent by anyone who wants. Can be sent to broadcast address or to a specific address. Is used to test if the receiver(s) is alive. Any unit that has a CAN message shall answer to **Ping** messages. |
| 4 | **Pong** | Answer to Ping message, is sent back to the sender of the Ping message. |
| 6 | **StartTransmission** | Tells the receiver that the sender intends to transfer a VN message. Contains the number of **Transmission** messages needed to send the VN message. |
| 7 | **FlowControl** | Is sent as response of StartTransmission. Contains BlockSize. Also sent to control the flow of **Transmission** messages. |
| 8 | **Transmission** | Contains the VN message. All **Transmission** messages for a given VN message shall be set to the same priority as the **StartTransmission** message of the VN message. |
| 9 | **DiscoveryRequest** | Is sent by any unit. Any SM-CAN that receives this message shall respond with a ComponentType message. Is sent to CAN address 254 by any unit when it receives a CAN address as a part of the discovery process, or to a single unit's CAN address. |
| 10 | **ComponentType** | Informs the receiver about the sender's type in response to DiscoveryRequest. Sent to CAN address 255 if the DiscoveryRequest was sent to address 254 or to the CAN address of the sender of the DiscoveryRequest if it was sent to one's own CAN address. |



#### Message contents
This section declares the content of each message.

**RequestCANAddress**

Can contain zero or one byte. An ordinary node either sends zero bytes or sets the first byte equal to 5. SM-CANs set this byte equal to 3. 


**AssignCANAddress**

Bytes 0 – 3:  UCID of the receiving node.
Byte 4: CAN address assigned to the node.

**CANMasterAssigned**

No payload data.

**Ping**

Zero bytes. Whenever a node or SM-CAN has been given a CAN address it shall listen for this message and return a *Pong* message in return.

**Pong**
No payload data.

**StartTransmission**

Two bytes, an Unsigned_16 (most significant byte first) containing the number of **Transmission** messages that will be sent. 
<!---
If no FlowControl message is received, the StartTransmission message is resent a few times. If no response is gotten within a reasonable number of tries, the receiver is presumed as unavailable. Some form of error message should be sent to the higher level protocol.
-->
**FlowControl**

Zero or two bytes, an Unsigned_16 (most significant byte first) containing the BlockSize. 
<!---
This message is sent as a response to a StartTransmission message. It is only to be sent by the the once the receiver is ready to receive the message. Hence, the receiver can simply refrain from sending this message if it is too busy to receive a transmission of a VN message.
If the receiver does not care about flow control, it will send a FlowControl message with zero bytes.
BlockSize is the number of Transmission the sender shall send before a consecutive FlowControl message is to be sent by the receiver. Consecutive FlowControl messages shall not contain any data, if they do anyway, the data shall not be read.
-->
**Transmission**

One to eight bytes. Contains a part of the VN message itself.

**DiscoveryRequest**

No payload data.

**ComponentType**

One byte. An ordinary node sets the first byte equal to 5. SM-CANs set this byte equal to 3. 


#### CAN Addresses

| **Address** | **Meaning** | **Comment**  |
| ----------------------- | ----------- | ------------ |
| 255 | Broadcast address | All units on the CAN network should listen to this address at all times. |
| 254 | Selective broadcast address | Broadcast address to all SM-CANs, but not nodes. |
| 128..253 | Currently not used | These addresses are not used at the time of writing. |
| 1..127 | Node CAN addresses | These addresses are given to nodes and SM-CAN slaves. |
| 0 | SM-CAN master address | CAN address of SM-CAN master. |

#### SM-CAN master negotiation process
 1.  Each SM-CAN shall send a RequestCANAddress message when it starts.
 2.  The SM-CAN shall delay for XXXXX ms. During this time it shall listen to **RequestCANAddress** and any **Normal CAN message**. 
 2.1.  If the SM-CAN receives a **Normal CAN message**, or a **RequestCANAddress** message from an SM-CAN with a lower UCID, it shall become an SM-CAN slave.  <br/>
 2.2.  If the  SM-CAN receives a **RequestCANAddress** message from an SM-CAN with a higher UCID it shall respond with a **RequestCANAddress** message of its own. 
 3.  If the delay has passed without the SM-CAN becoming a slave it shall become an SM-CAN master.
 4.  If the SM-CAN master receives a **RequestCANAddress** message from an SM-CAN it shall respond with a *Normal CAN message*, such as the **CANMasterAssigned** message. *This responsibility of the SM-CAN master remains indefinitely.*
 5.  Once assigned as a slave, an SM-CAN has no further responsibilities with regards to the SM-CAN master negotiation process.
 6.  Once a SM-CAN has been assigned an SM-CAN master, it shall be ready to receive VN messages. <br/>
This means that it shall listen for StartTransmission and Transmission messages and be ready to answer with FlowControl messages.

### Discovery process
*This process takes place after the SM-CAN master negotiation process.*

#### For nodes and SM-CAN slaves
*Please note that both nodes and SM-CAN slaves are referred to as “nodes” in this section.*

1. All SM-CANs shall participate in the Discovery process on their Processing nodes according to VN. See VN-CAN high level protocol for further details.
2. All nodes shall listen to any **Normal CAN message** (such as the **CANMasterAssigned** message). Once the node receives a 
Normal CAN message** this indicates that an SM-CAN master has been assigned.
3. All nodes shall then start to send **RequestCANAddress** messages at regular intervals.
4. When node receives a **AssignCANAddress** message intended for it (containing the node's *Universally Unique CAN Identifier*, *UCID*) it will stop sending **RequestCANAddress** messages and start to use the assigned CAN address.
5. Once the node has an CAN address it shall be ready to receive VN messages, meaning it shall listen for **StartTransmission** and **Transmission** messages and be ready to answer with **FlowControl** messages.


##### For the SM-CAN master
1. All SM-CANs shall participate in the Discovery process on their Processing nodes according to VN. See VN-CAN high level protocol for further details.
2. The SM-CAN master shall listen to **RequestCANAddress** messages. 
It shall use the **AssignCANAddress** message to assign CAN addresses to those it receives **RequestCANAddress** messages from. SM-CAN slaves shall be assigned CAN addresses before nodes.
This responsibility of the SM-CAN master remains indefinitely.
3. ~~When the SM-CAN master receives the FirstCUUIDHalf and SecondCUUIDHalf messages from a node it shall register the node's CUUID.~~
4. Whenever the SM-CAN master starts to assign CAN addresses, it shall be ready to receive VN messages. <br/>
This means that it shall listen for **StartTransmission** and **Transmission** messages and be ready to answer with **FlowControl** messages.
5. ~~Point 4 means that the SM-CAN master might start to receive VN Request Address Block Messages from other SM-CANs. When this happens these requests shall take priority over assigning CAN addresses to nodes.~~


##### For all units (nodes or SM-CANs)
After an unit has received a CAN address it shall send a **DiscoveryRequest** message to CAN address 254, thus causing all SM-CANs to respond with a **ComponentType** message. This way the unit learns the CAN addresses of all SM-CANs present on the CAN network. <br/>
The unit shall then send a **LocalHello** message to each SM-CAN it has discovered. The **LocalHello** message will contain the unit's CUUID and component type. The sender and receiver addresses of the message are set to 2. If no **LocalAck** message is received within XXXXX milliseconds the **LocalHello** message shall be resent.  <br/>
When a **LocalHello** message is received over the subnet the following shall be done:

1. Actions according to Route discovery process shall be taken.
2. Respond with a **LocalAck** message.
3. The **LocalHello** message shall be passed on to the overlying protocol.

The **LocalHello** message is a mid level message that is used to inform the recipient about the sender's presence. The **LocalHello** message is described further in higher level protocols.

##### For all SM-CANs
All SM-CANs shall respond to **DiscoveryRequest** messages with a **ComponentType** message.

#### Transmission of VN messages
The following section describes how the transmission of a VN message shall be done. It applies to any unit on the CAN network, the SM-CANs (master or slaves) and nodes. This section assumes that the receiver's CAN address is known. <br/>
Transmission of a VN message will be done as follows:
 
1. The unit will send an **StartTransmission** message containing the number of **Transmission** messages needed to send the VN message.
If there is no **FlowControl** message in response the sending unit shall retry a few times before giving up. Perferably, the failure to send the message should be reported to the overlying protocol.
2. The receiver will answer with a **FlowControl** message containing its preferred block size. If the receiver is too busy at the moment, it can deny the transmission by simply not replying.
3. Once the sending node receives the **FlowControl** message it will send **Transmission** messages according to:
A) If the FlowControl message did not indicate the use of flow control, or if the Block Size is smaller than the number of **Transmission** messages needed to send the VN message, all **Transmission** messages will be sent.
B) Otherwise, the sender will send as many **Transmission** messages as specified in the **FlowControl** message.
4. Once the sender receives another FlowControl message it will send another block size of Transmission messages, or the remaining **Transmission** messages if the number of remaining **Transmission** messages is smaller than the block size.
This will continue until all **Transmission** messages have been sent.
5. Once a sender has sent a **StartTransmission** message to a receiver regarding a particular VN message it shall not send another  **StartTransmission** message regarding another VN message to that receiver before having transferred all of the previous VN message (all **Transmission** messages). <br/>
Hence, the transmission a VN message cannot be interrupted by the transmission of another VN message that is sent from the same sender to the same receiver. <br/>
However, any sender is allowed to simultaneously transmit VN messages to several receivers and receivers are allowed to receive VN messages from several senders simultaneously.

