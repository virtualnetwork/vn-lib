VN CAN Subnet Protocol
==========================

### High level

#### Identifiers and addresses


**There exists two forms of identifiers in this protocol:** 
  * **The Component Universally Unique Identifier (CUUID)**  is a high level identifier that is to be unique world world for the particular component. The length of the CUUID is 128 bits (16 bytes). Any component on a VN network needs a CUUID.
  * **The Universally Unique CAN Identifier (UCID)** is a low level identifier that is to be unique world world for the particular component. The length of the UCID is 28 bits. Any component on the VN-CAN network will need a UCID.

**There exists two forms of addresses in this protocol:**
  * **The logical addresses.**  Defined in the VN protocol.
  * **CAN addresses.** The CAN address. Any component on the VN-CAN network will be assigned a CAN address. The CAN addresses are only used 	on the CAN bus on the low level part of the VN-CAN protocol. The CAN addresses are managed by the SM-CAN master.


#### Stored data
**The following data is stored in this protocol:**
  * Send buffer, for VN-messages.
  * Receive buffer, for VN-messages.
  * A primary routing table, connecting a given logical address to a low level
    address.
  * A CUUID routing table, connecting a given CUUID to a subnet address. This
    table is used in some special cases of VN messages (e.g. **AssignAddr**)
    that cannot rely on logical addresses for routing.


#### Interface
**There exists two public functions stored in this protocol:**
  * Send. Will take a VN message as argument, an out-variable will tell the
    outcome of the call, i.e. “Transmission OK” or “Buffer full”. The message
    is written to the send buffer if it is not full.
  * Receive. Will give a VN message as an out-variable, another out-variable
    will tell the outcome of the call, i.e. “No message received”, “Message
    received, buffer empty” and “Message received, more available”.

### Route discovery process
##### For routes to overlying units
See Section *For routes to overlying units* in *VN generic subnet protocol*.

##### For routes to underlying units
See Section *For routes to underlying units* in *VN generic subnet protocol*.

##### Transmission of VN messages
See Section *Transmission of VN messages* in *VN generic subnet protocol*.

##### Reception of VN messages
When a VN message is received on the subnet it shall be pushed to the receive buffer. 
Actions according to sections *Route discovery process* and *Unit discovery process* shall also be performed.
*Unit discovery process ?????????????????* 

##### Logical addresses
Assignment of logical addresses is handled by higher level protocols and is not described in this protocol.




### Low level

##### Division of message IDs
Bits are numbered 28-0 where bit 28 is most significant and is sent first.
There are two groups of message types: *RequestCANAddress* message (one message type) and *Normal* CAN Messages. These are defined by the first bit of the message ID.
The message ID is divided according to:

1. msgID[28] = 1: RequestCANAddress message<br/>
	msgID[27 .. 0] = Universally Unique CAN Identifier (UCID) for the node
2. msgID[28] = 0: Normal CAN messages <br/>
	msgID[27 .. 22] = Message priority <br/>
	msgID[21 .. 15] = Message type <br/>
	msgID[14 .. 7] = Receiver address <br/>
	msgID[6 .. 0] = Sender address 

*Please note:* Even though CAN addresses are 8 bit, addresses over 127 will never be used as a sender address since the maximum number of allowed CAN nodes on a CAN network is 128 one only needs addresses 0 through 127. This means that only 7 bits are needed for the Sender address.


##### Message priority
To be determined, set by higher protocols?

##### Message type
The types of messages present are listed below. Please note that in the case of two messages having equal priory fields the message type field will determine priority. A lower Message type number will give a higher priority.



| **Message type number** | **Meaning** | **Comment**  |
| ----------------------- | ----------- | ------------ |
| Not in this list | **RequestCANAddress** | Sent at regular intervals by any unit on the CAN network that does not have an CAN address yet. Contains information telling whether its a normal node or an SM-CAN. <br/>
Nodes shall wait to send this message until they have received a **Normal CAN message**[^1](This shows that a SM-CAN master has been assigned.) such as the **CANMasterAssigned** message, since nobody will care about the **RequestCANAddress** except the SM-CAN master.
| 0 | **AssignCANAddress** | Assignment of CAN-address from SM-CAN master.
Is sent to address 255 (Broadcast address). Contains the  Universally Unique CAN Identifier (UCID) for the node and its assigned CAN address |
| 1 | **CANMasterAssigned** |  |
| 2 | **AssignCANAddress** |  |
| 3 | **AssignCANAddress** |  |
| 4 | **AssignCANAddress** |  |
| 5 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |
| 0 | **AssignCANAddress** |  |

##### Message contents
This section declares the content of each message.

**RequestCANAddress**
Can contain zero or one byte. An ordinary node either sends zero bytes or sets the first byte equal to 5. SM-CANs set this byte equal to 3. 


**AssignCANAddress**
Bytes 0 – 3:  UCID of the receiving node.
Byte 4: CAN address assigned to the node.

**CANMasterAssigned**
No payload data.



##### CAN Addresses

| **Address** | **Meaning** | **Comment**  |
| ----------------------- | ----------- | ------------ |
| 255 | Broadcast address | All units on the CAN network should listen to this address at all times. |
| 254 | Selective broadcast address | Broadcast address to all SM-CANs, but not nodes. |
| 128..253 | Currently not used | These addresses are not used at the time of writing. |
| 1..127 | Node CAN addresses | These addresses are given to nodes and SM-CAN slaves. |
| 0 | SM-CAN master address | CAN address of SM-CAN master. |

##### SM-CAN master negotiation process
 1.  Each SM-CAN shall send a RequestCANAddress message when it starts.
 2.  The SM-CAN shall delay for XXX ms. During this time it shall listen to **RequestCANAddress** and any **Normal CAN message**.
 2.1  If the SM-CAN receives a **Normal CAN message**, or a **RequestCANAddress** message from an SM-CAN with a lower UCID, it shall become an SM-CAN slave.
 2.2  If the  SM-CAN receives a **RequestCANAddress** message from an SM-CAN with a higher UCID it shall respond with a **RequestCANAddress** message of its own.
 3.  If the delay has passed without the SM-CAN becoming a slave it shall become an SM-CAN master.
 4.  If the SM-CAN master receives a **RequestCANAddress** message from an SM-CAN it shall respond with a *Normal CAN message*, such as the **CANMasterAssigned** message. *This responsibility of the SM-CAN master remains indefinitely.*
 5.  Once assigned as a slave, an SM-CAN has no further responsibilities with regards to the SM-CAN master negotiation process.
 6.  Once a SM-CAN has been assigned an SM-CAN master, it shall be ready to receive VN messages. <br/>
This means that it shall listen for StartTransmission and Transmission messages and be ready to answer with FlowControl messages.

##### Discovery process
*This process takes place after the SM-CAN master negotiation process.*


##### Transmission of VN messages

