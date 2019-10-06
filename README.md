### This package is the server part for the interaction of a mobile application with the SAP ERP system. 

Corresponding mobile app package developed on Flutter [sap_connect](https://pub.dev/packages/sap_connect)

To install this package on SAP server, use [abapGit](https://docs.abapgit.org/)

Allows respond to requests from a mobile application.
There are the minimum necessary tools for administration.

## Main objects of package:

**ICF-service:**

- YDKWEBS        Entry point for mobile application

**Classes:**

- YDK_CL_WEBS                        Entry point
- YDK_CL_WEBS_ACTION        Abstract for action

**Tables:**

- YDK_WEBS_USR        Users
- YDK_WEBS_DEV        Devices
- YDK_WEBS_ACT        Available actions
- YDK_WEBS_LOG        Log

**Programs:**

- YDK_WEBS_USR        User maintenance

**Transaction:**

- YDK_WEBS_USR        User maintenance

**Enhancement point:**

- YDK_WEBS_PASSWORD_VALIDATION        Condition for checking password

There are few auxiliary objects as: classes,  data domain&#39;s, data&#39;s elements, search help&#39;s, message&#39;s class… - we think that those objects sufficiently describe themselves and there is no need in additional description

## Description:

### ICF-service YDKWEBS Entry point for mobile application

Transaction SICF, then path in the tree /default_host/sap/bc/ ydkwebs

There is class YDK_CL_WEBS is written In the service as a handler so data of http request go to processing into the method YDK_CL_WEBS>IF_HTTP_EXTENSION~HANDLE_REQUEST

### Class YDK_CL_WEBS Entry point

Method  IF_HTTP_EXTENSION~HANDLE_REQUEST is called by  ICF-service YDKWEBS upon arrival of http request. Inside the method all headers and the body of request are available.

In the request coming from mobile device must have following headers:

- X-YDK-HANDLER_ID  – identifier of handler
- X-YDK-ACTION      – identifier of action/command

The body of request keeps data required for execution of action/command

Based on received HANDLER_ID and ACTION, the corresponding entry is searched in the table YDK_WEBS_ACT, and determined an SAP object with which data will be processed
It can be a call to a class-method of a class or a call to a subroutine in a program

Called class-method of class must have a certain set of incoming and outgoing parameters:
``` ABAP
  CLASS-METHODS <name of method>
    IMPORTING
      !action_data TYPE string
    EXPORTING
      !return_status TYPE string
      !return_data TYPE string.
```
Called subroutine must have a certain set of incoming and changing parameters:
``` ABAP
  FORM <name of form>
    USING
      action_data TYPE string
    CHANGING
      return_status TYPE string
      return_data TYPE string.
```
Incoming parameter action_data contains the string with the body of received request. Usually it is JSON

Return parameter return_status must contain result status of request processing. Usually it is:
- «OK» - the processing of request was successful, constant YDK_CL_WEBS_ACTION=>STATUS_OK
- «ERR» - processing of request with error, constant YDK_CL_WEBS_ACTION=>STATUS_ERR. In that case parameter return_data **must contain the string with the description of error**.

Return parameter return_data in the case of successful request processing contains string with the result of request processing. Usually it is JSON

### Class YDK_CL_WEBS_ACTION Abstract for action

An abstract class contains constants and methods simplifying work with requests coming from mobile devices

It is comfortable to inherit class handler of requests from that class

Class contains:

**Constants:**

- STATUS_OK – processing of request was successful
- STATUS_ERR – processing of request was with error

**Methods:**

- FROM_JSON – transfer data from incoming string contains JSON into the outgoing structure
- GET_JSON – transfer data from incoming structure into outgoing string in format JSON
- USER – return link to user data

### Tables

**YDK_WEBS_USR** – Contains credentials of users for whom entrance with mobile app is available. Account is created via program «YDK_WEBS_USR»

**YDK_WEBS_DEV** – Identifiers of apps. If field YDK_WEBS_USR-DEVCT is not empty – record in the table is automatically formed upon users connection to the SAP service. Upon connection to the server the amount of devices used by user is controlled so it could not exceed amount of devices specified in YDK_WEBS_USR-DEVCT

**YDK_WEBS_ACT** – Contains combinations of handler identifier and identifier of action/command – which determine which SAP object will be called to process the request. 
If note is absent the error with description «action is not authorized» is returned as an answer to request<br>
field HANDLER_TYPE - contains the type of the called object:
* "M" or empty - field HANDLER_NAME contain class name; field HANDLER_ACTION contain name of class-method
* "S" - field HANDLER_NAME contain program name; field HANDLER_ACTION contain name of subroutine (form)

if HANDLER_NAME is empty HANDLER_ID value is used
if HANDLER_ACTION is empty ACTION  value is used

**YDK_WEBS_LOG** – Log of implementation operations by user. Log is writes for operations for which field YDK_WEBS_ACT-SAVELOG = abap_true. Login, date and time of operation are written

### Transaction/Program YDK_WEBS_USR

Via this program implements creation and editing of user account. If at the moment of creation of account there is no tick in front of inscription «secondary account» there must have to be account (SU01) with the same name in the SAP system.

## Secondary accounts

**Warning** – this paragraph is pretty controversial and might contradict to licensing policy of SAP. So I recommend you to read carefully the final part of license for that package

Via program YDK_WEBS_USR you can create account with note «secondary account». In that case users account (SU01) with the same name might absent in SAP system.

Establishing connection with SAP server is done with any special (public) account of SAP user (that user might be marked as «Communications Data»), after that implements secondary authorization with account marked as «Secondary account»

Pluses:

1. Accounts like that do not get under SAP license so cost nothing (please, read the warning in the beginning of that paragraph)
2. Provide more flexible connection/entrance to SAP system – opportunity of entrance without entering the password (for example by use of biometric check on the device)

Minuses:

1. Different users can enter to SAP system at the same time with one and the same SAP account – standard SAP facilities cannot distinguish users like this so arise problems with standard system of users authorization separation and standard mechanism of users actions logging

### Enhancement point YDK_WEBS_PASSWORD_VALIDATION

Via this point it is possible to override requirements which are presented to the password upon its change from mobile device. Change of the password from mobile device is only available for «secondary accounts».

Controlled parameters of password:

- Length
- Amount of numerals
- Amountfnot-lettersymbols

### Example
An example of use is set with the package, class YDK_CL_WEBS_FLIGHTS, see folder Example
The sflight DB can be filled with test data by running the programs SAPBC_DATA_GENERATOR and SFLIGHT_DATA_GEN
``` ABAP
CLASS YDK_CL_WEBS_FLIGHTS IMPLEMENTATION.
  METHOD get_flights.
*    importing
*      !ACTION_DATA type STRING
*    exporting
*      !RETURN_STATUS type STRING
*      !RETURN_DATA type STRING .

* The sflight DB can be filled with test data by running the programs SAPBC_DATA_GENERATOR and SFLIGHT_DATA_GEN

    TYPES: BEGIN OF ty_query,
             carrid TYPE RANGE OF sflight-carrid,
             connid TYPE RANGE OF sflight-connid,
             fldate TYPE RANGE OF sflight-fldate,
           END   OF ty_query.

    DATA: query TYPE ty_query.

* to simplify the work with the request data, they are loaded into the corresponding structure
    from_json( EXPORTING json = action_data CHANGING data = query ). 

    DATA: lt_sflight TYPE STANDARD TABLE OF sflight.

    SELECT * INTO TABLE lt_sflight
      FROM sflight
     WHERE carrid IN query-carrid
       AND connid IN query-connid
       AND fldate IN query-fldate.
	
    IF lt_sflight IS INITIAL.
* return error message	
      return_data = 'No flights found by query criteria'.
      return_status = status_err.
      RETURN.
    ENDIF.
	
* return query results, return_status is set to STATUS_OK
    get_json( EXPORTING data = lt_sflight IMPORTING return_status = return_status return_data = return_data ).
  ENDMETHOD.
ENDCLASS.
```