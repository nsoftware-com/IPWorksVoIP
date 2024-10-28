/*
 * IPWorks VoIP 2024 C++ Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks VoIP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksvoip
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksvoip.h"
#define LINE_LEN 80

bool connected;

class MyIPPhone : public IPPhone
{
public:
  int FireCallReady(IPPhoneCallReadyEventParams* e)
  {
    connected = true;
    return 0;
  }

  int FireCallTerminated(IPPhoneCallTerminatedEventParams* e)
  {
    connected = false;
    return 0;
  }

  int FireLog(IPPhoneLogEventParams* e) {
    printf("%s: %s\r\n", e->LogType, e->Message);
    return 0;
  }
};

int main(int argc, char *argv[])
{
  MyIPPhone phone;

  printf("*********************************************************************\n");
  printf("* This demo shows how to call yourself using the IPPhone component. *\n");
  printf("*********************************************************************\n");

  if (argc < 4) {
    fprintf(stderr, "usage: ipphone sip_server sip_port sip_username [sip_password]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "sip_server:       SIP Server\n");
    fprintf(stderr, "sip_port:         SIP Port\n");
    fprintf(stderr, "sip_username:     SIP Server username\n");
    fprintf(stderr, "sip_password:     SIP Server password\n");
    fprintf(stderr, "\nExample: ipphone server 5060 user test\n");
    printf("\nPress enter to continue.\n");
    getchar();
  }
  else {
    connected = false;

    phone.SetUser(argv[3]);
    if (argc == 5) {
      phone.SetPassword(argv[4]);
    }
    phone.SetServer(argv[1]);
    phone.SetPort(atoi(argv[2]));
    phone.Activate();

    if (phone.GetLastErrorCode()) {
      goto done;
    }

    char command[LINE_LEN];
    while (true) {
      printf("\nPlease input command: \r\n- 1 Make a Call \r\n- 2 Quit \r\n");
      printf("> ");

      fgets(command, LINE_LEN, stdin);
      command[strlen(command) - 1] = '\0';

      if (!strcmp(command, "1")) {
        char aux[LINE_LEN];
        phone.ListSpeakers();
        for (int i = 0; i < phone.GetSpeakerCount(); i++) {
          printf("%d: %s\n", i, phone.GetSpeakerName(i));
        }
        printf("\nChoose speaker # from list above: ");
        fgets(aux, LINE_LEN, stdin);
        aux[strlen(aux) - 1] = '\0';
        phone.SetSpeaker(phone.GetSpeakerName(atoi(aux)));

        phone.ListMicrophones();
        for (int i = 0; i < phone.GetMicrophoneCount(); i++) {
          printf("%d: %s\n", i, phone.GetMicrophoneName(i));
        }
        printf("\nChoose microphone # from list above: ");
        fgets(aux, LINE_LEN, stdin);
        aux[strlen(aux) - 1] = '\0';
        phone.SetMicrophone(phone.GetMicrophoneName(atoi(aux)));

        char number[LINE_LEN];
        printf("\nPlease enter number to call: ");
        fgets(number, LINE_LEN, stdin);
        number[strlen(number) - 1] = '\0';
        phone.Dial(number, "", true);

        while (connected) {
          phone.DoEvents();
        }
      }
      else if (!strcmp(command, "2")) {
        goto done;
      }
      else {
        printf("Command not recognized.\n");
      }

      if (phone.GetLastErrorCode()) {
        goto done;
      }
    }
  done:
    if (phone.GetLastErrorCode()) {
      printf("Error: [%i] %s\n", phone.GetLastErrorCode(), phone.GetLastError());
    }

    printf("Exiting... (press enter)\n");
    getchar();

    return 0;
  }
}






