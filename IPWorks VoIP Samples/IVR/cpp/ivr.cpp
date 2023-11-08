/*
 * IPWorks VoIP 2022 C++ Edition - Sample Project
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
#include <stdlib.h>
#include <map>
#include <iostream>
#include <string>
#include "../../include/ipworksvoip.h"
#define LINE_LEN 80

std::map<std::string, std::string> digitList;

class MyIVR : public IVR
{
public:
    void updateMap(std::string key, std::string value)
    {
        std::map<std::string, std::string>::iterator itr;
        itr = digitList.find(key);
        if (itr != digitList.end()) {
            itr->second = value;
        }
        return;
    }

    int FireActivated(IVRActivatedEventParams* e)
    {
        printf("\nActivated\n");
        return 0;
    }

    int FireCallReady(IVRCallReadyEventParams* e)
    {
        this->PlayText(e->CallId, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.");
        return 0;
    }

    int FireCallTerminated(IVRCallTerminatedEventParams* e)
    {
        digitList.erase(e->CallId);
        return 0;
    }

    int FireDigit(IVRDigitEventParams* e)
    {
        std::string currentDigits = digitList.at(e->CallId) + e->Digit;
        updateMap(e->CallId, currentDigits);
        if (currentDigits[0] == '1') {
            if (currentDigits.length() == 1) {
                this->PlayText(e->CallId, "Please input your account number followed by #");
            }
            else if (strcmp(e->Digit, "#") == 0) {
                std::string digits = "";
                for (int i = 1; i < currentDigits.length() - 1; i++) {
                    digits.push_back(currentDigits.at(i));
                    digits.push_back(' ');
                }
                std::string msg = "Your account number is " + digits + ". This account is currently active.";
                this->PlayText(e->CallId, msg.c_str());
                updateMap(e->CallId, "");
            }
        }
        else if (currentDigits[0] == '2') {
            this->Hangup(e->CallId);
        }
        else {
            updateMap(e->CallId, "");
        }
        return 0;
    }

    int FireIncomingCall(IVRIncomingCallEventParams* e)
    {
        this->Answer(e->CallId);
        digitList.insert(std::pair<std::string, std::string>(e->CallId, ""));
        return 0;
    }

    int FireLog(IVRLogEventParams* e) {
        printf("%s: %s\n", e->LogType, e->Message);
        return 0;
    }

    int FirePlayed(IVRPlayedEventParams* e)
    {
        std::string currentDigits = digitList.at(e->CallId);
        if (currentDigits.length() == 0) {
            this->PlayText(e->CallId, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.");
        }
        return 0;
    }
};

int main(int argc, char *argv[])
{
    MyIVR ivr1;

    printf("**************************************************************************************\n");
    printf("* This is a demo to show how to set up a sample IVR menu using the IVR component.    *\n");
    printf("* Once activation is complete, the component will be able to receive incoming calls. *\n");
    printf("**************************************************************************************\n");

    if (argc < 4) {
        fprintf(stderr, "\nusage: ivr sip_server sip_port sip_username [sip_password]\n\n");
        fprintf(stderr, "sip_server:       SIP Server\n");
        fprintf(stderr, "sip_port:         SIP Port\n");
        fprintf(stderr, "sip_username:     SIP Server username\n");
        fprintf(stderr, "sip_password:     SIP Server password\n");
        fprintf(stderr, "\nExample: ivr server 5060 user test\n");
        printf("\nPress enter to continue.\n");
        getchar();
    }
    else {
        ivr1.SetUser(argv[3]);
        if (argc == 5) {
            ivr1.SetPassword(argv[4]);
        }
        ivr1.SetServer(argv[1]);
        ivr1.SetPort(atoi(argv[2]));
        ivr1.Activate();

        if (ivr1.GetLastErrorCode()) {
            goto done;
        }

        while (true) {
            ivr1.DoEvents();

            if (ivr1.GetLastErrorCode()) {
                goto done;
            }
        }
    done:
        if (ivr1.GetLastErrorCode()) {
            printf("Error: [%i] %s\n", ivr1.GetLastErrorCode(), ivr1.GetLastError());
        }

        printf("Exiting... (press enter)\n");
        getchar();

        return 0;
    }
}








