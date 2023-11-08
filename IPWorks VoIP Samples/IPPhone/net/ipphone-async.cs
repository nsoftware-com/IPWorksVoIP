/*
 * IPWorks VoIP 2022 .NET Edition - Sample Project
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
 * 
 */

using System.Collections.Generic;

using System;
using System.Diagnostics;
using System.Threading.Tasks;
using nsoftware.async.IPWorksVoIP;

class ipphoneDemo
{
  private static Ipphone ipphone1;
  private static bool connected = true; // keep track of whether we are in a call or not

  private static void ipphone1_OnActivated(object sender, IpphoneActivatedEventArgs e)
  {
    Console.WriteLine("Activated\n");
  }

  private static void ipphone1_OnCallTerminated(object sender, IpphoneCallTerminatedEventArgs e)
  {
    connected = false;
  }

  private static void ipphone1_OnLog(object sender, IpphoneLogEventArgs e)
  {
    Console.WriteLine(e.LogType + ": " + e.Message);
  }

  static async Task Main(string[] args)
  {
    ipphone1 = new Ipphone();
    Console.WriteLine("********************************************************************");
    Console.WriteLine("* This demo shows how to call yourself using the IPPhone component *");
    Console.WriteLine("********************************************************************\n");

    if (args.Length < 4)
    {
      Console.WriteLine("\nusage: ipphone sip_server sip_port sip_username [sip_password]\n");
      Console.WriteLine("sip_server:       SIP Server");
      Console.WriteLine("sip_port:         SIP Port");
      Console.WriteLine("sip_username:     SIP Server username");
      Console.WriteLine("sip_password:     SIP Server password");
      Console.WriteLine("\nExample: ipphone server 5060 user test\n");
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }
    else
    {
      ipphone1.OnActivated += ipphone1_OnActivated;
      ipphone1.OnCallTerminated += ipphone1_OnCallTerminated;
      ipphone1.OnLog += ipphone1_OnLog;

      try
      {
        await ipphone1.Config("LogLevel=2");
        ipphone1.Server = args[1];
        ipphone1.Port = int.Parse(args[2]);
        ipphone1.User = args[3];
        if (args.Length == 5)
        {
          ipphone1.Password = args[4];
        }

        await ipphone1.Activate();

        Console.WriteLine("Type \"?\" for a list of commands.");
        Console.Write("ipphone> ");
        string command;

        while (true)
        {
          command = Console.ReadLine();

          if (command.Equals("?"))
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                            Display the list of valid commands");
            Console.WriteLine("  c                            Make a call");
            Console.WriteLine("  q                            Exit the application");
          }
          else if (command.Equals("c"))
          {
            int idx = 0;
            string number = "";

            await ipphone1.ListSpeakers();
            for (int i = 0; i < ipphone1.Speakers.Count; i++)
            {
              Console.WriteLine(i + ": " + ipphone1.Speakers[i].Name);
            }

            if (ipphone1.Speakers.Count > 0)
            {
              Console.Write("Choose speaker # from list above: ");
              idx = int.Parse(Console.ReadLine());
              await ipphone1.SetSpeaker(ipphone1.Speakers[idx].Name);
            }

            await ipphone1.ListMicrophones();
            for (int i = 0; i < ipphone1.Microphones.Count; i++)
            {
              Console.WriteLine(i + ": " + ipphone1.Microphones[i].Name);
            }

            if (ipphone1.Microphones.Count > 0)
            {
              Console.Write("Choose microphone # from list above: ");
              idx = int.Parse(Console.ReadLine());
              await ipphone1.SetMicrophone(ipphone1.Microphones[idx].Name);
            }

            Console.Write("Please enter the number to call: ");
            number = Console.ReadLine();

            await ipphone1.Dial(number, "", false);

            while (connected)
            {
              await ipphone1.DoEvents();
            }
            connected = true;
          }
          else if (command.Equals("q"))
          {
            break;
          }
          else
          {
            Console.WriteLine("Invalid command.");
          }
          await ipphone1.DoEvents();
          Console.Write("ipphone> ");
        }
      } catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }
  }
}







class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}