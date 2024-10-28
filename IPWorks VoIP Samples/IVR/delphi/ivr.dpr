(*
 * IPWorks VoIP 2024 Delphi Edition - Sample Project
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
 *)

program ivr;

uses
  Forms,
  ivrf in 'ivrf.pas' {FormIvr};

begin
  Application.Initialize;

  Application.CreateForm(TFormIvr, FormIvr);
  Application.Run;
end.


         
