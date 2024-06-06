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
unit ipphonef;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ipvcore, ipvtypes, ipvipphone,
  Vcl.StdCtrls;

type
  TFormIpphone = class(TForm)
    Label1: TLabel;
    SIPInfo: TGroupBox;
    serverLabel: TLabel;
    portLabel: TLabel;
    userLabel: TLabel;
    passwordLabel: TLabel;
    passwordText: TEdit;
    userText: TEdit;
    serverText: TEdit;
    portText: TEdit;
    AUXInfo: TGroupBox;
    speakersLabel: TLabel;
    microphonesLabel: TLabel;
    speakersBox: TComboBox;
    microphonesBox: TComboBox;
    GroupBox1: TGroupBox;
    calleeLabel: TLabel;
    calleeText: TEdit;
    callButton: TButton;
    HangupButton: TButton;
    ipvIPPhone1: TipvIPPhone;
    lbLog: TListBox;
    LogInfo: TGroupBox;
    procedure FormShow(Sender: TObject);
    procedure callButtonClick(Sender: TObject);
    procedure HangupButtonClick(Sender: TObject);
    procedure ipvIPPhone1Log(Sender: TObject; LogLevel: Integer; const Message,
      LogType: string);
  private

  public
    callId: String;
  end;

var
  FormIpphone: TFormIpphone;

implementation

{$R *.dfm}

procedure TFormIpphone.callButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if not ipvIPPhone1.Active then
    begin
      ipvIPPhone1.User := userText.Text;
      ipvIPPhone1.Password := passwordText.Text;
      ipvIPPhone1.Server := serverText.Text;
      ipvIPPhone1.Port := StrToInt(portText.Text);
      ipvIPPhone1.Activate();
    end;

    ipvIPPhone1.SetSpeaker(speakersBox.Items[speakersBox.ItemIndex]);
    ipvIPPhone1.SetMicrophone(microphonesBox.Items[microphonesBox.ItemIndex]);
    callId := ipvIPPhone1.Dial(calleeText.Text, '', true);
  except on ex: EIPWorksVoIP do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormIpphone.HangupButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ipvIPPhone1.Hangup(callId);
  except on ex: EIPWorksVoIP do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormIpphone.ipvIPPhone1Log(Sender: TObject; LogLevel: Integer;
  const Message, LogType: string);
begin
  lbLog.Items.Add(LogType + ': ' + Message);
  lbLog.perform( WM_VSCROLL, SB_BOTTOM, 0 );
  lbLog.perform( WM_VSCROLL, SB_ENDSCROLL, 0 );
end;

procedure TFormIpphone.FormShow(Sender: TObject);
var
  i: Integer;
begin
  ipvIPPhone1.Config('LogLevel=2');
  ipvIPPhone1.ListSpeakers();
  for i := 0 to ipvIPPhone1.Speakers.Count - 1 do
    speakersBox.Items.Add(ipvIPPhone1.Speakers[i].Name);

  ipvIPPhone1.ListMicrophones();
  for i := 0 to ipvIPPhone1.Microphones.Count - 1 do
    microphonesBox.Items.Add(ipvIPPhone1.Microphones[i].Name);
end;

end.


