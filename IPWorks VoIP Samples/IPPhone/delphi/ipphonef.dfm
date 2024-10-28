object FormIpphone: TFormIpphone
  Left = 0
  Top = 0
  Caption = 'IPPhone Demo'
  ClientHeight = 558
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 464
    Height = 45
    Caption = 
      'This demo shows how to use the IPPhone component to interact wit' +
      'h a SIP Server. To '#13#10'start, enter the below information, and cli' +
      'ck '#39'Activate'#39'. Then, select the microphone and '#13#10'speaker you wan' +
      't to use, enter in the phone number to call, and press '#39'Call'#39
    Color = clBtnFace
    ParentColor = False
  end
  object SIPInfo: TGroupBox
    Left = 8
    Top = 59
    Width = 464
    Height = 134
    Caption = 'SIP Server Information'
    TabOrder = 0
    object serverLabel: TLabel
      Left = 16
      Top = 32
      Width = 54
      Height = 15
      Caption = 'SIP Server:'
    end
    object portLabel: TLabel
      Left = 342
      Top = 32
      Width = 44
      Height = 15
      Caption = 'SIP Port:'
    end
    object userLabel: TLabel
      Left = 16
      Top = 64
      Width = 45
      Height = 15
      Caption = 'SIP User:'
    end
    object passwordLabel: TLabel
      Left = 16
      Top = 96
      Width = 72
      Height = 15
      Caption = 'SIP Password:'
    end
    object passwordText: TEdit
      Left = 112
      Top = 90
      Width = 337
      Height = 23
      PasswordChar = '*'
      TabOrder = 3
    end
    object userText: TEdit
      Left = 112
      Top = 61
      Width = 337
      Height = 23
      TabOrder = 2
    end
    object serverText: TEdit
      Left = 112
      Top = 32
      Width = 224
      Height = 23
      TabOrder = 0
    end
    object portText: TEdit
      Left = 400
      Top = 29
      Width = 49
      Height = 23
      TabOrder = 1
      Text = '5060'
    end
  end
  object AUXInfo: TGroupBox
    Left = 8
    Top = 208
    Width = 464
    Height = 105
    Caption = 'Auxilliary Information'
    TabOrder = 1
    object speakersLabel: TLabel
      Left = 16
      Top = 32
      Width = 49
      Height = 15
      Caption = 'Speakers:'
    end
    object microphonesLabel: TLabel
      Left = 16
      Top = 64
      Width = 73
      Height = 15
      Caption = 'Microphones:'
    end
    object speakersBox: TComboBox
      Left = 112
      Top = 24
      Width = 337
      Height = 23
      TabOrder = 0
      Text = 'Available Speakers...'
    end
    object microphonesBox: TComboBox
      Left = 112
      Top = 64
      Width = 337
      Height = 23
      TabOrder = 1
      Text = 'Available Microphones...'
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 328
    Width = 464
    Height = 65
    Caption = 'Callee Information'
    TabOrder = 2
    object calleeLabel: TLabel
      Left = 16
      Top = 32
      Width = 84
      Height = 15
      Caption = 'Phone Number:'
    end
    object calleeText: TEdit
      Left = 106
      Top = 29
      Width = 95
      Height = 23
      TabOrder = 0
    end
    object callButton: TButton
      Left = 223
      Top = 27
      Width = 99
      Height = 25
      Caption = 'Call'
      TabOrder = 1
      OnClick = callButtonClick
    end
    object HangupButton: TButton
      Left = 350
      Top = 27
      Width = 99
      Height = 25
      Caption = 'Hangup'
      TabOrder = 2
      OnClick = HangupButtonClick
    end
  end
  object LogInfo: TGroupBox
    Left = 8
    Top = 407
    Width = 464
    Height = 145
    Caption = 'Log Information'
    TabOrder = 3
    object lbLog: TListBox
      Left = 16
      Top = 25
      Width = 433
      Height = 104
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object ipvIPPhone1: TipvIPPhone
    OnLog = ipvIPPhone1Log
    Left = 136
    Top = 16
  end
end


