{
  Criador: Bruno Deuner
  Contato
  Email: bruno_deuner@hotmail.com

  Informações
  Autorização de contas:
  Gmail: https://myaccount.google.com/lesssecureapps
  Hotmail: Não precisa
}


unit Tora.Email;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Inifiles,
  IdSMTP,
  IdSSLOpenSSL,
  IdMessage,
  IdText,
  IdAttachmentFile,
  IdExplicitTLSClientServerBase,
  IdEMailAddress,
  IdPOP3;

type

  TSMTP = (smNenhum, smHotmail, smGmail);
  TAfterOperation = procedure(Sender: TObject; Status: Boolean) of object;
  TReadMessage = procedure(Sender: TObject; Mensagem: TIdMessage) of object;
  TErroConnection = procedure(Sender: TObject; Mensagem: String) of object;
  TErroAuthentication = TErroConnection;
  TOrder = (toCrescente, toDecrecente);

  TAccount = class(TPersistent)
  private
    FSMTP: TSMTP;
    FPassword: String;
    FUserName: String;
    FName: String;
    procedure DiscoverySMTP;
    function FileNameDefault: String;
    procedure SetPassword(const Value: String);
    procedure SetUserName(const Value: String);
    procedure SetName(const Value: String);
  public
    constructor Create;
    property Name: String read FName write SetName;
    property Password: String read FPassword write SetPassword;
    property UserName: String read FUserName write SetUserName;
    function IsValid: Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(NameLoad: string = 'Default');
    procedure LoadFromFileByName;
    procedure SaveToFile; overload;
  end;

  IEmail = interface
    // End thread
    procedure TerminateThread(Sender: TObject);
    // Metodo
    procedure Execute;
    // Getter and Setters
    procedure SetAccount(Value: TAccount);
    procedure SetOnAfterConnect(const Value: TAfterOperation);
    procedure SetOnBeforeConnect(const Value: TNotifyEvent);
    procedure SetOnErrorConnection(const Value: TErroConnection);
    function GetAccount: TAccount;
    function GetOnBeforeConnect: TNotifyEvent;
    function GetOnAfterConnect: TAfterOperation;
    function GetOnErrorConnection: TErroConnection;
    // Metodos
    procedure Connect;
    procedure Cancel;
    // Propriedades
    property Account: TAccount read GetAccount write SetAccount;
    // Callbacks
    property OnBeforeConnect: TNotifyEvent read GetOnBeforeConnect
      write SetOnBeforeConnect;
    property OnAfterConnect: TAfterOperation read GetOnAfterConnect
      write SetOnAfterConnect;
    property OnErrorConnection: TErroConnection read GetOnErrorConnection
      write SetOnErrorConnection;
  end;

  TSend = class(TInterfacedObject, IEmail)
  private
    IdSMTP: TIdSMTP;
    SSL: TIdSSLIOHandlerSocketOpenSSL;
    FThread: TThread;
    Enviou: Boolean;
    // Callbacks
    FOnAfterSend: TAfterOperation;
    FOnBeforeConnect: TNotifyEvent;
    FOnAfterConnect: TAfterOperation;
    FOnBeforeSend: TNotifyEvent;
    FOnErrorAuthentication: TErroAuthentication;
    FOnErrorConnection: TErroConnection;
    // Fields
    FAssunto: string;
    FAnexos: TStringList;
    FTexto: TStringList;
    FDestino: TStringList;
    FRemetenteNome: string;
    FRemetenteEmail: string;
    FAowner: TObject;
    FAccount: TAccount;
    // End thread
    procedure TerminateThread(Sender: TObject);
    // Metodo
    procedure Execute;
    procedure ConfigSMTP;
    // Getter and Setters
    procedure SetAccount(Value: TAccount);
    procedure SetOnAfterConnect(const Value: TAfterOperation);
    procedure SetOnBeforeConnect(const Value: TNotifyEvent);
    procedure SetOnErrorAuthentication(const Value: TErroAuthentication);
    procedure SetOnErrorConnection(const Value: TErroConnection);
    function GetAccount: TAccount;
    function GetOnBeforeConnect: TNotifyEvent;
    function GetOnAfterConnect: TAfterOperation;
    function GetOnErrorConnection: TErroConnection;
    procedure SetAfterSend(const Value: TAfterOperation);
    procedure SetAnexos(const Value: TStringList);
    procedure SetAssunto(const Value: string);
    procedure SetBeforeSend(const Value: TNotifyEvent);
    procedure SetDestino(const Value: TStringList);
    procedure SetRemetenteEmail(const Value: string);
    procedure SetRemetenteNome(const Value: string);
    procedure SetTexto(const Value: TStringList);
  public
    // Construtor
    constructor Create; overload;
    constructor Create(Aowner: TObject); overload;
    constructor Create(Aowner: TObject; Account: TAccount); overload;
    destructor Destroy; override;
    // Metodos
    procedure Connect;
    procedure Send;
    procedure Cancel;
    // Propriedades
    property RemetenteEmail: string read FRemetenteEmail
      write SetRemetenteEmail;
    property RemetenteNome: string read FRemetenteNome write SetRemetenteNome;
    property Assunto: string read FAssunto write SetAssunto;
    property Anexos: TStringList read FAnexos write SetAnexos;
    property Texto: TStringList read FTexto write SetTexto;
    property Destino: TStringList read FDestino write SetDestino;
    property Account: TAccount read GetAccount write SetAccount;
    // Callbacks
    property OnBeforeSend: TNotifyEvent read FOnBeforeSend write SetBeforeSend;
    property OnAfterSend: TAfterOperation read FOnAfterSend write SetAfterSend;
    property OnBeforeConnect: TNotifyEvent read GetOnBeforeConnect
      write SetOnBeforeConnect;
    property OnAfterConnect: TAfterOperation read GetOnAfterConnect
      write SetOnAfterConnect;
    property OnErrorConnection: TErroConnection read FOnErrorConnection
      write SetOnErrorConnection;
    property OnErrorAuthentication: TErroAuthentication
      read FOnErrorAuthentication write SetOnErrorAuthentication;
  end;

  TReceive = class(TInterfacedObject, IEmail)
  private
    IdPOP: TIdPop3;
    SSL: TIdSSLIOHandlerSocketOpenSSL;
    FThread: TThread;
    // Callbacks
    FOnAfterConnect: TAfterOperation;
    FOnBeforeReceive: TNotifyEvent;
    FOnBeforeConnect: TNotifyEvent;
    FOnAfterReceive: TAfterOperation;
    FOnReadMessage: TReadMessage;
    FOnErrorConnection: TErroConnection;
    // Fields
    FEmails: TList<TIdMessage>;
    FInicial: Cardinal;
    FQuantidade: Cardinal;
    FOrdem: TOrder;
    FAowner: TObject;
    FAccount: TAccount;
    MsgProcess: Cardinal;
    MsgPos: Cardinal;
    MsgCount: Cardinal;
    FOnErrorReceiveMessage: TNotifyEvent;
    // End Thread
    procedure TerminateThread(Sender: TObject);
    // Metodos
    procedure AdicionaEmail
      (Lista: System.Generics.Collections.TList<TIdMessage>; i: Integer);
    procedure Execute;
    // Getters and Setters
    procedure SetAccount(Value: TAccount);
    procedure SetOnAfterConnect(const Value: TAfterOperation);
    procedure SetOnBeforeConnect(const Value: TNotifyEvent);
    procedure SetOnErrorConnection(const Value: TErroConnection);
    function GetAccount: TAccount;
    function GetOnBeforeConnect: TNotifyEvent;
    function GetOnAfterConnect: TAfterOperation;
    function GetOnErrorConnection: TErroConnection;
    procedure SetAfterReceive(const Value: TAfterOperation);
    procedure SetBeforeReceive(const Value: TNotifyEvent);
    procedure SetInicial(const Value: Cardinal);
    procedure SetOrdem(const Value: TOrder);
    procedure SetQuantidade(const Value: Cardinal);
    function GetFinished: Boolean;
    procedure SetReadMessage(const Value: TReadMessage);
    procedure SetOnErrorReceiveMessage(const Value: TNotifyEvent);
  public
    // Construtor/Destructor
    constructor Create; overload;
    constructor Create(Aowner: TObject); overload;
    constructor Create(Aowner: TObject; Account: TAccount); overload;
    destructor Destroy; override;
    // Metodos
    procedure Connect;
    procedure Receive;
    procedure Cancel;
    // Propriedades
    property Emails: TList<TIdMessage> read FEmails;
    property Inicial: Cardinal read FInicial write SetInicial default 0;
    property Quantidade: Cardinal read FQuantidade write SetQuantidade
      default 0;
    property Ordem: TOrder read FOrdem write SetOrdem default toDecrecente;
    property Account: TAccount read GetAccount write SetAccount;
    property Finished: Boolean read GetFinished;
    // CallBack
    property OnBeforeConnect: TNotifyEvent read GetOnBeforeConnect
      write SetOnBeforeConnect;
    property OnAfterConnect: TAfterOperation read GetOnAfterConnect
      write SetOnAfterConnect;
    property OnErrorConnection: TErroConnection read GetOnErrorConnection
      write SetOnErrorConnection;
    property OnBeforeReceive: TNotifyEvent read FOnBeforeReceive
      write SetBeforeReceive;
    property OnAfterReceive: TAfterOperation read FOnAfterReceive
      write SetAfterReceive;
    property OnReadMessage: TReadMessage read FOnReadMessage
      write SetReadMessage;
    property OnErrorReceiveMessage: TNotifyEvent read FOnErrorReceiveMessage
      write SetOnErrorReceiveMessage;
  end;

  TEmail = class
  private
    // Fields
    FSend: TSend;
    FReceive: TReceive;
    FAccount: TAccount;
    // Metodos
    function GetAccount: TAccount;
    procedure SetAccount(Value: TAccount);
    procedure SetReceive(const Value: TReceive);
    procedure SetSend(const Value: TSend);
  public
    // Construtor/Destrutor
    constructor Create;
    destructor Destroy; Override;
    // Propriedades
    property Send: TSend read FSend write SetSend;
    property Receive: TReceive read FReceive write SetReceive;
    property Account: TAccount read GetAccount write SetAccount;
    // Metódos
    procedure Cancel;
    procedure Enviar;
    procedure Receber;
  end;

implementation

uses
  IdAssignedNumbers, Vcl.Dialogs;

{ TEmail }

constructor TEmail.Create;
begin
  // Cria Listas
  FReceive := TReceive.Create(Self);
  FSend := TSend.Create(Self);
  FAccount := TAccount.Create;
end;

destructor TEmail.Destroy;
begin
  FreeAndNil(FReceive);
  FreeAndNil(FSend);
  UnLoadOpenSSLLibrary;
  FreeAndNil(FAccount);
end;

procedure TEmail.Enviar;
begin
  FSend.Account.Assign(Account);
  FSend.Send;
end;

procedure TEmail.Cancel;
begin
  FSend.Cancel;
  FReceive.Cancel;
end;

function TEmail.GetAccount: TAccount;
begin
  result := FAccount;
end;

procedure TEmail.Receber;
begin
  FReceive.Account.Assign(Account);
  FReceive.Receive;
end;

procedure TEmail.SetAccount(Value: TAccount);
begin
  FAccount := Value;
end;

procedure TEmail.SetReceive(const Value: TReceive);
begin
  FReceive := Value;
end;

procedure TEmail.SetSend(const Value: TSend);
begin
  FSend := Value;
end;

{ TSend }

procedure TSend.Cancel;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    if not FThread.Finished then
      FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TSend.ConfigSMTP;
begin
  with IdSMTP do
  begin
    IOHandler := SSL;
    AuthType := satDefault;
    UserName := Account.UserName;
    Password := Account.Password;
    if Account.FSMTP = smGmail then
    begin
      UseTLS := utUseImplicitTLS;
      Port := IdPORT_ssmtp;
      Host := 'smtp.gmail.com';
    end
    else
    begin
      UseTLS := utUseExplicitTLS;
      Port := Id_PORT_submission;
      Host := 'smtp.live.com';
    end;
  end;
end;

procedure TSend.Connect;
var
  MessageError: string;
begin
  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnBeforeConnect) then
    FOnBeforeConnect(FAowner);
  if FThread.CheckTerminated then
    abort;
  // Instancia objetos
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  IdSMTP := TIdSMTP.Create;

  // Configuração do protocolo SSL (TIdSSLIOHandlerSocketOpenSSL)
  SSL.SSLOptions.Method := sslvSSLv23;
  SSL.SSLOptions.Mode := sslmClient;
  // Configuração do servidor SMTP (TIdSMTP)
  ConfigSMTP;
  if FThread.CheckTerminated then
    abort;
  // Conexão e autenticação
  try
    IdSMTP.Connect;
  except
    if Assigned(FOnErrorConnection) then
    begin
      MessageError :=
        'Ocorreu um erro durante a conexão com o servidor de e-mail.';
      FOnErrorConnection(Self, MessageError);
    end;
    abort;
  end;
  if FThread.CheckTerminated then
    abort;
  try
    IdSMTP.Authenticate;
  except
    if Assigned(FOnErrorAuthentication) then
    begin
      MessageError :=
        'Conta configurada teve problemas de autenticação, verifique a conta!';
      FOnErrorAuthentication(Self, MessageError);
    end;
    abort;
  end;

  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnAfterConnect) then
    FOnAfterConnect(FAowner, IdSMTP.Connected);
  if FThread.CheckTerminated then
    abort;
end;

constructor TSend.Create;
begin
  FAnexos := TStringList.Create;
  FTexto := TStringList.Create;
  FDestino := TStringList.Create;
  FAccount := TAccount.Create;
end;

constructor TSend.Create(Aowner: TObject);
begin
  Create;
  FAowner := Aowner;
end;

constructor TSend.Create(Aowner: TObject; Account: TAccount);
begin
  Create(Aowner);
  Self.Account := Account;
end;

destructor TSend.Destroy;
begin
  // Desconecta do servidor
  if Assigned(IdSMTP) then
    IdSMTP.Disconnect;
  // Liberação dos objetos da memória
  if Assigned(SSL) then
    FreeAndNil(SSL);
  if Assigned(IdSMTP) then
    FreeAndNil(IdSMTP);
  // Elimina listas
  FreeAndNil(FAnexos);
  FreeAndNil(FTexto);
  FreeAndNil(FDestino);
  FreeAndNil(FAccount);
  inherited;
end;

procedure TSend.Execute;
var
  IdMessage: TIdMessage;
  IdText: TIdText;
  i: Integer;
begin
  if Assigned(FOnBeforeSend) then
    FOnBeforeSend(FAowner);
  if FThread.CheckTerminated then
    abort;
  Enviou := False;
  try

    // Conecta se ainda não estiver conectado
    if not Assigned(IdSMTP) or not IdSMTP.Connected then
      Connect;

    // Assume remetente com os dados de origem caso não informado
    if RemetenteEmail = EmptyStr then
      RemetenteEmail := Account.UserName;
    if RemetenteNome = EmptyStr then
      RemetenteNome := Account.UserName;

    // Cria mensagem
    IdMessage := TIdMessage.Create;
    // Configuração da mensagem
    IdMessage.From.Address := RemetenteEmail;
    IdMessage.From.Name := RemetenteNome;
    IdMessage.Subject := FAssunto;
    IdMessage.Encoding := meMIME;
    // Adiciona destinatários
    IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
    for i := 0 to FDestino.Count - 1 do
      IdMessage.Recipients.Add.Text := FDestino[i];
    // Configuração do corpo do email
    IdText := TIdText.Create(IdMessage.MessageParts);
    IdText.Body := FTexto;
    // Anexa anexos
    for i := 0 to FAnexos.Count - 1 do
      if FileExists(FAnexos[i]) then
        TIdAttachmentFile.Create(IdMessage.MessageParts, FAnexos[i]);

    if FThread.CheckTerminated then
      abort;

    // Envia e-mail
    IdSMTP.Send(IdMessage);
    Enviou := true;

  finally
    FreeAndNil(IdMessage);
    FreeAndNil(FDestino);
    FreeAndNil(FAnexos);
    FreeAndNil(FTexto);
  end;
end;

procedure TSend.Send;
begin
  // Cria thread para conectar se não conectado ainda e enviar email
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      Execute;
    end);
  with FThread do
  begin
    FreeOnTerminate := False;
    OnTerminate := TerminateThread;
    Start;
  end;
end;

procedure TSend.TerminateThread(Sender: TObject);
begin
  if Assigned(FOnAfterSend) then
    FOnAfterSend(FAowner, Enviou);
end;

procedure TSend.SetAccount(Value: TAccount);
begin
  FAccount := Value;
end;

procedure TSend.SetAfterSend(const Value: TAfterOperation);
begin
  FOnAfterSend := Value;
end;

procedure TSend.SetAnexos(const Value: TStringList);
begin
  FAnexos := Value;
end;

procedure TSend.SetAssunto(const Value: string);
begin
  FAssunto := Value;
end;

procedure TSend.SetBeforeSend(const Value: TNotifyEvent);
begin
  FOnBeforeSend := Value;
end;

procedure TSend.SetDestino(const Value: TStringList);
begin
  FDestino := Value;
end;

procedure TSend.SetOnAfterConnect(const Value: TAfterOperation);
begin
  FOnAfterConnect := Value;
end;

procedure TSend.SetOnBeforeConnect(const Value: TNotifyEvent);
begin
  OnBeforeConnect := Value;
end;

procedure TSend.SetOnErrorAuthentication(const Value: TErroAuthentication);
begin
  FOnErrorAuthentication := Value;
end;

procedure TSend.SetOnErrorConnection(const Value: TErroConnection);
begin
  FOnErrorConnection := Value;
end;

procedure TSend.SetRemetenteEmail(const Value: string);
begin
  FRemetenteEmail := Value;
end;

procedure TSend.SetRemetenteNome(const Value: string);
begin
  FRemetenteNome := Value;
end;

procedure TSend.SetTexto(const Value: TStringList);
begin
  FTexto := Value;
end;

function TSend.GetAccount: TAccount;
begin
  result := FAccount;
end;

function TSend.GetOnAfterConnect: TAfterOperation;
begin
  result := FOnAfterConnect;
end;

function TSend.GetOnBeforeConnect: TNotifyEvent;
begin
  result := OnBeforeConnect;
end;

function TSend.GetOnErrorConnection: TErroConnection;
begin
  result := OnErrorConnection;
end;

{ TReceive }

procedure TReceive.AdicionaEmail
  (Lista: System.Generics.Collections.TList<TIdMessage>; i: Integer);
var
  IdMessage: TIdMessage;
begin
  if FThread.CheckTerminated then
    abort;
  IdMessage := TIdMessage.Create(nil);
  IdPOP.Retrieve(i, IdMessage);
  Lista.Add(IdMessage);
  MsgPos := i;
  if Assigned(FOnReadMessage) then
    FOnReadMessage(FAowner, IdMessage);
  if FThread.CheckTerminated then
    abort;
end;

procedure TReceive.Cancel;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    if not FThread.Finished then
      FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TReceive.Connect;
var
  MessageError: string;
begin
  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnBeforeConnect) then
    FOnBeforeConnect(FAowner);
  if FThread.CheckTerminated then
    abort;
  if not Assigned(IdPOP) then
    IdPOP := TIdPop3.Create(nil);
  if not Assigned(SSL) then
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  begin
    if Account.FSMTP = smGmail then
    begin
      IdPOP.Host := 'pop.gmail.com';
      IdPOP.Port := 995;
    end
    else
    begin
      raise Exception.Create('Função não disponivel para Hotmail');
    end;
    IdPOP.UserName := Account.UserName;
    IdPOP.Password := Account.Password;
    SSL.Host := IdPOP.Host;
    SSL.Port := IdPOP.Port;
    SSL.Destination := SSL.Host + ':' + IntToStr(SSL.Port);
    IdPOP.IOHandler := SSL;
    IdPOP.UseTLS := utUseImplicitTLS;
    if FThread.CheckTerminated then
      abort;
    try
      IdPOP.Connect;
    except
      MessageError := 'Erro durante a conexão com o servidor de email!' + #13#10
        + 'Verifique a conta e se o servidor de e-mail está funcionando.';
      if Assigned(FOnErrorConnection) then
        FOnErrorConnection(Self, MessageError);
      abort;
    end;
  end;
  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnAfterConnect) then
    FOnAfterConnect(FAowner, IdPOP.Connected);
  if FThread.CheckTerminated then
    abort;
end;

constructor TReceive.Create;
begin
  Self.FAccount := TAccount.Create;
end;

constructor TReceive.Create(Aowner: TObject);
begin
  Create;
  FAowner := Aowner;
end;

constructor TReceive.Create(Aowner: TObject; Account: TAccount);
begin
  Create(Aowner);
  Self.Account := Account;
end;

destructor TReceive.Destroy;
var
  i: Integer;
begin
  if Assigned(FEmails) then
  begin
    for i := 0 to FEmails.Count - 1 do
    begin
      FEmails[i].Free;
      FEmails[i] := nil;
    end;
    FreeAndNil(FEmails);
  end;
  if Assigned(IdPOP) then
    FreeAndNil(IdPOP);
  if Assigned(SSL) then
    FreeAndNil(SSL);
  if Assigned(FAccount) then
    FreeAndNil(FAccount);
  inherited;
end;

procedure TReceive.Execute;
var
  i: Cardinal;
  MessageError: string;
begin
  MsgCount := 0;
  if Assigned(FOnBeforeReceive) then
    OnBeforeReceive(FAowner);
  if FThread.CheckTerminated then
    abort;
  if not Assigned(IdPOP) or not IdPOP.Connected then
    Connect;
  FEmails := TList<TIdMessage>.Create;
  try
    MsgCount := IdPOP.CheckMessages;
  except
    MessageError :=
      'Conta foi conectada, porem, ao tentar obter mensagem ocorreu um erro inesperado!';
    if Assigned(OnErrorReceiveMessage) then
      OnErrorReceiveMessage(Self);
    abort;
  end;
  if (Quantidade = 0) or (Quantidade > MsgCount) then
    MsgProcess := MsgCount
  else
    MsgProcess := Quantidade;
  if Ordem = toCrescente then
    for i := Inicial to MsgProcess - 1 Do
      AdicionaEmail(FEmails, i)
  else
    for i := MsgCount - Inicial downto MsgCount - Inicial - MsgProcess - 1 do
      AdicionaEmail(FEmails, i);
end;

procedure TReceive.Receive;
begin
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      Execute;
    end);
  with FThread do
  begin
    FreeOnTerminate := False;
    OnTerminate := TerminateThread;
    Start;
  end;
end;

procedure TReceive.TerminateThread(Sender: TObject);
begin
  if Assigned(FOnAfterReceive) then
    FOnAfterReceive(FAowner, Finished);
end;

procedure TReceive.SetAccount(Value: TAccount);
begin
  FAccount := Value;
end;

procedure TReceive.SetAfterReceive(const Value: TAfterOperation);
begin
  FOnAfterReceive := Value;
end;

procedure TReceive.SetBeforeReceive(const Value: TNotifyEvent);
begin
  FOnBeforeReceive := Value;
end;

procedure TReceive.SetInicial(const Value: Cardinal);
begin
  FInicial := Value;
end;

procedure TReceive.SetOnAfterConnect(const Value: TAfterOperation);
begin
  FOnAfterConnect := Value;
end;

procedure TReceive.SetOnBeforeConnect(const Value: TNotifyEvent);
begin
  OnBeforeConnect := Value;
end;

procedure TReceive.SetOnErrorConnection(const Value: TErroConnection);
begin
  FOnErrorConnection := Value;
end;

procedure TReceive.SetOnErrorReceiveMessage(const Value: TNotifyEvent);
begin
  FOnErrorReceiveMessage := Value;
end;

procedure TReceive.SetOrdem(const Value: TOrder);
begin
  FOrdem := Value;
end;

procedure TReceive.SetQuantidade(const Value: Cardinal);
begin
  FQuantidade := Value;
end;

procedure TReceive.SetReadMessage(const Value: TReadMessage);
begin
  FOnReadMessage := Value;
end;

function TReceive.GetAccount: TAccount;
begin
  result := FAccount;
end;

function TReceive.GetFinished: Boolean;
begin
  if (MsgCount = 0) or (MsgProcess < MsgCount) then
    result := False
  else if MsgProcess = MsgPos then
    result := true
  else
    result := False;
end;

function TReceive.GetOnAfterConnect: TAfterOperation;
begin
  result := FOnAfterConnect;
end;

function TReceive.GetOnBeforeConnect: TNotifyEvent;
begin
  result := OnBeforeConnect;
end;

function TReceive.GetOnErrorConnection: TErroConnection;
begin
  result := OnErrorConnection;
end;

{ TAccount }

procedure TAccount.Assign(Source: TPersistent);
begin
  if Source is TAccount then
    with TAccount(Source) do
    begin
      Self.Password := Password;
      Self.UserName := UserName;
    end
  else
    inherited Assign(Source);
end;

constructor TAccount.Create;
begin
  FName := 'Default';
end;

procedure TAccount.DiscoverySMTP;
begin
  if Pos('@HOTMAIL', UpperCase(Self.FUserName)) > 0 then
    FSMTP := smHotmail
  else if Pos('@GMAIL', UpperCase(Self.FUserName)) > 0 then
    FSMTP := smGmail
  else
    FSMTP := smNenhum;
end;

function TAccount.IsValid: Boolean;
begin
  result := (FSMTP <> smNenhum) and (Password <> EmptyStr);
end;

function TAccount.FileNameDefault: String;
begin
  result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Config\Email.ini';
end;

procedure TAccount.LoadFromFile(NameLoad: string = 'Default');
var
  Ini: TIniFile;
begin
  if not FileExists(FileNameDefault) then
    exit;
  Ini := TIniFile.Create(FileNameDefault);
  try
    if Ini.SectionExists(NameLoad) then
    begin
      UserName := Ini.ReadString(NameLoad, 'Username', EmptyStr);
      Password := Ini.ReadString(NameLoad, 'Password', EmptyStr);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TAccount.LoadFromFileByName;
begin
  LoadFromFile(FName);
end;

procedure TAccount.SaveToFile;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileNameDefault);
  try
    Ini.WriteString(Name, 'Username', FUserName);
    Ini.WriteString(Name, 'Password', FPassword);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TAccount.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TAccount.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAccount.SetUserName(const Value: String);
begin
  FUserName := Value;
  DiscoverySMTP;
end;

end.
