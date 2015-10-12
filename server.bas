' The Land of Stuff Server
' by Andrew Wang
' 2009-2010
'

Const CLIENTVERSION = 20
Const MAX_PLAYERS = 255

Const PORT = 8501

shell "echo Server is now running!"
Declare Sub checkrecv (zcur As Any ptr)
Declare Sub SendMSG (msg$,length,n As integer)
Declare Sub CreateSocketInformation (s As integer)
Declare Sub FreeSocketInformation(index As integer)
Declare Function StrTok$(Srce$,Delim$)

#include "win\winsock2.bi"
#include "file.bi"
#include "vbcompat.bi"

extern "c"
Declare Function strlen (ByVal As zstring ptr) As size_t
End extern

Sub FD_SETx(fd,set)															
		dim i As integer											
																	
		For i = 0 to cptr(fd_set ptr, set)->fd_count-1				
			If( cptr(fd_set ptr, set)->fd_array(i) = fd ) Then		
				Exit For											
			End If													
		Next 														
																	
		If( i = cptr(fd_set ptr, set)->fd_count ) Then				
			If( cptr(fd_set ptr, set)->fd_count < FD_SETSIZE ) Then	
				cptr(fd_set ptr, set)->fd_array(i) = fd				
				cptr(fd_set ptr, set)->fd_count += 1				
			End If													
		End If
End Sub

Type PlayerType
   x As integer
   y As integer
   shirtcol As integer
   pantcol As integer
   user As string
   pass As string
   speech As string
   sptime As integer
   bank(1 to 20) As integer
   sword As byte
   shield As byte
   thread As Any ptr
   used As Byte
   fighttime As Single
   whofight As integer 
   fightmsg As string
End Type

Type SocketType
   buffer As string
   databuf As wsabuf
   socket As socket
   sendbytes As integer
   recvbytes As integer
End Type

dim shared TotalSockets As integer = 0
dim Shared socketlist(MAX_PLAYERS) As SocketType

dim shared Player(MAX_PLAYERS) As PlayerType

Print "The Land of Stuff server v0.2"
Print "by Andrew Wang"
Print

' Make users directory if it doesn't already exist
dim cur_folder as string = Curdir
if chdir("Users") then
   mkdir("Users")
else
   chdir(cur_folder)
end if

dim shared As socket listensock,acceptsock
dim shared As sockaddr_in inetaddr
dim shared As wsadata wsad
dim shared As fd_set writer,reader
dim shared As ulong nonblock = 1
dim shared As integer flags

dim shared none As playertype = player(0)

If (WSAStartup(&h202, @wsad))=-1 Then
   If (WSAStartup(&h101, @wsad))=-1 Then End
End If

ListenSock = WSASocket(AF_INET, SOCK_STREAM, 0, 0, 0,WSA_FLAG_OVERLAPPED)
If listensock = socket_error Then Print "FAIL":End

InetAddr.sin_family = AF_INET
InetAddr.sin_addr.s_addr = htonl(INADDR_ANY)
InetAddr.sin_port = htons(PORT)

d = bind(ListenSock,@InetAddr, sizeof(InetAddr))
If d = socket_error Then Print "FAIL":End

d = listen(ListenSock, 5)
If d = socket_error Then Print "FAIL":End

'd = ioctlsocket(ListenSock, FIONBIO, @NonBlock)
If d = socket_error Then Print "FAIL":End
          
dim dat As zstring * 4000
                     dim Inv(8) As integer
                     dim choice(4000) As byte
                     dim sp(4000) As byte
                     dim fe(4000) As byte
                     dim accum(4000) As byte
                     dim usage(4000) As Byte

Do
   FD_ZERO (@reader)
   FD_ZERO (@writer)
   FD_SETx (listensock,@reader)
   
   For i = 0 to TotalSockets - 1
         If (SocketList(i).RecvBytes > SocketList(i).SendBytes) Then
              FD_SETx(SocketList(i).Socket, @Writer)
         Else
              FD_SETx(SocketList(i).Socket, @Reader)
         End If
   Next i                                               
   
   Total = Selectsocket(0, @Reader, @Writer, 0, 0)

   If (FD_ISSET(ListenSock, @Reader)) Then
             Total-=1
             AcceptSock = accept(ListenSock, 0, 0)
             
             NonBlock = 1
             'd = ioctlsocket(acceptsock, FIONBIO, @NonBlock)
             If d = socket_error Then Print "FAIL"

             CreateSocketInformation(AcceptSock)
   End If

   For i = 0 to TotalSockets - 1
        dim SocketInfo As SocketType = SocketList(i)
        If Total = 0 Then Exit For
        
        If Player(cur).fighttime >= Timer + 0.2 Then
        	  Player(cur).fighttime = 0
        	  If Player(Player(cur).whofight).fighttime = 0 Then
        	  	  SendMSG Player(cur).fightmsg,Len(Player(cur).fightmsg),Player(cur).whofight
        	  	  Player(cur).fighttime = timer
        	  End If
        End If

        If (FD_ISSET(SocketInfo.Socket, @Reader)) Then
            Total-=1

            'SocketInfo.DataBuf.buf = SocketInfo.Buffer
            'SocketInfo.DataBuf.len = 4000
            
            d = recv(SocketInfo.socket,@dat,4000,0)
            mid$(dat,d+1) = string$(4000-d,Chr$(0))
            
            If d = -1 Then
               FreeSocketInformation(i)               
            End If

            SocketInfo.RecvBytes = length
            
            SocketList(i).buffer = rtrim$(dat)
            
            cur = i
            
            '' COMMANDS
               cmd$ = left$(dat,instr(dat," ")-1)
               param$ = mid$(dat,len(cmd$)+2)
               If cmd$ = "" Then cmd$ = dat: param$ = ""

               Select Case(cmd$)
                  Case "CLIENT"                  
                     'Print "Client";cur
                     SendMSG "CLIENT "+str$(CLIENTVERSION),len("CLIENT "+str$(CLIENTVERSION)),cur
                  Case "ONLINE"
                     n = 0
                     For playernumber = 0 to MAX_PLAYERS
                        If Player(playernumber).user <> "" Then n += 1
                     Next playernumber
                     number$ = str$(n)
                     SendMSG "ONLINE "+number$,len("ONLINE "+number$),cur
                  Case "NEWUSR"' New user
                     user$ = left$(param$,instr(param$," ")-1)
                     pass$ = mid$(param$,len(user$)+2)
                     
                     If FileExists("Users\"+user$+".txt") Then  ' User already exists
                        SendMSG "BAD",3,cur
                     Else
                        ' Create user
                        Open "Users\"+user$+".txt" For output As #1
                        Print #1,user$
                        Print #1,pass$  ' It's already encrypted when the client sends it to us
                        Print #1,315    ' Start x
                        Print #1,329    ' Start y
                        Print #1,3
                        Print #1,0
                        Print #1,3
                        Print #1,0
                        Print #1,10
                        Print #1,10
                        Print #1,10
                        Print #1,rgb(255,255,255)     ' It's only 100% white before you set it differently
                        Print #1,rgb(255,255,255)     ' (i.e. first time playing)
                        Print #1,1
                        Print #1,5
                        Print #1,9
                        Print #1,0
                        Print #1,0
                        Print #1,0
                        Print #1,0
                        Print #1,0
                        Print #1,"X"
                        For x = 1 to 20
                           Print #1,0
                        Next x
                        Close #1
                        SendMSG "GOOD",4,cur
                     End If                     
                  Case "LGON"  ' Logon
                     
                     user$ = left$(param$,instr(param$," ")-1)
                     pass$ = mid$(param$,len(user$)+2)
                     
                     notgood = 0
                     For x = 0 to MAX_PLAYERS
                        If Player(x).user = user$ Then notgood = 1:SendMSG "2USER",5,cur   ' If someone else is logged in too
                     Next x

                     If FileExists("Users\"+user$+".txt") And notgood = 0 Then
                        Player(cur).user = user$
                        Player(cur).pass = pass$
                     
                        Open "Users\"+user$+".txt" For input As #1
                        line input #1,u$
                        line input #1,p$
                        input #1,px
                        input #1,py
                        input #1,ps,pse
                        input #1,pd,pde
                        input #1,php,pmaxhp
                        input #1,pmoney
                        input #1,shirtcol
                        input #1,pantcol
                        For x = 1 to 8
                           input #1,Inv(x)
                        Next x
                        
                        For x = 0 To 1000
                        	usage(x) = 0
                        	sp(x) = 0
                        	fe(x) = 0
                        	accum(x) = 0
                        Next

                        For x = 0 to 1000
                           input #1,b$
                           If b$ <> "X" Then
                              usage(val(b$)) = 1
                              input #1,sp(val(b$))
                              input #1,fe(val(b$))
                              input #1,accum(val(b$))
                           Else
                              Exit For
                           End If
                        Next x
                        For x = 1 to 20
                           input #1,player(cur).Bank(x)
                        Next x                        
                        Close #1
                        
                        If p$ = pass$ Then
                           msg$ = "L "
                           msg$ += str$(px) + " " + str$(py) + " "
                           msg$ += str$(ps) + " " + str$(pse) + " "
                           msg$ += str$(pd) + " " + str$(pde) + " "
                           msg$ += str$(php) + " " + str$(pmaxhp) + " "
                           msg$ += str$(pmoney) + " "
                           msg$ += str$(shirtcol) + " " + str$(pantcol) + " "
                           For x = 1 to 8
                              msg$ += str$(Inv(x)) + " "
                           Next x
                           For x = 0 to 1000
                              If choice(x) > 0 Or sp(x) > 0 Or fe(x) > 0 Or accum(x) > 0 Then
                                 msg$ += str$(x)+" "+str$(sp(x))+" "+str$(fe(x))+" "+str$(accum(x))+" "
                              End If
                           Next x
                           Player(cur).x = px
                           Player(cur).y = py
                           Player(cur).shirtcol = shirtcol
                           Player(cur).pantcol = pantcol
                           Player(cur).user = user$
                           SendMSG msg$,4000,cur
                           msg$ = "BANK "
                           For ii = 1 to 20
                              msg$ += str$(player(cur).bank(ii)) + " "
                           Next ii
                           
                           SendMSG msg$,200,cur
                        Else
                           ' Wrong password!
                           SendMSG "BADLOGIN",8,cur
                           Player(cur).user = ""
                           Player(cur).pass = ""
                        End If
                     Else
                        ' Wrong username!
                        If notgood = 0 Then SendMSG "BADLOGIN",8,cur    ' If it's 1 then it's already covered
                        Player(cur).user = ""
                        Player(cur).pass = ""
                     End If
                  Case "BANK"  ' Save bank data
                     Player(cur).bank(1) = val(strtok$(param$," "))
                     For x = 2 to 20
                        Player(cur).bank(x) = val(strtok$(""," "))
                     Next x
                  Case "LGOF"  ' Log off
                     ' Save data back into file
                     Open "Users\"+Player(cur).user+".txt" For output As #1
                        Print #1,Player(cur).user
                        Print #1,Player(cur).pass
                        Print #1,val(strtok$(param$," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        For x = 1 to 8
                           Print #1,val(strtok$(""," "))
                        Next x

                        For x = 0 to 1000
                           a$ = strtok$(""," ")
                           b$ = strtok$(""," ")
                           c$ = strtok$(""," ")
                           d$ = strtok$(""," ")
                           If a$ <> "X" And a$ <> "" And b$ <> "" And c$ <> "" And d$ <> "" Then
                              Print #1,a$
                              Print #1,b$
                              Print #1,c$
                              Print #1,d$
                           End If
                        Next x                        
                        Print #1,"X"
                        For x = 1 to 20
                           Print #1,Player(cur).bank(x)
                        Next x
                     Close #1
                     
                     'SendMSG "CF",2,cur
                     'closesocket plsk(cur)
                     'plsk(cur) = 0
                     Player(cur).x = -1
                     
                     'Exit For
                  Case "CLOTHES"   ' Define color of clothes
                     shirt$ = left$(param$,instr(param$," ")-1)
                     pants$ = mid$(param$,len(shirt$)+2)
                     shirt = val(shirt$)
                     pants = val(pants$)
                     Player(cur).shirtcol = shirt
                     Player(cur).pantcol = pants
                  Case "M"     ' Move
                     x$ = left$(param$,instr(param$," ")-1)
                     y$ = mid$(param$,len(x$)+2)

                     If not (val(x$) = 0 And x$ <> "0") Or (val(y$) = 0 And y$ <> "0") Then
                        x = val(x$)
                        y = val(y$)                  
                        Player(cur).x = x                                                 ' Move and Teleport used to be different
                        Player(cur).y = y                                                 ' but now they're exactly the same
                     End If                                                               ' oh well
                  Case "TELE"   ' Teleport
                     x$ = left$(param$,instr(param$," ")-1)
                     y$ = mid$(param$,len(x$)+2)

                     If not (val(x$) = 0 And x$ <> "0") Or (val(y$) = 0 And y$ <> "0") Then
                        x = val(x$)
                        y = val(y$)
                        Player(cur).x = x
                        Player(cur).y = y
                     End If
                  Case "G"     ' Get players' positions & info
                     m$ = "G "
                     For ii = 0 to MAX_PLAYERS
                        If ii <> cur And Player(ii).user <> "" Then
                           m$+=str$(Player(ii).x)+" "+str$(Player(ii).y)+" "+str$(Player(ii).shirtcol)+" "+str$(Player(ii).pantcol)+" "+str$(ii)+" "+str$(Player(ii).user)+" "+str$(Player(ii).sword)+str$(Player(ii).shield)+" "
                        End If
                     Next ii
                     m$ = rtrim$(m$)                 
                     SendMSG m$,len(m$),cur
                  Case "SAY"    ' Say stuff
                     Player(cur).speech = param$
                     Player(cur).sptime = Timer
                  Case "TELL"   ' Buddy chat
                     who$ = strtok$(param$," ")
                     what$ = mid$(param$,instr$(param$," ")+1)
                     whoto$ = strtok$(what$," ")
                     what$ = mid$(what$,instr$(what$," ")+1)
                     gsendto = -1
                     For x = 0 to MAX_PLAYERS
                        If Player(x).user = whoto$ Then gsendto = x
                     Next x
                     If gsendto >= 0 Then
                        SendMSG "TELL "+who$+" tells you: "+what$,len("TELL "+who$+" tells you: "+what$),gsendto
                        SendMSG "TELL You tell "+whoto$+": "+what$,len("TELL You tell "+whoto$+": "+what$),cur
                     Else
                        SendMSG "TELL That player is either offline or doesn't exist.",len("TELL That player is either offline or doesn't exist."),cur
                     End If
                  Case "SAVE"   ' Save; must do this once in a while in case the client crashes, to save data
                     Open "Users\"+Player(cur).user+".txt" For output As #1
                        Print #1,Player(cur).user
                        Print #1,Player(cur).pass
                        Print #1,val(strtok$(param$," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        Print #1,val(strtok$(""," "))
                        For x = 1 to 8
                           Print #1,val(strtok$(""," "))
                        Next x
                        For x = 0 to 1000
                           a$ = strtok$(""," ")
                           b$ = strtok$(""," ")
                           c$ = strtok$(""," ")
                           d$ = strtok$(""," ")
                           If a$ <> "X" And a$ <> "" And b$ <> "" And c$ <> "" And d$ <> "" Then
                              Print #1,a$
                              Print #1,b$
                              Print #1,c$
                              Print #1,d$
                           End If
                        Next x
                        Print #1,"X"
                        For x = 1 to 20
                           Print #1,player(cur).bank(x)
                        Next x
                     Close #1                     
                  Case "S"      ' Get current speech
                    
                     m$ = "S "
                     
                     For ii = 0 to MAX_PLAYERS
                        If Player(ii).used <> 0 And Timer - Player(ii).sptime < 3 And Player(ii).speech <> "" And Player(ii).x > Player(cur).x - 20 And Player(ii).x < Player(cur).x + 20 And Player(ii).y > Player(cur).y - 16 And Player(ii).y < Player(cur).y + 16 Then
                           m$ += Chr$(34)   ' quotes
                           m$ += Player(ii).user+": " + Player(ii).speech
                           m$ += Chr$(34) + " "
                        End If
                     Next ii
                     m$ = rtrim$(m$)
                     
                     SendMSG m$,len(m$),cur
                  Case "FIGHT"  ' Someone fights you
                     hp$ = (strtok$(param$," "))+" "
                     maxhp$ = (strtok$(""," "))+" "
                     Def$ = (strtok$(""," "))+" "
                     num = val(strtok$(""," "))
                     msg$ = hp$+maxhp$+Def$+str$(cur)
                     Player(cur).fightmsg = msg$
                     SendMSG "FIGHT "+msg$,len("FIGHT "+msg$),num
                     Player(cur).fighttime = Timer
                     Player(cur).whofight = num
                     'SendMSG "FIGHT "+hp$+maxhp$+Def$+str$(cur),len("FIGHT "+hp$+maxhp$+Def$+str$(cur)),cur
                     Print "FIGHT";num
                  Case "MAGIC"  ' Info about magic-related stuff
                     who = val(mid$(param$,3))
                     param$ = left$(param$,2)
                     SendMSG "MAGIC "+param$,len("MAGIC "+param$),who
                  Case "HIT"    ' How much you hit
                     hit$ = left$(param$,instr(param$," ")-1)
                     guy$ = mid$(param$,len(hit$)+2)
                     SendMSG "HIT "+hit$,len("HIT "+hit$),val(guy$)
                  Case "HP"     ' How much HP left
                     hp$ = left$(param$,instr(param$," ")-1)
                     guy$ = mid$(param$,len(hp$)+2)
                     SendMSG "HP "+hp$,len("HP "+hp$),val(guy$)                     
                  Case "LOSE"   ' Aw, you lost. Oh well
                     SendMSG "LOSE",4,val(param$)
                  Case "TRADE"  ' Ask for trade
                     SendMSG "TRADE "+str$(cur),len("TRADE "+str$(cur)),val(param$)
                  Case "ITEMS"  ' Give items
                     guy$ = left$(param$,instr(param$," ")-1)
                     items$ = mid$(param$,len(guy$)+2)
                     SendMSG "ITEMS "+items$,len("ITEMS "+items$),val(guy$)
                  Case "ADDI"   ' Add item
                     theitem$ = left$(param$,instr(param$," ")-1)
                     guy$ = mid$(param$,len(theitem$)+2)
                     SendMSG "ADDI "+theitem$,len("ADDI "+theitem$),val(guy$)
                  Case "DELI"   ' Delete item
                     theitem$ = left$(param$,instr(param$," ")-1)
                     guy$ = mid$(param$,len(theitem$)+2)
                     SendMSG "DELI "+theitem$,len("DELI "+theitem$),val(guy$)
                  Case "PAY"    ' Change payment
                     money$ = left$(param$,instr(param$," ")-1)
                     guy$ = mid$(param$,len(money$)+2)
                     SendMSG "PAY "+money$,len(money$)+4,val(guy$)
                  Case "ACCEPT" ' Accept trade
                     SendMSG "ACCEPT",6,val(param$)
                  Case "NOACCEPT"   ' When player changes value
                     SendMSG "NOACCEPT",8,val(param$)
                     SendMSG "NOACCEPT",8,cur
                  Case "DECLINE"' Decline trade
                     SendMSG "DECLINE",7,val(param$)
                     SendMSG "DECLINE",7,cur
                  Case "SW"     ' Equip / dequip sword
                     If Player(cur).sword = 0 Then Player(cur).sword = 1:Else Player(cur).sword = 0
                  Case "SH"     ' Equip / dequip shield
                     If Player(cur).shield = 0 Then Player(cur).shield = 1:Else Player(cur).shield = 0
                  Case "~"      ' Nothing
                  Case Else
                     cmi = 1
               End Select
               If cmi = 0 Then SendMSG "CF",2,cur: Else cmi = 0

      End If              

   Next i     
           
   If (FD_ISSET(SocketInfo.Socket, @Writer)) Then
          Total-=1

          SocketInfo.DataBuf.buf = SocketInfo.Buffer + SocketInfo.SendBytes
          SocketInfo.DataBuf.len = SocketInfo.RecvBytes - SocketInfo.SendBytes

          WSASend(SocketInfo.Socket, @(SocketInfo.DataBuf), 1, @SendBytes, 0, 0, 0)
       
   Else
      
          SocketInfo.SendBytes += SendBytes

          If (SocketInfo.SendBytes = SocketInfo.RecvBytes) Then
             SocketInfo.SendBytes = 0
             SocketInfo.RecvBytes = 0
          End If
   End If

loop

Function StrTok$(Srce$,Delim$)
static Start%, SaveStr$

   ' If first call, make a copy of the string.
   If Srce$<>"" Then
      Start%=1 : SaveStr$=Srce$
   End If

   BegPos%=Start% : Ln%=len(SaveStr$)
   ' Look for start of a token (character that isn't delimiter).
   while BegPos%<=Ln% And instr(Delim$,mid$(SaveStr$,BegPos%,1))<>0
      BegPos%=BegPos%+1
   Wend
   ' Test for token start found.
   If BegPos% > Ln% Then
      StrTok$="" : Exit Function
   End If
   ' Find the end of the token.
   EndPos%=BegPos%
   while EndPos% <= Ln% And instr(Delim$,mid$(SaveStr$,EndPos%,1))=0
      EndPos%=EndPos%+1
   Wend
   StrTok$=mid$(SaveStr$,BegPos%,EndPos%-BegPos%)
   ' Set starting point for search for next token.
   Start%=EndPos%

End Function
  
Sub CreateSocketInformation(s As integer)
     dim SI As SocketType

' Prepare SocketInfo structure For use.

     SI.Socket = s
     SI.SendBytes = 0
     SI.RecvBytes = 0

     SocketList(TotalSockets) = SI
     Player(TotalSockets).used = 1

     TotalSockets+=1
     
     Print "New connection"
End Sub

Sub FreeSocketInformation(Index As integer)
     dim SI As sockettype = SocketList(Index)

     closesocket(SI.Socket)
     
     Player(index) = none
     For i = Index to MAX_PLAYERS-1
         SocketList(i) = SocketList(i+1)
         Player(i) = Player(i+1)
     Next i

     TotalSockets-=1
     
     Print "Connection closed"
End Sub

Sub SendMSG (msg As string, length As integer,n As integer)
   dim l As integer = strlen(msg)
   dummy = Send(SocketList(n).socket,"LS",3,0)       ' Send signature
   dummy = Send(SocketList(n).socket,@l,4,0)         ' Send length
   dummy = Send(SocketList(n).socket,msg,l,0)        ' Send message
End Sub