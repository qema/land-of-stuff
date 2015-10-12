#Include "win\winsock.bi"
#include "file.bi"
#include "win\wininet.bi"
#Define WIN32_LEAN_AND_MEAN
#Include "windows.bi"
#inclib "winmm"

Declare Function MciSendString Lib "winmm.dll" alias _
         "mciSendStringA" (ByVal lpstrCommand As String, ByVal  _
         lpstrReturnString As size_t, ByVal uReturnLength As Long, ByVal _
         hwndCallback As Long) As Long
 
extern "c"
Declare Function strlen (ByVal As zstring ptr) As size_t
End extern

Const CLIENTVERSION = 20
Const MAX_PLAYERS = 255

randomize Timer

' The Land of Stuff Client v0.1
' by Andrew Wang
'

Type NPCType
   x As integer
   y As integer
   name As string
   speech(99) As string
   reply(99) As string
   choice As byte
   sp As byte
   fe As byte
   accum As byte
   o As byte
   num As integer
   typ As integer
End Type

Type EnemyType
   name As string * 24
   str As integer
   Def As integer
   hp As integer
   mindrop As integer
   maxdrop As integer
   img As Any ptr
   item1 As integer
   item2 As integer
End Type

Type EnemyLocType
   startx As integer
   starty As integer
   x As single
   y As single
   id As byte
   dir As byte
   turned As byte
   used As byte
   respawn As integer
   hp As integer
   maxhp As integer
End Type

Type ItemType
   name As string
   cost As integer
   effect As integer
   Type As string * 1
   reqlvl As Integer
End Type

Type PosType
   ox As single
   oy As single
   nx As single
   ny As single
   pt As single
   ct As single
   x As single
   y As single
   s As integer
   p As integer
   u As integer
   sw As byte
   sh As byte
   m As string
   n As string
   w As byte
End Type

Const dup = 0,dright = 1,ddown = 2,dleft = 3,bottom = 4,walking = 4

Declare Function Encrypt$(a As string)
Declare Sub SendMSG (msg As string, length As integer)
Declare Sub SMNE (msg As string, length As integer)
Declare Sub checkrecv ()
Declare Sub Move ()
Declare Sub Redraw ()
Declare Sub Init ()
Declare Function Tile (n$) As Any ptr
Declare Sub MoveEnemies ()
Declare Sub CheckLevel (e)
Declare Sub ER (a,b,c,d,e,f)
Declare Sub DoScript ()
Declare Function InInv(n$) As byte
Declare Sub AddInv(i)
Declare Sub Trade ()
Declare Sub InputX (n$,s$)
Declare Sub SetClothes ()
Declare Sub Shop ()
Declare Sub DoBank ()
Declare Sub Interpolate ()
Declare Sub W ()
Declare Function dlfile(sURL As String,LFileName As String) As Integer
Declare Sub Fight (i,x,y)
Declare Sub Attack (x)
Declare Sub CallUpdater ()

Open "config.ini" For input As #1 
line input #1,ip$
Close

dim wsad As WSAData, sa As sockaddr_in
dim shared curmsg As string

dim shared user As string
dim shared playerx As integer,playery As Integer
dim shared oldplayerx As integer,oldplayery As integer
dim shared startx As integer,starty As integer
dim shared GuyImg(8) As Any ptr
dim shared cdir As byte, walkcycle As byte,udlr As byte

dim shared shirtcolor As integer
dim shared pantcolor As integer

dim shared swordimg As Any ptr
dim shared shieldimg As Any ptr
dim shared shbkimg As Any ptr
dim shared ssimg As Any ptr
         
dim shared swimg As Any ptr
dim shared shimg As Any ptr

dim shared sword As integer, shield As integer, magic As integer
dim shared Inv(1 to 8) As integer
dim shared Item(100) As ItemType

dim shared Map(999,999) As string * 1
dim shared Tiles(20) As Any ptr
dim shared Enemy(99) As EnemyType
dim shared EnemyLoc(999) As EnemyLocType
dim shared NPC (399) As NPCType

dim shared OtherPos(MAX_PLAYERS) As PosType
dim shared Message(5) As string
dim shared speechbar As string

dim shared playerstr = 3,playerdef = 3
dim shared strexp = 0,defexp = 0

dim shared money = 10,validmoney = 10

dim shared playerhp = 10, maxhp = 10

dim shared scrmsg As string,msgtime As integer

dim shared Killed(99) As integer,kn As integer

dim shared fightmode As byte
dim shared netdelay As integer
dim shared redrawflag As byte
dim shared curfight As integer
dim shared healtime As integer

dim shared youtrade As integer=-1, theytrade As integer=-2
dim shared theyaccept As byte
dim shared theysell(1 to 8) As byte,theypay As integer

dim shared dontblockon As integer
dim shared menumsg As string

dim shared savecount As integer = 500

dim shared userapproved As byte = 0,userused As byte = 0

dim shared Bank(1 to 20) As integer

dim shared alwayssend As integer
dim shared theenemy As integer = -1

dim shared wlwarning As Any ptr
dim shared youfight As integer = -2,theyfight As integer = -1
dim shared otherdef As integer,otherhp As integer,othermaxhp As Integer,otherprev As integer
dim shared theylose As integer = 0,sentfight As integer = 0
dim shared theyusemagic As integer = 0,theyuseshield As integer = 0
Dim Shared nofighttime As Integer = 30

Dim Shared difx As Integer,dify As integer
Dim Shared moved As Integer

dim shared currentkeypress$

If (WSAStartup(&h202, @wsad))=-1 Then
   If (WSAStartup(&h101, @wsad))=-1 Then End
End If

' Initialize
dim shared s As socket

windowtitle "The Land of Stuff"
screenres 640,480,32,2
  
Init

dim t As Any ptr
t = threadcreate(@checkrecv)    ' Create thread

Do  
   For x = 0 to ubound(Killed)
      Killed(x) = -1
   Next x
   For x = 0 to ubound(NPC)
      NPC(x).choice = 0
      NPC(x).sp = 0
      NPC(x).fe = 0
      NPC(x).accum = 0
   Next x
   
drawmenu:
   closesocket s
   
   scrmsg = ""
   msgtime = 0
   userapproved = 0
   udlr = 5
   cdir = ddown
   theyfight = -1
   youfight = -2
   sentfight = 0
   theylose = 0
   ' Login screen
   screenlock
   screenset 1,0
   bload "data\login.bmp"
   For y = 0 to 239
      For x = 0 to 319
         n = point(x,y)
         screenset 0,0
         pset (x*2,y*2),n
         pset (x*2+1,y*2+1),n
         pset (x*2+1,y*2),n
         pset (x*2,y*2+1),n
         screenset 1,0
      Next x
   Next y

   screenset 0,0
   draw string (520,2),"by Andrew Wang",0
   draw string (100,204),"Username:",0
   line (190,200)-(408,220),0,b
   draw string (100,244),"Password:",0
   line (190,240)-(408,260),0,b

   If menumsg = Chr$(20) Then Locate 1,1:Print "The client has been updated! Please download the newest one at landofstuff.net":menumsg = ""
   draw string (10,460),menumsg,rgb(0,0,0)
   menumsg = ""
   screenunlock
      
   user = ""
   p$ = ""
   
   d = getmouse (mx,my,,b)
   Do Until (mx >= 530 And mx <= 608 And my >= 442 And my <= 466 And b = 1) And user <> "" And p$ <> ""
      W
      d = getmouse (mx,my,,b)
      l$ = ""
      s$ = user
      Do
         W
         k$ = inkey$
         If k$ = Chr$(255)+"k" Then End
      loop Until k$ = ""
      
      If mx >= 386 And mx <= 518 And my >= 442 And my <= 466 And b = 1 Then
               s = opensocket( AF_INET, SOCK_STREAM, IPPROTO_TCP )
               If s <= 0 Then
                  Print "Unable to open socket!"
                  End
               End If
               sa.sin_port = htons( 8501 )
               sa.sin_family = AF_INET
               sa.sin_addr.S_addr = inet_addr(ip$)
               
               ncon = Connect( s, @sa, Len( sa ) )
               If ncon = socket_error Then menumsg = "Unable to connect to server!":goto drawmenu

         n = point (0,0)
         Do
            username$ = "LOGIN"
            password$ = "LOGIN"
            userisgood = 0
            Do Until instr$(username$," ") = 0 And Len(username$) >= 2 And Len(username$) <= 26 And Len(password$) >= 4 And Len(password$) <= 26 And userisgood = 1
               loginmenuyes = 1
               line (0,0)-(639,479),n,bf
               Draw String (591,471),"Back"
               draw string (20,20),"New User"
               Locate 15,2
               If userused Then Print "Sorry, that username is already taken!";:userused = 0
               Locate 10,2
               InputX "Enter your desired username (2-26 characters, no spaces): ",username$
               If username$ = "\ @" Then GoTo drawmenu
               Locate 11,2
               InputX "Enter your desired password (4-26 characters): ",password$
               If password$ = "\ @" Then GoTo drawmenu
               Locate 13,2
               loginmenuyes = 0
               
               Open username$ For output As #1
               Close #1
               If FileExists(username$) Then userisgood = 1
               kill username$
            Loop

            userused = 0
            Print "Contacting server...";
            SendMSG "NEWUSR "+username$+" "+Encrypt$(password$),Len("NEWUSR "+username$+" "+Encrypt$(password$))
            Do Until curmsg = "":loop
            Do Until userused <> 0:loop
         loop Until userused = 2
         userused = 0
         closesocket s
         goto drawmenu
      End If
      If mx >= 190 And mx <= 408 And my >= 200 And my <= 220 And b = 1 Then
         draw string (100,204),"Username:",rgb(200,200,200)
         Do Until l$ = Chr$(13) Or (b = 1 And not (mx >= 190 And mx <= 408 And my >= 200 And my <= 220))
            l$ = inkey$
            If l$ = Chr$(255) + "k" Then End
            s$ += l$
            If l$ = Chr$(13) Then l$ = "":enterpass = 1:user = s$:Exit Do
            If Len(s$) > 26 And l$ <> Chr$(8) Then s$ = left$(s$,26)
            If l$ = Chr$(8) Then s$ = left$(s$,Len(s$)-2):line (191,201)-(407,219),rgb(47,54,153),bf
            draw string (194,204),s$
            d = getmouse (mx,my,,b)
            user = s$
         loop
         If right$(s$,1) = Chr$(13) Then s$ = left$(s$,Len(s$)-1)
         user = s$
      Else
         enterpass = 0
      End If
      l$ = ""
      s$ = p$
      draw string (100,204),"Username:",0
      If (mx >= 190 And mx <= 408 And my >= 240 And my <= 260 And b = 1) Or enterpass = 1 Then
         draw string (100,244),"Password:",rgb(200,200,200)
         Do Until l$ = Chr$(13) Or (b = 1 And not (mx >= 190 And mx <= 408 And my >= 240 And my <= 260))
            l$ = inkey$
            If l$ = Chr$(255) + "k" Then End
            s$ += l$
            If l$ = Chr$(13) Then l$ = "":exitdo = 1:p$ = s$:Exit Do
            If Len(s$) > 26 And l$ <> Chr$(8) Then s$ = left$(s$,26)
            If l$ = Chr$(8) Then s$ = left$(s$,Len(s$)-2):line (191,241)-(407,259),rgb(47,54,153),bf
            draw string (194,244),string$(Len(s$),"*")
            d = getmouse (mx,my,,b)
            p$ = s$
         loop
         If right$(s$,1) = Chr$(13) Then s$ = left$(s$,Len(s$)-1)
         p$ = s$      
      End If
      draw string (100,204),"Username:",0
      draw string (100,244),"Password:",0
      If exitdo = 1 Then Exit Do
   loop
   Do Until b = 0
      d = getmouse (mx,my,,b)
      W
   loop
   For x = 1 to 30
      If right$(user,1) = Chr$(13) Then user = left$(user,Len(user)-1)
      If right$(p$,1) = Chr$(13) Then p$ = left$(p$,Len(p$)-1)
   Next x

   exitdo = 0
   enterpass = 0
   
   p$ = Encrypt$(p$)
   
   s = opensocket( AF_INET, SOCK_STREAM, IPPROTO_TCP )
   If s <= 0 Then
      Print "Unable to open socket!"
      End
   End If
   sa.sin_port = htons( 8501 )
   sa.sin_family = AF_INET
   sa.sin_addr.S_addr = inet_addr(ip$)
   
   ncon = Connect( s, @sa, Len( sa ) )
   If ncon = socket_error Then menumsg = "Unable to connect to server!":goto drawmenu
   
   n1! = Timer
   SendMSG "CLIENT",6
   netdelay = int((Timer-n1!)*10)+1
   If netdelay < 1 Then netdelay = 1
   
   SendMSG "LGON "+user+" "+p$,Len("LGON "+user+" "+p$)  

   alwayssend = 0
   n1! = Timer
   Do Until userapproved = 1 Or Timer >= n1! + 1
      If menumsg <> "" Then goto drawmenu
      W
   loop
   If Timer >= n1! + 1 Then menumsg="Unable to login! Please try again.":goto drawmenu
   move
   'closesocket s
   's = opensocket( AF_INET, SOCK_STREAM, IPPROTO_TCP )
   'ncon = Connect( s, @sa, Len( sa ) )
   'If ncon = socket_error Then End   
loop

Sub move
   dim movecount As Integer
   oldplayerx = playerx
   oldplayery = playery

   Do' Until multikey(1)
      If nofighttime < 30 Then nofighttime += 1
      
      n1! = Timer
      ox = playerx
      oy = playery
      moved = 0
      If multikey (&h48) And dify = 0 And difx = 0 Then   ' Up 
         playery -= 1
         cdir = dup
         moved = 1
         timestill = 0
      End If
      If multikey (&h4b) And difx = 0 And dify = 0 And moved = 0 Then   ' Left
         playerx -= 1
         cdir = dleft
         moved = 1
         timestill = 0
      End If
      If multikey (&h4d) And difx = 0 And dify = 0 And moved = 0 Then   ' Right
         playerx += 1
         cdir = dright
         moved = 1
         timestill = 0
      End If
      If multikey (&h50) And dify = 0 And difx = 0 And moved = 0 Then   ' Down
         playery += 1
         cdir = ddown
         moved = 1
         timestill = 0
      End If
               
      therightone = playerstr+playerdef+4
      If therightone > 99 Then therightone = 99
      If playerhp > maxhp Or maxhp <> therightone Then menumsg = "Hacker!"
      If money <> validmoney Then menumsg = "Hacker!"
      If playerstr > 99 Then menumsg = "Hacker!"
      If playerdef > 99 Then menumsg = "Hacker!"
      If maxhp > 99 Or playerhp > 99 Then menumsg = "Hacker!"
      If sword Then
         If Item(Inv(sword)).reqlvl > playerstr Then menumsg = "Hacker!"
      End If
      If shield Then
         If Item(Inv(shield)).reqlvl > playerdef Then menumsg = "Hacker!"
      End If
      If magic Then
         If Item(Inv(magic)).reqlvl > playerstr Then menumsg = "Hacker!"
      End If
            
      If menumsg <> "" Then Exit Sub
      
      Do
         jkl = 0
         If Map(playerx,playery) > "4" Then playerx = ox:playery = oy:moved = 0:jkl = 1
         If playerx = ox And playery = oy And Map(playerx,playery) > "4" Then playery += 1:oy += 1:oldplayery += 1:jkl = 1
         If playerx < 0 Then playerx = 0: jkl = 1
         If playery < 0 Then playery = 0: jkl = 1
         If playerx >= 999 Then playerx = 998: jkl = 1
         If playery >= 999 Then playery = 998: jkl = 1
         If jkl = 1 Then difx = 0:dify = 0:moved = 0
      loop Until jkl = 0
      
      playerx = ox
      playery = oy     
            
      If moved = 1 Then
      	Select Case(cdir)
      	   Case dleft
      	      difx -= 4
      	   Case dright
      	      difx += 4
      	   Case dup
      	      dify -= 4
      	   Case ddown
      	      dify += 4
      	End select
      EndIf
      If difx <> 0 Then
         If difx < 0 Then difx -= 4
      	If difx > 0 Then difx += 4
      EndIf
      If dify <> 0 Then
         If dify < 0 Then dify -= 4
      	If dify > 0 Then dify += 4
      EndIf
      If Abs(difx) >= 32 Then
         oldplayerx = playerx
         If difx < 0 Then playerx -= 1
      	If difx > 0 Then playerx += 1
         difx = 0
      EndIf
      If Abs(dify) >= 32 Then      	
         oldplayery = playery
         If dify < 0 Then playery -= 1
      	If dify > 0 Then playery += 1
         dify = 0
      EndIf
      
      DoScript
      MoveEnemies
      Interpolate
      Redraw
                  
      If currentkeypress$ = Chr$(255) + "k" Then Message(5) = "You must log out before exiting!"
      
      If playerx = ox And playery = oy Then timestill += 1: Else timestill = 0
      
      If theyfight = youfight Then Attack theyfight:theyfight = -1:youfight = -2
      
      If playerhp <= 0 Then
         savecount = 0
         Redraw
         scrmsg = "Oh no, you've died!"
         msgtime = 30
         
         playerx = startx
         playery = starty
         If sword Then SMNE "SW",2
         If shield Then SMNE "SH",2
         sword = 0
         shield = 0
         magic = 0
         line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
         line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf
         line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
         playerhp = maxhp
         For iii = 1 to 8
            Inv(iii) = 0
         Next iii
         Redraw
      End If

      For x = 0 to ubound(Killed)
         If Killed(x) = 9 And hat = 0 Then hat = 1
      Next x
      If hat = 1 Then
         For x = 10 to 100
            circle (340,200),x,rgb(0,0,200)
            paint (340,200),rgb(0,0,200)
            n1! = Timer
            Do Until Timer >= n1! + 1/120:loop
         Next x
         SendMSG "TELE 315 329",12
         For x = 1 to 8
            if Inv(x) = 12 then Inv(x) = 5:Message(5) = "The shield has lost its magic and is now a plain wooden shield."
         Next x
         playerx = 315
         playery = 329
         oldplayerx = playerx
         oldplayery = playery
         hat = 2
      End If
            
      If shirtcolor = rgb(255,255,255) And pantcolor = rgb(255,255,255) Then
         ' New user, so set clothes color
         SetClothes
      End If
      
      If youtrade = theytrade Then
         Trade
         theytrade = -1
         youtrade = -2
         theyaccept = 0
         youaccept = 0
         Redraw
      End If
      
      healtime += 1
      If healtime = 1800 Then healtime = 0:If playerhp < maxhp Then playerhp += 1
      
      'If alwayssend <= 0 Then
         If playerx <> oldplayerx Or playery <> oldplayery Then SMNE "M "+str$(playerx)+" "+str$(playery),Len("M "+str$(playerx)+" "+str$(playery))
         SMNE "G",1
      '   alwayssend = netdelay * 3
      'End If
      alwayssend -= 1
      'If countdown = 0 Then
          SMNE "S",1
      '   countdown = netdelay
      'End If
      countdown -= 1
   
      fps = 30
      
      If savecount = 0 Then
         curmsg = " "
         'Do Until curmsg = ""
            msg$ = "SAVE "
            msg$ += str$(playerx) + " "
            msg$ += str$(playery) + " "
            msg$ += str$(playerstr) + " " + str$(strexp) + " "
            msg$ += str$(playerdef) + " " + str$(defexp) + " "
            msg$ += str$(playerhp) + " " + str$(maxhp) + " "
            msg$ += str$(money) + " "
            msg$ += str$(shirtcolor) + " " + str$(pantcolor) + " "
            For x = 1 to 8
               msg$ += str$(Inv(x)) + " "
            Next x
            For x = 0 to ubound(NPC)
               If NPC(x).sp <> 0 Or NPC(x).fe <> 0 Or NPC(x).accum <> 0 Then
                  msg$ += str$(x) + " "+str$(NPC(x).sp) + " "+str$(NPC(x).accum) + " "+str$(NPC(x).o) + " "
               End If
            Next x
            msg$ += "X"

            SendMSG msg$,4000
         'loop
         savecount = 500    ' About every minute
      End If
      savecount -= 1
      
      msgtime -= 1

      If playerx <> ox Or playery <> oy Then
         
         If cdir = dup Or cdir = ddown Then udlr = 6:Else udlr = 7
         If walkcycle = 1 Then walkcycle = 0:udlr = 5: Else walkcycle = 1
         
         walked = 1
      End If

      Redraw
            
      ScreenSync
      Do Until Timer >= n1! + 1/fps
         W
      Loop
      
      ' Mine
      If Map(playerx-1,playery) = "5" Or Map(playerx+1,playery) = "5" Or Map(playerx,playery-1) = "5" Or Map(playerx,playery+1) = "5" Then    ' Iron ore
         If InInv("14") >= 0 Then
            draw string (40,50),"You swing at the ore..."
            n1! = Timer
            Do Until Timer >= n1! + (Rnd+1)*2
               W
            loop
            scrmsg = "You get some iron!"
            msgtime = 33
            playerx = ox
            playery = oy
            AddInv(15)
         Else
            scrmsg = "You need a pickaxe!"
            msgtime = 30
         End If
      End If

      d = getmouse (mx,my,,b)
      ' Check if they want to log out
      If (mx >= 584 And my >= 404 And mx < 640 And my < 420 And b = 1) Or timestill >= 3000 Then
         Locate 1,1
         Print "Loading..."
         curmsg = " "
         'Do Until curmsg = ""
            msg$ = "BANK "
            For x = 1 to 20
               msg$ += str$(bank(x)) + " "
            Next x
            SendMSG msg$,200
            
            msg$ = "LGOF "
            msg$ += str$(playerx) + " "
            msg$ += str$(playery) + " "
            msg$ += str$(playerstr) + " " + str$(strexp) + " "
            msg$ += str$(playerdef) + " " + str$(defexp) + " "
            msg$ += str$(playerhp) + " " + str$(maxhp) + " "
            msg$ += str$(money) + " "
            msg$ += str$(shirtcolor) + " " + str$(pantcolor) + " "
            For x = 1 to 8
               msg$ += str$(Inv(x)) + " "
            Next x
            For x = 0 to ubound(NPC)
               If NPC(x).sp <> 0 Or NPC(x).fe <> 0 Or NPC(x).accum <> 0 Then
                  msg$ += str$(x) + " "+str$(NPC(x).sp) + " "+str$(NPC(x).accum) + " "+str$(NPC(x).o) + " "
               End If
            Next x
            
            msg$ += "X"
            
            SendMSG msg$,4000
            'If sword Then SMNE "SW",2
            'If shield Then SMNE "SH",2
            sword = 0
            shield = 0
            magic = 0
            line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
            line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf
            line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
         'loop
         
         Exit Sub  
      End If

      ' Check if they want to equip something
      If b = 1 Then
         dontblockon = 1
         For x = 1 to 8
            If mx >= 464 And mx <= 614 And my >= x*10+80+20 And my <= x*10+90+20 Then
               line (462,x*10+80+18)-(614,x*10+90+20),,b
               Do Until b = 0
                  d = getmouse (mx,my,,b)
                  W
               loop
               
               If Item(Inv(x)).Type = "W" Then
                  If playerstr >= Item(Inv(x)).reqlvl Then
                     wmi = sword
                     magic = 0
                     If sword <> x Then sword = x: bload "data\sword.bmp",swordimg:Else sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
                     SMNE "SW",2
                     If wmi <> 0 And sword <> 0 Then SMNE "SW",2  ' New sword instead of taking it off
                  Else
                     scrmsg = "You need "+str$(Item(Inv(x)).reqlvl)+" strength to wear this!"
                     msgtime = 33
                  End If
               ElseIf Item(Inv(x)).Type = "S" Then
                  If playerdef >= Item(Inv(x)).reqlvl Then
                     wmi = shield
                     If shield <> x Then shield = x: bload "data\shield.bmp",shieldimg:bload "data\shbk.bmp",shbkimg:Else shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
                     SMNE "SH",2
                     If wmi <> 0 And shield <> 0 Then SMNE "SH",2
                  Else
                     scrmsg = "You need "+str$(Item(Inv(x)).reqlvl)+" defense to wear this!"
                     msgtime = 33                  
                  End If
               ElseIf Item(Inv(x)).Type = "M" Then
                  If playerstr >= Item(Inv(x)).reqlvl Then
                     If sword > 0 Then
                        sword = 0
                        line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
                        SMNE "SW",2
                     End If
                     If magic = x Then magic = 0: Else magic = x
                  Else
                     scrmsg = "You need "+str$(Item(Inv(x)).reqlvl)+" strength to use this!"
                     msgtime = 33
                  End If
               ElseIf Item(Inv(x)).Type = "H" Then
                  Inv(x) = 0
                  playerhp = maxhp
               End If
               Redraw
               Exit For               
            End If
         Next x
         dontblockon = 0
      End If
      If b = 2 Then
         dontblockon = 1
         For x = 1 to 8
            If mx >= 464 And mx <= 600 And my >= x*10+80+20 And my <= x*10+90+20 Then
               line (462,x*10+80+18)-(600,x*10+90+20),,b
               Do Until b = 0
                  d = getmouse (mx,my,,b)
                  W
               loop
               
               If sword = x Then sword = 0:SMNE "SW",2:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
               If shield = x Then shield = 0:SMNE "SH",2:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
               If magic = x Then magic = 0
               Inv(x) = 0
               Redraw
               Exit For               
            End If
            
         Next x
         dontblockon = 0      
      End If
   loop
End Sub

Function StrTok$(Srce$,Delim$)
static Start%, SaveStr$

   ' If first call, make a copy of the string.
   If Srce$<>"" Then
      Start%=1 : SaveStr$=Srce$
   End If

   BegPos%=Start% : Ln%=Len(SaveStr$)
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

Sub checkrecv
   dim a As zstring * 3,n1 As single
   
   dim length As integer
   dim ts As zstring * 4000
   dim dat As string
   
   dim none As postype
   
   Do
      d = recv(s,@a,3,0)
      If a = "LS" Then
         d = recv(s,@length,4,0)         
         d = recv(s,@ts,length,0)
         
         dat = left$(rtrim$(ts),length)   ' Dumb FB bug
         
         cmd$ = trim$(left$(dat,instr(dat," ")))
         param$ = trim$(mid$(dat,Len(cmd$)+1))
         If cmd$ = "" Then cmd$ = dat: param$ = ""
     
         Select Case(cmd$)
            Case "CLIENT"   ' Send client version
               v = val(param$)
               If CLIENTVERSION < v Then
                  menumsg = Chr$(20)
                  CallUpdater
               End If
            Case "ONLINE"
               scrmsg = "There are currently "+param$+" people playing."
               msgtime = 33
            Case "CF"
            Case "G"
               dim ot(MAX_PLAYERS) As byte
               
               ' Get x
               x$ = strtok$(param$," ")
               ' Get y
               y$ = strtok$(""," ")
               ' Get clothes info
               sh$ = strtok$(""," ")
               pa$ = strtok$(""," ")
               ' Get user number and name
               num$ = strtok$(""," ")
               username$ = strtok$(""," ")
               ' Get sword and shield
               ss$ = strtok$(""," ")
               sw = val(left$(ss$,1))
               sh = val(right$(ss$,1)) 

               Do Until username$ = ""
                  ' Store x and y from previous
                  If x$ <> "" And y$ <> "" Then
                     ot(val(num$)) = 1
                     OtherPos(val(num$)).ox = OtherPos(val(num$)).nx
                     OtherPos(val(num$)).oy = OtherPos(val(num$)).ny
                     OtherPos(val(num$)).pt = OtherPos(val(num$)).ct
                     OtherPos(val(num$)).ct = Timer
                     OtherPos(val(num$)).nx = val(x$)
                     OtherPos(val(num$)).ny = val(y$)
                     OtherPos(val(num$)).s = val(sh$)
                     OtherPos(val(num$)).p = val(pa$)
                     OtherPos(val(num$)).n = username$
                     OtherPos(val(num$)).u = 1
                     OtherPos(val(num$)).sw = sw
                     OtherPos(val(num$)).sh = sh
                  Else
                     OtherPos(val(num$)).w = Timer
                     If Timer >= OtherPos(val(num$)).w + 1 Then OtherPos(val(num$)).x = -1:OtherPos(val(num$)).w = Timer   ' Player doesn't exist
                  End If
                  ' Get x & y
                  x$ = strtok$(""," ")
                  y$ = strtok$(""," ")
                  ' Get clothes info
                  sh$ = strtok$(""," ")
                  pa$ = strtok$(""," ")
                  ' Get user number and name
                  num$ = strtok$(""," ")
                  username$ = strtok$(""," ")
                  ' Get sword and shield
                  ss$ = strtok$(""," ")
                  sw = val(left$(ss$,1))
                  sh = val(right$(ss$,1))                  
               loop
               For x = 0 to MAX_PLAYERS
                  If ot(x) = 0 Then OtherPos(x) = none
               Next x
            Case "TELL"
               ' Somebody tells you something
               For j = 0 to 4
                  Message(j) = Message(j+1)
               Next j
               Message(5) = param$
            Case "S"
               ' Get speech
               param$ = mid$(param$,2)
               m$ = left$(param$,instr(param$,Chr$(34))-1)
               param$ = ltrim$(mid$(param$,Len(m$)+2))
               For i = 1 to MAX_PLAYERS
                  If m$ <> "" And m$ <> Message(5) Then
                     OtherPos(i-1).m = m$
                     For j = 0 to 4
                        Message(j) = Message(j+1)
                     Next j
                     If m$ <> Message(4) Then Message(5) = m$
                  End If
                  m$ = left$(param$,instr(param$,Chr$(34))-1)
                  param$ = ltrim$(mid$(param$,Len(m$)+2))
                  If trim$(param$) = "" Then Exit For
               Next i
            Case "FIGHT"
               hp = val(strtok$(param$," "))
               omaxhp = val(strtok$(""," "))
               Def = val(strtok$(""," "))
               num = val(strtok$(""," "))
               
               theyfight = num
              
               otherdef = Def
               otherhp = hp
               othermaxhp = omaxhp
               theylose = 0

               If sentfight = 0 Then
                  If magic > 0 Then ismagic$="1":Else ismagic$="0"
                  If shield > 0 Then isshield$="1":Else isshield$="0"
                  SMNE "MAGIC "+ismagic$+isshield$+str$(theyfight),Len("MAGIC "+ismagic$+isshield$+str$(theyfight))
                  SMNE "FIGHT "+str$(playerhp)+" "+str$(maxhp)+" "+str$(playerdef+Item(shield).effect)+" "+str$(theyfight),Len("FIGHT "+str$(playerhp)+" "+str$(maxhp)+" "+str$(playerdef+Item(shield).effect)+" "+str$(theyfight))
                  
                  youfight = num
                  sentfight = 1
               End If
               sentfight = 1
            Case "MAGIC"
               If left$(param$,1) = "1" Then theyusemagic = 1:Else theyusemagic = 0
               If right$(param$,1) = "1" Then theyuseshield = 1:Else theyuseshield = 0
            Case "HIT"
               If Map(playerx,playery) = "4" And youfight >= 0 Then
                  amt = val(param$)
                  If Item(Inv(sword)).name = "Dilemma" Then amt = amt * 1.5 + 2
                  If amt > playerhp Then amt = playerhp
                  playerhp -= amt
               End If
            Case "HP"
               otherprev = otherhp
               otherhp = val(param$)
            Case "LOSE"
               theylose = 1
            Case "ITEMS"
               unlucky = Rnd*100
               If unlucky <> 1 Then
                  dim theirinv(1 to 8) As integer
                  theirinv(1) = val(strtok$(param$," "))
                  For i = 2 to 8
                     theirinv(i) = val(strtok$(""," "))
                  Next i
                  For i = 1 to 7   ' Sort
                     If Item(theirinv(i)).cost < Item(theirinv(i+1)).cost Then swap theirinv(i),theirinv(i+1)
                  Next i
                  For i = 1 to 8   ' Add to inventory
                     If theirinv(i) > 0 Then AddInv theirinv(i)
                  Next i
               Else
                  AddInv 37+Rnd*4
               End If   
            Case "TRADE"
               Message(5) = OtherPos(val(param$)).n+" wishes to trade with you."
               theytrade = val(param$)
            Case "ADDI"
               For x = 1 to 8
                  If theysell(x) = 0 Then theysell(x) = val(param$):Exit For
               Next x
            Case "DELI"
               For x = 1 to 8
                  If theysell(x) = val(param$) Then theysell(x) = 0:Exit For
               Next x
            Case "PAY"
               theypay = val(param$)
            Case "ACCEPT"
               theyaccept = 1
            Case "NOACCEPT"
               theyaccept = 0
               youaccept = 0
            Case "L"    ' Log on
               playerx = val(strtok$(param$," "))
               playery = val(strtok$(""," "))
               playerstr = val(strtok$(""," "))
               strexp = val(strtok$(""," "))
               playerdef = val(strtok$(""," "))
               defexp = val(strtok$(""," "))
               playerhp = val(strtok$(""," "))
               maxhp = val(strtok$(""," "))
               money = val(strtok$(""," "))
               shirtcolor = val(strtok$(""," "))
               pantcolor = val(strtok$(""," "))
               For iii = 1 to 8
                  Inv(iii) = val(strtok$(""," "))
               Next iii
               For iii = 0 to ubound(NPC)
                  this$ = strtok$(""," ")
                  blah = val(this$)
                  NPC(blah).sp = val(strtok$(""," "))
                  NPC(blah).accum = val(strtok$(""," "))
                  NPC(blah).o = val(strtok$(""," "))
                  If NPC(blah).o <> 0 Then NPC(blah).o = 1
               Next iii
               Redraw
               userapproved = 1
               validmoney = money
            Case "BANK"
               bank(1) = val(strtok$(param$," "))
               For iii = 2 to 20
                  bank(iii) = val(strtok$(""," "))
               Next iii
            Case "DECLINE"
               For x = 1 to 8
                  theysell(x) = 0
               Next x
               theyaccept = 1
               youaccept = 1
               theytrade = -1
               youtrade = -2
               theypay = 0
            Case "BADLOGIN"
               menumsg = "Incorrect username or password!"
               userapproved = 0
            Case "2USER"
               menumsg = "You are already logged on to another computer!"
               userapproved = 0
            Case "BAD"
               userused = 1
            Case "GOOD"
               userused = 2
         End Select

         curmsg = ""
         
      End If
       
      msg$ = ""
      a = "  "
      
   loop
End Sub

Sub Redraw
      
   px1 = playerx - 10
   px2 = playerx + 10
   py1 = playery - 6
   py2 = playery + 6
   If px1 < 0 Then chgx = abs(px1):px1 = 0: px2 += chgx: absolutex = 1
   If py1 < 0 Then chgy = abs(py1):py1 = 0: py2 += chgy: absolutey = 1
   If absolutex = 1 Then xabsolute = 1
   If absolutey = 1 Then xabsolute = 2
   If absolutex = 1 And absolutey = 1 Then xabsolute = 3

   screenset 1,0
   
   ' Draw map
   For x = px1-1 to px2+1
      For y = py1-1 to py2+1
         realx = (x - px1) * 32
         realy = (y - py1) * 32

         If Map(x,y) = "5" Then   ' Transparent underneath
            ' Check the most likely square it should be
            dim T(0 to 7) As string * 1
            T(0) = Map(x-1,y-1)
            T(1) = Map(x,y-1)
            T(2) = Map(x+1,y-1)
            T(3) = Map(x-1,y)
            T(4) = Map(x+1,y)
            T(5) = Map(x-1,y+1)
            T(6) = Map(x,y+1)
            T(7) = Map(x+1,y+1)
            ' Find the mode
            For i = 0 to 7
               For j = 0 to 7
                  If T(j) = str$(i) Then checknum += 1
               Next j
               If checknum > highnum Then highnum = checknum: tileis = i
               checknum = 0
            Next i
            put (realx-difx,realy-dify),Tile(str$(tileis)),trans
         End If
         If x < 999 And y < 999 Then put (realx-difx,realy-dify),Tile(Map(x,y)),trans
         If x = 999 Or y = 999 Then put (realx-difx,realy-dify),Tile("6"),pset
         If x > 999 Or y > 999 Then put (realx-difx,realy-dify),Tile("3"),pset
      Next y
   Next x

   If redrawflag = 0 Then
      ' If you're next to people, draw "trade" or "attack" option and check too -- also draw "attack" for enemies
      i = 1
      For x = 0 to ubound(OtherPos)
         If abs(playerx-OtherPos(x).x) < 2 And abs(playery-OtherPos(x).y) < 2 And OtherPos(x).n <> "" Then
            d = getmouse(mx,my,,b)
            ' Attack other players
            If Map(playerx,playery) = "4" And Map(OtherPos(x).x,OtherPos(x).y) = "4" Then
            	If nofighttime = 30 then
	               If mx >= 10 And mx <= 200 And my >= i*10 And my <= i*10+9 And b = 1 then
	               	nofighttime = 0
	                  draw string (10,i*10),"Attack "+OtherPos(x).n,rgb(0,0,255)
	                  playerx = OtherPos(x).x + 1
	                  playery = OtherPos(x).y
	                  SMNE "M "+str$(playerx)+" "+str$(playery),Len("M "+str$(playerx)+" "+str$(playery))
	                  If magic > 0 Then ismagic$="1":Else ismagic$="0"
	                  If shield > 0 Then isshield$="1":Else isshield$="0"
	                  SMNE "MAGIC "+ismagic$+isshield$+str$(x),Len("MAGIC "+ismagic$+isshield$+str$(x))
	                  SMNE "FIGHT "+" "+str$(playerhp)+" "+str$(maxhp)+" "+str$(playerdef+Item(shield).effect)+" "+str$(x),Len("FIGHT "+" "+str$(playerhp)+" "+str$(maxhp)+" "+str$(playerdef+Item(shield).effect)+" "+str$(x))
	                  youfight = x
	                  theylose = 0
	                  
	                  'Attack x
	                  'youfight = -2
	                  'theyfight = -1
	                  Exit Sub
	               Else
	                  draw string (10,i*10),"Attack "+OtherPos(x).n
	               End If
               End if
            Else
               If mx >= 10 And mx <= 200 And my >= i*10 And my <= i*10+9 And b = 1 And Map(playerx,playery) < "5" Then
                  draw string (10,i*10),"Trade with "+OtherPos(x).n,rgb(0,0,255)
                  If youtrade >= 0 And youtrade <> x Then SMNE "DECLINE "+str$(youtrade),Len("DECLINE "+str$(youtrade))
                  SMNE "TRADE "+str$(x),Len("TRADE "+str$(x))
                  youtrade = x
               Else
                  draw string (10,i*10),"Trade with "+OtherPos(x).n
               End If            
            End If
            i += 1
         End If
      Next x
      For x = 0 to ubound(EnemyLoc)
         If Abs(playerx-EnemyLoc(x).x) <= 1 And abs(playery-EnemyLoc(x).y) <= 1 And Map(Int(EnemyLoc(x).x),Int(EnemyLoc(x).y)) <= "4" And EnemyLoc(x).used And EnemyLoc(x).respawn = 0 Then
            d = getmouse(mx,my,,b)
            If mx >= 10 And mx <= 200 And my >= i*10 And my <= i*10+9 And b = 1 Then
               draw string (10,i*10),"Attack "+Enemy(EnemyLoc(x).id).name,rgb(0,0,255)
               theenemy = x
            Else
               draw string (10,i*10),"Attack "+Enemy(EnemyLoc(x).id).name
            End If
            i += 1
         End If
      Next x
   End If
   
   ' Draw wastelands pking warning image
   If Map(playerx,playery) = "4" Then put (600,360),wlwarning,trans
   
   ' Draw other players
   For x = 0 to ubound(OtherPos)
      If OtherPos(x).u <> 0 Then
         paint GuyImg(ddown),(8,30),OtherPos(x).s,rgb(0,0,0)
         paint GuyImg(ddown),(24,30),OtherPos(x).s,rgb(0,0,0)
         paint GuyImg(5),(16,2),OtherPos(x).s,rgb(0,0,0)
         paint GuyImg(5),(15,17),OtherPos(x).p,rgb(0,0,0)
         
         If xabsolute = 1 Then
            put (OtherPos(x).x*32-difx,(OtherPos(x).y-(playery-6))*32-32-dify),GuyImg(ddown),trans
            put (OtherPos(x).x*32-difx,(OtherPos(x).y-(playery-6))*32-dify),GuyImg(5),trans
            draw string (OtherPos(x).x*32+16-Len(OtherPos(x).n)*4-difx,(OtherPos(x).y-(playery-6))*32+32-dify),OtherPos(x).n
            
            If OtherPos(x).sw Then put (OtherPos(x).x*32-difx,(OtherPos(x).y-(playery-6))*32-dify),swordimg,trans
            If OtherPos(x).sh Then put (OtherPos(x).x*32+20-difx,(OtherPos(x).y-(playery-6))*32+4-dify),shieldimg,trans
         ElseIf xabsolute = 2 Then
            put ((OtherPos(x).x-(playerx - 10))*32-difx,OtherPos(x).y*32-32-dify),GuyImg(ddown),trans
            put ((OtherPos(x).x-(playerx - 10))*32-difx,OtherPos(x).y*32-dify),GuyImg(5),trans
            draw string ((OtherPos(x).x-(playerx - 10))*32+16-Len(OtherPos(x).n)*4-difx,OtherPos(x).y*32+32-dify),OtherPos(x).n
            
            If OtherPos(x).sw Then put ((OtherPos(x).x-(playerx - 10))*32-difx,OtherPos(x).y*32-dify),swimg,trans
            If OtherPos(x).sh Then put ((OtherPos(x).x-(playerx - 10))*32+20-difx,OtherPos(x).y*32+4-dify),shimg,trans
         ElseIf xabsolute = 3 Then
            put (OtherPos(x).x*32-difx,OtherPos(x).y*32-32-dify),GuyImg(ddown),trans
            put (OtherPos(x).x*32-difx,OtherPos(x).y*32-dify),GuyImg(5),trans
            draw string (OtherPos(x).x*32+16-Len(OtherPos(x).n)*4-difx,OtherPos(x).y*32+32-dify),OtherPos(x).n
            
            If OtherPos(x).sw Then put (OtherPos(x).x*32-difx,OtherPos(x).y*32-dify),swimg,trans
            If OtherPos(x).sh Then put (OtherPos(x).x*32+20-difx,OtherPos(x).y*32+4-dify),shimg,trans
         Else
            put ((OtherPos(x).x-(playerx - 10))*32-difx,(OtherPos(x).y-(playery-6))*32-32-dify),GuyImg(ddown),trans
            put ((OtherPos(x).x-(playerx - 10))*32-difx,(OtherPos(x).y-(playery-6))*32-dify),GuyImg(5),trans
            draw string ((OtherPos(x).x-(playerx - 10))*32+16-Len(OtherPos(x).n)*4-difx,(OtherPos(x).y-(playery-6))*32+32-dify),OtherPos(x).n
            
            If OtherPos(x).sw Then put ((OtherPos(x).x-(playerx - 10))*32-difx,(OtherPos(x).y-(playery-6))*32-dify),swimg,trans
            If OtherPos(x).sh Then put ((OtherPos(x).x-(playerx - 10))*32+20-difx,(OtherPos(x).y-(playery-6))*32+4-dify),shimg,trans
         End If
      End If   
   Next x

   ' Draw enemies
   For x = 0 to ubound(EnemyLoc)
      If EnemyLoc(x).used = 1 And EnemyLoc(x).respawn <= 0 Then          ' If it's an enemy and not null 
         If EnemyLoc(x).x >= px1-1 And EnemyLoc(x).x <= px2+1 And EnemyLoc(x).y >= py1-1 And EnemyLoc(x).y <= py2+1 Then          ' If in range
            If xabsolute = 1 Then
               put (EnemyLoc(x).x*32-difx,(EnemyLoc(x).y-(playery-6))*32-dify),Enemy(EnemyLoc(x).id).img,trans
            ElseIf xabsolute = 2 Then
               put ((EnemyLoc(x).x-(playerx - 10))*32-difx,EnemyLoc(x).y*32-dify),Enemy(EnemyLoc(x).id).img,trans
            ElseIf xabsolute = 3 Then
               put (EnemyLoc(x).x*32-difx,EnemyLoc(x).y*32-dify),Enemy(EnemyLoc(x).id).img,trans
            Else
               put ((EnemyLoc(x).x-(playerx - 10))*32-difx,(EnemyLoc(x).y-(playery-6))*32-dify),Enemy(EnemyLoc(x).id).img,trans
            End If
         End If 
      End If
   Next x
   
   paint GuyImg(ddown),(8,30),rgb(255,255,255),rgb(0,0,0)
   paint GuyImg(ddown),(24,30),rgb(255,255,255),rgb(0,0,0)
   paint GuyImg(5),(16,2),rgb(255,255,255),rgb(0,0,0)
   paint GuyImg(5),(15,17),rgb(255,255,255),rgb(0,0,0)
   
   ' Draw NPCs
   For x = 0 to ubound(NPC)
      If NPC(x).name <> "" Then
         If xabsolute = 1 Then
            put (NPC(x).x*32-difx,(NPC(x).y-(playery-6))*32-32-dify),GuyImg(ddown),trans
            put (NPC(x).x*32-difx,(NPC(x).y-(playery-6))*32-dify),GuyImg(5),trans
            draw string (NPC(x).x*32+16-Len(NPC(x).name)*4-difx,(NPC(x).y-(playery-6))*32+32-dify),NPC(x).name
         ElseIf xabsolute = 2 Then
            put ((NPC(x).x-(playerx - 10))*32-difx,NPC(x).y*32-32-dify),GuyImg(ddown),trans
            put ((NPC(x).x-(playerx - 10))*32-difx,NPC(x).y*32-dify),GuyImg(5),trans
            draw string ((NPC(x).x-(playerx - 10))*32+16-Len(NPC(x).name)*4-difx,NPC(x).y*32+32-dify),NPC(x).name 
         ElseIf xabsolute = 3 Then
            put (NPC(x).x*32-difx,NPC(x).y*32-32-dify),GuyImg(ddown),trans
            put (NPC(x).x*32-difx,NPC(x).y*32-dify),GuyImg(5),trans
            draw string (NPC(x).x*32+16-Len(NPC(x).name)*4-difx,NPC(x).y*32+32-dify),NPC(x).name
         Else
            put ((NPC(x).x-(playerx - 10))*32-difx,(NPC(x).y-(playery-6))*32-32-dify),GuyImg(ddown),trans
            put ((NPC(x).x-(playerx - 10))*32-difx,(NPC(x).y-(playery-6))*32-dify),GuyImg(5),trans
            draw string ((NPC(x).x-(playerx - 10))*32+16-Len(NPC(x).name)*4-difx,(NPC(x).y-(playery-6))*32+32-dify),NPC(x).name
         End If
      End If   
   Next x

   paint GuyImg(cdir),(8,30),shirtcolor,rgb(0,0,0)
   paint GuyImg(cdir),(24,30),shirtcolor,rgb(0,0,0)
   paint GuyImg(udlr),(16,2),shirtcolor,rgb(0,0,0)
   paint GuyImg(udlr),(15,17),pantcolor,rgb(0,0,0)
   
   If udlr = 8 And sword Then swap swordimg,ssimg:ifb = 10
   
   ' Draw player
   If xabsolute = 1 Then     
      If cdir = dup Then
         put (playerx*32-4,160+34),shbkimg,trans
         put (playerx*32+25,160+30),swordimg,trans
      End If
      
      put (playerx*32,160),GuyImg(cdir),trans
      put (playerx*32,192),GuyImg(udlr),trans
   
      If cdir <> dup Then
         put (playerx*32-ifb,160+32),swordimg,trans
         put (playerx*32+20,160+36),shieldimg,trans
      End If
   ElseIf xabsolute = 2 Then
      If cdir = dup Then
         put (316,playery*32+2),shbkimg,trans
         put (345,playery*32-2),swordimg,trans
      End If
      
      put (320,playery*32-32),GuyImg(cdir),trans
      put (320,playery*32),GuyImg(udlr),trans
      
      If cdir <> dup Then
         put (320-ifb,playery*32),swordimg,trans
         put (340,playery*32+4),shieldimg,trans
      End If
   ElseIf xabsolute = 3 Then
      If cdir = dup Then
         put (playerx*32-4,playery*32+2),shbkimg,trans
         put (playerx*32+25,playery*32),swordimg,trans
      End If
      
      put (playerx*32,playery*32-32),GuyImg(cdir),trans
      put (playerx*32,playery*32),GuyImg(udlr),trans
      
      If cdir <> dup Then
         put (playerx*32-ifb,playery*32),swordimg,trans
         put (playerx*32+20,playery*32+4),shieldimg,trans
      End If
   Else
      If cdir = dup Then
         put (316,160+34),shbkimg,trans
         put (345,160+30),swordimg,trans
      End If
      
      put (320,160),GuyImg(cdir),trans
      put (320,192),GuyImg(udlr),trans
   
      If cdir <> dup Then
         put (320-ifb,160+32),swordimg,trans
         put (340,160+36),shieldimg,trans
      End If
   End If
   
   If udlr = 8 Then swap swordimg,ssimg
   
   ' Draw stats
   dim box As Any ptr = ImageCreate (160,190,rgba(0,100,100,192))
   dim es As Any ptr = ImageCreate (64,32)
   line box,(0,0)-(159,189),rgb(255,255,255),b
   draw string box,(20,4),"   - Stats -"
   d = getmouse (mx,my,,b)
   If mx >= 464 And mx <= 600 And my >= 40 And my <= 49 Then
      draw string box,(4,20),"Exp: "+str$(strexp),rgb(255,255,0)
   Else 
      draw string box,(4,20),"Str: "+str$(playerstr)
   End If
   If mx >= 464 And mx <= 600 And my >= 50 And my <= 59 Then
      draw string box,(4,30),"Exp: "+str$(defexp),rgb(255,255,0)
   Else
      draw string box,(4,30),"Def: "+str$(playerdef)
   End If
   
   draw string box,(4,40),"HP: "+str$(playerhp)+"/"+str$(maxhp)
   draw string box,(4,50),"Money: "+str$(money)
   
   draw string box,(10,70),"  - Inventory -"
   For x = 1 to ubound(Inv)
      draw string box,(4,x*10+80),Item(Inv(x)).name
      If x = sword Or x = shield Or x = magic Then draw string box,(4,x*10+80),Item(Inv(x)).name,rgb(0,0,200)
   Next x
   put (460,20),box,alpha
   ImageDestroy box
   ImageDestroy es
   
   draw string (584,404),"Logout"
   
   line (0,416)-(639,479),0,bf
   draw string (4,404),user+": "+speechbar+"_"
   
   If redrawflag <> 2 Then
      For x = 0 to 5
         draw string (4,420+x*10),Message(x)
      Next x
   End If
               
   ' Draw message, if there is one
   If msgtime > 0 Then
      draw string (40,40),scrmsg
   End If
            
   ' Say stuff
   k$ = inkey$
   currentkeypress$ = k$
   If k$ <> "" And asc(k$) <> 255 Then
      If k$ = Chr$(8) Then
         speechbar = left$(speechbar,Len(speechbar)-1)
      ElseIf k$ = Chr$(13) Then
         If lcase$(speechbar) = "::online" Then
            SendMSG "ONLINE",6
         ElseIf left$(speechbar,1) = "/" Then
            speechbar = mid$(speechbar,2)
            If not (lcase$(strtok$(speechbar," ")) = lcase$(user)) Then
               SendMSG "TELL "+user+" "+speechbar,Len("TELL "+user+" "+speechbar)
            End If
         Else
            SendMSG "SAY "+speechbar,Len("SAY "+speechbar)
         End If
         speechbar = ""
      Else
         If k$ = Chr$(34) Then k$ = "'"
         speechbar += k$
      End If
   End If
   
   screencopy
   screenset 0,0
End Sub

Sub Trade
   If sword Then SendMSG "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
   If shield Then SendMSG "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
   If magic Then magic = 0
   
   dim sale(1 to 8) As byte,youpay As integer
   theyaccept = 0
   youaccept = 0
   For x = 1 to 8
      sale(x) = 0
   Next x
   youpay = 0
   
   youpayshow$ = "0"
drawtrade:
   
   Do Until (theyaccept = 1 And youaccept = 1 And invok = 1) Or theytrade < 0
      W
      screenset 1,0
      line (0,0)-(639,479),rgb(100,100,100),bf
      draw string (20,20),"Trading with "+OtherPos(theytrade).n
      
      draw string (10,40),"You trade:"
      For x = 1 to 8
         If Sale(x) = 0 Then c = rgb(255,255,255):Else c = rgb(0,0,255)
         draw string (10,40+x*10),Item(Inv(x)).name,c
      Next x
      draw string (10,130),"Money: "+youpayshow$+" (click to change)"
      draw string (10,160),"They trade:"
      For x = 1 to 8
         draw string (10,160+x*10),Item(theysell(x)).name
      Next x
      ntifff = 0
      yourff = 0
      For x = 1 to 8
         If theysell(x) > 0 Then ntifff += 1
         If inv(x) = 0 Then yourff += 1
         If sale(x) <> 0 Then yourff += 1
      Next x
      If ntifff > yourff Then invok = 0:themsg$ = "Not enough room in inventory!": Else invok = 1
      
      draw string (10,250),"They pay "+str$(theypay)
      
      draw string (2,2),themsg$

      draw string (500,468),"Accept   Decline"
      screencopy
      screenset 0,0
      If mx >= 500 And mx <= 556 And my >= 468 And my <= 478 And b = 1 And invok = 1 Then SendMSG "ACCEPT "+str$(theytrade),Len("ACCEPT "+str$(theytrade)):youaccept = 1
      If mx >= 580 And mx <= 640 And my >= 468 And my <= 478 And b = 1 Then         
         youaccept = 1
         youpay = 0
         theypay = 0
         For x = 1 to 8
            sale(x) = 0
            theysell(x) = 0
         Next x
         SendMSG "DECLINE "+str$(theytrade),Len("DECLINE "+str$(theytrade))
         savecount = 0
         goto declinetrade
      End If
      If mx >= 10 And mx <= 200 And my >= 130 And my <= 140 And b = 1 Then
         Do Until b = 0
            d = getmouse(mx,my,,b)
            W
         loop
         Locate 1,1
         Print "Enter amount: ";
         l$ = ""
         s$ = ""
         Do Until l$ = Chr$(13)
            l$ = inkey$
            If l$ <> Chr$(13) And l$ <> Chr$(8) Then Print l$;
            If l$ = Chr$(8) And charcount > 0 Then
               l$ = ""
               charcount -= 1
               If Len(s$) > 0 Then s$ = left$(s$,Len(s$)-1):Else s$ = ""
               If Pos > 1 Then Locate ,Pos - 1: Else Locate CsrLin-1,80
               Print " ";
               If Pos > 1 Then Locate ,Pos - 1: Else Locate CsrLin-1,80
            End If
            If l$ >= " " Then s$ = s$ + l$: charcount += 1
            W
         loop
         If s$ = "" Then s$ = "0"
         If valint(s$) < 0 Then s$ = "0"
         If valint(s$) > money Then s$ = str$(money)
         youpay = valint(s$)
         youpayshow$ = s$
         youaccept = 0
         theyaccept = 0
         
         SendMSG "NOACCEPT "+str$(theytrade),Len("NOACCEPT "+str$(theytrade))
         SendMSG "PAY "+s$+" "+str$(theytrade),Len("PAY "+s$+" "+str$(theytrade))
         themsg$ = ""
         goto drawtrade
      End If
      If theyaccept = 1 Then themsg$ = OtherPos(theytrade).n+" has accepted.":Else themsg$ = ""
      d = getmouse (mx,my,,b)
      For x = 1 to 8
         If mx >= 10 And mx <= 10+Len(Item(Inv(x)).name)*8 And my >= 40+x*10 And my <= 50+x*10 And b = 1 And Inv(x) <> 0 Then
            Sale(x) = not Sale(x)
               
            SendMSG "NOACCEPT "+str$(theytrade),Len("NOACCEPT "+str$(theytrade))
            If Sale(x) = 0 Then SendMSG "DELI "+str$(Inv(x))+" "+str$(theytrade),Len("DELI "+str$(Inv(x))+" "+str$(theytrade))
            If Sale(x) <> 0 Then SendMSG "ADDI "+str$(Inv(x))+" "+str$(theytrade),Len("ADDI "+str$(Inv(x))+" "+str$(theytrade))
            themsg$ = ""
            theyaccept = 0
            youaccept = 0
            Do Until b = 0
               d = getmouse (mx,my,,b)
               W
            loop
            goto drawtrade
         End If
      Next x
   loop
   
   If theytrade < 0 Then goto declinetrade
   
   dim n As integer
   For x = 1 to 8
      n = theysell(x)
      If theysell(x) <> 0 Then AddInv n
      theysell(x) = 0
   Next x
   For x = 1 to 8
      If sale(x) Then Inv(x) = 0
   Next x
   money += theypay
   money -= youpay
   validmoney = money
   theypay = 0
   
declinetrade:
   
   youaccept = 0
   theyaccept = 0
   theytrade = -1
   youtrade = -2
   screenunlock
   Message(5) = ""
   Redraw
   savecount = 0
End Sub

Sub DoBank

drawbank:
   line (0,0)-(639,479),rgb(100,100,100),bf
   draw string (20,20),"Welcome to the Bank!"

   draw string (20,40),"Withdraw:"
   For x = 1 to 20
      draw string (20,40+x*10),Item(Bank(x)).name
   Next x
   
   draw string (20,300),"Deposit:"
   For x = 1 to 8
      draw string (10,300+x*10),Item(Inv(x)).name
   Next x
   
   draw string (600,460),"Exit"
   
   b = 1
   Do Until b = 0
      d = getmouse (x,y,,b)
      W
   loop
   
   Do Until multikey(1) Or (mx >= 600 And mx <= 640 And my >= 460 And my <= 480 And b)
      W
      d = getmouse (mx,my,,b)
      For x = 1 to 20
         If mx >= 19 And mx <= 20+Len(trim$(Item(Bank(x)).name))*8 And my >= 39+x*10 And my <= 50+x*10 And b = 1 Then
            line (19,39+x*10)-(20+Len(trim$(Item(Bank(x)).name))*8,50+x*10),rgb(255,255,255),b
            Do while b = 1
               d = getmouse (mx,my,,b)
               W
            loop                      
            line (19,39+x*10)-(20+Len(trim$(Item(Bank(x)).name))*8,50+x*10),rgb(100,100,100),b

            nnn = 0
            For i = 1 to 8
               If Inv(i) > 0 Then nnn += 1
            Next i
            If nnn = 8 Then goto drawbank
            whattoadd = bank(x)
            AddInv whattoadd
            bank(x) = 0
            If sword = x Then SMNE "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
            If shield = x Then SMNE "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
            If magic = x Then magic = 0
            
            goto drawbank
         End If
      Next x
      For x = 1 to 8
         If mx >= 9 And mx <= 20+Len(trim$(Item(Inv(x)).name))*8 And my >= 299+x*10 And my <= 309+x*10 And b = 1 Then
            line (9,299+x*10)-(20+Len(trim$(Item(Inv(x)).name))*8,309+x*10),rgb(255,255,255),b
            Do while b = 1
               d = getmouse (mx,my,,b)
               W
            loop                      
            line (9,299+x*10)-(20+Len(trim$(Item(Inv(x)).name))*8,309+x*10),rgb(100,100,100),b
            line (20,460)-(640,480),rgb(100,100,100),bf
            nnn = 0
            For i = 1 to 20
               If Bank(i) > 0 Then nnn += 1
            Next i
            If nnn = 20 Then goto drawbank
            For i = 1 to 20
               If Bank(i) = 0 Then
                  Bank(i) = Inv(x)
                  Inv(x) = 0
               End If
            Next i
            If sword = x Then SMNE "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
            If shield = x Then SMNE "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
            If magic = x Then magic = 0
            
            goto drawbank
         End If
      Next x
   loop
   
   msg$ = "BANK "
   For x = 1 to 20
      msg$ += str$(bank(x)) + " "
   Next x
   SendMSG msg$,200
   
   while b
      d = getmouse(x,y,,b)
   Wend
End Sub

Sub Shop
   
drawshop:
   line (0,0)-(639,479),rgb(100,100,100),bf
   draw string (20,20),"Welcome to the Shop!"

   For x = 1 to 9
      If trim$(Item(x).name) = "" Then Exit For
      n$ = Item(x).name + space$(30-Len(trim$(Item(x).name))) + "$"+str$(Item(x).cost)
      c = rgb(255,255,255)
      If Item(x).cost > money Then c = rgb(127,0,0)
      draw string (20,30+x*10),n$,c
   Next x
   
   draw string (20,300),"Sell:"
   For x = 1 to 8
      draw string (10,300+x*10),Item(Inv(x)).name+space$(30-Len(trim$(Item(Inv(x)).name)))+"$"+str$(Item(Inv(x)).cost)
   Next x
   
   line (600-(Len(str$(money))+2)*8,20)-(640,30),rgb(100,100,100),bf
   draw string (600-Len(str$(money))*8,20),"$"+str$(money)
   draw string (600,460),"Exit"
   
   b = 1
   Do Until b = 0
      d = getmouse (x,y,,b)
      W
   loop
   
   Do Until multikey(1) Or (mx >= 600 And mx <= 640 And my >= 460 And my <= 480 And b)
      W
      d = getmouse (mx,my,,b)
      For x = 1 to 8
         If mx >= 10 And mx <= 11+Len(trim$(Item(Inv(x)).name))*8 And my >= 300+x*10 And my <= 310+x*10 And b = 1 Then
            line (9,x*10+299)-(8+Len(trim$(Item(Inv(x)).name))*8+1,x*10+308),rgb(255,255,255),b
            Do while b = 1
               d = getmouse (mx,my,,b)
               W
            loop                      
            line (9,x*10+299)-(8+Len(trim$(Item(Inv(x)).name))*8+1,x*10+308),rgb(100,100,100),b
            
            money += Item(Inv(x)).cost
            validmoney = money
            Inv(x) = 0
            If sword = x Then SMNE "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
            If shield = x Then SMNE "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
            If magic = x Then magic = 0
            goto drawshop
         End If
      Next x
      For x = 1 to 9
         If mx >= 20 And mx <= 20+Len(trim$(Item(x).name))*8 And my >= 30+x*10 And my <= 40+x*10 And b = 1 Then
            line (19,29+x*10)-(20+Len(trim$(Item(x).name))*8,39+x*10),rgb(255,255,255),b
            Do while b = 1
               d = getmouse (mx,my,,b)
               W
            loop                      
            line (19,29+x*10)-(20+Len(trim$(Item(x).name))*8,39+x*10),rgb(100,100,100),b
            line (20,460)-(640,480),rgb(100,100,100),bf
            nnn = 0
            For i = 1 to 8
               If Inv(i) > 0 Then nnn += 1
            Next i
            If nnn = 8 Then goto drawshop
            If Item(x).cost <= money Then
               a$ = "a"
               AddInv x
               money -= Item(x).cost
               validmoney = money
               d$ = " "
               n$ = left$(Item(x).name,1)
               If n$ = "A" Or n$ = "E" Or n$ = "I" Or n$ = "O" Or n$ = "U" Then
                  d$ = "n "
               End If
               draw string (20,460),"You bought "+a$+d$+trim$(Item(x).name)+"!"
            Else
               draw string (20,460),"Not enough money!"
            End If
            goto drawshop
         End If
      Next x
      If money <> validmoney Then menumsg = "Hacker!":Exit Sub
   loop
   
   while b
      d = getmouse(x,y,,b)
   Wend
   validmoney = money
End Sub

Function dlfile(sURL As String,LFileName As String) As Integer
    ' This is from freebasic.net, credits to the author but I don't know who made it
    
    dim As HINTERNET hOpen,hFile
    dim As Integer ctot,fh,nret,res,tbsize,t
    dim As String tbuff,ts
    dim As String scUserAgent = "LOS"
    dim As byte ptr mybuff
    mybuff = allocate(2048)
'
    'Create an internet connection
    hOpen=InternetOpen(scUserAgent,INTERNET_OPEN_TYPE_DIRECT,NULL,NULL,0)
    If hOpen=0 Then return -1 'failed..
    '
    'Open the url
    hFile=InternetOpenUrl(hOpen,sURL,NULL,0,INTERNET_FLAG_RELOAD,0)
    If hFile=0 Then return -2 'failed..
    '
    'Let's get the file size..
    ' I think this requires IE 4.0 engine
    tbuff=space(32)
    tbsize=32
    res=HttpQueryInfo(hFile,_
                      HTTP_QUERY_CONTENT_LENGTH,_
                      strptr(tbuff),_
                      @tbsize,_
                      NULL)

    tbsize=val(trim(tbuff))
    'if res=FALSE or tbsize<1 then return -3 'failed, but don't care..
    'tbsize isn't used in this function.
    '
    fh=freefile
    Open LFileName For binary Access write As #fh
    nret=99

    while nret>0
        res=InternetReadFile(hFile,mybuff,2048,@nret)
        If nret>0 Then
            put #fh,,mybuff[0],nret
            ctot+=nret
            '
            'comment this if you want silent running..
            If c = 10 Then Print Chr$(219);:c = 0
            c += 1
        End If
    Wend
    Close fh
    '
    InternetCloseHandle(hFile)
    InternetCloseHandle(hOpen)
    deallocate mybuff
    return ctot'tbsize
'
End Function

Sub playmus
   Do
      ret = MciSendString("open data\music.mid type sequencer alias losmusic",0&,0,0)
      ret = MciSendString("play losmusic wait",0&,0,0)
      ret = MciSendString("close losmusic",0&,0,0)
   loop
End Sub

Sub Init
   ''' Outdated update mechanism, has no effect now since link is broken.
   Print "Checking data.los..."
   res=dlfile("http://www.landofstuff.net/downloads/ver.txt","ver.txt")
   
   Open "ver.txt" For input As #1
   Open "config.ini" For input As #2
   input #2,ipsothisisntanything$
   input #2,thisver
   input #1,thever
   
   If thever > thisver Then dated = 1
   Close #2
   Close #1
   kill "ver.txt"
   If dated = 1 Then
     kill "data.los"

     Print "Downloading data file..."
     res=dlfile("http://www.landofstuff.net/downloads/data.los","data.los")
     
     Open "config.ini" For input As #1
     input #1,ip$
     Close #1
     Open "config.ini" For output As #1
     Print #1,ip$
     Print #1,thever
     Close #1
   End If
   cls
 
   dim length As long,size As long
   
   ' Extract files to Data\
   Print "Loading..."
   Print
   mkdir "Data"
   Open "data.los" For binary As #1
   Get #1,,length
   For x = 1 to length
      Locate 3,x
      Print Chr$(219);
      Locate 5,1
      
      Get #1,,size
      dim f As string * 12
      Get #1,,f
      If rtrim$(f) = "" Then Exit For
      Print "Extracting ";f
      s$ = space$(size)
      Get #1,,s$
      For i = 1 to Len(s$)
         ' Decrypt
         j$ = mid$(s$,i,1)
         j$ = Chr$(255-asc(j$))
         mid$(s$,i,1) = j$
      Next i
      Open "Data\"+rtrim$(f) For binary As #2
      put #2,,s$
      Close #2
   Next x
   Print  
   Close #1
   
   Locate 5,1
   Print "Loading images..."
   
   ' Load character images
   
   For x = 0 to 8
      GuyImg(x) = ImageCreate (32,32)
   Next x
   bload "data\guyu.bmp",GuyImg(dup)
   bload "data\guyr.bmp",GuyImg(dright)
   bload "data\guyd.bmp",GuyImg(ddown)
   bload "data\guyl.bmp",GuyImg(dleft)
   
   bload "data\gb.bmp",GuyImg(5)
   bload "data\gu.bmp",GuyImg(6)
   bload "data\gl.bmp",GuyImg(7)
   
   bload "data\gf.bmp",GuyImg(8)
   
   swordimg = ImageCreate (8,20)
   shieldimg = ImageCreate (16,20)
   shbkimg = ImageCreate (16,20)
   ssimg = ImageCreate (20,20)
   
   swimg = ImageCreate (8,20)
   shimg = ImageCreate (16,20)
   wlwarning = ImageCreate (32,32)
      
   bload "data\sword.bmp",swimg
   bload "data\shield.bmp",shimg   
   bload "data\wl.bmp",wlwarning
   bload "data\ss.bmp",ssimg
   
   cdir = ddown
   walkcycle = 1
   
   i = 0
   
   Locate 5,1
   Print "Loading map...        "
   ' Load map
   Open "data\map.txt" For input As #1
   input #1,startx,starty
   Do Until eof(1)        
      line input #1,s$
      For x = 0 to Len(s$) - 1
         Map(x,i) = mid$(s$,x+1,1)
      Next x
      i += 1
   loop
   Close #1
   
   Locate 5,1
   Print "Loading tiles...      "
   ' Load tiles
   For x = 0 to 9
      Tiles(x) = ImageCreate (32,32)
      bload "Data\t"+trim$(str$(x))+".bmp",Tiles(x)
   Next x
   
   i = 0   
   
   Locate 5,1
   Print "Loading items...      "
   ' Load items
   Open "data\item.txt" For input As #1
   Do Until eof(1)
      line input #1,Item(i).name
      line input #1,Item(i).Type
      input #1,Item(i).cost
      input #1,Item(i).effect
      input #1,Item(i).reqlvl
      i += 1
   loop
   Close #1
   
   i = 0
   
   Locate 5,1
   Print "Loading enemies...    "
   ' Init enemies
   Open "data\enemy.txt" For input As #1
   Do Until eof(1)
      line input #1,Enemy(i).name
      input #1,Enemy(i).str
      input #1,Enemy(i).Def
      input #1,Enemy(i).hp
      input #1,Enemy(i).mindrop
      input #1,Enemy(i).maxdrop
      input #1,Enemy(i).item1
      input #1,Enemy(i).item2
      Enemy(i).img = ImageCreate (32,32)
      bload "Data\"+trim$(str$(i))+".bmp",Enemy(i).img
      i += 1
   loop
   Close #1
   
   ' Init enemy locations
   i = 0
   Open "data\enemyloc.txt" For input As #1
   Do Until eof(1)
      input #1,EnemyLoc(i).x
      input #1,EnemyLoc(i).y
      input #1,EnemyLoc(i).id

      EnemyLoc(i).startx = EnemyLoc(i).x
      EnemyLoc(i).starty = EnemyLoc(i).y
      EnemyLoc(i).used = 1
      EnemyLoc(i).maxhp = Enemy(EnemyLoc(i).id).hp
      EnemyLoc(i).hp = EnemyLoc(i).maxhp
      i += 1
   loop
   Close #1
   
   Locate 5,1
   Print "Loading script...     "
   i = 0
   ' Compile script
   Open "Data\script.txt" For input As #1
   Do Until eof(1)
      line input #1,cmdline$
      cmdline$ = trim$(cmdline$)
      cmd$ = trim$(ucase$(left$(cmdline$,instr(cmdline$," "))))
      If cmd$ = "" Then cmd$ = cmdline$
      param$ = mid$(cmdline$,Len(cmd$)+2)
      If cmd$ = "PRINT" Then
         Print param$
      End If
      If cmd$ = "SLEEP" Then Sleep
      If cmd$ = "NPC" Then
         num = val(left$(param$,2))
         objtyp$ = "NPC"
         cur = num
         spn = 0
      End If
      If cmd$ = "X" Then
         If objtyp$ = "NPC" Then
            NPC(cur).x = val(param$)
         End If
      End If
      If cmd$ = "Y" Then
         If objtyp$ = "NPC" Then
            NPC(cur).y = val(param$)
         End If
      End If
      If cmd$ = "NAME" Then
         If objtyp$ = "NPC" Then
            NPC(cur).name = param$
         End If
      End If
      If cmd$ = "SPEECH" Then
         If objtyp$ = "NPC" Then
            NPC(cur).speech(spn) = param$
         End If
      End If
      If cmd$ = "REPLY" Then
         If objtyp$ = "NPC" Then
            NPC(cur).reply(spn) = param$
            spn += 1
         End If      
      End If
      If cmd$ = "CSC" Then
         spn = 0
      End If
      If cmd$ = "IF_YES" Then
         NPC(cur).speech(spn) = "C1S"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "IF_NO" Then
         NPC(cur).speech(spn) = "C2S"
         NPC(cur).reply(spn) = "."
         spn += 1      
      End If
      If cmd$ = "IF_AC0" Then
         NPC(cur).speech(spn) = "IF0"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "IF_AC1" Then
         NPC(cur).speech(spn) = "IF1"
         NPC(cur).reply(spn) = "."
         spn += 1      
      End If
      If cmd$ = "MONEY" Then
         NPC(cur).speech(spn) = "$$$"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "END_IF" Then
         NPC(cur).speech(spn) = "C=0"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "GOTO" Then
         NPC(cur).speech(spn) = "JMP"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If right$(cmd$,1) = ":" Then
         NPC(cur).speech(spn) = cmd$
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "SLAY" Then
         NPC(cur).speech(spn) = "SLAY"
         NPC(cur).reply(spn) = "."
         NPC(cur).speech(spn+1) = strtok$(param$," ")
         NPC(cur).reply(spn+1) = "."
         NPC(cur).speech(spn+2) = strtok$(""," ")
         NPC(cur).reply(spn+2) = "."
         spn += 3
      End If
      If cmd$ = "TALK" Then
         NPC(cur).speech(spn) = "TALK"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "AC1" Then
         NPC(cur).speech(spn) = "AC1"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "AC0" Then
         NPC(cur).speech(spn) = "AC0"
         NPC(cur).reply(spn) = "."
         spn += 1      
      End If
      If cmd$ = "INV" Then
         NPC(cur).speech(spn) = "INV"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "MSG" Then
         NPC(cur).speech(spn) = "MSG"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "STORE" Then
         NPC(cur).speech(spn) = "STORE"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "BANK" Then
         NPC(cur).speech(spn) = "BANK"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
      If cmd$ = "IF_INV" Then
         NPC(cur).speech(spn) = "IFV"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "IF_MONEY" Then
         NPC(cur).speech(spn) = "IF$"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "RMINV" Then
         NPC(cur).speech(spn) = "RMI"
         NPC(cur).reply(spn) = param$
         spn += 1
      End If
      If cmd$ = "TELE" Then
         NPC(cur).speech(spn) = "TELE"
         NPC(cur).reply(spn) = "."
         NPC(cur).speech(spn+1) = strtok$(param$," ")
         NPC(cur).reply(spn+1) = strtok$(""," ")
         spn += 2
      End If
      If cmd$ = "TALKED" Then
         NPC(cur).speech(spn) = "T++"
         NPC(cur).reply(spn) = "."
         spn += 1
      End If
   loop
     
   Close #1
   
   dim d As Any ptr
   d = threadcreate (@playmus)
   
   udlr = 5
   walkcycle = 6
End Sub

Sub DoScript
   For x = 0 to ubound (NPC)
      If NPC(x).num > 0 Then
         n = 0
         
         For wwwww = 0 to ubound(Killed)
            If Killed(wwwww) = NPC(x).typ Then n += 1
         Next wwwww
         
         If n >= NPC(x).num Then NPC(x).accum = 1: Else NPC(x).accum = 0
      End If
      
      If abs(NPC(x).x-playerx) <= 1 And abs(NPC(x).y-playery) <= 1 And NPC(x).name <> "" Then
         dontblockon = 1
         
         redrawflag = 2
         
         For i = NPC(x).sp to ubound(NPC(x).speech)
           
            Do
               W
               ' If message is empty, it's done
               If NPC(x).speech(i) = "" And NPC(x).reply(i) = "" Then    
                  playerx = oldplayerx
                  playery = oldplayery
                  Redraw
                  Exit Sub
               End If

               Redraw
               
               ' Check for special instructions
               Select Case (NPC(x).speech(i))
                  Case "INV"
                     AddInv val(NPC(x).reply(i))
                     i += 1
                  Case "RMI"
                     If InInv(NPC(x).reply(i)) <> -1 Then
                        If sword=(InInv(NPC(x).reply(i))) Then SMNE "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
                        If shield=(InInv(NPC(x).reply(i))) Then SMNE "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
                        If magic=(InInv(NPC(x).reply(i))) Then magic = 0

                        Inv(InInv(NPC(x).reply(i)))=0
                     End If
                     i += 1
                  Case "MSG"
                     scrmsg = NPC(x).reply(i)
                     msgtime = 30

                     i += 1
                  Case "C1S"
                     If NPC(x).choice <> 1 Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0" And nis = 0
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If
                        loop
                     End If
                     i += 1
                  Case "C2S"
                     If NPC(x).choice <> 2 Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0" And nis = 0
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If
                        loop 
                     End If
                     i += 1
                  Case "C=0"
                     NPC(x).choice = 0
                     i += 1
                  Case "AC0"
                     NPC(x).accum = 0
                     i += 1
                  Case "AC1"
                     NPC(x).accum = 1
                     i += 1
                  Case "IF0"
                     If NPC(x).accum <> 0 Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0"
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If
                        loop                     
                     End If
                     i += 1
                  Case "IF1"
                     If NPC(x).accum <> 1 Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0"
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If                           
                        loop                     
                     End If
                     i += 1
                  Case "IFV"
                     If InInv(NPC(x).reply(i)) = -1 Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0"
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If                           
                        loop                     
                     End If                     
                     i += 1
                  Case "IF$"
                     If money < val(NPC(x).reply(i)) Then
                        nis = 1
                        Do Until NPC(x).speech(i) = "C=0"
                           i += 1
                           If left$(NPC(x).speech(i),2) = "IF" Or NPC(x).speech(i) = "C1S" Or NPC(x).speech(i) = "C2S" Then
                              nis += 1
                           End If
                           If NPC(x).speech(i) = "C=0" Then
                              nis -= 1
                           End If                           
                        loop                     
                     End If                     
                     i += 1                  
                  Case "$$$"
                     money += val(NPC(x).reply(i))
                     validmoney = money
                     i += 1
                  Case "JMP"
                     l$ = NPC(x).reply(i)+":"
                     Do Until NPC(x).speech(i) = l$
                        i += 1
                     loop
                     i += 1
                     NPC(x).sp = i
                     NPC(x).o = 1
                     'For wwwww = 0 to ubound(Killed)
                     '   Killed(wwwww) = 255
                     '   Talk(wwwww) = 255
                     'Next wwwww
                     If NPC(x).speech(i) = "SLAY" Then
                        NPC(x).num = val(NPC(x).speech(i+1))
                        NPC(x).typ = val(NPC(x).speech(i+2))
                        i += 3
                     End If
                     playerx = oldplayerx
                     playery = oldplayery
                     Exit Do                     
                  Case "SLAY"
                     NPC(x).num = val(NPC(x).speech(i+1))
                     NPC(x).typ = val(NPC(x).speech(i+2))
                     i += 3
                  Case "TALK"
                     num = val(NPC(x).reply(i))
                     If NPC(num).o = 1 Then NPC(x).accum = 1: Else NPC(x).accum = 0

                     i += 1                     
                  Case "BANK"
                     DoBank
                     i += 1
                  Case "STORE"
                     Shop
                     i += 1
                  Case "TELE"
                     playerx = val(NPC(x).speech(i+1))
                     playery = val(NPC(x).reply(i+1))
                     oldplayerx = playerx
                     oldplayery = playery
                     SendMSG "TELE "+str$(playerx)+" "+str$(playery),Len("TELE "+str$(playerx)+" "+str$(playery))
                     Redraw
                     i += 2
                  Case "T++"                           
                     NPC(x).o = 1
                     i += 1
                  Case Else
                     If abs(NPC(x).x-playerx) <= 1 And abs(NPC(x).y-playery) <= 1 And NPC(x).name <> "" Then
                        ' Draw message
                        draw string (4,420),NPC(x).speech(i)
                        paint GuyImg(ddown),(8,30),rgb(255,255,255),rgb(0,0,0)
                        paint GuyImg(ddown),(24,30),rgb(255,255,255),rgb(0,0,0)
                        put (4,430),GuyImg(ddown),trans
                        draw string (4,466),NPC(x).name,rgb(100,100,100)
                        
                        ' Click here to continue message
                        draw string (200,460),"Click here to continue"
                        Do Until b = 1 And mx >= 200 And mx <= 200+22*8 And my >= 456 And my <= 472
                           d = getmouse (mx,my,,b)
                           If mx >= 200 And mx <= 200+22*8 And my >= 456 And my <= 472 Then
                              draw string (200,460),"Click here to continue",rgb(0,0,255)
                           Else
                              draw string (200,460),"Click here to continue"
                           End If
                           W
                        loop
                        Do Until b = 0
                           d = getmouse (my,my,,b)
                           W
                        loop
                        
                        ' Now draw your speech
                        If NPC(x).reply(i) = "" Then
                           playerx = oldplayerx
                           playery = oldplayery
                           Redraw
                           Exit Sub
                        End If
                        
                        Redraw
                        
                        ' Draw message
                        If NPC(x).reply(i) = "_YES_NO" Then     ' Choice of yes or no
                           draw string (4,420),"Yes"
                           draw string (600,420),"No"
                           
                           ' Choose one
                           Do Until b = 1 And NPC(x).choice <> 0
                              d = getmouse (mx,my,,b)
                              If mx >= 4 And mx <= 28 And my >= 416 And my <= 432 Then
                                 draw string (4,420),"Yes",rgb(0,0,255)
                                 NPC(x).choice = 1
                              ElseIf mx >= 600 And mx <= 616 And my >= 416 And my <= 432 Then
                                 draw string (600,420),"No",rgb(0,0,255)
                                 NPC(x).choice = 2
                              Else
                                 draw string (4,420),"Yes"
                                 draw string (600,420),"No"
                                 NPC(x).choice = 0                  
                              End If
                              W
                           loop
                           Do Until b = 0
                              d = getmouse (mx,my,,b)
                              W
                           loop
                        ElseIf NPC(x).reply(i) = "_SKIP" Then  ' Skip reply
                           
                        Else
                           paint GuyImg(ddown),(8,30),shirtcolor,rgb(0,0,0)
                           paint GuyImg(ddown),(24,30),shirtcolor,rgb(0,0,0)
                           draw string (636-Len(NPC(x).reply(i))*8,420),NPC(x).reply(i)
                           put (600,430),GuyImg(ddown),trans
                           draw string (636-Len(user)*8,466),user
                                       
                           ' Click here to continue message
                           draw string (200,460),"Click here to continue"
                           Do Until b = 1
                              d = getmouse (mx,my,,b)
                              If mx >= 200 And mx <= 200+22*8 And my >= 456 And my <= 472 Then
                                 draw string (200,460),"Click here to continue",rgb(0,0,255)
                              Else
                                 draw string (200,460),"Click here to continue"
                              End If
                              W
                           loop
                           Do Until b = 0
                              d = getmouse (mx,my,,b)
                              W
                           loop
                        End If
                        i += 1
                     Else
                        Exit Sub
                     End If
                End Select
            loop
            dontblockon = 0
         Next i
      End If
   Next x
   If money <> validmoney Then menumsg = "Hacker!":Exit Sub
   redrawflag = 0
End Sub

Sub SMNE (msg As string, length As integer)
   dim l As integer = length

   dummy = Send(s,msg,4000,0)        ' Send message
End Sub

Sub SendMSG (msg As string, length As integer)
   dim l As integer = length
   If l > strlen(msg) Then l = strlen(msg)
   curmsg = msg

   For x = 1 to 5      
      dummy = Send(s,msg,4000,0)        ' Send message
      n1! = Timer
      Do Until curmsg = "" Or Timer >= n1! + 0.5:loop
      If curmsg = "" Then Exit Sub
   Next x
   menumsg = "Lost connection to server!"
   Exit Sub
End Sub

Function Tile (n As string) As Any ptr
   If val(n) <> 0 Or n = "0" Then Tile = Tiles(val(n)):Exit Function
End Function

Sub Attack (x)
   cdir = dleft
   udlr = 5
   redrawflag = 1
   
   Redraw
   
   playerx = OtherPos(x).x + 1
   playery = OtherPos(x).y
   
   screencopy
   screenset 0,0
   
   px1 = playerx - 10
   px2 = playerx + 10
   py1 = playery - 6
   py2 = playery + 6
   If px1 < 0 Then chgx = abs(px1):px1 = 0: px2 += chgx: xabsolutex = 1
   If py1 < 0 Then chgy = abs(py1):py1 = 0: py2 += chgy: xabsolutey = 1
   If xabsolutex Then xabsolute = 1
   If xabsolutey Then xabsolute = 2
   If xabsolutex And xabsolutey Then xabsolute = 3   
   If xabsolute = 1 Then
      yx = playerx
      yy = 6
   ElseIf xabsolute = 2 Then
      yx = 10
      yy = playery
   ElseIf xabsolute = 3 Then
      yy = playery
      yx = playerx
   Else
      yx = 10:yy = 6
   End If
   
   yy -= 1 
   ex = yx - 1 
   ey = yy

   Do
      SMNE "S",1
      SMNE "G",1
      W
      n1! = Timer
      
      ' Check if they want to change fight styles
      d = getmouse(mx,my,,b)
      If mx >= 20 And mx <= 200 And my >= 44 And my <= 53 And b = 1 Then fightmode = 0
      If mx >= 20 And mx <= 200 And my >= 54 And my <= 63 And b = 1 Then fightmode = 1
      
      ' draw fightmode selector
      line (20,20)-(200,100),rgb(0,128,200),bf
      If fightmode = 0 Then     ' Attack
         line (20,43)-(200,52),rgb(128,0,0),bf
      Else                      ' Defend
         line (20,53)-(200,62),rgb(128,0,0),bf
      End If
      draw string (24,24),"Choose style:"
      draw string (24,44),"Attack (+4 str exp)"
      draw string (24,54),"Defend (+4 def exp)"
      
      'Redraw
      if Item(Inv(sword)).name = "Dual Strike" Then
         hit = (1/2)*yourdamage!
         SMNE "HIT "+Str$(hit)+" "+Str$(youfight),Len("HIT "+Str$(hit)+" "+Str$(youfight))
         otherhp -= hit
         If hit > 0 Then Draw String (ex*32+12,ey*32-4),Str$(hit)
      EndIf
      
      ybar = playerhp/maxhp*28-2
      tbar = otherhp/othermaxhp*28-2
      If tbar < 0 Then tbar = 0
      If ybar < 0 Then ybar = 0
      line (yx*32+2,yy*32-16)-(yx*32+30,yy*32-8),0,bf
      If playerhp > 0 Then line (yx*32+3,yy*32-15)-(yx*32+3+ybar,yy*32-9),rgb(0,255,0),bf
      line (ex*32+2,ey*32-16)-(ex*32+30,ey*32-8),0,bf
      If otherhp > 0 Then line (ex*32+3,ey*32-15)-(ex*32+3+tbar,ey*32-9),rgb(0,255,0),bf
      
      yourdamage! = int(Rnd+((Rnd * (playerstr+(Rnd*(Item(Inv(sword)).effect)))/5)))
      If Rnd*playerstr < Rnd*otherdef Then yourdamage! = 0
      If Rnd < 1/2 Then yourdamage! = 0
      If theyuseshield = 0 Then chance! = 1/4: Else chance! = 3/4
      If magic > 0 And Rnd < chance! Then yourdamage! = int(Rnd*(Item(Inv(magic)).effect+1))
      
      If yourdamage! < 0 Then yourdamage! = 0
      
      SMNE "HIT "+str$(yourdamage!)+" "+str$(youfight),Len("HIT "+str$(yourdamage!)+" "+str$(youfight))
	  
	  If fightmode = 0 Then strexp += 4*yourdamage!
	  If fightmode = 1 Then defexp += 4*yourdamage!
	  
	  SMNE "HP "+str$(playerhp)+" "+str$(youfight),Len("HP "+str$(playerhp)+" "+str$(youfight))
	  
	  Do Until Timer >= n1! + 3/4
	     
	  Loop
	  
	  roundno += 1

      If sword Then udlr = 8: Else udlr = 7	  

      curfight = i
      
      Redraw

      ' Check if they want to change fight styles
      d = getmouse(mx,my,,b)
      If mx >= 20 And mx <= 200 And my >= 44 And my <= 53 And b = 1 Then fightmode = 0
      If mx >= 20 And mx <= 200 And my >= 54 And my <= 63 And b = 1 Then fightmode = 1
      
      ' draw fightmode selector
      line (20,20)-(200,100),rgb(0,128,200),bf
      If fightmode = 0 Then     ' Attack
         line (20,43)-(200,52),rgb(128,0,0),bf
      Else                      ' Defend
         line (20,53)-(200,62),rgb(128,0,0),bf
      End If
      draw string (24,24),"Choose style:"
      draw string (24,44),"Attack (+4 str exp)"
      draw string (24,54),"Defend (+4 def exp)"
      
      ybar = playerhp/maxhp*28-2
      tbar = otherhp/othermaxhp*28-2
      If tbar < 0 Then tbar = 0
      If ybar < 0 Then ybar = 0
      line (yx*32+2,yy*32-16)-(yx*32+30,yy*32-8),0,bf
      If playerhp > 0 Then line (yx*32+3,yy*32-15)-(yx*32+3+ybar,yy*32-9),rgb(0,255,0),bf
      line (ex*32+2,ey*32-16)-(ex*32+30,ey*32-8),0,bf
      If otherhp > 0 Then line (ex*32+3,ey*32-15)-(ex*32+3+tbar,ey*32-9),rgb(0,255,0),bf
      Draw String (ex*32+12,ey*32-4),Str$(yourdamage!)
      
      If theyusemagic > 0 Then
         ' draw a blue circle
         circle (yx*32+4,yy*32+48),4,rgb(0,0,255)
         paint (yx*32+4,yy*32+48),rgb(40,0,160),rgb(0,0,255)      
      End If

      If magic > 0 Then
      	If Inv(magic) = 27 or Inv(magic) = 30 then
      	   ' Lightning
      		
      		' 1. Make points where line bends
      		Dim lx(9) As Integer
      		Dim ly(9) As Integer
      		Dim bx(5 To 9) As integer
      		For ii = 0 To 8
      			lx(ii) = 320+(Rnd*20-20)
      			ly(ii) = 140+ii*6
      		Next
      		lx(9) = 300-Rnd*8
      		ly(9) = 210
      		
      		If Inv(magic) = 27 Then col = RGB(255,255,0):Else col = RGB(200,0,0)
      		
      		' 2. Plot the points
      		For ii = 0 To 8
      			Line (lx(ii),ly(ii))-(lx(ii+1),ly(ii+1)),col
      			Line (lx(ii)+1,ly(ii))-(lx(ii+1)+1,ly(ii+1)),col
      		Next
         ElseIf Inv(magic) = 36 Then
            ' draw a red circle + box
            Line (yx*32-10,yy*32+38)-(yx*32+4,yy*32+58),RGB(200,20,0),bf
            Line (yx*32+4,yy*32+38)-(yx*32+12,yy*32+44),RGB(200,20,0)
            Line (yx*32+4,yy*32+58)-(yx*32+12,yy*32+52),RGB(200,20,0)
            Line (yx*32+12,yy*32+44)-(yx*32+12,yy*32+52),RGB(200,20,0)           
            paint (yx*32+6,yy*32+44),rgb(200,20,0)
            circle (yx*32-10,yy*32+48),10,rgb(255,0,0)
            paint (yx*32-10,yy*32+48),rgb(255,0,0)            
         else
            ' draw a blue circle
            circle (yx*32-4,yy*32+48),4,rgb(0,0,255)
            paint (yx*32-4,yy*32+48),rgb(40,0,160),rgb(0,0,255)
         End if
      End If
            
      curfight = 0
      SMNE "S",1
      Do Until Timer >= n1! + 1
         
      loop
      udlr = 5
      Redraw
      
      If playerhp <= 0 Then ' You lose
      	nofighttime = 0
         savecount = 0
         m$ = str$(youfight) + " "
         For iii = 1 to 8
            invpiece = Inv(iii)
            m$ += str$(invpiece) + " "
         Next iii
         
         SendMSG "ITEMS "+m$,Len("ITEMS "+str$(m$))

         SendMSG "LOSE "+str$(youfight),Len("LOSE "+str$(youfight))
         SendMSG "HP 0 "+str$(youfight),Len("HP 0 "+str$(youfight))
         sentfight = 0
         youfight = -2
         theyfight = -1
         redrawflag = 0
         theylose = 0
         Exit Sub
      End If

      If theylose = 1 Then    ' You win!
      	nofighttime = 0
         savecount = 0
         ' Get unstuck
         If Map(playerx,playery) > "4" Then playerx = oldplayerx:playery = oldplayery
         If Inv(sword) >= 37 And Inv(sword) <= 40 Then
         	Inv(sword) -= 36
         	Message(5) = "Your weapon heals you slightly."
         	playerhp += 1/4*maxhp
         	If playerhp > maxhp Then playerhp = maxhp
         EndIf
         Redraw
         CheckLevel 0
         dontblockon = 0
         theylose = 0
         sentfight = 0
         redrawflag = 0      
         Exit Sub
      End If
      
      If multikey(1) Then End
   loop Until (multikey(&h48) Or multikey(&h4b) Or multikey(&h4d) Or multikey(&h50)) And roundno > 2
   If Inv(sword) >= 37 And Inv(sword) <= 40 Then Inv(sword) -= 36:Message(5) = "Your weapon degrades."   ' Empowered dagger becomes dagger, etc.
   nofighttime = 0
   theylose = 0
  
   SMNE "LOSE "+str$(youfight),Len("LOSE "+str$(youfight))    ' So they stop attacking you
   youfight = -2
   theyfight = -1
   sentfight = 0
   
   playerx = oldplayerx
   playery = oldplayery
   redrawflag = 0
   Redraw
End Sub

Sub Fight (i,x,y)
   Static countdownhat
   
   dontblockon = 1
   redrawflag = 1
   
   Redraw
   screencopy
   screenset 0,0

   yx = x     
   yy = y - 1 
   ex = x - 1 
   ey = y
   
   hp = EnemyLoc(i).hp
   beginhp = EnemyLoc(i).maxhp
   enemystr = Enemy(EnemyLoc(i).id).str
   enemydef = Enemy(EnemyLoc(i).id).Def

   Do
      SMNE "S",1
      SMNE "G",1
      W
      n1! = Timer
      
      ' Check If they want to change fight styles
      d = getmouse(mx,my,,b)
      If mx >= 20 And mx <= 200 And my >= 44 And my <= 53 And b = 1 Then fightmode = 0
      If mx >= 20 And mx <= 200 And my >= 54 And my <= 63 And b = 1 Then fightmode = 1
      
      ' draw fightmode selector
      line (20,20)-(200,100),rgb(0,128,200),bf
      If fightmode = 0 Then     ' Attack
         line (20,43)-(200,52),rgb(128,0,0),bf
      Else                      ' Defend
         line (20,53)-(200,62),rgb(128,0,0),bf
      End If
      draw string (24,24),"Choose style:"
      draw string (24,44),"Attack (+4 str exp)"
      draw string (24,54),"Defend (+4 def exp)"
      
      'Redraw
      if Item(Inv(sword)).name = "Dual Strike" Then
         hit = (1/2)*yourdamage!
         hp -= hit
         If hit > 0 Then Draw String (ex*32+12,ey*32-4),Str$(hit)
      EndIf
      
      ybar = playerhp/maxhp*28-2
      tbar = hp/beginhp*28-2
      If tbar < 0 Then tbar = 0
      If ybar < 0 Then ybar = 0
      line (yx*32+2,yy*32-16)-(yx*32+30,yy*32-8),0,bf
      If playerhp > 0 Then line (yx*32+3,yy*32-15)-(yx*32+3+ybar,yy*32-9),rgb(0,255,0),bf
      line (ex*32+2,ey*32-16)-(ex*32+30,ey*32-8),0,bf
      If hp > 0 Then line (ex*32+3,ey*32-15)-(ex*32+3+tbar,ey*32-9),rgb(0,255,0),bf
      
      yourdamage! = int(Rnd+((Rnd * (playerstr+(Rnd*(Item(Inv(sword)).effect)))/5)))
      If Rnd*playerstr < Rnd*Rnd*enemydef Then yourdamage! = 0
      If magic > 0 Then yourdamage! = int(Rnd*(Item(Inv(magic)).effect+1))
      If Rnd<1/2 Then yourdamage! = 0
      If yourdamage! < 0 Then yourdamage! = 0
      If yourdamage! > hp Then yourdamage! = hp
      
      If Enemy(EnemyLoc(i).id).name = "The Hat" Then
      	
	  	  If countdownhat>0 Then countdownhat += 1
	  	  If hp < 200 And countdownhat = 0 Then countdownhat = 1
	  	  	If countdownhat>0 Then Circle (ex*32+16,ey*32+16),40-countdownhat*8,RGB(255,0,0),,,,f
	  	  	  If countdownhat = 5 Then
	  	  	  	  hp = 400
	  	  	  ElseIf countdownhat > 5 Then
	  	  	  	  playerhp -= yourdamage!
	  	  	  	  	x = Rnd*8+1
	  	  	  	  	  If sword = x Then SMNE "SW",2:sword = 0:line swordimg,(0,0)-(50,50),rgb(255,0,255),bf
            If shield = x Then SMNE "SH",2:shield = 0:line shieldimg,(0,0)-(50,50),rgb(255,0,255),bf:line shbkimg,(0,0)-(50,50),rgb(255,0,255),bf
	  	  	  If magic = x Then magic = 0
	  	  	  	If shield = 0 Then playerhp /= 2
	  	  	  	  If playerhp < 0 Then playerhp = 0
	  	  	  EndIf
	  	  EndIf
	  hp -= yourdamage!
	  
	  theirdamage! = int(((Rnd * (enemystr+1)/5)))
	  If Rnd*enemystr < Rnd*Rnd*playerdef+Rnd*Item(Inv(shield)).effect Then theirdamage! = 0	  
	  If theirdamage! < 0 Then theirdamage! = 0
	  if rnd<1/2 then theirdamage! = 0
	  If Item(Inv(sword)).name = "Dilemma" Then theirdamage! = theirdamage! * 1.5 + 2
	  If theirdamage! > playerhp Then theirdamage! = playerhp
	  
	  If Enemy(EnemyLoc(i).id).name = "The Hat" Then
	   If theirdamage! >= playerhp And roundno <= 3 Then theirdamage! = playerhp - Rnd*10
	  	If theirdamage! < 0 Then theirdamage! = 0
	  EndIf
	  
	  playerhp -= theirdamage!
	  
	  If fightmode = 0 Then strexp += 4*yourdamage!
	  If fightmode = 1 Then defexp += 4*yourdamage!

	  Do Until Timer >= n1! + 3/4
	  loop
	  roundno += 1

      If sword Then udlr = 8: Else udlr = 7

      curfight = i
      
      Redraw

      ' Check If they want to change fight styles
      d = getmouse(mx,my,,b)
      If mx >= 20 And mx <= 200 And my >= 44 And my <= 53 And b = 1 Then fightmode = 0
      If mx >= 20 And mx <= 200 And my >= 54 And my <= 63 And b = 1 Then fightmode = 1
      
      ' draw fightmode selector
      line (20,20)-(200,100),rgb(0,128,200),bf
      If fightmode = 0 Then     ' Attack
         line (20,43)-(200,52),rgb(128,0,0),bf
      Else                      ' Defend
         line (20,53)-(200,62),rgb(128,0,0),bf
      End If
      draw string (24,24),"Choose style:"
      draw string (24,44),"Attack (+4 str exp)"
      draw string (24,54),"Defend (+4 def exp)"
      
      ybar = playerhp/maxhp*28-2
      tbar = hp/beginhp*28-2
      If tbar < 0 Then tbar = 0
      If ybar < 0 Then ybar = 0
      line (yx*32+2,yy*32-16)-(yx*32+30,yy*32-8),0,bf
      If playerhp > 0 Then line (yx*32+3,yy*32-15)-(yx*32+3+ybar,yy*32-9),rgb(0,255,0),bf
      line (ex*32+2,ey*32-16)-(ex*32+30,ey*32-8),0,bf
      If hp > 0 Then line (ex*32+3,ey*32-15)-(ex*32+3+tbar,ey*32-9),rgb(0,255,0),bf
      Draw String (ex*32+12,ey*32-4),Str$(yourdamage!)
      
      If magic > 0 Then
      	If Inv(magic) = 27 or Inv(magic) = 30 then
      	   ' Lightning
      		
      		' 1. Make points where line bends
      		Dim lx(9) As Integer
      		Dim ly(9) As Integer
      		Dim bx(5 To 9) As integer
      		For ii = 0 To 8
      			lx(ii) = 320+(Rnd*20-20)
      			ly(ii) = 140+ii*6
      		Next
      		lx(9) = 300-Rnd*8
      		ly(9) = 210
      		
      		If Inv(magic) = 27 Then col = RGB(255,255,0):Else col = RGB(200,0,0)
      		
      		' 2. Plot the points
      		For ii = 0 To 8
      			Line (lx(ii),ly(ii))-(lx(ii+1),ly(ii+1)),col
      			Line (lx(ii)+1,ly(ii))-(lx(ii+1)+1,ly(ii+1)),col
      		Next
         ElseIf Inv(magic) = 36 Then
            ' draw a red circle + box
            Line (yx*32-10,yy*32+38)-(yx*32+4,yy*32+58),RGB(200,20,0),bf
            Line (yx*32+4,yy*32+38)-(yx*32+12,yy*32+44),RGB(200,20,0)
            Line (yx*32+4,yy*32+58)-(yx*32+12,yy*32+52),RGB(200,20,0)
            Line (yx*32+12,yy*32+44)-(yx*32+12,yy*32+52),RGB(200,20,0)           
            paint (yx*32+6,yy*32+44),rgb(200,20,0)
            circle (yx*32-10,yy*32+48),10,rgb(255,0,0)
            paint (yx*32-10,yy*32+48),rgb(255,0,0)            
         else
            ' draw a blue circle
            circle (yx*32-4,yy*32+48),4,rgb(0,0,255)
            paint (yx*32-4,yy*32+48),rgb(40,0,160),rgb(0,0,255)
         End if
      End If
      
      curfight = 0
      SMNE "S",1
	  
      Do Until Timer >= n1! + 1
      loop
      timestill += 10
      udlr = 5
      Redraw
      
      If playerhp <= 0 Then ' You lose
      	sentfight = 0
         dontblockon = 0
         redrawflag = 0
         EnemyLoc(i).hp = hp
         Exit Sub
      End If      
                  
      If hp <= 0 Then    ' You win!
         savecount = 0
         sentfight = 0
         
         ' Item1 = percent chance, Item2 = item
         chance = Rnd * 100 + 1
         If chance >= (100-Enemy(EnemyLoc(i).id).Item1) And Enemy(EnemyLoc(i).id).Item2 <> 0 Then AddInv Enemy(EnemyLoc(i).id).Item2
    
         ' For quests
         Killed(kn) = EnemyLoc(i).id
         kn += 1
         
         ' Get unstuck
         If Map(playerx,playery) > "4" Then playerx = oldplayerx:playery = oldplayery
         If Inv(sword) >= 37 And Inv(sword) <= 40 Then
         	Inv(sword) -= 36
         	Message(5) = "Your weapon heals you slightly."
         	playerhp += 1/4*maxhp
         	If playerhp > maxhp Then playerhp = maxhp
         EndIf
         
         EnemyLoc(i).respawn = 1
         EnemyLoc(i).hp = EnemyLoc(i).maxhp
         Redraw
         CheckLevel i
         dontblockon = 0
         redrawflag = 0      
         Exit Sub
      End If
      
   loop Until (multikey(&h48) Or multikey(&h4b) Or multikey(&h4d) Or multikey(&h50)) And roundno > 2
   
   If Inv(sword) >= 37 And Inv(sword) <= 40 Then Inv(sword) -= 36:Message(5) = "Your weapon degrades."
   
   EnemyLoc(i).hp = hp
   EnemyLoc(i).respawn = -36
   
   sentfight = 0
   
   Redraw
   
   dontblockon = 0
   redrawflag = 0  
End Sub

Sub ER (a,b,c,d,e,f)
   If f >= a And f <= b And e <= c Then e = d
End Sub

Sub CheckLevel (e)
   oldstr = playerstr
   olddef = playerdef
   
   ex = strexp
   lv = playerstr
   ' Str exp
   For i = 3 to 99
      ER 3*(i+1)^3+1,3*(i+2)^3+1,i,i+1,lv,ex
   Next i
   lvs = lv
   
   ex = defexp
   lv = playerdef
   ' Def exp
   For i = 3 to 99
      ER 3*(i+1)^3+1,3*(i+2)^3+1,i,i+1,lv,ex
   Next i      
   lvd = lv   

   gm = int (Rnd * (Enemy(EnemyLoc(e).id).maxdrop-Enemy(EnemyLoc(e).id).mindrop)) + Enemy(EnemyLoc(e).id).mindrop
   If money <> validmoney Then menumsg = "Hacker!":Exit Sub
   money += gm
   validmoney = money
   
   newstr = lvs
   newdef = lvd   
   
   d = newstr - oldstr + newdef - olddef
   If playerstr <> lvs Or playerdef <> lvd Then
      playerstr = lvs
      playerdef = lvd
      
      maxhp += d
      If maxhp > 99 Then maxhp = 99         ' Max stat is 99, so be consistent
      playerhp += d

      ' Level up message
      screenset 0,0
      draw string (40,40),"You've leveled up!",rgb(255,255,0)
      draw string (40,60),"New strength level: "+str$(playerstr)',0
      draw string (40,70),"New defense level: "+str$(playerdef)',0
      draw string (40,80),"New max HP: "+str$(maxhp)
      draw string (200,200),"Okay"
      Do Until (mx >= 200 And my >= 200 And mx <= 232 And my <= 210 And b)
         d = getmouse (mx,my,,b)
         W
      loop
      savecount = 0
   End If
End Sub

Sub MoveEnemies
   px1 = playerx - 16
   px2 = playerx + 16
   py1 = playery - 12
   py2 = playery + 12
   If px1 < 0 Then chgx = abs(px1):px1 = 0: px2 += chgx: xabsolutex = 1
   If py1 < 0 Then chgy = abs(py1):py1 = 0: py2 += chgy: xabsolutey = 1
   If xabsolutex Then xabsolute = 1
   If xabsolutey Then xabsolute = 2
   If xabsolutex And xabsolutey Then xabsolute = 3

   For x = 0 to ubound(EnemyLoc)
       If EnemyLoc(x).respawn > 0 Then
          EnemyLoc(x).respawn += 1
          If EnemyLoc(x).respawn = 1500 Then
             EnemyLoc(x).respawn = 0
             EnemyLoc(x).x = EnemyLoc(x).startx
             EnemyLoc(x).y = EnemyLoc(x).starty
          End If
       End If
       If EnemyLoc(x).respawn < 0 Then
          EnemyLoc(x).respawn += 1
       End If
       If EnemyLoc(x).used = 1 And EnemyLoc(x).respawn = 0 And EnemyLoc(x).x >= px1 And EnemyLoc(x).x <= px2 And EnemyLoc(x).y >= py1 And EnemyLoc(x).y <= py2 Then                                          
          If (Enemy(EnemyLoc(x).id).str + Enemy(EnemyLoc(x).id).Def) / 2 > (playerstr+playerdef) / 4 Or theenemy = x Then
            ' Check if you're in fighting range
            If abs(playerx-EnemyLoc(x).x) <= 1 And abs(playery-EnemyLoc(x).y) <= 1 And Map(Int(EnemyLoc(x).x),Int(EnemyLoc(x).y)) <= "4" Then
               ' Move so that you're next to each other
               EnemyLoc(x).x = playerx - 1
               EnemyLoc(x).y = playery
               difx = 0
               dify = 0
               ' It can't be on an unwalkable tile
               If Map(EnemyLoc(x).x,EnemyLoc(x).y) > "4" Then
                  EnemyLoc(x).x += 1
                  playerx += 1
                  'Redraw
               End If
               'Redraw

               ' Fight!                           
               If xabsolute = 1 Then
                  yx = playerx
                  yy = 6
               ElseIf xabsolute = 2 Then
                  yx = 10
                  yy = playery
               ElseIf xabsolute = 3 Then
                  yy = playery
                  yx = playerx
               Else
                  yx = 10:yy = 6
               End If
               cdir = dleft
               udlr = 5
               Redraw              
              
               Fight (x,yx,yy)
               theenemy = -1
            End If
          End If

            ' Move the enemy
            oldtile$ = Map(EnemyLoc(x).x,EnemyLoc(x).y)
            
            direction = int(Rnd * 5)
            ox = EnemyLoc(x).x
            oy = EnemyLoc(x).y
            If EnemyLoc(x).turned Then EnemyLoc(x).turned = FALSE:direction = 0
            Select Case(direction)
               Case 0,1  ' Forward
                  Select Case(EnemyLoc(x).dir)
                     Case 0
                        If Map(Int(EnemyLoc(x).x),Int(EnemyLoc(x).y-1)) <= "4" Then EnemyLoc(x).y -= 0.2
                     Case 1
                        If Map(Int(EnemyLoc(x).x)+1,Int(EnemyLoc(x).y)) <= "4" Then EnemyLoc(x).x += 0.2
                     Case 2
                        If Map(Int(EnemyLoc(x).x),Int(EnemyLoc(x).y+1)) <= "4" Then EnemyLoc(x).y += 0.2
                     Case 3
                        If Map(Int(EnemyLoc(x).x)-1,Int(EnemyLoc(x).y)) <= "4" Then EnemyLoc(x).x -= 0.2
                  End Select
               Case 2  ' Left
                  ox = EnemyLoc(x).x
                  oy = EnemyLoc(x).y               
                  EnemyLoc(x).dir -= 1
                  If EnemyLoc(x).dir < 0 Then EnemyLoc(x).dir = 3
                  EnemyLoc(x).turned = 1
               Case 3  ' Right
                  ox = EnemyLoc(x).x
                  oy = EnemyLoc(x).y
                  EnemyLoc(x).dir += 1
                  If EnemyLoc(x).dir > 3 Then EnemyLoc(x).dir = 0
                  EnemyLoc(x).turned = 1
            End Select
            If Map(EnemyLoc(x).x,EnemyLoc(x).y) > "4" Or EnemyLoc(x).x < 0 Or EnemyLoc(x).y < 0 Then EnemyLoc(x).x = ox:EnemyLoc(x).y = oy
            If Map(EnemyLoc(x).x,EnemyLoc(x).y) <> oldtile$ Then EnemyLoc(x).x = ox:EnemyLoc(x).y = oy 
       End If     
   Next
End Sub

Sub SetClothes
   line (0,0)-(639,479),rgb(1,1,1),bf
   draw string (20,20),"Choose your clothes"
   line (6,36)-(46,104),rgb(0,200,200),bf
   line (6,36)-(46,104),rgb(200,200,200),b
   shirtcolor = rgb(254,254,254)
   pantcolor = rgb(254,254,254)
   draw string (601,471),"Done"
   Do Until mx >= 600 And mx <= 640 And my >= 470 And my <= 480 And b = 1
      W
      d = getmouse (mx,my,,b)
      put (10,40),GuyImg(ddown),trans
      put (10,72),GuyImg(5),trans
      color rgb(255,255,255)
      Locate 29,6
      Print "Shirt color       White Black Red Green Blue Magenta Cyan Yellow Gray"
      Locate 30,6
      color ,rgb(0,0,0)
      Print "                  ";
      color ,rgb(254,254,254)
      Print "      ";
      color ,rgb(1,1,1)
      Print "      ";
      color ,rgb(200,0,0)
      Print "    ";
      color ,rgb(0,200,0)
      Print "      ";
      color ,rgb(0,0,100)
      Print "     ";
      color ,rgb(254,0,254)
      Print "        ";
      color ,rgb(0,255,255)
      Print "     ";
      color ,rgb(255,255,0)
      Print "       ";
      color ,rgb(100,100,100)
      Print "    ";
      If b = 1 And mx >= 200 And mx <= 640 And my >= 232 And my <= 238 Then shirtcolor = point(mx,my)
      
      color rgb(255,255,255)
      Locate 31,6
      Print "Pant color        White Black Red Green Blue Magenta Cyan Yellow Gray"
      Locate 32,6
      color ,rgb(0,0,0)
      Print "                  ";
      color ,rgb(254,254,254)
      Print "      ";
      color ,rgb(1,1,1)
      Print "      ";
      color ,rgb(200,0,0)
      Print "    ";
      color ,rgb(0,200,0)
      Print "      ";
      color ,rgb(0,0,100)
      Print "     ";
      color ,rgb(254,0,254)
      Print "        ";
      color ,rgb(0,255,255)
      Print "     ";
      color ,rgb(255,255,0)
      Print "       ";
      color ,rgb(100,100,100)
      Print "    ";
      If b = 1 And mx >= 200 And mx <= 640 And my >= 248 And my <= 254 Then pantcolor = point(mx,my)
            
      paint GuyImg(cdir),(8,30),shirtcolor,rgb(0,0,0)
      paint GuyImg(cdir),(24,30),shirtcolor,rgb(0,0,0)
      paint GuyImg(udlr),(16,2),shirtcolor,rgb(0,0,0)
      paint GuyImg(udlr),(15,17),pantcolor,rgb(0,0,0)
   loop 
   SendMSG "CLOTHES "+str$(shirtcolor)+" "+str$(pantcolor),Len("CLOTHES "+str$(shirtcolor)+" "+str$(pantcolor))
   color rgb(255,255,255),0
End Sub

Function InInv(n$) As byte
   ininv = -1
   For x = 1 to 8
      If Inv(x) = val(n$) Then return x:Exit For
   Next x
End Function

Sub AddInv (i)
   For x = 1 to 8
      If Inv(x) = 0 Then Inv(x) = i:Exit Sub
   Next x
   scrmsg = "Inventory is full!"
   msgtime = 30
End Sub

DefSng A-Z
Sub Interpolate
   dim tu As single
   ' Interpolate points of other players
   For x = 0 to MAX_PLAYERS
      'tu = OtherPos(x).ct - OtherPos(x).pt
      'OtherPos(x).x = int(OtherPos(x).ox + ((OtherPos(x).nx-OtherPos(x).ox)/tu)*1/2*(Timer-OtherPos(x).pt))
      'OtherPos(x).y = int(OtherPos(x).oy + ((OtherPos(x).ny-OtherPos(x).oy)/tu)*1/2*(Timer-OtherPos(x).pt))
      
      OtherPos(x).x = OtherPos(x).nx
      OtherPos(x).y = OtherPos(x).ny
   Next x
End Sub

DefDbl a-z
Function Encrypt$ (a As STRING)
   AC = asc(a) 'ASC first letter
   For i = 2 to Len(a)
      AP = AC
      AC = asc(mid$(a, i, 1))
      E = E + (AP * AC * i) + asc("$")
   Next i

   For i = 1 to Len(E)
      ST$ = ST$ + Chr$(val(mid$(ltrim$(str$(E)), i, 1)) * 9 + 30)
   Next i

   Encrypt$ = ST$
End Function

Sub InputX (n$,v$)
	Dim As Integer mx,my,b
   Print n$;
   Do Until l$ = Chr$(13)
      W
      If v$ = "LOGIN" Then
      	d = GetMouse(mx,my,,b)
         If mx >= 579 And my >= 470 And mx < 640 And my < 480 And b = 1 Then
         	s$ = "\ @"
         	Exit Do
         End If 
      End If
      l$ = inkey$
      If l$ = Chr$(255)+"k" Then end
      If l$ <> Chr$(13) And l$ <> Chr$(8) Then Print l$;
      If l$ = Chr$(8) And charcount > 0 Then
         l$ = ""
         charcount -= 1
         If Len(s$) > 0 Then s$ = left$(s$,Len(s$)-1):Else s$ = ""
         If Pos > 1 Then Locate ,Pos - 1: Else Locate CsrLin-1,80
         Print " ";
         If Pos > 1 Then Locate ,Pos - 1: Else Locate CsrLin-1,80
      End If
      If l$ >= " " Then s$ = s$ + l$: charcount += 1
   loop
   Print
   v$ = s$
End Sub

Sub W
   ' Useless right now
End Sub

' Outdated update mechanism. Disabled now
Sub CallUpdater
	'Cls
	'Print "Updating..."
	' Download updater
	'dlfile "http://www.landofstuff.net/downloads/LOS.exe","LOS.exe"
	' Run updater
	'run "LOS.exe"
End Sub
