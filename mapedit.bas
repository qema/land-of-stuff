' Map editor for The Adventures of Carl
' by Andrew Wang
'

Type EnemyLocType
   x As integer
   y As integer
   i As byte
   img As Any ptr
End Type

dim shared Tiles(9) As Any ptr
dim shared Map(999,999) As string * 1
dim shared camx,camy
dim shared EnemyLoc(499) As EnemyLocType,cur As Integer

Dim Shared recordmode


Function Tile (n As string) As Any ptr
   Tile = Tiles(val(n))
End Function

Sub Redraw'
   screenset 1,0
    
   px1 = camx - 10
   px2 = camx + 10
   py1 = camy - 6
   py2 = camy + 6
   If px1 < 0 Then chgx = abs(px1):px1 = 0: px2 += chgx: absolutex = 1
   If py1 < 0 Then chgy = abs(py1):py1 = 0: py2 += chgy: absolutey = 1
   If absolutex Then absolute = 1
   If absolutey Then absolute = 2
   If absolutex And absolutey Then absolute = 3
   
   For x = px1 to px2
      For y = py1 to py2
         realx = (x - px1) * 32
         realy = (y - py1) * 32
         If Map(x,y) = "5" Then   ' Store
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
            put (realx,realy),Tile(str$(tileis)),trans
         End If
         put (realx,realy),Tile(Map(x,y)),trans
      Next y
   Next x
   
   For x = 0 to cur
      If EnemyLoc(x).x >= px1 And EnemyLoc(x).x <= px2 And EnemyLoc(x).y >= py1 And EnemyLoc(x).y <= py2 Then
         If absolute = 1 Then
            put (EnemyLoc(x).x*32,(EnemyLoc(x).y-(camy-6))*32),EnemyLoc(x).img,trans
         ElseIf absolute = 2 Then
            put ((EnemyLoc(x).x-(camx - 10))*32,EnemyLoc(x).y*32),EnemyLoc(x).img,trans
         ElseIf absolute = 3 Then
            put (EnemyLoc(x).x*32,EnemyLoc(x).y*32),EnemyLoc(x).img,trans
         Else
            put ((EnemyLoc(x).x-(camx - 10))*32,(EnemyLoc(x).y-(camy-6))*32),EnemyLoc(x).img,trans
         End If
      End If
   Next x
   
   If recordmode = 0 then
	   If absolute = 1 Then
	      line (camx*32,192)-(camx*32+31,192+31),rgb(255,255,255),b
	   ElseIf absolute = 2 Then
	      line (320,camy*32)-(320+31,camy*32+31),rgb(255,255,255),b
	   ElseIf absolute = 3 Then
	      line (camx*32,camy*32)-(camx*32+31,camy*32+31),rgb(255,255,255),b
	   Else
	      line (320,192)-(320+31,192+31),rgb(255,255,255),b
	   End If
	End if
   
   line (4,460)-(640,480),0,bf
   draw string (4,460),str$(camx)+","+str$(camy)
   
   screencopy
   screenset 0,0
End Sub

' Main program
screenres 640,480,32,2

For x = 0 to 99
   For y = 0 to 99
      Map(x,y) = Chr$(0)
   Next y
Next x

For x = 0 to 9
   Tiles(x) = ImageCreate(32,32)
   bload "Data\t"+str$(x)+".bmp",Tiles(x)
Next x

Open "Data\enemyloc.txt" For input As #1
For x = 0 to 99
   If eof(1) Then Exit For
   input #1,EnemyLoc(x).x
   input #1,EnemyLoc(x).y
   input #1,EnemyLoc(x).i
   EnemyLoc(x).img = ImageCreate(32,32)
   bload "Data\"+str$(EnemyLoc(x).i)+".bmp",EnemyLoc(x).img
   cur += 1
Next x
Close #1

   Open "data\map.txt" For input As #1
   input #1,camx,camy
   startx = camx
   starty = camy
   Do Until eof(1)        
      line input #1,s$
      For x = 0 to Len(s$) - 1
         Map(x,i) = mid$(s$,x+1,1)
      Next x
      i += 1
   loop
   Close #1
Redraw

dim n1 As single
Do Until multikey(1)
   n1 = Timer
   If multikey (&h48) Then   ' Up 
      camy -= 1
   End If
   If multikey (&h4b) Then   ' Left
      camx -= 1
   End If
   If multikey (&h4d) Then   ' Right
      camx += 1
   End If
   If multikey (&h50) Then   ' Down
      camy += 1
   End If
   
   t = -1
   If multikey(11) Then t = 0
   If multikey(2) Then t = 1
   If multikey(3) Then t = 2
   If multikey(4) Then t = 3
   If multikey(5) Then t = 4
   If multikey(6) Then t = 5
   If multikey(7) Then t = 6
   If multikey(8) Then t = 7
   If multikey(9) Then t = 8
   If multikey(10) Then t = 9
   If t >= 0 Then Map(camx,camy) = str$(t)
   
   i$ = inkey$
   If i$ = "n" Or i$ = "N" Then
      ' New
      input "Enter width: ",wid
      input "Enter height: ",hei
      For x = 0 to 999
         For y = 0 to 999
            Map(x,y) = chr$(0)
         Next y
      Next x
      For y = 0 to hei
         For x = 0 to wid
            Map(x,y) = "0"
         Next x
      Next y
      Redraw
   End If
   
   If i$ = "c" Or i$ = "C" Then
      ' Create NPC
      input "Enter name for NPC: ",n$
      Open "data\script.txt" For input As #1
      Do Until eof(1)
         line input #1,l$
         If left$(l$,4) = "NPC " Then npcnum += 1
      loop
      Close #1
      Open "data\script.txt" For append As #1
      Print #1,"NPC "+str$(npcnum)
      Print #1,"X "+str$(camx)
      Print #1,"Y "+str$(camy)
      Print #1,"NAME "+n$
      Close #1
      npcnum = 0
   End If
   
   If i$ = "p" Or i$ = "P" Then
      ' Set start point
      startx = camx
      starty = camy
   End If
   
   If i$ = "s" Or i$ = "S" Then
      ' Save
      l$ = ""
      Open "Data\map.txt" For output As #1
      Print #1,startx;",";starty
      For y = 0 to 999
         For x = 0 to 999
            If Map(x,y) = Chr$(0) Then Exit For
            l$ += Map(x,y)
         Next x
         If l$ <> "" Then Print #1,l$
         l$ = ""
      Next y
      Close #1
      Open "Data\enemyloc.txt" For output As #1
      For x = 0 to cur
         If EnemyLoc(x).img <> 0 Then   
            Print #1,str$(EnemyLoc(x).x)
            Print #1,str$(EnemyLoc(x).y)
            Print #1,str$(EnemyLoc(x).i)
         End If
      Next x
      Close #1
      shell "archive.exe > nul"
      kill "nul"
   End If
   
   If i$ = "t" Or i$ = "T" Then 
      l$ = Map(camx,camy)
      input "Enter tile letter: ",l$
      Map(camx,camy) = l$
   End If
   
   If i$ = "d" Or i$ = "D" Then
      For x = 0 to cur
         If EnemyLoc(x).x = camx and EnemyLoc(x).y = camy then i = x: exit for
      Next x
      dim empty As EnemyLocType
      EnemyLoc(i) = empty
   End If
   
   If i$ = "e" Or i$ = "E" Then
      Locate 1,1
      input "Input enemy ID: ",i
      EnemyLoc(cur).x = camx
      EnemyLoc(cur).y = camy
      EnemyLoc(cur).i = i
      EnemyLoc(cur).img = ImageCreate(32,32)
      bload "Data\"+str$(i)+".bmp",EnemyLoc(cur).img
      cur += 1
   End If
   
   If MultiKey(&h3c) Then
      If recordmode = 1 Then recordmode = 0:Else recordmode = 1
   	Do Until Not(MultiKey(&h3c)):loop
   EndIf
   
   If i$ = "v" Or i$ = "V" Then
      ' View the entire map
      Do Until k$ = "x" Or k$ = "X"
         Signal = 0      
         cls
         screenlock
         For y = 0 to 999 step 2.5
            For x = 0 to 999 step 2.5
               col$ = Map(x,y)
               If col$ = "0" Then col = rgb(0,200,0)
               If col$ = "1" Then col = rgb(100,100,100)
               If col$ = "2" Then col = rgb(200,200,0)
               If col$ = "3" Then col = 0
               If col$ = "4" Then col = rgb(100,100,0)
               If col$ = "7" Then col = rgb(0,100,100)
               pset (x/2.5,y/2.5),col
               col = 0
            Next x
         Next y
         col$ = str$(curcolor)
         If col$ = "0" Then col = rgb(0,200,0)
         If col$ = "1" Then col = rgb(100,100,100)
         If col$ = "2" Then col = rgb(200,200,0)
         If col$ = "3" Then col = 0
         If col$ = "4" Then col = rgb(100,100,0)
         If col$ = "7" Then col = rgb(0,100,100)         
         line (600,10)-(632,42),col,bf
         screenunlock
         Do Until Signal = 1 Or k$ = "x" Or k$ = "X"
            k$ = inkey$
            If k$ = "t" Or k$ = "T" Then
               Locate 1,1
               Print "Select region:"
               Do Until b <> 0
                  d = getmouse (mx,my,,b)
               loop
               tx1 = mx * 2.5
               ty1 = my * 2.5
               Do Until b = 0
                  d = getmouse (mx,my,,b)
                  tx2 = mx * 2.5
                  ty2 = my * 2.5
                  line (tx1/2.5,ty1/2.5)-(tx2/2.5,ty2/2.5),0,b
               loop
               If tx1 > tx2 Then swap tx1,tx2
               If ty1 > ty2 Then swap ty1,ty2
               input "Enter density (1-1000):",density
               For yy = ty1 to ty2
                  For xx = tx1 to tx2
                     n = int(Rnd * 1000)
                     If n >= 1000-density Then
                        tot$ = Map(xx,yy)
                        If Map(xx,yy) = "0" Then tot$ = "9"
                        If Map(xx,yy) = "2" Then tot$ = "8"
                        
                        Map(xx,yy) = tot$
                     End If
                  Next xx
               Next yy
               Signal = 1
               Exit Do
            End If
            If k$ = "g" Or k$ = "G" Then
               Locate 1,1
               Print "Click where to go:";
               Do Until b <> 0
                  d = getmouse (mx,my,,b)
                  xx = mx * 2.5
                  yy = my * 2.5
               loop
               camx = xx
               camy = yy
               k$ = "x"
               Exit Do
            End If
            If k$ = "c" Or k$ = "C" Then
               Locate 1,1
               input "Enter tile number:",curcolor
               Signal = 1
            End If
            d = getmouse (mx,my,,b)
            If b = 1 And mx <= 999/2.5 And my <= 999/2.5 Then
               tx1 = mx * 2.5
               ty1 = my * 2.5
               Do Until b = 0
                  d = getmouse (mx,my,,b)
                  tx2 = mx * 2.5
                  ty2 = my * 2.5
                  line (tx1/2.5,ty1/2.5)-(tx2/2.5,ty2/2.5),0,b
               loop
               Signal = 1
               If tx1 > tx2 Then swap tx1,tx2
               If ty1 > ty2 Then swap ty1,ty2
               For i = ty1 to ty2
                  For j = tx1 to tx2
                     if i < ubound(Map,0) and j < ubound(Map,1) then Map(j,i) = str$(curcolor)
                  Next j
               Next i
            End If
         loop
      loop
      cls
      k$ = ""
   End If
   l$ = ""
   
   Redraw
   Do Until Timer >= n1 + 1/11
   loop
loop