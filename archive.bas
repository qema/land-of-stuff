' Archives the contents of data\ to data.los
' by Andrew Wang

Dim filename As String = Dir("Data\*", &h21)
Dim length As Long = 0
Do While Len(filename)
   length += 1
   filename = Dir()
Loop

open "data.los" for binary as #1
put #1,,length

filename = Dir("Data\*", &h21)
Do While Len(filename)
   print "Packing "; filename; "..."
   open "Data\" + filename for binary as #2
   dim size as long = lof(2)
   dim buffer as string = space(size)
   get #2,,buffer

   put #1,,size
   dim filename12bytes as string*12 = filename + space(12 - len(filename))
   put #1,,filename12bytes

   dim encoded as string = ""
   for i = 1 to len(buffer)
      encoded += chr(255 - asc(mid(buffer, i, 1)))
   next
   put #1,,encoded
   close #2
   
   filename = Dir()
Loop

close #1
print "Done."