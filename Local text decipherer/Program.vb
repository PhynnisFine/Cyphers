Imports System
Imports System.ComponentModel.DataAnnotations
Imports System.Data.SqlTypes
Imports System.Formats.Asn1.AsnWriter
Imports System.Net.Http
Imports System.Text
Imports System.Threading
Imports System.Windows

Module Program

    Sub morse(starttext)

        Dim currentcode As String = ""
        Dim currentchar As Char
        Dim plaintext As String = ""
        Dim dotdashtext As String = ""

        Dim dotchar As Char
        Dim dashchar As Char

        Console.WriteLine("Input dot character (tip: it should be the one that has four in a row in one code")
        dotchar = Console.ReadLine().ToUpper
        Console.WriteLine("Input dash character")
        dashchar = Console.ReadLine().ToUpper


        For i = 0 To starttext.length - 1
            currentchar = starttext.substring(i, 1)
            If currentchar = dotchar Then
                dotdashtext += "."
            ElseIf currentchar = dashchar Then
                dotdashtext += "-"
            Else
                dotdashtext += currentchar
            End If
        Next

        Console.WriteLine(dotdashtext)

        For i = 0 To dotdashtext.Length - 1
            currentchar = dotdashtext.Substring(i, 1)
            If currentchar = "." Or currentchar = "-" Then
                currentcode += currentchar

            ElseIf currentcode.Length > 0 Then
                Select Case currentcode
                    Case ".-"
                        plaintext += "A"
                    Case "-..."
                        plaintext += "B"
                    Case "-.-."
                        plaintext += "C"
                    Case "-.."
                        plaintext += "D"
                    Case "."
                        plaintext += "E"
                    Case "..-."
                        plaintext += "F"
                    Case "--."
                        plaintext += "G"
                    Case "...."
                        plaintext += "H"
                    Case ".."
                        plaintext += "I"
                    Case ".---"
                        plaintext += "J"
                    Case "-.-"
                        plaintext += "K"
                    Case ".-.."
                        plaintext += "L"
                    Case "--"
                        plaintext += "M"
                    Case "-."
                        plaintext += "N"
                    Case "---"
                        plaintext += "O"
                    Case ".--."
                        plaintext += "P"
                    Case "--.-"
                        plaintext += "Q"
                    Case ".-."
                        plaintext += "R"
                    Case "..."
                        plaintext += "S"
                    Case "-"
                        plaintext += "T"
                    Case "..-"
                        plaintext += "U"
                    Case "...-"
                        plaintext += "V"
                    Case ".--"
                        plaintext += "W"
                    Case "-..-"
                        plaintext += "X"
                    Case "-.--"
                        plaintext += "Y"
                    Case "--.."
                        plaintext += "Z"
                End Select
                currentcode = ""
            End If
        Next

        Console.WriteLine(plaintext)

    End Sub

    Sub binary(starttext)

        starttext = justnumbersandletters(starttext)

        Dim plaintext As String = ""
        Dim currentbinary As String = ""

        For i = 0 To starttext.length - 1
            currentbinary += starttext.substring(i, 1)

            If currentbinary.Length = 5 Then
                Select Case currentbinary
                    Case "00000"
                        plaintext += "A"
                    Case "00001"
                        plaintext += "B"
                    Case "00010"
                        plaintext += "C"
                    Case "00011"
                        plaintext += "D"
                    Case "00100"
                        plaintext += "E"
                    Case "00101"
                        plaintext += "F"
                    Case "00110"
                        plaintext += "G"
                    Case "00111"
                        plaintext += "H"
                    Case "01000"
                        plaintext += "I"
                    Case "01001"
                        plaintext += "J"
                    Case "01010"
                        plaintext += "K"
                    Case "01011"
                        plaintext += "L"
                    Case "01100"
                        plaintext += "M"
                    Case "01101"
                        plaintext += "N"
                    Case "01110"
                        plaintext += "O"
                    Case "01111"
                        plaintext += "P"
                    Case "10000"
                        plaintext += "Q"
                    Case "10001"
                        plaintext += "R"
                    Case "10010"
                        plaintext += "S"
                    Case "10011"
                        plaintext += "T"
                    Case "10100"
                        plaintext += "U"
                    Case "10101"
                        plaintext += "V"
                    Case "10110"
                        plaintext += "W"
                    Case "10111"
                        plaintext += "X"
                    Case "11000"
                        plaintext += "Y"
                    Case "11001"
                        plaintext += "Z"
                End Select
                currentbinary = ""
            End If
        Next

        Console.WriteLine(plaintext)
        Console.ReadLine()

    End Sub

    Sub hex(starttext)

        starttext = justnumbersandletters(starttext)

        Dim plaintext As String = ""
        Dim currenthex As String = ""

        For i = 0 To starttext.length - 1 Step 2
            currenthex = starttext.substring(i, 2)
            Select Case currenthex
                Case "00"
                    plaintext += "A"
                Case "01"
                    plaintext += "B"
                Case "02"
                    plaintext += "C"
                Case "03"
                    plaintext += "D"
                Case "04"
                    plaintext += "E"
                Case "05"
                    plaintext += "F"
                Case "06"
                    plaintext += "G"
                Case "07"
                    plaintext += "H"
                Case "08"
                    plaintext += "I"
                Case "09"
                    plaintext += "J"
                Case "0A"
                    plaintext += "K"
                Case "0B"
                    plaintext += "L"
                Case "0C"
                    plaintext += "M"
                Case "0D"
                    plaintext += "N"
                Case "0E"
                    plaintext += "O"
                Case "0F"
                    plaintext += "P"
                Case "10"
                    plaintext += "Q"
                Case "11"
                    plaintext += "R"
                Case "12"
                    plaintext += "S"
                Case "13"
                    plaintext += "T"
                Case "14"
                    plaintext += "U"
                Case "15"
                    plaintext += "V"
                Case "16"
                    plaintext += "W"
                Case "17"
                    plaintext += "X"
                Case "18"
                    plaintext += "Y"
                Case "19"
                    plaintext += "Z"
            End Select

            currenthex = ""
        Next

        Console.WriteLine(plaintext)
    End Sub


    Function caesershift(currentchar As Char, shift As Integer)      'caeser shift 1 letter by shift
        currentchar = Chr(((Asc(currentchar) - 65 + shift) Mod 26) + 65)
        Return currentchar
    End Function
    Function comparefreq(letterfreq() As Integer, letterlength As Integer, expectedfreq() As Single)    'IMPORTANT BOI

        Dim frequencyscore As Single = 0

        For i As Integer = 0 To 25
            frequencyscore += Math.Abs(letterfreq(i) / letterlength - expectedfreq(i))
        Next

        Return frequencyscore
    End Function

    Function fillzeros(array() As Integer)
        For i = 0 To array.Length - 1
            array(i) = 0
        Next

        Return array
    End Function
    Function fillblanks(array() As String)

        For i = 0 To array.Length - 1
            array(i) = ""
        Next

        Return array
    End Function
    Function fillfalse(array() As Boolean)

        For i = 0 To array.Length - 1
            array(i) = False
        Next

        Return array
    End Function

    Function justnumbersandletters(starttext)

        Dim ciphertext As String = ""
        Dim currentchar As Char

        For i = 0 To starttext.length - 1
            currentchar = starttext.substring(i, 1)
            If IsNumeric(currentchar) Or Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                ciphertext += currentchar
            End If
        Next

        Return ciphertext
    End Function

    Function scorewords(starttext As String)  'scores a text based on how many common words it has

        Dim score As Integer = 0
        Dim wordpos As Integer
        Dim found As Boolean
        Dim words() = New String() {"DEAR", "THERE", "THEIR", "THEY", "THE", "AND", "THAT", "HAVE", "NOT", "WITH", "YOUR", "YOU", "THIS", "BUT", "HIS", "FROM", "SAY", "HER", "SHE", "WILL", "ONE", "ALL", "WOULD", "WHAT", "ABOUT", "OUT", "WHO", "GET", "WHICH", "WHEN", "MAKE", "CAN", "LIKE", "TIME", "JUST", "HIM", "PEOPLE", "OVER", "BECAUSE", "WORK", "CIFRAR", "ASSOCIATION", "BOX", "VAULT", "INVESTIGATE", "INVESTIGATION", "FRANK"}
        Dim firstchar As Char = ""
        Dim currentword As String

        'to be optimised maybe
        For i = 0 To starttext.Length - 14
            wordpos = 0
            found = False
            firstchar = starttext.Substring(i, 1)
            Do
                currentword = words(wordpos)
                If firstchar = currentword.Substring(0, 1) Then
                    If starttext.Substring(i, currentword.Length) = currentword Then
                        score += words(wordpos).Length
                        found = True
                    End If
                End If
                wordpos += 1
            Loop Until wordpos = words.Length - 1 Or found
        Next

        Return score
    End Function

    'If a letter appears in a 2 different found words, it should not be changed (and be 'locked')
    Function lockletters(starttext As String, correctletters() As Boolean)

        Dim words() = New String() {"DEAR", "THERE", "THEIR", "THEM", "THEY", "THE", "AND", "THAT", "HAVE", "NOT", "WITH", "YOUR", "YOU", "THIS", "BUT", "HIS", "FROM", "SAY", "HER", "SHE", "WILL", "ONE", "ALL", "WOULD", "WHAT", "ABOUT", "OUT", "WHO", "GET", "WHICH", "WHEN", "MAKE", "CAN", "LIKE", "TIME", "JUST", "HIM", "PEOPLE", "OVER", "BECAUSE", "WORK", "CIFRAR", "ASSOCIATION", "BOX", "VAULT", "INVESTIGATE", "INVESTIGATION", "FRANK", "HARRY", "OF"}
        Dim wordpos As Integer
        Dim wordfound(words.Length - 1) As Boolean
        Dim lettersinword(25) As Boolean
        Dim lettercount(25) As Integer
        lettercount = fillzeros(lettercount)
        Dim found As Boolean
        Dim firstchar As Char = ""

        wordfound = fillfalse(wordfound)
        lettersinword = fillfalse(lettersinword)
        correctletters = fillfalse(correctletters)


        For i = 0 To starttext.Length - 14                'Finding and storing all found words from starttext
            wordpos = 0
            found = False
            firstchar = starttext.Substring(i, 1)

            Do
                If firstchar = words(wordpos).Substring(0, 1) Then
                    If starttext.Substring(i, words(wordpos).Length) = words(wordpos) Then
                        wordfound(wordpos) = True
                        found = True
                    End If
                End If
                wordpos += 1
            Loop Until wordpos = words.Length Or found

        Next

        For k = 0 To words.Length - 1                                   'Counting how many times each letter appears in a found word
            lettersinword = fillfalse(lettersinword)
            If wordfound(k) Then
                Console.WriteLine(words(k) & ", " & k & ", " & wordfound(k))

                For l = 0 To words(k).Length - 1
                    lettersinword(Asc(words(k).Substring(l, 1)) - 65) = True
                Next

                For l = 0 To 25
                    If lettersinword(l) Then
                        lettercount(l) += 1
                    End If

                Next
            End If
        Next

        For i = 0 To 25                                          'If a letter appears in 4 different found words, it is correct (hopefully)
            If lettercount(i) > 3 Then
                correctletters(i) = True
            End If
        Next

        For j = 0 To 25
            If correctletters(j) = True Then
                Console.WriteLine(Chr(j + 65) & ", " & lettercount(j))
            End If
        Next

        Return correctletters
    End Function
    Function scorebigrams(starttext As String, letterlength As Integer)       'Compares the expected frequency of the 10 most common bigrams with how oftren they appear in the text

        Dim score As Single = 0
        Dim bigrampos As Integer
        Dim currentbigram As String
        Dim found As Boolean
        Dim bigramfreq(9) As Integer
        Dim bigrams() = New String() {"TH", "HE", "IN", "ER", "AN", "RE", "ON", "AT", "EN", "ND"}
        Dim expectedbigramfreq() = New Single() {0.0356, 0.0308, 0.0243, 0.0205, 0.0199, 0.0185, 0.0176, 0.0149, 0.0145, 0.0135}


        For i = 0 To starttext.Length - 2
            currentbigram = starttext.Substring(i, 2)
            found = False
            bigrampos = 0
            Do
                If currentbigram = bigrams(bigrampos) Then
                    bigramfreq(bigrampos) += 1
                End If
                bigrampos += 1
            Loop Until bigrampos = bigrams.Length
        Next

        For i As Integer = 0 To 9
            score += Math.Abs(bigramfreq(i) / letterlength - expectedbigramfreq(i))
        Next

        Return score
    End Function
    Function totalscore(starttext As String, expectedfreq() As Single)       'Outputs a compound score from 1000 to 4000 (used for hill climbing)

        Dim letterfreq(25) As Integer
        Dim letterlength As Integer = 0
        Dim currentchar As Char
        Dim frequencyscore As Single = 0
        Dim wordscore As Single
        Dim bigramscore As Single = 0
        Dim finalscore As Integer

        For i = 0 To starttext.Length - 1
            currentchar = starttext.Substring(i, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                letterfreq(Asc(currentchar) - 65) += 1
                letterlength += 1
            End If
        Next

        frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)
        frequencyscore = (2000 - Math.Round(1000 * frequencyscore))

        wordscore = scorewords(starttext)
        wordscore = wordscore / letterlength
        wordscore = Math.Round(4000 * wordscore)

        bigramscore = scorebigrams(starttext, letterlength)
        bigramscore = 2 * (500 - Math.Round(bigramscore * 1000))

        'Console.WriteLine(frequencyscore & ", " & wordscore & ", " & bigramscore)

        finalscore = frequencyscore + wordscore + bigramscore
        'Console.WriteLine(finalscore)
        Return finalscore
    End Function

    'CAESER CIPHER
    Sub caeser(starttext As String, expectedfreq() As Single)       'decipher a caeser cipher

        Console.WriteLine("Caeser cipher:")
        Console.WriteLine("")

        starttext = starttext.ToUpper
        Dim currentchar As Char
        Dim letterfreq(25) As Integer
        Dim letterlength As Integer = 0
        Dim frequencyscore(25) As Single
        Dim bestscore As Single = 2
        Dim bestscoreshift As Integer
        Dim plaintext As String = ""

        For i = 0 To 25

            frequencyscore(i) = 0
            For j = 0 To starttext.Length - 1
                currentchar = starttext.Substring(j, 1)
                If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                    currentchar = caesershift(currentchar, i)
                    letterfreq(Asc(currentchar) - 65) += 1
                    letterlength += 1
                End If

            Next
            frequencyscore(i) = comparefreq(letterfreq, letterlength, expectedfreq)
            If frequencyscore(i) < bestscore Then
                bestscore = frequencyscore(i)
                bestscoreshift = i
            End If
            letterlength = 0

            For k = 0 To 25
                letterfreq(k) = 0
            Next
        Next

        For l = 0 To starttext.Length - 1
            currentchar = starttext.Substring(l, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                currentchar = caesershift(currentchar, bestscoreshift)
            End If
            plaintext = plaintext & currentchar
        Next

        plaintext = plaintext.ToLower

        Console.WriteLine("Best shift: " & bestscoreshift & " " & bestscore)
        Console.WriteLine("")
        Console.WriteLine(plaintext)
        Console.WriteLine("")
        Console.WriteLine("Is that deciphered?")
    End Sub

    Sub swap(a, b)

        Dim placeholder As String

        placeholder = a
        a = b
        b = placeholder

    End Sub

    Sub getfactors(textlength)

        For i = 2 To textlength / 2
            If textlength Mod i = 0 Then
                Console.WriteLine(i)
            End If
        Next

    End Sub

    Function sortusing(array() As Single, smallesttolargest As Boolean)

        Dim order(array.Length - 1) As Integer

        For i = 0 To array.Length - 1
            order(i) = 1
        Next

        If smallesttolargest Then
            For i = 0 To array.Length - 2
                For j = 0 To array.Length - 2
                    If array(j) > array(j + 1) Then
                        swap(array(j), array(j + 1))
                        swap(order(j), order(j + 1))
                    End If
                Next
            Next
        Else
            For i = 0 To array.Length - 2
                For j = 0 To array.Length - 2
                    If array(j) < array(j + 1) Then
                        swap(array(j), array(j + 1))
                        swap(order(j), order(j + 1))
                    End If
                Next
            Next
        End If

        Return order
    End Function

    Function simpletranspos(starttext, numcolumns)

        Dim numrows As Integer

        numrows = Math.Truncate(starttext.Length / numcolumns)
        Console.WriteLine(numcolumns)
        Console.WriteLine(numrows)

        Dim grid(numcolumns - 1, numrows - 1)
        Dim plaintext As String
        plaintext = ""

        For i = 0 To numcolumns - 1
            For j = 0 To numrows - 1
                grid(i, j) = starttext.Substring(numrows * i + j, 1)
            Next j
        Next i

        For i = 0 To numrows - 1
            For j = 0 To numcolumns - 1
                plaintext += grid(j, i)
            Next j
        Next i

        Return plaintext

    End Function

    Function transposwithkey(starttext As String, key As String, numcolumns As Integer)

        Dim numrows As Integer

        numrows = Math.Truncate(starttext.Length / numcolumns)

        Dim grid(numcolumns - 1, numrows - 1)
        Dim plaintext As String
        plaintext = ""
        Dim keyarray(numcolumns - 1) As String
        Dim realkeyarray(numcolumns - 1) As Integer
        Dim mystop As Boolean = False
        Dim nospacekey As String = ""

        nospacekey = justnumbersandletters(key)

        If nospacekey.Length = key.Length Then         'Creates key array (if key has no spaces then do without spaces, else do with spaces)
            For i = 0 To numcolumns - 1
                keyarray(i) = key.Substring(i, 1)
            Next
        Else
            Dim i As Integer = 0
            Dim currentnum As String = ""
            Dim position As Integer = 0
            Do
                While key.Substring(position, 1) <> " " And mystop = False
                    currentnum += key.Substring(position, 1)
                    position += 1
                    If position = key.Length Then
                        position -= 1
                        mystop = True
                    End If
                End While
                keyarray(i) = currentnum
                currentnum = ""
                position += 1
                i += 1
            Loop Until i = numcolumns
        End If

        For i = 0 To numcolumns - 1
            For j = 0 To numrows - 1
                grid(i, j) = starttext.Substring(numrows * i + j, 1)
            Next j
        Next i

        For i = 0 To numrows - 1
            For j = 0 To numcolumns - 1
                plaintext += grid(keyarray(j), i)
            Next j
        Next i

        Return plaintext
    End Function

    Function railfence(starttext, rails)

        Dim plaintextarray(starttext.length - 1) As Char
        Dim plaintext As String = ""
        Dim position As Integer = 0
        Dim i As Integer = 0

        'First rail
        i = 0
        Do
            plaintextarray(i) = starttext.substring(position, 1)
            i += (rails - 1) * 2
            position += 1
        Loop Until i >= starttext.length

        'Other rails
        For rail = 1 To rails - 2
            i = 0
            Do
                plaintextarray(i + rail) = starttext.substring(position, 1)
                position += 1
                plaintextarray(i + (rails - 1) * 2 - rail) = starttext.substring(position, 1)
                position += 1
                i += (rails - 1) * 2
            Loop Until i + (rails - 1) * 2 - rail >= starttext.length

            If i + rail < starttext.length Then
                plaintextarray(i + rail) = starttext.substring(position, 1)
                position += 1
            End If
        Next rail

        'Last rail
        i = rails - 1
        Do
            plaintextarray(i) = starttext.substring(position, 1)
            i += (rails - 1) * 2
            position += 1
        Loop Until i >= starttext.length

        For j = 0 To plaintextarray.Length - 1
            plaintext += plaintextarray(j)
        Next

        Return plaintext

    End Function

    'Suggests key for columnar transposition based on 't's and 'h's in the same row (hopefully somewhat works)

    Sub suggestkey(plaintext, numcolumns)

        Dim Hpos(numcolumns - 1) As Integer
        Dim bestpair(numcolumns - 1) As Integer
        Dim bestpairscore As Integer
        Dim bestscore(numcolumns - 1) As Single
        Dim scoreorder(numcolumns - 1) As Integer
        Dim currentrow As String = ""
        Dim key As String = ""
        Dim potentiallinks As Integer = 0
        Dim prevpair(4) As Integer


        For i = 0 To numcolumns - 1
            scoreorder(i) = i
        Next

        For i = 0 To numcolumns - 1    'for each letter in a row
            fillzeros(Hpos)

            For j = 0 To plaintext.length - 1 Step numcolumns    ' for each row
                currentrow = plaintext.substring(j, numcolumns)

                If currentrow.Substring(i, 1) = "T" Then          'If we find a 't', search for a 'h' in the same row and record its position

                    For k = 0 To currentrow.Length - 1
                        If currentrow.Substring(k, 1) = "H" Then
                            Hpos(k) += 1
                        End If
                    Next k
                End If
            Next j

            bestpairscore = 0
            For j = 0 To Hpos.Length - 1           'for each score in Hpos (find the most common pair for t in position i)
                If Hpos(j) > bestpairscore Then
                    bestpair(i) = j
                    bestpairscore = Hpos(j)
                End If
            Next j

            Console.WriteLine(i & " --> " & bestpair(i) & "    " & bestpairscore)          't in position i is most commonly followed by h in position bestpair i
        Next i

        scoreorder = sortusing(bestscore, True)

        'And now we make the key (doesnt work yet, this turns out to be a really complicated algorithm, and i can do it fine atm so...)

        key = scoreorder(0) & key

        For i = 0 To numcolumns - 2
            For j = 0 To numcolumns - 1
                potentiallinks = 0
                fillzeros(prevpair)
                For k = 0 To 4
                    prevpair(k) = 0
                Next
                If key.Substring(0, 1) = bestpair(j) Then    'Find possible next links
                    prevpair(potentiallinks) = i
                    potentiallinks += 1
                End If

                If potentiallinks = 1 Then
                    key = key & j
                ElseIf potentiallinks = 0 Then

                End If
            Next
        Next

    End Sub

    'TRANSPOSITION CIPHER

    Sub transposition(starttext)

        Console.WriteLine("Transposition cipher")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        Dim numcolumns As Integer
        Dim myinput As String
        Dim plaintext As String
        Dim key As String
        starttext = justnumbersandletters(starttext)

        Dim textlength As Integer
        textlength = starttext.Length
        Console.WriteLine("Factors:")
        getfactors(textlength)

        Console.WriteLine("A - simple transposition")
        Console.WriteLine("B - repeated transposition")
        Console.WriteLine("C - Railfence cipher D:")
        myinput = Console.ReadLine.ToUpper

        Select Case myinput
            Case "A"
                Console.WriteLine("Input number of columns or -1 to bruteforce all")
                myinput = Console.ReadLine()
                If myinput = -1 Then
                    For numcolumns = 2 To textlength / 2

                        plaintext = simpletranspos(starttext, numcolumns)

                        Console.WriteLine(plaintext.ToLower)

                        If numcolumns < 21 Then
                            For i = 0 To 5
                                Console.WriteLine("Row " & i & ": " & plaintext.Substring(i * numcolumns, numcolumns))
                            Next

                            Console.WriteLine("Final row: " & plaintext.Substring(plaintext.Length - numcolumns, numcolumns))
                        End If

                        Do
                            Console.WriteLine("Input key or -1 if no key is required")
                            key = Console.ReadLine()
                            If key <> -1 Then
                                plaintext = transposwithkey(starttext, key, numcolumns)
                                Console.WriteLine(plaintext.ToLower)
                                Console.ReadLine()
                            End If
                        Loop Until key = -1

                    Next numcolumns
                Else

                    numcolumns = myinput

                    plaintext = simpletranspos(starttext, numcolumns)

                    Console.WriteLine(plaintext.ToLower)

                    Console.WriteLine("")

                    If numcolumns < 21 Then
                        For i = 0 To 5
                            Console.WriteLine("Row " & i & ": " & plaintext.Substring(i * numcolumns, numcolumns))
                        Next

                        Console.WriteLine("Final row: " & plaintext.Substring(plaintext.Length - numcolumns, numcolumns))
                    End If

                    suggestkey(plaintext, numcolumns)

                    Do
                        Console.WriteLine("Input key or -1 if no key is required")
                        key = Console.ReadLine()
                        If key <> "-1" Then
                            plaintext = transposwithkey(starttext, key, numcolumns)
                            Console.WriteLine(plaintext.ToLower)
                            Console.ReadLine()
                        End If
                    Loop Until key = -1
                    Console.ReadLine()
                End If

                'Repeated 
            Case "B"
                Console.WriteLine("Input number of columns or -1 to bruteforce all")
                myinput = Console.ReadLine()

                If myinput = -1 Then

                    For numcolumns = 2 To textlength / 2

                        plaintext = simpletranspos(starttext, numcolumns)
                        plaintext = simpletranspos(plaintext, numcolumns)

                        Console.WriteLine(plaintext.ToLower)
                        Console.ReadLine()

                    Next numcolumns

                Else

                    numcolumns = myinput

                    plaintext = simpletranspos(starttext, numcolumns)
                    plaintext = simpletranspos(plaintext, numcolumns)

                    Console.WriteLine(plaintext.ToLower)
                    Console.ReadLine()
                End If

                'Rail fence stupid fucking dumb ass why the fuck do you exist rail fence
            Case "C"
                Console.WriteLine("")
                Console.WriteLine("Trying all rails up to 10, you have no free will")

                For i = 2 To 10
                    Console.WriteLine(i)
                    plaintext = railfence(starttext, i)
                    Console.WriteLine(plaintext.ToLower)
                    Console.ReadLine()
                Next
        End Select

    End Sub

    'AFFINE SHFIT CIPHER
    Sub affine(starttext As String, expectedfreq() As Single)                      'decipher affine cipher (multiply and then caeser shift)

        Console.WriteLine("Affine cipher")
        Console.WriteLine("")

        starttext = starttext.ToUpper
        Dim multiplier = New Integer() {3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25}
        Dim currentchar As Char
        Dim plaintext As String = ""
        Dim letterfreq(25) As Integer
        Dim letterlength As Integer
        Dim frequencyscore As Single
        Dim bestscore As Single = 2
        Dim bestscoremultiplier As Integer
        Dim bestscoreshift As Integer
        Dim currentshifts(25) As Integer

        For i = 0 To 10
            For shift = 0 To 25
                frequencyscore = 0
                For k = 0 To 25
                    currentshifts((k * multiplier(i) + shift) Mod 26) = k - ((k * multiplier(i) + shift) Mod 26)
                Next
                For j = 0 To starttext.Length - 1
                    currentchar = starttext.Substring(j, 1)
                    If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                        currentchar = caesershift(currentchar, currentshifts(Asc(currentchar) - 65))
                        letterfreq(Asc(currentchar) - 65) += 1
                        letterlength += 1
                    End If
                Next

                frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)

                Console.WriteLine(multiplier(i) & "x + " & shift & " = " & frequencyscore)

                If frequencyscore < bestscore Then
                    bestscore = frequencyscore
                    bestscoreshift = shift
                    bestscoremultiplier = multiplier(i)
                    Console.WriteLine(bestscore)
                    Console.WriteLine(bestscoreshift)
                    Console.WriteLine(bestscoremultiplier)
                End If
                letterlength = 0

                For k = 0 To 25
                    letterfreq(k) = 0
                Next

            Next
        Next

        For k = 0 To 25
            currentshifts((k * bestscoremultiplier + bestscoreshift) Mod 26) = k - ((k * bestscoremultiplier + bestscoreshift) Mod 26)
        Next

        For l = 0 To starttext.Length - 1
            currentchar = starttext.Substring(l, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                currentchar = caesershift(currentchar, currentshifts(Asc(currentchar) - 65))
            End If
            plaintext += currentchar
        Next

        plaintext = plaintext.ToLower

        Console.WriteLine("Best shift: " & bestscoremultiplier & "x + " & bestscoreshift & " = " & bestscore)
        Console.WriteLine("")
        Console.WriteLine(plaintext)
        Console.WriteLine("")
        Console.WriteLine("Is that deciphered?")
    End Sub
    Function createorderedkey(expectedorder() As Integer, ciphertextorder() As Integer)       'for rndsubstitution, creates an ordered key of shifts from a-z

        Dim currentshifts(25) As Integer
        Dim placeholder As Integer
        Dim save(25) As Integer

        For k = 0 To 25         'so that ciphertextorder is not changed
            save(k) = ciphertextorder(k)
        Next
        For k = 0 To 25
            currentshifts(k) = expectedorder(k) - ciphertextorder(k)
        Next

        For i = 0 To 23
            For j = 0 To 24
                If ciphertextorder(j) > ciphertextorder(j + 1) Then
                    placeholder = ciphertextorder(j)
                    ciphertextorder(j) = ciphertextorder(j + 1)
                    ciphertextorder(j + 1) = placeholder
                    placeholder = currentshifts(j)
                    currentshifts(j) = currentshifts(j + 1)
                    currentshifts(j + 1) = placeholder
                End If
            Next
        Next
        For k = 0 To 25         'so that ciphertextorder is not changed
            ciphertextorder(k) = save(k)
        Next
        Return (currentshifts)
    End Function
    Function translatewithkey(starttext As String, currentshifts() As Integer)
        Dim currentchar As Char
        Dim plaintext As String = ""

        For l = 0 To starttext.Length - 1
            currentchar = starttext.Substring(l, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                currentchar = caesershift(currentchar, currentshifts(Asc(currentchar) - 65))
            End If
            plaintext += currentchar
        Next
        Return (plaintext)
    End Function

    'RANDOM SUBSTITUTION CIPHER
    Sub rndsubstitution(starttext As String, expectedfreq() As Single)     'decipher substitution cipher (a-z -> a-z)

        Console.WriteLine("Random substitution cipher:")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        starttext = starttext.ToUpper
        Randomize()

        Dim currentchar As Char                               'really should have written the algorithms with a 26 letter key in mind, rather than currentshifts
        Dim letterfreq(25) As Integer                         'which makes the code hard to understand (even for me its that bad). i will change in future ciphers
        Dim letterlength As Integer
        Dim placeholder As Single
        Dim plaintext As String = ""
        Dim solvetype As String
        Dim swappair As String
        Dim key As String = ""
        Dim bestexpectedorder(25) As Integer
        Dim oldexpectedorder(25) As Integer
        Dim textscore As Integer
        Dim besttextscore As Integer
        Dim failedswaps As Integer = 0
        Dim randompos1 As Integer
        Dim randompos2 As Integer
        Dim correctletters(25) As Boolean
        Dim removedletter As String = ""
        Dim newkey As String = ""
        Dim myinput As String
        Dim tryagain As Boolean = False

        Dim currentshifts(25) As Integer
        Dim expectedorder(25) As Integer      'The first part of the code will match up letters in the ciphertext to plaintext letters (and create a key) based on frequency ranking
        Dim ciphertextorder(25) As Integer    'order (largest to smallest), so that the most common letter will be matched to 'e' and the least common will be 'z'
        For h = 0 To 25                       'This will not be a full solution, but it will (should) be a good starting point
            expectedorder(h) = h
            ciphertextorder(h) = h
        Next

        For i = 0 To starttext.Length - 1              'honestly this should be a function but there are so many little variations on it i cant be bothered
            currentchar = starttext.Substring(i, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                letterfreq(Asc(currentchar) - 65) += 1
                letterlength += 1
            End If
        Next

        For i = 0 To 23                                 'Bubble sort :)
            For j = 0 To 24
                If expectedfreq(j) < expectedfreq(j + 1) Then
                    placeholder = expectedfreq(j)                 'when the frequency list is sorted, the rank order is sorted at the same time, so item 4 ('e') will be in position 0
                    expectedfreq(j) = expectedfreq(j + 1)
                    expectedfreq(j + 1) = placeholder

                    placeholder = expectedorder(j)
                    expectedorder(j) = expectedorder(j + 1)
                    expectedorder(j + 1) = placeholder
                End If
            Next
        Next



        For i = 0 To 23
            For j = 0 To 24
                If letterfreq(j) < letterfreq(j + 1) Then        'and same for the ciphertext frequencies
                    placeholder = letterfreq(j)
                    letterfreq(j) = letterfreq(j + 1)
                    letterfreq(j + 1) = placeholder

                    placeholder = ciphertextorder(j)
                    ciphertextorder(j) = ciphertextorder(j + 1)
                    ciphertextorder(j + 1) = placeholder
                End If
            Next
        Next

        currentshifts = createorderedkey(expectedorder, ciphertextorder)

        plaintext = translatewithkey(starttext, currentshifts)
        'plaintext = starttext      '(for if the start point is being shit)

        textscore = scorewords(plaintext)

        Console.WriteLine(plaintext)          'Finally, the first step is complete, and we have what might be close to recognisable english
        plaintext = ""
        Console.WriteLine("")
        Console.WriteLine("Starting score: " & textscore)
        besttextscore = textscore
        Console.WriteLine("")
        Console.WriteLine("Automatic solve? y/n")       'If the textlooks similar to english, the user can manually decipher it from here

        solvetype = Console.ReadLine().ToUpper

        If solvetype = "Y" Then                  'HILLCLIMBING GOES HERE
            Console.WriteLine("Here we go...")
            Console.WriteLine("")

            For i = 0 To 25
                oldexpectedorder(i) = expectedorder(i)
            Next

            Do
                For i = 0 To 25
                    expectedorder(i) = oldexpectedorder(i)
                Next
                For i = 0 To 25
                    bestexpectedorder(i) = expectedorder(i)
                Next

                failedswaps = 0
                besttextscore = 2
                correctletters = fillfalse(correctletters)

                currentshifts = createorderedkey(expectedorder, ciphertextorder)

                plaintext = translatewithkey(starttext, currentshifts)
                Console.WriteLine(plaintext)
                plaintext = ""

                Do
                    For i = 0 To Int(3 * Rnd())
                        Do
                            randompos1 = Int(23 * Rnd())                               'Randomly swap 2 reasonably close characters in the key
                            randompos2 = randompos1 + Int(3 * Rnd()) + 1
                        Loop Until (correctletters(expectedorder(randompos1))) = False And (correctletters(expectedorder(randompos2))) = False

                        placeholder = expectedorder(randompos1)
                        expectedorder(randompos1) = expectedorder(randompos2)
                        expectedorder(randompos2) = placeholder

                    Next

                    currentshifts = createorderedkey(expectedorder, ciphertextorder)

                    plaintext = translatewithkey(starttext, currentshifts)

                    textscore = scorewords(plaintext)

                    If textscore > besttextscore Then
                        besttextscore = textscore
                        Console.WriteLine(besttextscore)


                        failedswaps = 0
                        For i = 0 To 25
                            bestexpectedorder(i) = expectedorder(i)
                        Next
                        correctletters = lockletters(plaintext, correctletters)

                        Console.WriteLine(plaintext)
                        'Console.ReadLine()
                    Else
                        For i = 0 To 25
                            expectedorder(i) = bestexpectedorder(i)
                        Next
                    End If
                    failedswaps += 1
                Loop Until failedswaps > 1000

                currentshifts = createorderedkey(bestexpectedorder, ciphertextorder)

                plaintext = translatewithkey(starttext, currentshifts)

                Console.WriteLine(plaintext)

                Console.WriteLine("")
                Console.WriteLine("Go again?")
                myinput = Console.ReadLine().ToUpper
                tryagain = False
                If myinput = "Y" Then
                    tryagain = True
                End If

            Loop Until tryagain = False
        End If


        Dim a As Char
        Dim b As Char
        Dim apos As Integer = 0
        Dim bpos As Integer = 0
        Dim count As Integer = 0
        Console.WriteLine("Input pairs of letters to swap, or 'stop' to stop")
        Do
            swappair = Console.ReadLine.ToUpper

            If swappair.Length = 2 Then
                a = swappair.Substring(0, 1)             'Now it swaps the position where a is stored in 'ciphertextorder', the the position of b
                b = swappair.Substring(1, 1)             'so it finds them in ciphertext order, assigns apos and bpos, and swaps them
                apos = -1
                bpos = -1
                count = 0
                Do
                    If expectedorder(count) = Asc(a) - 65 Then          'searching
                        apos = count
                    End If
                    count += 1
                Loop Until apos > -1 Or count > 24
                count = 0
                Do
                    If expectedorder(count) = Asc(b) - 65 Then
                        bpos = count
                    End If
                    count += 1
                Loop Until bpos > -1 Or count > 25

                If apos = -1 Or bpos = -1 Then
                    Console.WriteLine("lmao get good")
                Else

                    placeholder = expectedorder(apos)                      'swapping
                    expectedorder(apos) = expectedorder(bpos)
                    expectedorder(bpos) = placeholder
                End If
            End If

            currentshifts = createorderedkey(expectedorder, ciphertextorder)
            plaintext = translatewithkey(starttext, currentshifts)
            plaintext = plaintext.ToLower
            Console.WriteLine("")
            Console.WriteLine(plaintext)
            Console.WriteLine("")
        Loop Until swappair = "STOP"

        currentshifts = createorderedkey(ciphertextorder, expectedorder)

        For i = 0 To 25
            key += Chr(i + currentshifts(i) + 65)
        Next

        key = key.ToUpper
        Console.WriteLine(key)
        Console.WriteLine("Hopefully that's deciphered now")

        Console.WriteLine("Key for polybius:")
        removedletter = "J"
        For i = 0 To 25
            If i <> Asc(removedletter) - 65 Then
                newkey += key.Substring(i, 1)
            End If
        Next
        Console.WriteLine(newkey)
        Console.WriteLine("But its not so simple...")
        Console.WriteLine("THIS is the real key")

        Dim polybiuskeyarray(25) As String
        polybiuskeyarray = fillblanks(polybiuskeyarray)
        Dim polybiuskey As String = ""
        currentchar = ""
        For k = 0 To 25
            currentchar = key.Substring(k, 1)
            If removedletter <> Chr(k + 65) Then
                polybiuskeyarray(Asc(currentchar) - 65) = Chr(k + 65)
            End If
        Next

        For j = 0 To 25
            polybiuskey += polybiuskeyarray(j)
        Next j
        Console.WriteLine(polybiuskey)
        Console.WriteLine("Please copy and paste this key, I cant be bothered to change this whole damn thing into a function that returns it")
        Console.ReadLine()
    End Sub

    'VIGENERE CIPHER

    Sub vigenere(starttext As String, expectedfreq() As Single)

        Console.WriteLine("Vigenere cipher:")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        Console.WriteLine("Input max key length:")
        Dim maxlength As Integer
        maxlength = Console.ReadLine()
        starttext = starttext.ToUpper
        Dim bestscoreshift(maxlength) As Integer
        Dim subtexts(maxlength) As String
        Dim letterfreq(25) As Integer
        Dim letterlength As Integer
        Dim keylength As Integer
        Dim subtextnumber As Integer
        Dim myinput As String = ""
        Dim bestscore As Single
        Dim currentshift As Integer
        Dim currentchar As Char
        Dim frequencyscore As Single
        Dim plaintext As String = ""
        Dim currenttext As String
        Dim key As String = ""
        Dim letterstext As String = ""
        Dim letterscount As Integer = 0

        letterstext = justnumbersandletters(starttext)
        keylength = 3

        Do
            bestscoreshift = fillzeros(bestscoreshift)
            subtexts = fillblanks(subtexts)
            plaintext = ""
            key = ""
            Console.WriteLine(keylength)

            For i = 0 To letterstext.Length - 1
                subtexts(i Mod keylength) += letterstext.Substring(i, 1)
            Next

            subtextnumber = 0
            Do
                bestscore = 2
                currenttext = subtexts(subtextnumber)
                For currentshift = 0 To 25
                    For j = 0 To currenttext.Length - 1
                        currentchar = currenttext.Substring(j, 1)
                        If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                            currentchar = caesershift(currentchar, currentshift)
                            letterfreq(Asc(currentchar) - 65) += 1
                            letterlength += 1
                        End If
                    Next j

                    frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)

                    If frequencyscore < bestscore Then
                        bestscore = frequencyscore
                        bestscoreshift(subtextnumber) = currentshift
                    End If

                    letterfreq = fillzeros(letterfreq)
                    letterlength = 0
                Next currentshift

                Console.WriteLine(bestscore & ",   " & bestscoreshift(subtextnumber))
                subtextnumber += 1

            Loop Until subtextnumber = keylength Or bestscore > 0.5

            If subtextnumber = keylength Then
                letterscount = 0
                For i = 0 To starttext.Length - 1
                    currentchar = starttext.Substring(i, 1)
                    If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                        currentchar = caesershift(currentchar, bestscoreshift(letterscount Mod keylength))
                        letterscount += 1
                        plaintext += currentchar
                    End If
                    'plaintext += currentchar
                Next

                plaintext = plaintext.ToLower

                For i = 0 To keylength - 1
                    key += Chr((26 - bestscoreshift(i)) Mod 26 + 65)
                Next
                Console.WriteLine("")
                Console.WriteLine(plaintext)
                Console.WriteLine("")
                Console.WriteLine("Key = " & key)
                Console.WriteLine("Continue? (y/n)")
                myinput = Console.ReadLine().ToLower
            End If
            keylength += 1
        Loop Until keylength = maxlength Or myinput = "n"

    End Sub

    '0 = [, 1 = /...   makes dealing with number to letter shifts easier with ascii

    Function numberstoascii(starttext)

        Dim newtext As String = ""
        Dim currentchar As Char

        For i = 0 To starttext.length - 1
            currentchar = starttext.substring(i, 1)
            If IsNumeric(currentchar) Then
                currentchar = Chr(Asc(currentchar) + 43)
            End If
            newtext += currentchar
        Next

        Return newtext
    End Function

    Sub vigenerwithnumbers(starttext As String, expectedfreq() As Single)

        Console.WriteLine("Input max key length:")
        Dim maxlength As Integer
        maxlength = Console.ReadLine()
        starttext = starttext.ToUpper
        Dim bestscoreshift(maxlength) As Integer
        Dim subtexts(maxlength) As String
        Dim letterfreq(25) As Integer
        Dim letterlength As Integer
        Dim keylength As Integer
        Dim subtextnumber As Integer
        Dim myinput As String = ""
        Dim bestscore As Single
        Dim currentshift As Integer
        Dim currentchar As Char
        Dim frequencyscore As Single
        Dim plaintext As String = ""
        Dim currenttext As String
        Dim key As String = ""
        Dim letterstext As String = ""
        Dim letterscount As Integer = 0

        letterstext = justnumbersandletters(starttext)
        keylength = 3
        letterstext = numberstoascii(letterstext)

        Do
            bestscoreshift = fillzeros(bestscoreshift)
            subtexts = fillblanks(subtexts)
            plaintext = ""
            key = ""
            Console.WriteLine(keylength)

            For i = 0 To letterstext.Length - 1
                subtexts(i Mod keylength) += letterstext.Substring(i, 1)
            Next

            subtextnumber = 0
            Do
                bestscore = 2
                currenttext = subtexts(subtextnumber)
                For currentshift = 0 To 35
                    For j = 0 To currenttext.Length - 1
                        currentchar = currenttext.Substring(j, 1)
                        If Asc(currentchar) > 64 And Asc(currentchar) < 101 Then
                            currentchar = Chr(((Asc(currentchar) - 65 + currentshift) Mod 36) + 65)
                            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                                letterfreq(Asc(currentchar) - 65) += 1
                                letterlength += 1
                            End If
                        End If
                    Next j

                    frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)

                    If frequencyscore < bestscore Then
                        bestscore = frequencyscore
                        bestscoreshift(subtextnumber) = currentshift
                    End If

                    letterfreq = fillzeros(letterfreq)
                    letterlength = 0
                Next currentshift

                Console.WriteLine(bestscore & ",   " & bestscoreshift(subtextnumber))
                subtextnumber += 1

            Loop Until subtextnumber = keylength Or bestscore > 0.5

            If subtextnumber = keylength Then
                letterscount = 0
                For i = 0 To starttext.Length - 1
                    currentchar = starttext.Substring(i, 1)
                    If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                        currentchar = caesershift(currentchar, bestscoreshift(letterscount Mod keylength))
                        letterscount += 1
                        plaintext += currentchar
                    End If
                    'plaintext += currentchar
                Next

                plaintext = plaintext.ToLower

                For i = 0 To keylength - 1
                    key += Chr((26 - bestscoreshift(i)) Mod 26 + 65)
                Next
                Console.WriteLine("")
                Console.WriteLine(plaintext)
                Console.WriteLine("")
                Console.WriteLine("Key = " & key)
                Console.WriteLine("Continue? (y/n)")
                myinput = Console.ReadLine().ToLower
            End If
            keylength += 1
        Loop Until keylength = maxlength Or myinput = "n"

    End Sub

    'RANDOM THING
    Function converttoalphabet(starttext As String, length As Integer)

        starttext = starttext.ToUpper
        Dim charsets(35) As String
        charsets = fillblanks(charsets)
        Dim currentchar As Char
        Dim currentset As String = ""
        Dim charsetpos As Integer
        Dim semiplaintext As String = ""
        Dim found As Boolean = False

        For i = 0 To starttext.Length - 1
            currentchar = starttext.Substring(i, 1)
            If Asc(currentchar) > 64 And Asc(currentchar) < 91 Or IsNumeric(currentchar) Then
                currentset += currentchar
                If currentset.Length = length Then
                    found = False
                    charsetpos = -1
                    Do
                        charsetpos += 1
                        If currentset = charsets(charsetpos) Then
                            found = True
                        End If
                    Loop Until charsetpos = 35 Or charsets(charsetpos) = "" Or found

                    If found = False Then
                        If charsets(charsetpos) <> "" Then
                            Console.WriteLine("ERROR")
                        End If
                        charsets(charsetpos) = currentset
                    End If
                    semiplaintext += Chr(charsetpos + 65)
                    currentset = ""
                End If
            Else
                semiplaintext += currentchar
            End If
        Next


        For i = 0 To 25
            Console.WriteLine(charsets(i))
        Next

        semiplaintext = semiplaintext.ToLower
        Console.WriteLine(semiplaintext)

        Return (semiplaintext)

    End Function

    Sub printpolybiusgrid(colchars() As String, rowchars() As String, key As String)

        If key.Length <> 25 Then    'print simple grid with the 25 pairs
            Console.WriteLine("  " & " | " & colchars(0) & "  " & colchars(1) & "  " & colchars(2) & "  " & colchars(3) & "  " & colchars(4))
            Console.WriteLine("---|---------------")
            Console.WriteLine(" " & rowchars(0) & " | " & rowchars(0) & colchars(0) & " " & rowchars(0) & colchars(1) & " " & rowchars(0) & colchars(2) & " " & rowchars(0) & colchars(3) & " " & rowchars(0) & colchars(4))
            Console.WriteLine(" " & rowchars(1) & " | " & rowchars(1) & colchars(0) & " " & rowchars(1) & colchars(1) & " " & rowchars(1) & colchars(2) & " " & rowchars(1) & colchars(3) & " " & rowchars(1) & colchars(4))
            Console.WriteLine(" " & rowchars(2) & " | " & rowchars(2) & colchars(0) & " " & rowchars(2) & colchars(1) & " " & rowchars(2) & colchars(2) & " " & rowchars(2) & colchars(3) & " " & rowchars(2) & colchars(4))
            Console.WriteLine(" " & rowchars(3) & " | " & rowchars(3) & colchars(0) & " " & rowchars(3) & colchars(1) & " " & rowchars(3) & colchars(2) & " " & rowchars(3) & colchars(3) & " " & rowchars(3) & colchars(4))
            Console.WriteLine(" " & rowchars(4) & " | " & rowchars(4) & colchars(0) & " " & rowchars(4) & colchars(1) & " " & rowchars(4) & colchars(2) & " " & rowchars(4) & colchars(3) & " " & rowchars(4) & colchars(4))
        Else 'print normal grid with key
            Console.WriteLine("  " & " | " & colchars(0) & "  " & colchars(1) & "  " & colchars(2) & "  " & colchars(3) & "  " & colchars(4))
            Console.WriteLine("---|---------------")
            Console.WriteLine(" " & rowchars(0) & " | " & key.Substring(0 * 5 + 0, 1) & "  " & key.Substring(0 * 5 + 1, 1) & "  " & key.Substring(0 * 5 + 2, 1) & "  " & key.Substring(0 * 5 + 3, 1) & "  " & key.Substring(0 * 5 + 4, 1))
            Console.WriteLine(" " & rowchars(1) & " | " & key.Substring(1 * 5 + 0, 1) & "  " & key.Substring(1 * 5 + 1, 1) & "  " & key.Substring(1 * 5 + 2, 1) & "  " & key.Substring(1 * 5 + 3, 1) & "  " & key.Substring(1 * 5 + 4, 1))
            Console.WriteLine(" " & rowchars(2) & " | " & key.Substring(2 * 5 + 0, 1) & "  " & key.Substring(2 * 5 + 1, 1) & "  " & key.Substring(2 * 5 + 2, 1) & "  " & key.Substring(2 * 5 + 3, 1) & "  " & key.Substring(2 * 5 + 4, 1))
            Console.WriteLine(" " & rowchars(3) & " | " & key.Substring(3 * 5 + 0, 1) & "  " & key.Substring(3 * 5 + 1, 1) & "  " & key.Substring(3 * 5 + 2, 1) & "  " & key.Substring(3 * 5 + 3, 1) & "  " & key.Substring(3 * 5 + 4, 1))
            Console.WriteLine(" " & rowchars(4) & " | " & key.Substring(4 * 5 + 0, 1) & "  " & key.Substring(4 * 5 + 1, 1) & "  " & key.Substring(4 * 5 + 2, 1) & "  " & key.Substring(4 * 5 + 3, 1) & "  " & key.Substring(4 * 5 + 4, 1))
        End If
    End Sub

    'POLYBIUS CIPHER (done without 2d arrays cause im bitch)
    Sub polybius(starttext As String, expectedfreq() As Single)

        Console.WriteLine("Polybius cipher:")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        starttext = starttext.ToUpper
        Dim pairs(24) As String
        pairs = fillblanks(pairs)
        Dim currentchar As Char
        Dim currentpair As String = ""
        Dim pairpos As Integer
        Dim plaintext As String = ""
        Dim found As Boolean = False
        Dim colchars(4) As String
        Dim rowchars(4) As String
        For j = 0 To 4
            colchars(j) = ""
            rowchars(j) = ""
        Next
        Dim charpos As Integer
        Dim i As Integer = 0
        Dim order As String
        Dim key As String = ""
        Dim myinput As String

        Console.WriteLine("Solve AND get key? y/n")
        myinput = Console.ReadLine().ToUpper

        If myinput = "Y" Then

            For i = 0 To starttext.Length - 1                  'get pairs
                currentchar = starttext.Substring(i, 1)
                currentpair += currentchar
                If currentpair.Length = 2 Then
                    found = False
                    pairpos = -1
                    Do
                        pairpos += 1
                        If currentpair = pairs(pairpos) Then
                            found = True
                        End If
                    Loop Until pairpos = 24 Or pairs(pairpos) = "" Or found

                    If found = False Then
                        If pairs(pairpos) <> "" Then
                            Console.WriteLine("ERROR")
                        Else
                            pairs(pairpos) = currentpair
                        End If

                    End If

                    currentpair = ""
                End If
            Next

            i = 0

            Do                           'get rows and columns
                found = False
                charpos = -1

                Do
                    charpos += 1
                    If pairs(i).Substring(0, 1) = rowchars(charpos) Then
                        found = True
                    End If
                Loop Until charpos = 4 Or rowchars(charpos) = "" Or found

                If found = False Then
                    If rowchars(charpos) = "" Then
                        rowchars(charpos) = pairs(i).Substring(0, 1)
                    Else
                        Console.WriteLine("ERROR")
                    End If
                End If

                found = False
                charpos = -1

                Do
                    charpos += 1
                    If pairs(i).Substring(1, 1) = colchars(charpos) Then
                        found = True
                    End If
                Loop Until charpos = 4 Or colchars(charpos) = "" Or found

                If found = False Then
                    If colchars(charpos) = "" Then
                        colchars(charpos) = pairs(i).Substring(1, 1)
                    Else
                        Console.WriteLine("ERROR")
                    End If
                End If
                i += 1
            Loop Until pairs(i) = ""

            If colchars(4) = "" Or rowchars(4) = "" Then
                Console.WriteLine("Not 5x5 polybius")
                Console.ReadLine()
            End If

            Console.WriteLine("")                                'create grid (this bit's cool)
            printpolybiusgrid(colchars, rowchars, "a")
            Console.WriteLine("")
            Console.WriteLine("Input column order (eg abcde)")
            order = Console.ReadLine().ToUpper
            For i = 0 To 4
                colchars(i) = order.Substring(i, 1)
            Next
            Console.WriteLine("Input row order (eg zyxwv)")
            order = Console.ReadLine().ToUpper
            For i = 0 To 4
                rowchars(i) = order.Substring(i, 1)
            Next
            Console.WriteLine("")
            printpolybiusgrid(colchars, rowchars, "a")
            Console.WriteLine("")

            While True

                plaintext = ""

                Console.WriteLine("Input a key to fill the grid, (should be 25 letters long and miss one letter)")
                Console.WriteLine("If you dont know the key, input 'a' or leave it blank, I will work out the key for you")
                Console.WriteLine("")
                key = Console.ReadLine().ToUpper
                If key = "A" Or key = "" Then
                    key = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
                End If

                printpolybiusgrid(colchars, rowchars, key)

                Console.WriteLine("Deciphering with current grid")

                Dim col As Integer
                Dim row As Integer

                For i = 0 To starttext.Length - 1 Step 2
                    currentpair = starttext.Substring(i, 2)
                    For j = 0 To 4
                        If currentpair.Substring(0, 1) = rowchars(j) Then
                            row = j
                        End If
                        If currentpair.Substring(1, 1) = colchars(j) Then
                            col = j
                        End If
                    Next
                    plaintext += key.Substring(row * 5 + col, 1)

                Next

                Console.WriteLine(plaintext.ToLower)
                Console.WriteLine("")
                Console.WriteLine("Hit enter to solve with random substitution, and remeber to get the key with 'stop'")
                Console.WriteLine("Remember to remove a letter (probably j) from the key")
                Console.WriteLine("")
                Console.ReadLine()

                rndsubstitution(plaintext, expectedfreq)

            End While

            Console.ReadLine()

        Else

            plaintext = converttoalphabet(starttext, 2)
            rndsubstitution(plaintext, expectedfreq)
        End If

    End Sub

    Function decipherplayfair(numrows As Integer, numcols As Integer, starttext As String, key As String)

        Dim playfairgrid(numcols - 1, numrows - 1) As Char
        Dim plaintext As String = ""
        Dim character As Integer
        Dim char1x As Integer
        Dim char1y As Integer
        Dim char2x As Integer
        Dim char2y As Integer
        Dim found As Boolean
        Dim letter1 As Char
        Dim letter2 As Char
        Dim currentpair As String

        For i = 0 To numrows - 1
            For j = 0 To numcols - 1
                playfairgrid(j, i) = key.Substring(i * numrows + j, 1)
            Next
        Next

        For i = 0 To starttext.Length - 1 Step 2
            currentpair = starttext.Substring(i, 2)
            letter1 = currentpair.Substring(0, 1)
            letter2 = currentpair.Substring(1, 1)
            character = 0
            found = False
            Do
                If key.Substring(character, 1) = letter1 Then
                    char1y = Int(character / numcols)
                    char1x = character Mod numcols
                    found = True
                End If
                character += 1
            Loop Until character = numcols * numrows Or found = True
            character = 0
            found = False
            Do
                If key.Substring(character, 1) = letter2 Then
                    char2y = Int(character / numcols)
                    char2x = character Mod numcols
                    found = True
                End If
                character += 1
            Loop Until character = numcols * numrows Or found = True

            If char1x = char2x Then                                          'Overall rule: normal order
                'plaintext += playfairgrid(char2x, (char2y + 4) Mod numrows)
                plaintext += playfairgrid(char1x, (char1y + 1) Mod numrows)       'Column rule: Vertical down 1
                plaintext += playfairgrid(char2x, (char2y + 1) Mod numrows)
            ElseIf char1y = char2y Then
                'plaintext += playfairgrid((char2x + 1) Mod numcols, char2y)
                plaintext += playfairgrid(char1x, (char1y + 1) Mod numrows)       'Row rule: Vertical down 1
                plaintext += playfairgrid(char2x, (char2y + 1) Mod numrows)
            Else
                plaintext += playfairgrid(char2x, char1y)
                plaintext += playfairgrid(char1x, char2y)
                'plaintext += playfairgrid(char2x, char1y)
            End If
        Next

        Return (plaintext)
    End Function

    Sub playfair(starttext, expectedfreq)

        Console.WriteLine("Playfair cipher:")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        Dim numcols As Integer
        Dim numrows As Integer
        Dim key As String = ""
        Dim plaintext As String = ""
        Dim myinput As String

        Console.WriteLine("Input '6' for 6x6 or '5' for 5x5")
        myinput = Console.ReadLine()

        If myinput = "5" Then
            numrows = 5
            numcols = 5
        ElseIf myinput = "6" Then
            numrows = 6
            numcols = 6
        End If

        Console.WriteLine("Input key, or just dont")
        key = Console.ReadLine().ToUpper

        plaintext = decipherplayfair(numrows, numcols, starttext, key)
        Console.WriteLine(plaintext)
        Console.ReadLine()

    End Sub

    'VIGENERE AUTOKEY CIPHER

    Sub vigenereautokey(starttext As String, expectedfreq() As Single)

        Console.WriteLine("Vigenere autokey cipher:")
        Console.WriteLine("")

        starttext = justnumbersandletters(starttext)

        Console.WriteLine("Input max key length:")
        Dim maxlength As Integer
        maxlength = Console.ReadLine()
        starttext = starttext.ToUpper
        Dim bestscoreshift(maxlength) As Integer
        Dim subtexts(maxlength) As String
        Dim letterfreq(25) As Integer
        Dim letterlength As Integer
        Dim keylength As Integer
        Dim subtextnumber As Integer
        Dim myinput As String = ""
        Dim bestscore As Single
        Dim currentchar As Char
        Dim frequencyscore As Single
        Dim plaintext As String = ""
        Dim currenttext As String
        Dim key As String
        Dim lastchar As Char
        keylength = 1

        Do
            bestscoreshift = fillzeros(bestscoreshift)
            subtexts = fillblanks(subtexts)
            plaintext = ""
            key = ""
            Dim keyletters(keylength) As Char
            Console.WriteLine(keylength)

            For i = 0 To starttext.Length - 1
                subtexts(i Mod keylength) += starttext.Substring(i, 1)
            Next

            subtextnumber = 0
            Do
                bestscore = 2
                currenttext = subtexts(subtextnumber)
                For keyletter = 0 To 25
                    lastchar = Chr(keyletter + 65)
                    For j = 0 To currenttext.Length - 1
                        currentchar = currenttext.Substring(j, 1)
                        currentchar = caesershift(currentchar, 26 - (Asc(lastchar) - 65) Mod 26)
                        lastchar = currentchar
                        letterfreq(Asc(currentchar) - 65) += 1
                        letterlength += 1
                    Next j

                    frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)

                    If frequencyscore < bestscore Then
                        bestscore = frequencyscore
                        keyletters(subtextnumber) = Chr(keyletter + 65)
                    End If

                    letterfreq = fillzeros(letterfreq)
                    letterlength = 0
                Next keyletter

                key += keyletters(subtextnumber)
                Console.WriteLine(bestscore & ",   " & keyletters(subtextnumber))
                subtextnumber += 1

            Loop Until subtextnumber = keylength Or bestscore > 0.5

            If subtextnumber = keylength Then
                Console.WriteLine(key)
                For i = 0 To keylength - 1
                    currentchar = starttext.Substring(i, 1)
                    currentchar = caesershift(currentchar, 26 - (Asc(keyletters(i)) - 65) Mod 26)
                    plaintext += currentchar
                Next
                For i = keylength To starttext.Length - 1
                    currentchar = starttext.Substring(i, 1)
                    currentchar = caesershift(currentchar, 26 - (Asc(plaintext.Substring(i - keylength)) - 65) Mod 26)
                    plaintext += currentchar
                Next

                plaintext = plaintext.ToLower

                For i = 0 To keylength - 1
                    key += Chr((26 - bestscoreshift(i)) Mod 26 + 65)
                Next
                Console.WriteLine("")
                Console.WriteLine(plaintext)
                Console.WriteLine("")
                Console.WriteLine("Key = " & key)
                Console.WriteLine("Continue? (y/n)")
                myinput = Console.ReadLine().ToLower
            End If
            keylength += 1
        Loop Until keylength = maxlength Or myinput = "n"

    End Sub

    Function compareprofile(expectedfreq() As Single, letterfreq() As Integer, letterlength As Integer)    'look at frequency profile to check for simple substitution cipher

        Dim placeholder As Single = 0
        Dim simplesub As Boolean = False
        Dim expected(25) As Single
        For i = 0 To 25
            expected(i) = expectedfreq(i)
        Next

        For i = 0 To 23                         'sort expectedfrequency and letterfrequency from smallest to largest
            For j = 0 To 24
                If expected(j) < expected(j + 1) Then
                    placeholder = expected(j)
                    expected(j) = expected(j + 1)
                    expected(j + 1) = placeholder
                End If
                If letterfreq(j) < letterfreq(j + 1) Then
                    placeholder = letterfreq(j)
                    letterfreq(j) = letterfreq(j + 1)
                    letterfreq(j + 1) = placeholder
                End If
            Next
        Next

        If letterfreq(0) / letterlength - letterfreq(23) / letterlength < 0.09 Then
            Console.WriteLine("Simple substitution: No")
            simplesub = False
        Else
            Console.WriteLine("Simple substitution: Yes")
            simplesub = True
        End If

        For i = 0 To 25
            Console.WriteLine(expected(i) & ", " & letterfreq(i) / letterlength)
        Next

        Return simplesub
    End Function

    'TEXTINFO

    Sub textinfo(starttext As String, expectedfreq() As Single)      'Get stats on starttext (eg length, frequency analysis)

        Dim charcount(127) As Boolean     'To find the number of unique characters
        Dim letterfreq(25) As Integer     'for frequency analysis
        Dim frequencyscore As Single = 0  'rate frequency analysis
        Dim numbers As Boolean = False    'does the string contain numbers
        Dim letterlength As Integer = 0
        Dim currentchar As Char
        Dim nofcharacters As Integer = 0
        Dim successscore As Single = 0.5
        Dim simplesub As Boolean
        Dim numbercount As Integer = 0
        Dim numtoletter As Single
        Dim bigramscore As Single
        Dim wordscore As Single

        For i As Integer = 0 To starttext.Length - 1      'Run through the ciphertext, doing frequency analysis etc.
            currentchar = starttext.Substring(i, 1)
            If currentchar <> "" Then
                If IsNumeric(currentchar) Then

                    charcount(Asc(currentchar)) = True
                    numbercount += 1
                End If
                If Asc(currentchar) > 64 And Asc(currentchar) < 91 Then
                    letterfreq(Asc(currentchar) - 65) += 1
                    charcount(Asc(currentchar)) = True
                    letterlength += 1
                End If
            End If
            If IsNumeric(currentchar) Then
                numbers = True
            End If
        Next

        For i As Integer = 0 To charcount.Length - 1      'Unique character counting
            If charcount(i) Then
                nofcharacters += 1
            End If
        Next

        frequencyscore = comparefreq(letterfreq, letterlength, expectedfreq)
        bigramscore = scorebigrams(starttext, letterlength)
        wordscore = scorewords(starttext)
        wordscore = wordscore / letterlength
        'overallscore = totalscore(starttext, expectedfreq)

        Console.WriteLine("")
        Console.WriteLine("Length: " & starttext.Length)                                      'Output text info
        Console.WriteLine("Unique characters (numbers and letters): " & nofcharacters)
        Console.WriteLine("Numbers: " & numbers)
        'Console.WriteLine("Bigram score: " & bigramscore)
        Console.WriteLine("Word score: " & wordscore)
        'Console.WriteLine("Overall score: " & overallscore)
        If numbercount > 0 Then
            numtoletter = letterlength / numbercount
            Console.WriteLine("Numbers : letters = 1:" & numtoletter)
        End If
        Console.WriteLine("Frequency score = " & frequencyscore)

        If frequencyscore < successscore Then
            Console.WriteLine("Transposition/plaintext: Yes")
        Else
            Console.WriteLine("Transposition/plaintext: No")
        End If

        If nofcharacters > 21 Then
            simplesub = compareprofile(expectedfreq, letterfreq, letterlength)
        Else
            Console.WriteLine("Simple substitution: No")
        End If
    End Sub

    Sub newcipher(starttext As String, expectedfreq() As Single)

    End Sub
    Sub Main(args As String())

        Dim starttext As String
        Dim expectedfreq() = New Single() {0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.0236, 0.0015, 0.01974, 0.00074}
        Dim myinput As String

        Console.WriteLine("Text decipherer v3.1415")
        Console.WriteLine("Currently using frequency analysis, common words, and the letters T and H (somehow)")
        Console.WriteLine("For a bunch of different ciphers (not hill cipher though)")
        'Console.WriteLine("By Finn Sellar")

        Console.WriteLine("")
        Console.WriteLine("Input ciphertext here:")
        starttext = Console.ReadLine().ToUpper
        starttext = justnumbersandletters(starttext)

        While True
            textinfo(starttext, expectedfreq)

        Console.WriteLine("1 - Morse code")
        Console.WriteLine("2 - Binary")
        Console.WriteLine("3 - Hex")
        Console.WriteLine("A - Caeser cipher")
        Console.WriteLine("B - Simple sub with numbers")
        Console.WriteLine("C - Transposition cipher")
        Console.WriteLine("D - Affine cipher")
        Console.WriteLine("E - Random substitution cipher")
        Console.WriteLine("F - Vigener cipher")
            Console.WriteLine("G - Polybius cipher")
            Console.WriteLine("H - Playfair cipher (beta)")
            Console.WriteLine("I - Vigenere autokey cipher")
            Console.WriteLine("Z - New cipher")
            myinput = Console.ReadLine().ToUpper
        Console.WriteLine("")

            Select Case myinput
                Case "1"
                    Console.WriteLine("Morse code")
                    morse(starttext)
                Case "2"
                    Console.WriteLine("Binary")
                    binary(starttext)
                Case "3"
                    Console.WriteLine("Hex")
                    hex(starttext)
                Case "A"
                    caeser(starttext, expectedfreq)
                Case "B"
                    Console.WriteLine("Simple sub with numbers")
                    starttext = justnumbersandletters(starttext)
                    starttext = converttoalphabet(starttext, 1)
                    rndsubstitution(starttext, expectedfreq)
                Case "C"
                    transposition(starttext)
                Case "D"
                    affine(starttext, expectedfreq)
                Case "E"
                    rndsubstitution(starttext, expectedfreq)
                Case "F"
                    vigenere(starttext, expectedfreq)
                Case "G"
                    polybius(starttext, expectedfreq)
                Case "H"
                    playfair(starttext, expectedfreq)
                Case "I"
                    vigenereautokey(starttext, expectedfreq)
                Case "Z"
                    newcipher(starttext, expectedfreq)
            End Select
            Console.ReadLine()

            Console.WriteLine("And go again! (you have to copy + paste your new text in again)")
            Console.WriteLine(starttext)
        End While
        Console.ReadLine()
    End Sub

End Module
