' The Blank Page item template is documented at https://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

''' <summary>
''' An empty page that can be used on its own or navigated to within a Frame.
''' </summary>
Public NotInheritable Class MainPage
    Inherits Page

#Region "stream i play"

    Dim mStrm As MemoryStream       ' bez naglowka
    Dim mStrWav As MemoryStream     ' z naglowkiem
    Dim mWriter As BinaryWriter
    Dim mFreq As Integer
    Dim miChan As Integer
    Private Sub PlayStream()
        mStrWav.Seek(0, SeekOrigin.Begin)

        Dim soundSource As Windows.Storage.Streams.IRandomAccessStream = mStrWav.AsRandomAccessStream()
        ' 20180903, w oryginale nie było AS, a po otwarciu w nowym targetSDK napisało taki własnie IOStream.
        soundSource.Seek(0)
        uiGrajek.SetSource(soundSource, "audio/wav")

        ' uiStatus.Background = New SolidColorBrush(Windows.UI.Colors.Red)

        uiGrajek.Play()

    End Sub
    Private Sub StreamRewrite()
        mStrWav = New MemoryStream
        Dim oWriter As BinaryWriter = New BinaryWriter(mStrWav)

        ' nagłówek WAV
        Dim iFileSize As Integer ' waveSize + headerSize + formatChunkSize + headerSize + dataChunkSize
        iFileSize = 4 + 8 + 16 + 8 + mStrm.Length ' naglowki: pliku WAV, FMT chunk, DATA chunk

        oWriter.Write(&H46464952)    ' bbbb = 'RIFF'
        oWriter.Write(iFileSize)      ' bbbb
        oWriter.Write(&H45564157)    ' bbbb = 'WAVE'

        ' naglowek FMT
        ' musi przejsc przez zmienne, zeby bylo wiadomo jaki typ (ile bajtow) zapisuje
        Dim formatChunkSize As Integer = 16
        Dim formatType As Int16 = 1
        Dim tracks As Int16 = miChan
        Dim bitsPerSample As Int16 = 16
        Dim frameSize As Int16 = (tracks * ((bitsPerSample + 7) / 8))
        Dim bytesPerSecond As Integer = mFreq * frameSize

        oWriter.Write(&H20746D66)    ' bbbb = 'FMT '
        oWriter.Write(formatChunkSize) ' bbbb = 16 (header len)
        oWriter.Write(formatType)    ' bb = 1: PCM
        oWriter.Write(tracks)        ' bb = 1: mono, 2: stereo, etc.
        oWriter.Write(mFreq) ' bbbb
        oWriter.Write(bytesPerSecond)    ' bbbb  == SampleRate * NumChannels * BitsPerSample/8
        oWriter.Write(frameSize)     ' bb  == NumChannels * BitsPerSample/8
        oWriter.Write(bitsPerSample) ' bb

        ' teraz naglowek danych
        Dim dataChunkSize As Integer = mStrm.Length

        oWriter.Write(&H61746164) '= encoding.GetBytes("data"); bbbb = 'DATA'
        oWriter.Write(dataChunkSize) ' bbbb, == NumSamples * NumChannels * BitsPerSample/8

        mStrm.WriteTo(mStrWav)

    End Sub
    Private Sub ResetStream(iFreq As Integer, iChan As Integer)
        If uiGrajek.CurrentState <> MediaElementState.Closed And
                uiGrajek.CurrentState <> MediaElementState.Paused And
            uiGrajek.CurrentState <> MediaElementState.Stopped Then
            Exit Sub
        End If

        If mWriter IsNot Nothing Then mWriter.Dispose()
        If mStrm IsNot Nothing Then mStrm.Dispose()
        If mStrWav IsNot Nothing Then mStrWav.Dispose()
        mStrm = New MemoryStream
        mWriter = New BinaryWriter(mStrm)
        mFreq = iFreq
        miChan = iChan

    End Sub
#End Region

#Region "Emulacja LIRC"

    Private Sub Lirc_Send_Pre(iData As Long, iBits As Integer, pre_p As Integer, pre_s As Integer)
        If iBits = 0 Then Exit Sub
        Lirc_Send_Data(iData, iBits)
        If pre_p + pre_s = 0 Then Exit Sub
        Lirc_Send_Pulse(pre_p)
        Lirc_Send_Space(pre_s)
    End Sub
    Private Function Lirc_Reverse(iCode As Long, iBits As Integer) As Long
        Dim iTmp As Long = 0

        For i As Integer = 1 To iBits
            If iCode And 1 Then
                iTmp = iTmp Or 1
            End If
            iTmp = iTmp << 1
            iCode = iCode >> 1
        Next

        iTmp = iTmp >> 1

        Return iTmp
    End Function

    Private Sub LircBit(bBit As Byte)

        If bBit Then
            Lirc_Send_Pulse(miOnePulse)
            Lirc_Send_Space(miOneSpace)
        Else
            Lirc_Send_Pulse(miZeroPulse)
            Lirc_Send_Space(miZeroSpace)
        End If
    End Sub

    Private Sub Lirc_Send_Data(iCode As Long, iBits As Integer, Optional bReverse As Boolean = False)
        If iBits > 60 Then Exit Sub

        If bReverse Then iCode = Lirc_Reverse(iCode, iBits)

        For i As Integer = 1 To iBits
            LircBit(iCode And &H1)
            iCode = iCode >> 1
        Next

    End Sub
    Private Sub Lirc_Send_Trail(ptrail As Integer)
        If ptrail = 0 Then Exit Sub
        Lirc_Send_Pulse(ptrail)
    End Sub

    Private Sub Lirc_Send_Foot(sfoot As Integer, pfoot As Integer)
        If sfoot + pfoot = 0 Then Exit Sub
        Lirc_Send_Space(sfoot)
        Lirc_Send_Pulse(pfoot)
    End Sub

    Dim miSrodek As Int16 = 0
    Dim miGora As Int16 = 32700
    Dim miDol As Int16 = -32700
    Dim mbLED1 As Boolean = True

    Private Sub Lirc_Send_Pulse(iLenUs As Integer)
        ' nosna przez iLenUs mikrosekund, albo liczba okresow

        Dim iLenOkres As Integer
        iLenOkres = iLenUs / 26      ' 562.5 / x = 22 (0x16) [Acer]

        For j As Integer = 1 To iLenOkres
            If mbLED1 Then
                mWriter.Write(miGora)
                mWriter.Write(miDol)
                mWriter.Write(miGora)    ' LEDa 1
                mWriter.Write(miDol)
                mWriter.Write(miSrodek)
                mWriter.Write(miSrodek)
            Else
                mWriter.Write(miDol)
                mWriter.Write(miGora)    ' LEDa 2
                mWriter.Write(miDol)
                mWriter.Write(miGora)
                mWriter.Write(miSrodek)
                mWriter.Write(miSrodek)
            End If
            mbLED1 = Not mbLED1
        Next

    End Sub
    Private Sub Lirc_Send_Space(iLenUs As Integer)
        ' nosna przez iLenUs mikrosekund
        Dim iLenOkres As Integer
        iLenOkres = iLenUs / 26      ' 562 / x = 22 (0x16) [Acer]

        ' to mozna dokladniej, nie co do 27.7 μs (tu może być 10 μs)

        For j As Integer = 1 To iLenOkres
            For i As Integer = 1 To 3 * 2    ' 3 sampli w 2 kanałach, odpowiednik 'pulse'
                mWriter.Write(miSrodek)
            Next
        Next

    End Sub

    Dim miOnePulse, miOneSpace, miZeroPulse, miZeroSpace As Integer

    Private Sub Lirc_SetConfigBitTimes(iOnePulse As Integer, iOneSpace As Integer, iZeroPulse As Integer, iZeroSpace As Integer)
        ' odpowiednik LIRC linii "one" oraz "zero", w tejze kolejnosci
        ' dla Elmak:
        ' one             0   833
        ' zero          833     0
        miOnePulse = iOnePulse
        miOneSpace = iOneSpace
        miZeroPulse = iZeroPulse
        miZeroSpace = iZeroSpace
    End Sub

    Private Sub Lirc_Send_Header(phead As Integer, shead As Integer)
        If phead + shead = 0 Then Exit Sub
        Lirc_Send_Pulse(phead)
        Lirc_Send_Space(shead)
    End Sub

#End Region


    Private Sub uiButtonTap(sender As Object, e As TappedRoutedEventArgs) Handles uiButton70.Tapped, uiButton71.Tapped, uiButton72.Tapped, uiButton73.Tapped, uiButton74.Tapped, uiButton75.Tapped, uiButton76.Tapped, uiButton77.Tapped, uiButton78.Tapped, uiButton79.Tapped, uiButton7a.Tapped, uiButton7b.Tapped, uiButton7c.Tapped, uiButton7d.Tapped, uiButton7e.Tapped, uiButton7f.Tapped
        Dim oTgl As Button = TryCast(sender, Button)
        Dim sName As String = oTgl.Name
        If sName.Substring(0, 8) <> "uiButton" Then Exit Sub

        Dim iCode As Byte = Convert.ToByte(sName.Substring(8, 2), 16)

        ResetStream(114000, 2)  ' 3*38 kHz
        Lirc_Send_Space(2048)   ' potrzebne, przynajmniej dla Elmak - inicjalizacja DAC

        Lirc_SetConfigBitTimes(562, 1650, 562, 562)

        Lirc_Send_Header(8950, 4450)

        Lirc_Send_Pre(&H1308, 16, 0, 0)   ' od LSB

        Dim iCodeR As Byte = iCode Xor &HFF
        Lirc_Send_Data(iCodeR, 8)
        Lirc_Send_Data(iCode, 8)

        Lirc_Send_Trail(562)
        Lirc_Send_Foot(50000, 0)


        StreamRewrite()

        PlayStream()

    End Sub

End Class
