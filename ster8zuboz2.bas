                                                               'zintegrowany sterownik v0.08
'wersja uproszczona praca parami

'nowy uklad pinów
'PD7 - szpilkowego lcd to 13  - dla zaplonu
'PD6 - szpilkowego lcd to 12  - wtrysk 1
'PD5 - szpilkowego lcd to 11  - wtrysk 2
'7 -10 masa

$regfile "m8def.dat"
$crystal = 8000000
$hwstack = 64                                               'powiekszony stos z uwagi na przerwania i gosub
Config Portd.7 = Output
Config Portd.6 = Output                                     'wyjœcia
Config Portd.5 = Output

Config Portd.4 = Output                                     'krok clk
Config Portd.3 = Output                                     'krok ccw/cw

Wyjsciezaplon Alias Portd.7                                 'aliasy dla przejrzystosci
Wyjsciewtrysk1 Alias Portd.6
Wyjsciewtrysk2 Alias Portd.5

Config Portb.1 = Output
Config Portd.0 = Output
Config Portb.2 = Output

D2 Alias Portb.2                                            'dioda2
D1 Alias Portd.0                                            'dioda3

Pompa Alias Portb.1                                         'pompa paliwa

Dim Herce(12) As Word                                       'uwaga na typy! word i byte
Dim Herces As Word
Dim Herceswolne As Word
Dim Przeplyw As Word
Dim Lambda As Word
Dim Dawka4(256) As Byte                                     'tablica dawki wtrysku
Dim Wzap(256) As Byte

Dim Kornap As Byte
Dim Poznap(5) As Word
Dim Poznapsr As Word
Dim Pozycjah As Word
Dim Pozycjav As Word
Dim Pozycja As Word
Dim Dawka4paliwa As Byte
Dim Wypzap As Byte
Dim Odliczaj As Bit                                         'zeby zmniejszyc czestosc wtryskow
Dim Lambdakorek As Byte                                     'wartosci korekcyjne od lambdy
Dim Obrstart As Byte
Dim Przejscie As Byte
Dim Ntcnap As Word
Dim Przepustnap As Word
Dim Ntckorek(16) As Byte
Dim Ntckor As Byte
Dim Pozntc As Word
Dim Dawkawolne As Byte
Dim Pierwszy As Bit
Dim Lambdaspowol As Byte

Obrstart = 0
Lambdakorek = 25                                            'wartosc startowa dla korekcji skladu od lambdy
Dawkawolne = 45
Lambdaspowol = 0

Config Timer1 = Timer , Prescale = 256                      'timer okresu
Config Timer0 = Timer , Prescale = 64                       '"szybki" timer - zaplon  64=>1cykl= 8us   max=2ms
Config Timer2 = Timer , Prescale = 256                      '"wolny" timer - wtrysk   256=>1cykl= 32us  max=8ms
'Config Timer2 = Timer , Prescale = 1024                     '"wolny" timer - wtrysk   1024=>1cykl= 128us  max=32ms
Config Pind.2 = Input                                       'wejscie impulsu
Config Int0 = Falling                                       'na spadku
Enable Interrupts
Enable Int0
Enable Timer0
Enable Timer2

Config Adc = Single , Prescaler = 8 , Reference = Avcc      'referencja - napiecie zasilania
'Cls
'Start Adc

On Int0 Impuls                                              'in4
On Timer0 Odliczanie
On Timer2 Odliczaniew

Set Pompa
Wait 3
Reset Pompa
'glowna petla
Do
   Przeplyw = Getadc(0)                                     ' odczyt z przeplywomierza

   Lambda = Getadc(1)                                       '200mV uboga (40) 800mV bogata(164), in2

   Poznap(1) = Poznap(2)
   Poznap(2) = Poznap(3)
   Poznap(3) = Poznap(4)
   Poznap(4) = Poznap(5)
   Poznap(5) = Getadc(2)
   Poznapsr = Poznap(1) + Poznap(2)
   Poznapsr = Poznapsr + Poznap(3)
   Poznapsr = Poznapsr + Poznap(4)
   Poznapsr = Poznapsr + Poznap(5)

   Ntcnap = Getadc(4)

   Przepustnap = Getadc(5)

'do tego miejsca powinno minac >10 us zeby czas czas edisowi
If Odliczaj = 1 Then
   Set Pompa

   If Przeplyw > 205 Then                                   'obciecie poziomu 1V z przeplywomierza
      Przeplyw = Przeplyw - 205
   Else
      Przeplyw = 0
   End If
   If Przeplyw > 347 Then                                   'zakladane nieprzekroczenie 3V (3/5*1023)-205=410
      Przeplyw = 347
   End If


   Pozycjav = Herces \ 132                                  '\dzielenie calk  /dzielenie z przec
   Pozycjah = Przeplyw \ 22                                 'ostatnie pole numer 15
   Pozycja = 16 * Pozycjav
   Pozycja = Pozycja + Pozycjah

   Dawka4paliwa = Lookup(pozycja , Dawka4)
   Wypzap = Lookup(pozycja , Wzap)

   Select Case Poznapsr                                     'korekcja od napiecia
      Case 0 To 3070 : Kornap = 29                          'od 9V
      Case 3071 To 3240 : Kornap = 26
      Case 3241 To 3580 : Kornap = 23
      Case 3581 To 4095 : Kornap = 19
      Case 4096 To 4435 : Kornap = 16
      Case Else : Kornap = 11
   End Select

      Incr Obrstart                                         'ilosc polobrotow od uruchomienia
      If Obrstart > 250 Then
         Obrstart = 251
      End If
      Pozntc = Ntcnap \ 64                                  'ntcnap kolumny
      Ntckor = Lookup(pozntc , Ntckorek)

      Select Case Obrstart
         Case 0 To 50 : Dawka4paliwa = 200                  'bogata stala paliwa przez pierwsz 25 obrotow
         Case 51 To 150 : Dawka4paliwa = 250 - Obrstart
      Case Else :
      End Select
      Dawka4paliwa = Dawka4paliwa + Ntckor                  'ponadto korekcja od temperatury


   Reset D1
   Reset D2
   If Ntcnap < 512 Then                                     'dla temperatury wiekszej ni¿ 50C                                                 'regulacja I lambda
      If Przepustnap < 850 And Przepustnap > 590 Then       'tylko jesli przepustnica nie jest calkiem otwarta lub zamknieta
         If Lambdaspowol = 10 Then                          'zwolnienie regulacji lambda

         If Lambda < 40 Then                                'korekta dla ubogiej (200 mV)
            Set D1
            Lambdakorek = Lambdakorek - 1
         End If
         If Lambda > 160 Then                               'korekta dla bogatej (800 mV)
            Set D2
            Lambdakorek = Lambdakorek + 1
         End If
         If Lambdakorek > 49 Then                           'zabezpieczenie przed ucieczka lambda
            Lambdakorek = 49
         End If
         If Lambdakorek < 1 Then
            Lambdakorek = 1
         End If
         Dawka4paliwa = Dawka4paliwa + 25                   'dodawanie i odejmowanie wartosci korekcyjnych lambdy
         Dawka4paliwa = Dawka4paliwa - Lambdakorek          ' przy domyslnej wielkosci (25) lambdakorek Dawka4 nie ulegnie zmianie
         Lambdaspowol = 0
        End If
        Incr Lambdaspowol
      End If

   End If

   If Przepustnap > 850 Then
      Dawka4paliwa = Dawka4paliwa + 5                       'wzbogacanie dla pelnego otwarcia przepustnicy
   End If

   Herceswolne = Herce(11) + Herce(12)
   If Przepustnap < 600 Then                                'sterowanie dawk¹ biegu ja³owego
      If Herceswolne < 50 And Dawkawolne < 60 Then
         Dawkawolne = Dawkawolne + 1
      End If
      If Herceswolne > 50 And Dawkawolne > 40 Then
         Dawkawolne = Dawkawolne - 1
      End If
      Dawka4paliwa = Dawkawolne + Ntckor
   End If







    '  If Herces < 2004 Then                                 'ogranicznik obrotów  167*4 (5000 obr/min)
         If Dawka4paliwa > 50 Then                          'wieksze dawki
            Dawka4paliwa = Dawka4paliwa + Kornap

            Counter2 = 255 - Dawka4paliwa
            Wyjsciewtrysk1 = 1
            Wyjsciewtrysk2 = 1
            Start Timer2
         Else
            Dawka4paliwa = Dawka4paliwa * 2                 'mniejsze dawki
            If Pierwszy = 1 Then
               Reset Pierwszy
               Dawka4paliwa = Dawka4paliwa + Kornap
               Counter2 = 255 - Dawka4paliwa
               Wyjsciewtrysk1 = 1
               Wyjsciewtrysk2 = 1
               Start Timer2
            Else
               Set Pierwszy
            End If
         End If
   '   Else
   '   Wyjsciewtrysk1 = 0                                    'odciecie wtrysku
   '   Wyjsciewtrysk2 = 0
   '   End If

   Counter0 = 255 - Wypzap                                  'wartosc poczatkowa licznika
   Wyjsciezaplon = 1
   Start Timer0                                             'z uwagi na rozne preskalery male prawdopodobienstwo wystapienia jednoczesnie od obu timerow
   Reset Odliczaj                                           'bit na 0
End If



If Przepustnap < 590 Then
   If Przejscie > 199 Then                                  'wysyla sygnal na sterownik silnika krokowego co iles petli
         If Herceswolne < 50 Then
            Portd.3 = 0                                     'kierunek
            Toggle Portd.4                                  'zegar
         End If
         If Herceswolne > 50 Then
            Portd.3 = 1
            Toggle Portd.4                                  'zegar
         End If

      Przejscie = 0
   Else
      Incr Przejscie
   End If
End If


Loop


Dawka4:                                                     'jesli integer to dodawac % do liczb, single to ! a long &
Data 26 , 39 , 53 , 66 , 80 , 95 , 111 , 120 , 120 , 120 , 120 , 107 , 115 , 120 , 120 , 120
Data 17 , 29 , 41 , 53 , 66 , 80 , 95 , 111 , 120 , 120 , 131 , 104 , 111 , 119 , 120 , 120
Data 8 , 20 , 31 , 43 , 53 , 65 , 80 , 95 , 120 , 120 , 121 , 99 , 107 , 115 , 120 , 120
Data 1 , 11 , 22 , 33 , 41 , 53 , 62 , 79 , 118 , 120 , 110 , 95 , 104 , 111 , 118 , 120
Data 1 , 6 , 16 , 24 , 30 , 40 , 48 , 60 , 103 , 120 , 99 , 92 , 100 , 107 , 114 , 120
Data 1 , 4 , 17 , 27 , 32 , 45 , 46 , 55 , 78 , 92 , 94 , 94 , 102 , 109 , 115 , 122
Data 1 , 6 , 16 , 23 , 30 , 49 , 58 , 59 , 74 , 84 , 95 , 101 , 105 , 110 , 116 , 122
Data 1 , 6 , 12 , 17 , 26 , 37 , 42 , 49 , 54 , 74 , 82 , 85 , 92 , 95 , 116 , 122
Data 1 , 1 , 6 , 13 , 22 , 30 , 37 , 45 , 49 , 59 , 82 , 85 , 89 , 95 , 111 , 116
Data 1 , 1 , 5 , 12 , 18 , 27 , 33 , 41 , 43 , 51 , 70 , 67 , 74 , 84 , 100 , 106
Data 1 , 1 , 5 , 11 , 18 , 24 , 30 , 40 , 46 , 49 , 54 , 62 , 72 , 89 , 97 , 103
Data 1 , 1 , 4 , 10 , 17 , 22 , 27 , 39 , 45 , 49 , 53 , 63 , 74 , 92 , 101 , 106
Data 1 , 1 , 1 , 8 , 14 , 20 , 26 , 31 , 42 , 45 , 49 , 55 , 63 , 77 , 105 , 109
Data 1 , 1 , 1 , 5 , 12 , 18 , 23 , 29 , 41 , 45 , 51 , 55 , 64 , 75 , 87 , 113
Data 1 , 1 , 1 , 1 , 8 , 14 , 20 , 23 , 30 , 43 , 50 , 54 , 63 , 77 , 87 , 110
Data 1 , 1 , 1 , 1 , 8 , 14 , 20 , 23 , 30 , 43 , 50 , 54 , 63 , 77 , 87 , 110


Wzap:
Data 110 , 118 , 133 , 140 , 139 , 138 , 137 , 136 , 138 , 140 , 143 , 146 , 149 , 153 , 157 , 160
Data 112 , 118 , 127 , 132 , 132 , 130 , 128 , 128 , 130 , 133 , 136 , 139 , 142 , 146 , 150 , 153
Data 111 , 115 , 119 , 122 , 124 , 123 , 121 , 121 , 124 , 127 , 130 , 133 , 135 , 139 , 143 , 147
Data 97 , 102 , 109 , 116 , 117 , 117 , 117 , 118 , 120 , 123 , 125 , 127 , 129 , 132 , 136 , 140
Data 78 , 82 , 98 , 111 , 110 , 111 , 114 , 115 , 117 , 119 , 120 , 120 , 122 , 125 , 130 , 134
Data 62 , 67 , 87 , 103 , 102 , 105 , 109 , 109 , 111 , 115 , 114 , 113 , 114 , 118 , 124 , 129
Data 50 , 61 , 77 , 90 , 93 , 96 , 100 , 100 , 102 , 109 , 108 , 104 , 107 , 113 , 120 , 125
Data 39 , 52 , 67 , 78 , 82 , 85 , 91 , 91 , 91 , 109 , 100 , 97 , 102 , 109 , 117 , 122
Data 27 , 41 , 54 , 64 , 70 , 74 , 80 , 83 , 86 , 97 , 96 , 95 , 100 , 108 , 115 , 120
Data 14 , 27 , 40 , 49 , 56 , 63 , 70 , 77 , 82 , 86 , 92 , 94 , 101 , 107 , 113 , 118
Data 2 , 14 , 25 , 35 , 43 , 50 , 59 , 69 , 77 , 81 , 91 , 93 , 100 , 105 , 110 , 115
Data 1 , 1 , 11 , 21 , 29 , 37 , 47 , 58 , 72 , 83 , 91 , 93 , 95 , 99 , 106 , 112
Data 1 , 1 , 1 , 7 , 16 , 24 , 34 , 46 , 65 , 84 , 90 , 89 , 89 , 94 , 103 , 109
Data 1 , 1 , 1 , 1 , 3 , 13 , 23 , 36 , 55 , 74 , 83 , 84 , 86 , 93 , 103 , 109
Data 1 , 1 , 1 , 1 , 1 , 1 , 13 , 26 , 42 , 59 , 72 , 79 , 82 , 94 , 106 , 109
Data 1 , 1 , 1 , 1 , 1 , 1 , 4 , 18 , 32 , 46 , 63 , 74 , 77 , 93 , 108 , 108

'V 1 1,12 1,24 1,37 1,49 1,61 1,73 1,85 1,98 2,1 2,22 2,34 2,59 2,71 2,83 2,95
'0 Hz
'11 Hz
'22 Hz
'33 Hz
'44 Hz
'55 Hz
'66 Hz
'77 Hz
'88 Hz
'99 Hz
'110 Hz
'121 Hz
'132 Hz
'143 Hz
'154 Hz
'165 Hz

Ntckorek:
Data 10 , 10 , 10 , 10 , 10 , 10 , 10 , 15 , 17 , 20 , 34 , 40 , 44 , 50 , 50 , 50


Impuls:                                                     'przerwanie od int0
    Herce(1) = Herce(2)
    Herce(2) = Herce(3)
    Herce(3) = Herce(4)
    Herce(4) = Herce(5)
    Herce(5) = Herce(6)
    Herce(6) = Herce(7)
    Herce(7) = Herce(8)
    Herce(8) = Herce(9)
    Herce(9) = Herce(10)
    Herce(10) = Herce(11)
    Herce(11) = Herce(12)
    Stop Timer1
    Herce(12) = 31250 \ Counter1
    Counter1 = 0
    Start Timer1
    If Herce(12) < 170 Then
      Herces = Herce(1) + Herce(2)
      Herces = Herces + Herce(3)
      Herces = Herces + Herce(4)
      Herces = Herces + Herce(5)
      Herces = Herces + Herce(6)
      Herces = Herces + Herce(7)
      Herces = Herces + Herce(8)
      Herces = Herces + Herce(9)
      Herces = Herces + Herce(10)
      Herces = Herces + Herce(11)
      Herces = Herces + Herce(12)
      Set Odliczaj
    Else
      Herce(12) = Herce(11)
    End If
Return
Odliczanie:                                                 'przerwanie od timera0
   Stop Timer0
   Wyjsciezaplon = 0

Return

Odliczaniew:
   Stop Timer2
   Wyjsciewtrysk1 = 0
   Wyjsciewtrysk2 = 0
Return