$regfile "m8def.dat"
$crystal = 8000000
$hwstack = 64

Config Portd.7 = Output
Config Portd.6 = Output                                     'outputs
Config Portd.5 = Output

Config Portd.4 = Output                                     'output for STEP - stepper motor driver
Config Portd.3 = Output                                     'output for DIR - stepper motor driver

Wyjsciezaplon Alias Portd.7                                 'alias output signal for EDIS
Wyjsciewtrysk1 Alias Portd.6                                'injector 1
Wyjsciewtrysk2 Alias Portd.5                                'injector 2

Config Portb.1 = Output

Pompa Alias Portb.1                                         'fuel pump relay

Config Portb.0 = Output
Config Portb.3 = Output                                     '
Config Portb.4 = Output
Config Portb.5 = Output
Config Portd.1 = Output
Set Portb.0
Set Portd.1

Dim Herce(12) As Word
Dim Herces As Word
Dim Herceswolne As Word
Dim Przeplyw As Word
Dim Lambda As Word
Dim Dawka4(512) As Byte                                     'fuel dose array
Dim Wzap(256) As Byte                                       'ignition angle arrat

Dim Kornap As Byte
Dim Poznap(5) As Word
Dim Poznapsr As Word
Dim Pozycjah As Word
Dim Pozycjav As Word
Dim Pozycja As Word
Dim Pozycjavw As Word
Dim Pozycjaw As Word
Dim Dawka4paliwa As Byte
Dim Wypzap As Byte
Dim Odliczaj As Bit
Dim Lambdakorek As Byte
Dim Obrstart As Byte
Dim Przejscie As Byte
Dim Ntcnap As Word
Dim Przepustnap As Word
Dim Ntckorek(16) As Byte
Dim Ntckor As Byte
Dim Pozntc As Word
Dim Dawkawolne As Byte
Dim Pierwszy As Bit
Dim Bylabogata As Bit
Dim Propskok As Byte

Obrstart = 0
Lambdakorek = 25                                            'start value for labda correction
Dawkawolne = 45                                             'start value of fuel dose for idling

Config Timer1 = Timer , Prescale = 256                      'timer measuring period beetween two EDIS pulses
Config Timer0 = Timer , Prescale = 64                       'timer - ignition 64=>1 period= 8us   max=2ms
Config Timer2 = Timer , Prescale = 256                      'timer - injection 256=>period= 32us  max=8ms
Config Pind.2 = Input                                       'pulse input from EDIS
Config Int0 = Falling                                       'activated on falling edge
Enable Interrupts
Enable Int0
Enable Timer0
Enable Timer2

Config Adc = Single , Prescaler = 8 , Reference = Avcc      'reference voltage for ADC: supply

On Int0 Impuls                                              'interrupts config
On Timer0 Odliczanie
On Timer2 Odliczaniew

Set Pompa
Wait 3
Reset Pompa

Do                                                          'main loop
   Przeplyw = Getadc(0)                                     'air flow meter reading

   Lambda = Getadc(1)                                       'lambda sensor reading

   Poznap(1) = Poznap(2)                                    'measurment and averaging supply voltage
   Poznap(2) = Poznap(3)
   Poznap(3) = Poznap(4)
   Poznap(4) = Poznap(5)
   Poznap(5) = Getadc(2)
   Poznapsr = Poznap(1) + Poznap(2)
   Poznapsr = Poznapsr + Poznap(3)
   Poznapsr = Poznapsr + Poznap(4)
   Poznapsr = Poznapsr + Poznap(5)

   Ntcnap = Getadc(4)                                       'oil temperature reading

   Przepustnap = Getadc(5)                                  'throttle sensor reading

If Odliczaj = 1 Then
   Set Pompa

   If Przeplyw > 205 Then                                   'set 1V at air flow meter as 0 signal
      Przeplyw = Przeplyw - 205
   Else
      Przeplyw = 0
   End If
   If Przeplyw > 347 Then                                   'max 3V (3/5*1023)-205=410
      Przeplyw = 347
   End If


   Pozycjav = Herces \ 132
   Pozycjah = Przeplyw \ 22                                 'ignition angle map position
   Pozycja = 16 * Pozycjav
   Pozycja = Pozycja + Pozycjah

   Pozycjavw = Herces \ 66
   Pozycjaw = 16 * Pozycjavw                                'fuel map position
   Pozycjaw = Pozycjaw + Pozycjah


   Dawka4paliwa = Lookup(pozycjaw , Dawka4)                 'read maps
   Wypzap = Lookup(pozycja , Wzap)


   Select Case Poznapsr                                     'supply voltage-based correction
      Case 0 To 3240 : Kornap = 26
      Case 3241 To 3580 : Kornap = 23
      Case 3581 To 4095 : Kornap = 19
      Case 4096 To 4435 : Kornap = 16
      Case Else : Kornap = 11
   End Select

      Incr Obrstart                                         'strokes since start
      If Obrstart > 250 Then
         Obrstart = 251
      End If
      Pozntc = Ntcnap \ 64
      Ntckor = Lookup(pozntc , Ntckorek)                    'temperature correction

      Select Case Obrstart
         Case 0 To 50 : Dawka4paliwa = 200                  'rich, constant AFR for first 25 revs
         Case 51 To 150 : Dawka4paliwa = 250 - Obrstart
      Case Else :
      End Select
      Dawka4paliwa = Dawka4paliwa + Ntckor

   If Ntcnap < 400 Then                                     'lambda sensor correction closed loop: for oil temperature higher than
      If Przepustnap < 850 And Przepustnap > 590 Then       'and if throttle partially opened (not WOT)

         If Lambda < 40 Then                                'correction for lean
            If Bylabogata = 1 Then
               Propskok = Dawka4paliwa
               Shift Propskok , Right , 4
               Lambdakorek = Lambdakorek - Propskok         'rich to lean jump - add fuel (proportianal to base fuel dose)
               Reset Bylabogata
            Else
               Lambdakorek = Lambdakorek - 1
            End If
         End If
         If Lambda > 160 Then                               'correction for rich
            If Bylabogata = 0 Then
               Propskok = Dawka4paliwa
               Shift Propskok , Right , 4
               Lambdakorek = Lambdakorek + Propskok
               Set Bylabogata
            Else
               Lambdakorek = Lambdakorek + 1
            End If
         End If
         If Lambdakorek > 49 Then                           'limit max and min lambda-derived correction
            Lambdakorek = 49
         End If
         If Lambdakorek < 1 Then
            Lambdakorek = 1
         End If
         Dawka4paliwa = Dawka4paliwa + 25                   'add/remove lambda corretions
         Dawka4paliwa = Dawka4paliwa - Lambdakorek          'with default value (25) lambdakorek Dawka4 will not change
        End If


   End If

   If Przepustnap > 850 Then
      Dawka4paliwa = Dawka4paliwa + 20                      'WOT fuel increase
   End If

   Herceswolne = Herce(11) + Herce(12)
   If Przepustnap < 580 Then                                'idling logic
      If Herceswolne < 50 And Dawkawolne < 60 Then
         Dawkawolne = Dawkawolne + 1
      End If
      If Herceswolne > 50 And Dawkawolne > 40 Then
         Dawkawolne = Dawkawolne - 1
      End If
      Dawka4paliwa = Dawkawolne + Ntckor
   End If

      If Herces < 2004 Then                                 'rev limiter (5000 rpm)
         If Dawka4paliwa > 50 Then                          'two injections per crankshaft revolution
            Dawka4paliwa = Dawka4paliwa + Kornap

            Counter2 = 255 - Dawka4paliwa
            Wyjsciewtrysk1 = 1
            Wyjsciewtrysk2 = 1
            Start Timer2
         Else
            Dawka4paliwa = Dawka4paliwa * 2                 'single per crankshaft revolution
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
      Else
         Wyjsciewtrysk1 = 0                                 'close injectors
         Wyjsciewtrysk2 = 0
      End If

   Counter0 = 255 - Wypzap
   Wyjsciezaplon = 1
   Start Timer0
   Reset Odliczaj                                           'bit set to 0
End If



If Przepustnap < 580 Then
   If Przejscie > 99 Then                                   'sends signal to stepper motor driver
         If Herceswolne < 50 Then
            Portd.3 = 0                                     'dir
            Toggle Portd.4                                  'step
         End If
         If Herceswolne > 50 Then
            Portd.3 = 1
            Toggle Portd.4                                  'step
         End If

      Przejscie = 0
   Else
      Incr Przejscie
   End If
End If


Loop

Dawka4:                                                     'fuel map 32x16
Data 33 , 49 , 66 , 83 , 100 , 119 , 126 , 127 , 129 , 132 , 134 , 135 , 145 , 151 , 151 , 151
Data 27 , 43 , 59 , 75 , 92 , 110 , 123 , 129 , 131 , 135 , 137 , 139 , 145 , 150 , 151 , 151
Data 21 , 36 , 52 , 67 , 83 , 101 , 120 , 131 , 134 , 139 , 141 , 142 , 145 , 149 , 151 , 151
Data 15 , 30 , 45 , 60 , 75 , 92 , 110 , 126 , 130 , 135 , 140 , 142 , 144 , 147 , 151 , 151
Data 9 , 25 , 39 , 54 , 67 , 82 , 100 , 120 , 127 , 132 , 139 , 141 , 142 , 145 , 151 , 151
Data 5 , 19 , 33 , 48 , 59 , 74 , 90 , 110 , 121 , 129 , 134 , 136 , 138 , 142 , 150 , 151
Data 1 , 14 , 27 , 42 , 51 , 66 , 80 , 99 , 115 , 126 , 129 , 131 , 134 , 140 , 148 , 151
Data 1 , 11 , 24 , 38 , 47 , 60 , 73 , 90 , 106 , 120 , 125 , 128 , 132 , 137 , 146 , 150
Data 1 , 8 , 21 , 34 , 42 , 55 , 66 , 80 , 98 , 113 , 121 , 125 , 130 , 134 , 144 , 148
Data 1 , 6 , 20 , 31 , 40 , 52 , 61 , 75 , 93 , 107 , 114 , 118 , 124 , 129 , 137 , 142
Data 1 , 4 , 19 , 28 , 38 , 49 , 56 , 70 , 88 , 101 , 108 , 112 , 118 , 125 , 130 , 136
Data 1 , 5 , 17 , 26 , 35 , 47 , 54 , 65 , 82 , 96 , 104 , 108 , 113 , 121 , 129 , 135
Data 1 , 7 , 15 , 25 , 33 , 45 , 52 , 60 , 76 , 90 , 99 , 104 , 109 , 118 , 127 , 134
Data 1 , 6 , 14 , 22 , 30 , 43 , 49 , 58 , 71 , 85 , 95 , 99 , 105 , 114 , 126 , 134
Data 1 , 6 , 13 , 19 , 28 , 41 , 47 , 56 , 66 , 81 , 91 , 95 , 101 , 111 , 126 , 134
Data 1 , 4 , 11 , 17 , 26 , 38 , 45 , 54 , 64 , 78 , 88 , 93 , 100 , 110 , 124 , 133
Data 1 , 3 , 8 , 15 , 25 , 36 , 43 , 53 , 62 , 74 , 85 , 91 , 98 , 108 , 123 , 132
Data 1 , 2 , 7 , 15 , 24 , 35 , 42 , 52 , 60 , 72 , 83 , 90 , 96 , 107 , 121 , 130
Data 1 , 1 , 6 , 14 , 23 , 34 , 41 , 52 , 59 , 70 , 81 , 88 , 93 , 106 , 120 , 127
Data 1 , 1 , 6 , 14 , 23 , 32 , 39 , 50 , 57 , 67 , 78 , 86 , 91 , 102 , 117 , 125
Data 1 , 1 , 6 , 14 , 23 , 30 , 38 , 49 , 56 , 64 , 76 , 84 , 90 , 98 , 113 , 123
Data 1 , 1 , 6 , 13 , 22 , 29 , 36 , 47 , 55 , 63 , 74 , 82 , 88 , 97 , 111 , 121
Data 1 , 1 , 5 , 13 , 21 , 28 , 34 , 45 , 53 , 61 , 73 , 80 , 86 , 95 , 108 , 119
Data 1 , 1 , 4 , 11 , 20 , 27 , 33 , 43 , 52 , 60 , 69 , 77 , 84 , 94 , 106 , 117
Data 1 , 1 , 3 , 10 , 18 , 26 , 32 , 41 , 50 , 59 , 66 , 74 , 83 , 92 , 104 , 115
Data 1 , 1 , 2 , 8 , 16 , 24 , 31 , 38 , 48 , 58 , 65 , 72 , 81 , 91 , 102 , 113
Data 1 , 1 , 1 , 6 , 15 , 23 , 29 , 36 , 46 , 57 , 65 , 69 , 78 , 90 , 101 , 112
Data 1 , 1 , 1 , 4 , 12 , 20 , 27 , 34 , 43 , 56 , 64 , 69 , 78 , 88 , 99 , 111
Data 1 , 1 , 1 , 3 , 10 , 18 , 25 , 31 , 39 , 54 , 64 , 68 , 77 , 87 , 98 , 109
Data 1 , 1 , 1 , 2 , 9 , 17 , 24 , 29 , 37 , 52 , 62 , 67 , 76 , 86 , 97 , 108
Data 1 , 1 , 1 , 1 , 8 , 17 , 24 , 28 , 35 , 50 , 60 , 66 , 76 , 85 , 95 , 106
Data 1 , 1 , 1 , 1 , 7 , 15 , 21 , 25 , 34 , 49 , 57 , 64 , 74 , 84 , 94 , 105


Wzap:                                                       'ignition angle map 16x16
Data 124 , 127 , 133 , 140 , 139 , 138 , 137 , 136 , 138 , 140 , 143 , 146 , 149 , 153 , 157 , 160
Data 120 , 124 , 127 , 132 , 132 , 130 , 128 , 128 , 130 , 133 , 136 , 139 , 142 , 146 , 150 , 153
Data 116 , 120 , 124 , 125 , 126 , 126 , 126 , 126 , 126 , 127 , 130 , 133 , 135 , 139 , 143 , 147
Data 105 , 111 , 114 , 116 , 117 , 117 , 117 , 118 , 120 , 123 , 125 , 127 , 129 , 132 , 136 , 140
Data 83 , 95 , 105 , 111 , 110 , 111 , 114 , 115 , 117 , 119 , 120 , 120 , 122 , 125 , 130 , 134
Data 62 , 67 , 87 , 103 , 102 , 105 , 109 , 109 , 111 , 112 , 113 , 115 , 116 , 118 , 124 , 129
Data 50 , 61 , 77 , 90 , 93 , 96 , 100 , 100 , 102 , 103 , 104 , 106 , 108 , 113 , 120 , 125
Data 39 , 52 , 67 , 78 , 82 , 85 , 91 , 91 , 91 , 95 , 100 , 103 , 106 , 109 , 117 , 122
Data 27 , 41 , 54 , 64 , 70 , 74 , 80 , 83 , 86 , 90 , 96 , 98 , 100 , 108 , 115 , 120
Data 14 , 27 , 40 , 49 , 56 , 63 , 70 , 77 , 82 , 86 , 92 , 94 , 101 , 107 , 113 , 118
Data 2 , 14 , 25 , 35 , 43 , 50 , 59 , 69 , 77 , 81 , 91 , 93 , 100 , 105 , 110 , 115
Data 1 , 1 , 11 , 21 , 29 , 37 , 47 , 58 , 72 , 83 , 91 , 93 , 95 , 99 , 106 , 112
Data 1 , 1 , 1 , 7 , 16 , 24 , 34 , 46 , 65 , 84 , 90 , 89 , 89 , 94 , 103 , 109
Data 1 , 1 , 1 , 1 , 3 , 13 , 23 , 36 , 55 , 74 , 83 , 84 , 86 , 93 , 103 , 109
Data 1 , 1 , 1 , 1 , 1 , 1 , 13 , 26 , 42 , 59 , 72 , 79 , 82 , 94 , 106 , 109
Data 1 , 1 , 1 , 1 , 1 , 1 , 4 , 18 , 32 , 46 , 63 , 74 , 77 , 93 , 108 , 108

Ntckorek:                                                   'temperature correction map.
Data 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 15 , 30 , 35 , 40 , 45 , 50 , 50


Impuls:                                                     'EDIS pulse (int0) interrupt
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
    Counter1 = 0
    Start Timer1
Return
Odliczanie:                                                 'timer pulse to EDIS (timer0) interrupt
   Stop Timer0
   Wyjsciezaplon = 0

Return

Odliczaniew:                                                'injector pulse timer interrupr
   Stop Timer2
   Wyjsciewtrysk1 = 0
   Wyjsciewtrysk2 = 0
Return
