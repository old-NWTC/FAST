
MODULE ElastoDyn_Parameters

      ! This module contains definitions of compile-time PARAMETERS for the StrucyDyn module.
      ! Every variable defined here MUST have the PARAMETER attribute.


   USE NWTC_Library

   REAL(ReKi), PARAMETER            :: SmallAngleLimit_Deg  =  15.0                     ! Largest input angle considered "small" (used as a check on input data), degrees
   

      ! Parameters related to degrees of freedom (formerly MODULE DOFs)

   INTEGER(IntKi), PARAMETER        :: MaxBl    =  3                                   ! Maximum number of blades allowed in simulation
   INTEGER(IntKi), PARAMETER        :: NumBE    =  1                                   ! Number of blade-edge modes
   INTEGER(IntKi), PARAMETER        :: NumBF    =  2                                   ! Number of blade-flap modes

   INTEGER(IntKi), PARAMETER        :: DOF_Sg   =  1                                   ! DOF index for platform surge
   INTEGER(IntKi), PARAMETER        :: DOF_Sw   =  2                                   ! DOF index for platform sway
   INTEGER(IntKi), PARAMETER        :: DOF_Hv   =  3                                   ! DOF index for platform heave
   INTEGER(IntKi), PARAMETER        :: DOF_R    =  4                                   ! DOF index for platform roll
   INTEGER(IntKi), PARAMETER        :: DOF_P    =  5                                   ! DOF index for platform pitch
   INTEGER(IntKi), PARAMETER        :: DOF_Y    =  6                                   ! DOF index for platform yaw
   INTEGER(IntKi), PARAMETER        :: DOF_TFA1 =  7                                   ! DOF index for 1st tower fore-aft mode
   INTEGER(IntKi), PARAMETER        :: DOF_TSS1 =  8                                   ! DOF index for 1st tower side-to-side mode
   INTEGER(IntKi), PARAMETER        :: DOF_TFA2 =  9                                   ! DOF index for 2nd tower fore-aft mode
   INTEGER(IntKi), PARAMETER        :: DOF_TSS2 = 10                                   ! DOF index for 2nd tower side-to-side mode
   INTEGER(IntKi), PARAMETER        :: DOF_Yaw  = 11                                   ! DOF index for nacelle-yaw
   INTEGER(IntKi), PARAMETER        :: DOF_RFrl = 12                                   ! DOF index for rotor-furl
   INTEGER(IntKi), PARAMETER        :: DOF_GeAz = 13                                   ! DOF index for the generator azimuth
   INTEGER(IntKi), PARAMETER        :: DOF_DrTr = 14                                   ! DOF index for drivetrain rotational-flexibility
   INTEGER(IntKi), PARAMETER        :: DOF_TFrl = 15                                   ! DOF index for tail-furl

   INTEGER(IntKi), PARAMETER        :: DOF_BE (MaxBl,NumBE) = RESHAPE(  &              ! DOF indices for blade edge:
                                               (/ 17, 20, 23 /),   (/MaxBl,NumBE/) )   !    1st blade edge mode for blades 1,2, and 3, respectively 17 + 3*(K-1)
   INTEGER(IntKi), PARAMETER        :: DOF_BF (MaxBl,NumBF) = RESHAPE(  &              ! DOF indices for blade flap:
                                               (/ 16, 19, 22,           &              !    1st blade flap mode for blades 1,2, and 3, respectively 16 + 3*(K-1)
                                                  18, 21, 24 /),   (/MaxBl,NumBF/) )   !    2nd blade flap mode for blades 1,2, and 3, respectively 18 + 3*(K-1)


   INTEGER(IntKi), PARAMETER        :: DOF_Teet = 22 !DOF_TFrl + 2*(NumBE+NumBF)+ 1    ! DOF index for rotor-teeter



   INTEGER(IntKi), PARAMETER        :: NPA      =  9                                   ! Number of DOFs that contribute to the angular velocity of the tail (body A) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPB      =  7                                   ! Number of DOFs that contribute to the angular velocity of the tower top / baseplate (body B) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPF      =  7                                   ! Number of DOFs that contribute to the angular velocity of the tower elements (body F) in the inertia frame                                           (body F) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPG      = 10                                   ! Number of DOFs that contribute to the angular velocity of the generator (body G) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPL      = 11                                   ! Number of DOFs that contribute to the angular velocity of the low-speed shaft (body L) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPN      =  8                                   ! Number of DOFs that contribute to the angular velocity of the nacelle (body N) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPR      =  9                                   ! Number of DOFs that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPX      =  3                                   ! Number of DOFs that contribute to the angular velocity of the platform (body X) in the inertia frame.

   INTEGER(IntKi), PARAMETER        :: PX(NPX)  = (/ DOF_R, DOF_P, DOF_Y /)                                                                                          ! Array of DOF indices (pointers) that contribute to the angular velocity of the platform                                                  (body X) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PF(NPF)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower elements                                            (body F) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PB(NPB)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower top / baseplate                                     (body B) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PN(NPN)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw /)                                         ! Array of DOF indices (pointers) that contribute to the angular velocity of the nacelle                                                   (body N) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PR(NPR)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PL(NPL)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /)           ! Array of DOF indices (pointers) that contribute to the angular velocity of the low-speed shaft                                           (body L) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PG(NPG)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz /)                     ! Array of DOF indices (pointers) that contribute to the angular velocity of the generator                                                 (body G) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PA(NPA)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_TFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the tail                                                      (body A) in the inertia frame.




      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   INTEGER(IntKi), PARAMETER        :: NMX      =  9                                   ! Used in updating predictor-corrector values.



      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   INTEGER(IntKi), PARAMETER        :: PolyOrd  =  6                                    ! Order of the polynomial describing the mode shape


! ==================================================================================================="
! NOTE: The following lines of code were generated by a Matlab script called "Write_ChckOutLst.m"
!      using the parameters listed in the "OutListParameters.xlsx" Excel file. Any changes to these 
!      lines should be modified in the Matlab script and/or Excel worksheet as necessary. 
! ==================================================================================================="
! This code was generated by Write_ChckOutLst.m at 12-Feb-2013 23:16:43.


     ! Parameters related to output length (number of characters allowed in the output data headers):

   INTEGER(IntKi), PARAMETER      :: OutStrLen   = 10
   INTEGER(IntKi), PARAMETER      :: OutStrLenM1 = OutStrLen - 1

     ! Indices for computing output channels:
     ! NOTES: 
     !    (1) These parameters are in the order stored in "OutListParameters.xlsx"
     !    (2) Array y%AllOuts() must be dimensioned to the value of the largest output parameter
     !    (3) If an index (MaxOutPts) ever becomes greater or equal to 1000, the logic to create ARRAY/1 in the FAST-to-ADAMS preprocessor will have to be changed.

     !  Time: 

   INTEGER(IntKi), PARAMETER      :: Time      =   0


  ! Wind Motions:

   INTEGER(IntKi), PARAMETER      :: WindVxi   =   1
   INTEGER(IntKi), PARAMETER      :: WindVyi   =   2
   INTEGER(IntKi), PARAMETER      :: WindVzi   =   3
   INTEGER(IntKi), PARAMETER      :: TotWindV  =   4
   INTEGER(IntKi), PARAMETER      :: HorWindV  =   5
   INTEGER(IntKi), PARAMETER      :: HorWndDir =   6
   INTEGER(IntKi), PARAMETER      :: VerWndDir =   7


  ! Blade 1 Tip Motions:

   INTEGER(IntKi), PARAMETER      :: TipDxc1   =   8
   INTEGER(IntKi), PARAMETER      :: TipDyc1   =   9
   INTEGER(IntKi), PARAMETER      :: TipDzc1   =  10
   INTEGER(IntKi), PARAMETER      :: TipDxb1   =  11
   INTEGER(IntKi), PARAMETER      :: TipDyb1   =  12
   INTEGER(IntKi), PARAMETER      :: TipALxb1  =  13
   INTEGER(IntKi), PARAMETER      :: TipALyb1  =  14
   INTEGER(IntKi), PARAMETER      :: TipALzb1  =  15
   INTEGER(IntKi), PARAMETER      :: TipRDxb1  =  16
   INTEGER(IntKi), PARAMETER      :: TipRDyb1  =  17
   INTEGER(IntKi), PARAMETER      :: TipRDzc1  =  18
   INTEGER(IntKi), PARAMETER      :: TipClrnc1 =  19


  ! Blade 2 Tip Motions:

   INTEGER(IntKi), PARAMETER      :: TipDxc2   =  20
   INTEGER(IntKi), PARAMETER      :: TipDyc2   =  21
   INTEGER(IntKi), PARAMETER      :: TipDzc2   =  22
   INTEGER(IntKi), PARAMETER      :: TipDxb2   =  23
   INTEGER(IntKi), PARAMETER      :: TipDyb2   =  24
   INTEGER(IntKi), PARAMETER      :: TipALxb2  =  25
   INTEGER(IntKi), PARAMETER      :: TipALyb2  =  26
   INTEGER(IntKi), PARAMETER      :: TipALzb2  =  27
   INTEGER(IntKi), PARAMETER      :: TipRDxb2  =  28
   INTEGER(IntKi), PARAMETER      :: TipRDyb2  =  29
   INTEGER(IntKi), PARAMETER      :: TipRDzc2  =  30
   INTEGER(IntKi), PARAMETER      :: TipClrnc2 =  31


  ! Blade 3 Tip Motions:

   INTEGER(IntKi), PARAMETER      :: TipDxc3   =  32
   INTEGER(IntKi), PARAMETER      :: TipDyc3   =  33
   INTEGER(IntKi), PARAMETER      :: TipDzc3   =  34
   INTEGER(IntKi), PARAMETER      :: TipDxb3   =  35
   INTEGER(IntKi), PARAMETER      :: TipDyb3   =  36
   INTEGER(IntKi), PARAMETER      :: TipALxb3  =  37
   INTEGER(IntKi), PARAMETER      :: TipALyb3  =  38
   INTEGER(IntKi), PARAMETER      :: TipALzb3  =  39
   INTEGER(IntKi), PARAMETER      :: TipRDxb3  =  40
   INTEGER(IntKi), PARAMETER      :: TipRDyb3  =  41
   INTEGER(IntKi), PARAMETER      :: TipRDzc3  =  42
   INTEGER(IntKi), PARAMETER      :: TipClrnc3 =  43


  ! Blade 1 Local Span Motions:

   INTEGER(IntKi), PARAMETER      :: Spn1ALxb1 =  44
   INTEGER(IntKi), PARAMETER      :: Spn1ALyb1 =  45
   INTEGER(IntKi), PARAMETER      :: Spn1ALzb1 =  46
   INTEGER(IntKi), PARAMETER      :: Spn2ALxb1 =  47
   INTEGER(IntKi), PARAMETER      :: Spn2ALyb1 =  48
   INTEGER(IntKi), PARAMETER      :: Spn2ALzb1 =  49
   INTEGER(IntKi), PARAMETER      :: Spn3ALxb1 =  50
   INTEGER(IntKi), PARAMETER      :: Spn3ALyb1 =  51
   INTEGER(IntKi), PARAMETER      :: Spn3ALzb1 =  52
   INTEGER(IntKi), PARAMETER      :: Spn4ALxb1 =  53
   INTEGER(IntKi), PARAMETER      :: Spn4ALyb1 =  54
   INTEGER(IntKi), PARAMETER      :: Spn4ALzb1 =  55
   INTEGER(IntKi), PARAMETER      :: Spn5ALxb1 =  56
   INTEGER(IntKi), PARAMETER      :: Spn5ALyb1 =  57
   INTEGER(IntKi), PARAMETER      :: Spn5ALzb1 =  58
   INTEGER(IntKi), PARAMETER      :: Spn6ALxb1 =  59
   INTEGER(IntKi), PARAMETER      :: Spn6ALyb1 =  60
   INTEGER(IntKi), PARAMETER      :: Spn6ALzb1 =  61
   INTEGER(IntKi), PARAMETER      :: Spn7ALxb1 =  62
   INTEGER(IntKi), PARAMETER      :: Spn7ALyb1 =  63
   INTEGER(IntKi), PARAMETER      :: Spn7ALzb1 =  64
   INTEGER(IntKi), PARAMETER      :: Spn8ALxb1 =  65
   INTEGER(IntKi), PARAMETER      :: Spn8ALyb1 =  66
   INTEGER(IntKi), PARAMETER      :: Spn8ALzb1 =  67
   INTEGER(IntKi), PARAMETER      :: Spn9ALxb1 =  68
   INTEGER(IntKi), PARAMETER      :: Spn9ALyb1 =  69
   INTEGER(IntKi), PARAMETER      :: Spn9ALzb1 =  70
   INTEGER(IntKi), PARAMETER      :: Spn1TDxb1 =  71
   INTEGER(IntKi), PARAMETER      :: Spn1TDyb1 =  72
   INTEGER(IntKi), PARAMETER      :: Spn1TDzb1 =  73
   INTEGER(IntKi), PARAMETER      :: Spn2TDxb1 =  74
   INTEGER(IntKi), PARAMETER      :: Spn2TDyb1 =  75
   INTEGER(IntKi), PARAMETER      :: Spn2TDzb1 =  76
   INTEGER(IntKi), PARAMETER      :: Spn3TDxb1 =  77
   INTEGER(IntKi), PARAMETER      :: Spn3TDyb1 =  78
   INTEGER(IntKi), PARAMETER      :: Spn3TDzb1 =  79
   INTEGER(IntKi), PARAMETER      :: Spn4TDxb1 =  80
   INTEGER(IntKi), PARAMETER      :: Spn4TDyb1 =  81
   INTEGER(IntKi), PARAMETER      :: Spn4TDzb1 =  82
   INTEGER(IntKi), PARAMETER      :: Spn5TDxb1 =  83
   INTEGER(IntKi), PARAMETER      :: Spn5TDyb1 =  84
   INTEGER(IntKi), PARAMETER      :: Spn5TDzb1 =  85
   INTEGER(IntKi), PARAMETER      :: Spn6TDxb1 =  86
   INTEGER(IntKi), PARAMETER      :: Spn6TDyb1 =  87
   INTEGER(IntKi), PARAMETER      :: Spn6TDzb1 =  88
   INTEGER(IntKi), PARAMETER      :: Spn7TDxb1 =  89
   INTEGER(IntKi), PARAMETER      :: Spn7TDyb1 =  90
   INTEGER(IntKi), PARAMETER      :: Spn7TDzb1 =  91
   INTEGER(IntKi), PARAMETER      :: Spn8TDxb1 =  92
   INTEGER(IntKi), PARAMETER      :: Spn8TDyb1 =  93
   INTEGER(IntKi), PARAMETER      :: Spn8TDzb1 =  94
   INTEGER(IntKi), PARAMETER      :: Spn9TDxb1 =  95
   INTEGER(IntKi), PARAMETER      :: Spn9TDyb1 =  96
   INTEGER(IntKi), PARAMETER      :: Spn9TDzb1 =  97
   INTEGER(IntKi), PARAMETER      :: Spn1RDxb1 =  98
   INTEGER(IntKi), PARAMETER      :: Spn1RDyb1 =  99
   INTEGER(IntKi), PARAMETER      :: Spn1RDzb1 = 100
   INTEGER(IntKi), PARAMETER      :: Spn2RDxb1 = 101
   INTEGER(IntKi), PARAMETER      :: Spn2RDyb1 = 102
   INTEGER(IntKi), PARAMETER      :: Spn2RDzb1 = 103
   INTEGER(IntKi), PARAMETER      :: Spn3RDxb1 = 104
   INTEGER(IntKi), PARAMETER      :: Spn3RDyb1 = 105
   INTEGER(IntKi), PARAMETER      :: Spn3RDzb1 = 106
   INTEGER(IntKi), PARAMETER      :: Spn4RDxb1 = 107
   INTEGER(IntKi), PARAMETER      :: Spn4RDyb1 = 108
   INTEGER(IntKi), PARAMETER      :: Spn4RDzb1 = 109
   INTEGER(IntKi), PARAMETER      :: Spn5RDxb1 = 110
   INTEGER(IntKi), PARAMETER      :: Spn5RDyb1 = 111
   INTEGER(IntKi), PARAMETER      :: Spn5RDzb1 = 112
   INTEGER(IntKi), PARAMETER      :: Spn6RDxb1 = 113
   INTEGER(IntKi), PARAMETER      :: Spn6RDyb1 = 114
   INTEGER(IntKi), PARAMETER      :: Spn6RDzb1 = 115
   INTEGER(IntKi), PARAMETER      :: Spn7RDxb1 = 116
   INTEGER(IntKi), PARAMETER      :: Spn7RDyb1 = 117
   INTEGER(IntKi), PARAMETER      :: Spn7RDzb1 = 118
   INTEGER(IntKi), PARAMETER      :: Spn8RDxb1 = 119
   INTEGER(IntKi), PARAMETER      :: Spn8RDyb1 = 120
   INTEGER(IntKi), PARAMETER      :: Spn8RDzb1 = 121
   INTEGER(IntKi), PARAMETER      :: Spn9RDxb1 = 122
   INTEGER(IntKi), PARAMETER      :: Spn9RDyb1 = 123
   INTEGER(IntKi), PARAMETER      :: Spn9RDzb1 = 124


  ! Blade 2 Local Span Motions:

   INTEGER(IntKi), PARAMETER      :: Spn1ALxb2 = 125
   INTEGER(IntKi), PARAMETER      :: Spn1ALyb2 = 126
   INTEGER(IntKi), PARAMETER      :: Spn1ALzb2 = 127
   INTEGER(IntKi), PARAMETER      :: Spn2ALxb2 = 128
   INTEGER(IntKi), PARAMETER      :: Spn2ALyb2 = 129
   INTEGER(IntKi), PARAMETER      :: Spn2ALzb2 = 130
   INTEGER(IntKi), PARAMETER      :: Spn3ALxb2 = 131
   INTEGER(IntKi), PARAMETER      :: Spn3ALyb2 = 132
   INTEGER(IntKi), PARAMETER      :: Spn3ALzb2 = 133
   INTEGER(IntKi), PARAMETER      :: Spn4ALxb2 = 134
   INTEGER(IntKi), PARAMETER      :: Spn4ALyb2 = 135
   INTEGER(IntKi), PARAMETER      :: Spn4ALzb2 = 136
   INTEGER(IntKi), PARAMETER      :: Spn5ALxb2 = 137
   INTEGER(IntKi), PARAMETER      :: Spn5ALyb2 = 138
   INTEGER(IntKi), PARAMETER      :: Spn5ALzb2 = 139
   INTEGER(IntKi), PARAMETER      :: Spn6ALxb2 = 140
   INTEGER(IntKi), PARAMETER      :: Spn6ALyb2 = 141
   INTEGER(IntKi), PARAMETER      :: Spn6ALzb2 = 142
   INTEGER(IntKi), PARAMETER      :: Spn7ALxb2 = 143
   INTEGER(IntKi), PARAMETER      :: Spn7ALyb2 = 144
   INTEGER(IntKi), PARAMETER      :: Spn7ALzb2 = 145
   INTEGER(IntKi), PARAMETER      :: Spn8ALxb2 = 146
   INTEGER(IntKi), PARAMETER      :: Spn8ALyb2 = 147
   INTEGER(IntKi), PARAMETER      :: Spn8ALzb2 = 148
   INTEGER(IntKi), PARAMETER      :: Spn9ALxb2 = 149
   INTEGER(IntKi), PARAMETER      :: Spn9ALyb2 = 150
   INTEGER(IntKi), PARAMETER      :: Spn9ALzb2 = 151
   INTEGER(IntKi), PARAMETER      :: Spn1TDxb2 = 152
   INTEGER(IntKi), PARAMETER      :: Spn1TDyb2 = 153
   INTEGER(IntKi), PARAMETER      :: Spn1TDzb2 = 154
   INTEGER(IntKi), PARAMETER      :: Spn2TDxb2 = 155
   INTEGER(IntKi), PARAMETER      :: Spn2TDyb2 = 156
   INTEGER(IntKi), PARAMETER      :: Spn2TDzb2 = 157
   INTEGER(IntKi), PARAMETER      :: Spn3TDxb2 = 158
   INTEGER(IntKi), PARAMETER      :: Spn3TDyb2 = 159
   INTEGER(IntKi), PARAMETER      :: Spn3TDzb2 = 160
   INTEGER(IntKi), PARAMETER      :: Spn4TDxb2 = 161
   INTEGER(IntKi), PARAMETER      :: Spn4TDyb2 = 162
   INTEGER(IntKi), PARAMETER      :: Spn4TDzb2 = 163
   INTEGER(IntKi), PARAMETER      :: Spn5TDxb2 = 164
   INTEGER(IntKi), PARAMETER      :: Spn5TDyb2 = 165
   INTEGER(IntKi), PARAMETER      :: Spn5TDzb2 = 166
   INTEGER(IntKi), PARAMETER      :: Spn6TDxb2 = 167
   INTEGER(IntKi), PARAMETER      :: Spn6TDyb2 = 168
   INTEGER(IntKi), PARAMETER      :: Spn6TDzb2 = 169
   INTEGER(IntKi), PARAMETER      :: Spn7TDxb2 = 170
   INTEGER(IntKi), PARAMETER      :: Spn7TDyb2 = 171
   INTEGER(IntKi), PARAMETER      :: Spn7TDzb2 = 172
   INTEGER(IntKi), PARAMETER      :: Spn8TDxb2 = 173
   INTEGER(IntKi), PARAMETER      :: Spn8TDyb2 = 174
   INTEGER(IntKi), PARAMETER      :: Spn8TDzb2 = 175
   INTEGER(IntKi), PARAMETER      :: Spn9TDxb2 = 176
   INTEGER(IntKi), PARAMETER      :: Spn9TDyb2 = 177
   INTEGER(IntKi), PARAMETER      :: Spn9TDzb2 = 178
   INTEGER(IntKi), PARAMETER      :: Spn1RDxb2 = 179
   INTEGER(IntKi), PARAMETER      :: Spn1RDyb2 = 180
   INTEGER(IntKi), PARAMETER      :: Spn1RDzb2 = 181
   INTEGER(IntKi), PARAMETER      :: Spn2RDxb2 = 182
   INTEGER(IntKi), PARAMETER      :: Spn2RDyb2 = 183
   INTEGER(IntKi), PARAMETER      :: Spn2RDzb2 = 184
   INTEGER(IntKi), PARAMETER      :: Spn3RDxb2 = 185
   INTEGER(IntKi), PARAMETER      :: Spn3RDyb2 = 186
   INTEGER(IntKi), PARAMETER      :: Spn3RDzb2 = 187
   INTEGER(IntKi), PARAMETER      :: Spn4RDxb2 = 188
   INTEGER(IntKi), PARAMETER      :: Spn4RDyb2 = 189
   INTEGER(IntKi), PARAMETER      :: Spn4RDzb2 = 190
   INTEGER(IntKi), PARAMETER      :: Spn5RDxb2 = 191
   INTEGER(IntKi), PARAMETER      :: Spn5RDyb2 = 192
   INTEGER(IntKi), PARAMETER      :: Spn5RDzb2 = 193
   INTEGER(IntKi), PARAMETER      :: Spn6RDxb2 = 194
   INTEGER(IntKi), PARAMETER      :: Spn6RDyb2 = 195
   INTEGER(IntKi), PARAMETER      :: Spn6RDzb2 = 196
   INTEGER(IntKi), PARAMETER      :: Spn7RDxb2 = 197
   INTEGER(IntKi), PARAMETER      :: Spn7RDyb2 = 198
   INTEGER(IntKi), PARAMETER      :: Spn7RDzb2 = 199
   INTEGER(IntKi), PARAMETER      :: Spn8RDxb2 = 200
   INTEGER(IntKi), PARAMETER      :: Spn8RDyb2 = 201
   INTEGER(IntKi), PARAMETER      :: Spn8RDzb2 = 202
   INTEGER(IntKi), PARAMETER      :: Spn9RDxb2 = 203
   INTEGER(IntKi), PARAMETER      :: Spn9RDyb2 = 204
   INTEGER(IntKi), PARAMETER      :: Spn9RDzb2 = 205


  ! Blade 3 Local Span Motions:

   INTEGER(IntKi), PARAMETER      :: Spn1ALxb3 = 206
   INTEGER(IntKi), PARAMETER      :: Spn1ALyb3 = 207
   INTEGER(IntKi), PARAMETER      :: Spn1ALzb3 = 208
   INTEGER(IntKi), PARAMETER      :: Spn2ALxb3 = 209
   INTEGER(IntKi), PARAMETER      :: Spn2ALyb3 = 210
   INTEGER(IntKi), PARAMETER      :: Spn2ALzb3 = 211
   INTEGER(IntKi), PARAMETER      :: Spn3ALxb3 = 212
   INTEGER(IntKi), PARAMETER      :: Spn3ALyb3 = 213
   INTEGER(IntKi), PARAMETER      :: Spn3ALzb3 = 214
   INTEGER(IntKi), PARAMETER      :: Spn4ALxb3 = 215
   INTEGER(IntKi), PARAMETER      :: Spn4ALyb3 = 216
   INTEGER(IntKi), PARAMETER      :: Spn4ALzb3 = 217
   INTEGER(IntKi), PARAMETER      :: Spn5ALxb3 = 218
   INTEGER(IntKi), PARAMETER      :: Spn5ALyb3 = 219
   INTEGER(IntKi), PARAMETER      :: Spn5ALzb3 = 220
   INTEGER(IntKi), PARAMETER      :: Spn6ALxb3 = 221
   INTEGER(IntKi), PARAMETER      :: Spn6ALyb3 = 222
   INTEGER(IntKi), PARAMETER      :: Spn6ALzb3 = 223
   INTEGER(IntKi), PARAMETER      :: Spn7ALxb3 = 224
   INTEGER(IntKi), PARAMETER      :: Spn7ALyb3 = 225
   INTEGER(IntKi), PARAMETER      :: Spn7ALzb3 = 226
   INTEGER(IntKi), PARAMETER      :: Spn8ALxb3 = 227
   INTEGER(IntKi), PARAMETER      :: Spn8ALyb3 = 228
   INTEGER(IntKi), PARAMETER      :: Spn8ALzb3 = 229
   INTEGER(IntKi), PARAMETER      :: Spn9ALxb3 = 230
   INTEGER(IntKi), PARAMETER      :: Spn9ALyb3 = 231
   INTEGER(IntKi), PARAMETER      :: Spn9ALzb3 = 232
   INTEGER(IntKi), PARAMETER      :: Spn1TDxb3 = 233
   INTEGER(IntKi), PARAMETER      :: Spn1TDyb3 = 234
   INTEGER(IntKi), PARAMETER      :: Spn1TDzb3 = 235
   INTEGER(IntKi), PARAMETER      :: Spn2TDxb3 = 236
   INTEGER(IntKi), PARAMETER      :: Spn2TDyb3 = 237
   INTEGER(IntKi), PARAMETER      :: Spn2TDzb3 = 238
   INTEGER(IntKi), PARAMETER      :: Spn3TDxb3 = 239
   INTEGER(IntKi), PARAMETER      :: Spn3TDyb3 = 240
   INTEGER(IntKi), PARAMETER      :: Spn3TDzb3 = 241
   INTEGER(IntKi), PARAMETER      :: Spn4TDxb3 = 242
   INTEGER(IntKi), PARAMETER      :: Spn4TDyb3 = 243
   INTEGER(IntKi), PARAMETER      :: Spn4TDzb3 = 244
   INTEGER(IntKi), PARAMETER      :: Spn5TDxb3 = 245
   INTEGER(IntKi), PARAMETER      :: Spn5TDyb3 = 246
   INTEGER(IntKi), PARAMETER      :: Spn5TDzb3 = 247
   INTEGER(IntKi), PARAMETER      :: Spn6TDxb3 = 248
   INTEGER(IntKi), PARAMETER      :: Spn6TDyb3 = 249
   INTEGER(IntKi), PARAMETER      :: Spn6TDzb3 = 250
   INTEGER(IntKi), PARAMETER      :: Spn7TDxb3 = 251
   INTEGER(IntKi), PARAMETER      :: Spn7TDyb3 = 252
   INTEGER(IntKi), PARAMETER      :: Spn7TDzb3 = 253
   INTEGER(IntKi), PARAMETER      :: Spn8TDxb3 = 254
   INTEGER(IntKi), PARAMETER      :: Spn8TDyb3 = 255
   INTEGER(IntKi), PARAMETER      :: Spn8TDzb3 = 256
   INTEGER(IntKi), PARAMETER      :: Spn9TDxb3 = 257
   INTEGER(IntKi), PARAMETER      :: Spn9TDyb3 = 258
   INTEGER(IntKi), PARAMETER      :: Spn9TDzb3 = 259
   INTEGER(IntKi), PARAMETER      :: Spn1RDxb3 = 260
   INTEGER(IntKi), PARAMETER      :: Spn1RDyb3 = 261
   INTEGER(IntKi), PARAMETER      :: Spn1RDzb3 = 262
   INTEGER(IntKi), PARAMETER      :: Spn2RDxb3 = 263
   INTEGER(IntKi), PARAMETER      :: Spn2RDyb3 = 264
   INTEGER(IntKi), PARAMETER      :: Spn2RDzb3 = 265
   INTEGER(IntKi), PARAMETER      :: Spn3RDxb3 = 266
   INTEGER(IntKi), PARAMETER      :: Spn3RDyb3 = 267
   INTEGER(IntKi), PARAMETER      :: Spn3RDzb3 = 268
   INTEGER(IntKi), PARAMETER      :: Spn4RDxb3 = 269
   INTEGER(IntKi), PARAMETER      :: Spn4RDyb3 = 270
   INTEGER(IntKi), PARAMETER      :: Spn4RDzb3 = 271
   INTEGER(IntKi), PARAMETER      :: Spn5RDxb3 = 272
   INTEGER(IntKi), PARAMETER      :: Spn5RDyb3 = 273
   INTEGER(IntKi), PARAMETER      :: Spn5RDzb3 = 274
   INTEGER(IntKi), PARAMETER      :: Spn6RDxb3 = 275
   INTEGER(IntKi), PARAMETER      :: Spn6RDyb3 = 276
   INTEGER(IntKi), PARAMETER      :: Spn6RDzb3 = 277
   INTEGER(IntKi), PARAMETER      :: Spn7RDxb3 = 278
   INTEGER(IntKi), PARAMETER      :: Spn7RDyb3 = 279
   INTEGER(IntKi), PARAMETER      :: Spn7RDzb3 = 280
   INTEGER(IntKi), PARAMETER      :: Spn8RDxb3 = 281
   INTEGER(IntKi), PARAMETER      :: Spn8RDyb3 = 282
   INTEGER(IntKi), PARAMETER      :: Spn8RDzb3 = 283
   INTEGER(IntKi), PARAMETER      :: Spn9RDxb3 = 284
   INTEGER(IntKi), PARAMETER      :: Spn9RDyb3 = 285
   INTEGER(IntKi), PARAMETER      :: Spn9RDzb3 = 286


  ! Blade Pitch Motions:

   INTEGER(IntKi), PARAMETER      :: PtchPMzc1 = 287
   INTEGER(IntKi), PARAMETER      :: PtchPMzc2 = 288
   INTEGER(IntKi), PARAMETER      :: PtchPMzc3 = 289


  ! Teeter Motions:

   INTEGER(IntKi), PARAMETER      :: TeetPya   = 290
   INTEGER(IntKi), PARAMETER      :: TeetVya   = 291
   INTEGER(IntKi), PARAMETER      :: TeetAya   = 292


  ! Shaft Motions:

   INTEGER(IntKi), PARAMETER      :: LSSTipPxa = 293
   INTEGER(IntKi), PARAMETER      :: LSSTipVxa = 294
   INTEGER(IntKi), PARAMETER      :: LSSTipAxa = 295
   INTEGER(IntKi), PARAMETER      :: LSSGagPxa = 296
   INTEGER(IntKi), PARAMETER      :: LSSGagVxa = 297
   INTEGER(IntKi), PARAMETER      :: LSSGagAxa = 298
   INTEGER(IntKi), PARAMETER      :: HSShftV   = 299
   INTEGER(IntKi), PARAMETER      :: HSShftA   = 300
   INTEGER(IntKi), PARAMETER      :: TipSpdRat = 301


  ! Nacelle IMU Motions:

   INTEGER(IntKi), PARAMETER      :: NcIMUTVxs = 302
   INTEGER(IntKi), PARAMETER      :: NcIMUTVys = 303
   INTEGER(IntKi), PARAMETER      :: NcIMUTVzs = 304
   INTEGER(IntKi), PARAMETER      :: NcIMUTAxs = 305
   INTEGER(IntKi), PARAMETER      :: NcIMUTAys = 306
   INTEGER(IntKi), PARAMETER      :: NcIMUTAzs = 307
   INTEGER(IntKi), PARAMETER      :: NcIMURVxs = 308
   INTEGER(IntKi), PARAMETER      :: NcIMURVys = 309
   INTEGER(IntKi), PARAMETER      :: NcIMURVzs = 310
   INTEGER(IntKi), PARAMETER      :: NcIMURAxs = 311
   INTEGER(IntKi), PARAMETER      :: NcIMURAys = 312
   INTEGER(IntKi), PARAMETER      :: NcIMURAzs = 313


  ! Rotor-Furl Motions:

   INTEGER(IntKi), PARAMETER      :: RotFurlP  = 314
   INTEGER(IntKi), PARAMETER      :: RotFurlV  = 315
   INTEGER(IntKi), PARAMETER      :: RotFurlA  = 316


  ! Tail-Furl Motions:

   INTEGER(IntKi), PARAMETER      :: TailFurlP = 317
   INTEGER(IntKi), PARAMETER      :: TailFurlV = 318
   INTEGER(IntKi), PARAMETER      :: TailFurlA = 319


  ! Nacelle Yaw Motions:

   INTEGER(IntKi), PARAMETER      :: YawPzn    = 320
   INTEGER(IntKi), PARAMETER      :: YawVzn    = 321
   INTEGER(IntKi), PARAMETER      :: YawAzn    = 322
   INTEGER(IntKi), PARAMETER      :: NacYawErr = 323


  ! Tower-Top / Yaw Bearing Motions:

   INTEGER(IntKi), PARAMETER      :: YawBrTDxp = 324
   INTEGER(IntKi), PARAMETER      :: YawBrTDyp = 325
   INTEGER(IntKi), PARAMETER      :: YawBrTDzp = 326
   INTEGER(IntKi), PARAMETER      :: YawBrTDxt = 327
   INTEGER(IntKi), PARAMETER      :: YawBrTDyt = 328
   INTEGER(IntKi), PARAMETER      :: YawBrTDzt = 329
   INTEGER(IntKi), PARAMETER      :: YawBrTAxp = 330
   INTEGER(IntKi), PARAMETER      :: YawBrTAyp = 331
   INTEGER(IntKi), PARAMETER      :: YawBrTAzp = 332
   INTEGER(IntKi), PARAMETER      :: YawBrRDxt = 333
   INTEGER(IntKi), PARAMETER      :: YawBrRDyt = 334
   INTEGER(IntKi), PARAMETER      :: YawBrRDzt = 335
   INTEGER(IntKi), PARAMETER      :: YawBrRVxp = 336
   INTEGER(IntKi), PARAMETER      :: YawBrRVyp = 337
   INTEGER(IntKi), PARAMETER      :: YawBrRVzp = 338
   INTEGER(IntKi), PARAMETER      :: YawBrRAxp = 339
   INTEGER(IntKi), PARAMETER      :: YawBrRAyp = 340
   INTEGER(IntKi), PARAMETER      :: YawBrRAzp = 341


  ! Local Tower Motions:

   INTEGER(IntKi), PARAMETER      :: TwHt1ALxt = 342
   INTEGER(IntKi), PARAMETER      :: TwHt1ALyt = 343
   INTEGER(IntKi), PARAMETER      :: TwHt1ALzt = 344
   INTEGER(IntKi), PARAMETER      :: TwHt2ALxt = 345
   INTEGER(IntKi), PARAMETER      :: TwHt2ALyt = 346
   INTEGER(IntKi), PARAMETER      :: TwHt2ALzt = 347
   INTEGER(IntKi), PARAMETER      :: TwHt3ALxt = 348
   INTEGER(IntKi), PARAMETER      :: TwHt3ALyt = 349
   INTEGER(IntKi), PARAMETER      :: TwHt3ALzt = 350
   INTEGER(IntKi), PARAMETER      :: TwHt4ALxt = 351
   INTEGER(IntKi), PARAMETER      :: TwHt4ALyt = 352
   INTEGER(IntKi), PARAMETER      :: TwHt4ALzt = 353
   INTEGER(IntKi), PARAMETER      :: TwHt5ALxt = 354
   INTEGER(IntKi), PARAMETER      :: TwHt5ALyt = 355
   INTEGER(IntKi), PARAMETER      :: TwHt5ALzt = 356
   INTEGER(IntKi), PARAMETER      :: TwHt6ALxt = 357
   INTEGER(IntKi), PARAMETER      :: TwHt6ALyt = 358
   INTEGER(IntKi), PARAMETER      :: TwHt6ALzt = 359
   INTEGER(IntKi), PARAMETER      :: TwHt7ALxt = 360
   INTEGER(IntKi), PARAMETER      :: TwHt7ALyt = 361
   INTEGER(IntKi), PARAMETER      :: TwHt7ALzt = 362
   INTEGER(IntKi), PARAMETER      :: TwHt8ALxt = 363
   INTEGER(IntKi), PARAMETER      :: TwHt8ALyt = 364
   INTEGER(IntKi), PARAMETER      :: TwHt8ALzt = 365
   INTEGER(IntKi), PARAMETER      :: TwHt9ALxt = 366
   INTEGER(IntKi), PARAMETER      :: TwHt9ALyt = 367
   INTEGER(IntKi), PARAMETER      :: TwHt9ALzt = 368
   INTEGER(IntKi), PARAMETER      :: TwHt1TDxt = 369
   INTEGER(IntKi), PARAMETER      :: TwHt1TDyt = 370
   INTEGER(IntKi), PARAMETER      :: TwHt1TDzt = 371
   INTEGER(IntKi), PARAMETER      :: TwHt2TDxt = 372
   INTEGER(IntKi), PARAMETER      :: TwHt2TDyt = 373
   INTEGER(IntKi), PARAMETER      :: TwHt2TDzt = 374
   INTEGER(IntKi), PARAMETER      :: TwHt3TDxt = 375
   INTEGER(IntKi), PARAMETER      :: TwHt3TDyt = 376
   INTEGER(IntKi), PARAMETER      :: TwHt3TDzt = 377
   INTEGER(IntKi), PARAMETER      :: TwHt4TDxt = 378
   INTEGER(IntKi), PARAMETER      :: TwHt4TDyt = 379
   INTEGER(IntKi), PARAMETER      :: TwHt4TDzt = 380
   INTEGER(IntKi), PARAMETER      :: TwHt5TDxt = 381
   INTEGER(IntKi), PARAMETER      :: TwHt5TDyt = 382
   INTEGER(IntKi), PARAMETER      :: TwHt5TDzt = 383
   INTEGER(IntKi), PARAMETER      :: TwHt6TDxt = 384
   INTEGER(IntKi), PARAMETER      :: TwHt6TDyt = 385
   INTEGER(IntKi), PARAMETER      :: TwHt6TDzt = 386
   INTEGER(IntKi), PARAMETER      :: TwHt7TDxt = 387
   INTEGER(IntKi), PARAMETER      :: TwHt7TDyt = 388
   INTEGER(IntKi), PARAMETER      :: TwHt7TDzt = 389
   INTEGER(IntKi), PARAMETER      :: TwHt8TDxt = 390
   INTEGER(IntKi), PARAMETER      :: TwHt8TDyt = 391
   INTEGER(IntKi), PARAMETER      :: TwHt8TDzt = 392
   INTEGER(IntKi), PARAMETER      :: TwHt9TDxt = 393
   INTEGER(IntKi), PARAMETER      :: TwHt9TDyt = 394
   INTEGER(IntKi), PARAMETER      :: TwHt9TDzt = 395
   INTEGER(IntKi), PARAMETER      :: TwHt1RDxt = 396
   INTEGER(IntKi), PARAMETER      :: TwHt1RDyt = 397
   INTEGER(IntKi), PARAMETER      :: TwHt1RDzt = 398
   INTEGER(IntKi), PARAMETER      :: TwHt2RDxt = 399
   INTEGER(IntKi), PARAMETER      :: TwHt2RDyt = 400
   INTEGER(IntKi), PARAMETER      :: TwHt2RDzt = 401
   INTEGER(IntKi), PARAMETER      :: TwHt3RDxt = 402
   INTEGER(IntKi), PARAMETER      :: TwHt3RDyt = 403
   INTEGER(IntKi), PARAMETER      :: TwHt3RDzt = 404
   INTEGER(IntKi), PARAMETER      :: TwHt4RDxt = 405
   INTEGER(IntKi), PARAMETER      :: TwHt4RDyt = 406
   INTEGER(IntKi), PARAMETER      :: TwHt4RDzt = 407
   INTEGER(IntKi), PARAMETER      :: TwHt5RDxt = 408
   INTEGER(IntKi), PARAMETER      :: TwHt5RDyt = 409
   INTEGER(IntKi), PARAMETER      :: TwHt5RDzt = 410
   INTEGER(IntKi), PARAMETER      :: TwHt6RDxt = 411
   INTEGER(IntKi), PARAMETER      :: TwHt6RDyt = 412
   INTEGER(IntKi), PARAMETER      :: TwHt6RDzt = 413
   INTEGER(IntKi), PARAMETER      :: TwHt7RDxt = 414
   INTEGER(IntKi), PARAMETER      :: TwHt7RDyt = 415
   INTEGER(IntKi), PARAMETER      :: TwHt7RDzt = 416
   INTEGER(IntKi), PARAMETER      :: TwHt8RDxt = 417
   INTEGER(IntKi), PARAMETER      :: TwHt8RDyt = 418
   INTEGER(IntKi), PARAMETER      :: TwHt8RDzt = 419
   INTEGER(IntKi), PARAMETER      :: TwHt9RDxt = 420
   INTEGER(IntKi), PARAMETER      :: TwHt9RDyt = 421
   INTEGER(IntKi), PARAMETER      :: TwHt9RDzt = 422
   INTEGER(IntKi), PARAMETER      :: TwHt1TPxi = 423
   INTEGER(IntKi), PARAMETER      :: TwHt1TPyi = 424
   INTEGER(IntKi), PARAMETER      :: TwHt1TPzi = 425
   INTEGER(IntKi), PARAMETER      :: TwHt2TPxi = 426
   INTEGER(IntKi), PARAMETER      :: TwHt2TPyi = 427
   INTEGER(IntKi), PARAMETER      :: TwHt2TPzi = 428
   INTEGER(IntKi), PARAMETER      :: TwHt3TPxi = 429
   INTEGER(IntKi), PARAMETER      :: TwHt3TPyi = 430
   INTEGER(IntKi), PARAMETER      :: TwHt3TPzi = 431
   INTEGER(IntKi), PARAMETER      :: TwHt4TPxi = 432
   INTEGER(IntKi), PARAMETER      :: TwHt4TPyi = 433
   INTEGER(IntKi), PARAMETER      :: TwHt4TPzi = 434
   INTEGER(IntKi), PARAMETER      :: TwHt5TPxi = 435
   INTEGER(IntKi), PARAMETER      :: TwHt5TPyi = 436
   INTEGER(IntKi), PARAMETER      :: TwHt5TPzi = 437
   INTEGER(IntKi), PARAMETER      :: TwHt6TPxi = 438
   INTEGER(IntKi), PARAMETER      :: TwHt6TPyi = 439
   INTEGER(IntKi), PARAMETER      :: TwHt6TPzi = 440
   INTEGER(IntKi), PARAMETER      :: TwHt7TPxi = 441
   INTEGER(IntKi), PARAMETER      :: TwHt7TPyi = 442
   INTEGER(IntKi), PARAMETER      :: TwHt7TPzi = 443
   INTEGER(IntKi), PARAMETER      :: TwHt8TPxi = 444
   INTEGER(IntKi), PARAMETER      :: TwHt8TPyi = 445
   INTEGER(IntKi), PARAMETER      :: TwHt8TPzi = 446
   INTEGER(IntKi), PARAMETER      :: TwHt9TPxi = 447
   INTEGER(IntKi), PARAMETER      :: TwHt9TPyi = 448
   INTEGER(IntKi), PARAMETER      :: TwHt9TPzi = 449
   INTEGER(IntKi), PARAMETER      :: TwHt1RPxi = 450
   INTEGER(IntKi), PARAMETER      :: TwHt1RPyi = 451
   INTEGER(IntKi), PARAMETER      :: TwHt1RPzi = 452
   INTEGER(IntKi), PARAMETER      :: TwHt2RPxi = 453
   INTEGER(IntKi), PARAMETER      :: TwHt2RPyi = 454
   INTEGER(IntKi), PARAMETER      :: TwHt2RPzi = 455
   INTEGER(IntKi), PARAMETER      :: TwHt3RPxi = 456
   INTEGER(IntKi), PARAMETER      :: TwHt3RPyi = 457
   INTEGER(IntKi), PARAMETER      :: TwHt3RPzi = 458
   INTEGER(IntKi), PARAMETER      :: TwHt4RPxi = 459
   INTEGER(IntKi), PARAMETER      :: TwHt4RPyi = 460
   INTEGER(IntKi), PARAMETER      :: TwHt4RPzi = 461
   INTEGER(IntKi), PARAMETER      :: TwHt5RPxi = 462
   INTEGER(IntKi), PARAMETER      :: TwHt5RPyi = 463
   INTEGER(IntKi), PARAMETER      :: TwHt5RPzi = 464
   INTEGER(IntKi), PARAMETER      :: TwHt6RPxi = 465
   INTEGER(IntKi), PARAMETER      :: TwHt6RPyi = 466
   INTEGER(IntKi), PARAMETER      :: TwHt6RPzi = 467
   INTEGER(IntKi), PARAMETER      :: TwHt7RPxi = 468
   INTEGER(IntKi), PARAMETER      :: TwHt7RPyi = 469
   INTEGER(IntKi), PARAMETER      :: TwHt7RPzi = 470
   INTEGER(IntKi), PARAMETER      :: TwHt8RPxi = 471
   INTEGER(IntKi), PARAMETER      :: TwHt8RPyi = 472
   INTEGER(IntKi), PARAMETER      :: TwHt8RPzi = 473
   INTEGER(IntKi), PARAMETER      :: TwHt9RPxi = 474
   INTEGER(IntKi), PARAMETER      :: TwHt9RPyi = 475
   INTEGER(IntKi), PARAMETER      :: TwHt9RPzi = 476


  ! Platform Motions:

   INTEGER(IntKi), PARAMETER      :: PtfmTDxt  = 477
   INTEGER(IntKi), PARAMETER      :: PtfmTDyt  = 478
   INTEGER(IntKi), PARAMETER      :: PtfmTDzt  = 479
   INTEGER(IntKi), PARAMETER      :: PtfmTDxi  = 480
   INTEGER(IntKi), PARAMETER      :: PtfmTDyi  = 481
   INTEGER(IntKi), PARAMETER      :: PtfmTDzi  = 482
   INTEGER(IntKi), PARAMETER      :: PtfmTVxt  = 483
   INTEGER(IntKi), PARAMETER      :: PtfmTVyt  = 484
   INTEGER(IntKi), PARAMETER      :: PtfmTVzt  = 485
   INTEGER(IntKi), PARAMETER      :: PtfmTVxi  = 486
   INTEGER(IntKi), PARAMETER      :: PtfmTVyi  = 487
   INTEGER(IntKi), PARAMETER      :: PtfmTVzi  = 488
   INTEGER(IntKi), PARAMETER      :: PtfmTAxt  = 489
   INTEGER(IntKi), PARAMETER      :: PtfmTAyt  = 490
   INTEGER(IntKi), PARAMETER      :: PtfmTAzt  = 491
   INTEGER(IntKi), PARAMETER      :: PtfmTAxi  = 492
   INTEGER(IntKi), PARAMETER      :: PtfmTAyi  = 493
   INTEGER(IntKi), PARAMETER      :: PtfmTAzi  = 494
   INTEGER(IntKi), PARAMETER      :: PtfmRDxi  = 495
   INTEGER(IntKi), PARAMETER      :: PtfmRDyi  = 496
   INTEGER(IntKi), PARAMETER      :: PtfmRDzi  = 497
   INTEGER(IntKi), PARAMETER      :: PtfmRVxt  = 498
   INTEGER(IntKi), PARAMETER      :: PtfmRVyt  = 499
   INTEGER(IntKi), PARAMETER      :: PtfmRVzt  = 500
   INTEGER(IntKi), PARAMETER      :: PtfmRVxi  = 501
   INTEGER(IntKi), PARAMETER      :: PtfmRVyi  = 502
   INTEGER(IntKi), PARAMETER      :: PtfmRVzi  = 503
   INTEGER(IntKi), PARAMETER      :: PtfmRAxt  = 504
   INTEGER(IntKi), PARAMETER      :: PtfmRAyt  = 505
   INTEGER(IntKi), PARAMETER      :: PtfmRAzt  = 506
   INTEGER(IntKi), PARAMETER      :: PtfmRAxi  = 507
   INTEGER(IntKi), PARAMETER      :: PtfmRAyi  = 508
   INTEGER(IntKi), PARAMETER      :: PtfmRAzi  = 509


  ! Blade 1 Root Loads:

   INTEGER(IntKi), PARAMETER      :: RootFxc1  = 510
   INTEGER(IntKi), PARAMETER      :: RootFyc1  = 511
   INTEGER(IntKi), PARAMETER      :: RootFzc1  = 512
   INTEGER(IntKi), PARAMETER      :: RootFxb1  = 513
   INTEGER(IntKi), PARAMETER      :: RootFyb1  = 514
   INTEGER(IntKi), PARAMETER      :: RootMxc1  = 515
   INTEGER(IntKi), PARAMETER      :: RootMyc1  = 516
   INTEGER(IntKi), PARAMETER      :: RootMzc1  = 517
   INTEGER(IntKi), PARAMETER      :: RootMxb1  = 518
   INTEGER(IntKi), PARAMETER      :: RootMyb1  = 519


  ! Blade 2 Root Loads:

   INTEGER(IntKi), PARAMETER      :: RootFxc2  = 520
   INTEGER(IntKi), PARAMETER      :: RootFyc2  = 521
   INTEGER(IntKi), PARAMETER      :: RootFzc2  = 522
   INTEGER(IntKi), PARAMETER      :: RootFxb2  = 523
   INTEGER(IntKi), PARAMETER      :: RootFyb2  = 524
   INTEGER(IntKi), PARAMETER      :: RootMxc2  = 525
   INTEGER(IntKi), PARAMETER      :: RootMyc2  = 526
   INTEGER(IntKi), PARAMETER      :: RootMzc2  = 527
   INTEGER(IntKi), PARAMETER      :: RootMxb2  = 528
   INTEGER(IntKi), PARAMETER      :: RootMyb2  = 529


  ! Blade 3 Root Loads:

   INTEGER(IntKi), PARAMETER      :: RootFxc3  = 530
   INTEGER(IntKi), PARAMETER      :: RootFyc3  = 531
   INTEGER(IntKi), PARAMETER      :: RootFzc3  = 532
   INTEGER(IntKi), PARAMETER      :: RootFxb3  = 533
   INTEGER(IntKi), PARAMETER      :: RootFyb3  = 534
   INTEGER(IntKi), PARAMETER      :: RootMxc3  = 535
   INTEGER(IntKi), PARAMETER      :: RootMyc3  = 536
   INTEGER(IntKi), PARAMETER      :: RootMzc3  = 537
   INTEGER(IntKi), PARAMETER      :: RootMxb3  = 538
   INTEGER(IntKi), PARAMETER      :: RootMyb3  = 539


  ! Blade 1 Local Span Loads:

   INTEGER(IntKi), PARAMETER      :: Spn1MLxb1 = 540
   INTEGER(IntKi), PARAMETER      :: Spn1MLyb1 = 541
   INTEGER(IntKi), PARAMETER      :: Spn1MLzb1 = 542
   INTEGER(IntKi), PARAMETER      :: Spn2MLxb1 = 543
   INTEGER(IntKi), PARAMETER      :: Spn2MLyb1 = 544
   INTEGER(IntKi), PARAMETER      :: Spn2MLzb1 = 545
   INTEGER(IntKi), PARAMETER      :: Spn3MLxb1 = 546
   INTEGER(IntKi), PARAMETER      :: Spn3MLyb1 = 547
   INTEGER(IntKi), PARAMETER      :: Spn3MLzb1 = 548
   INTEGER(IntKi), PARAMETER      :: Spn4MLxb1 = 549
   INTEGER(IntKi), PARAMETER      :: Spn4MLyb1 = 550
   INTEGER(IntKi), PARAMETER      :: Spn4MLzb1 = 551
   INTEGER(IntKi), PARAMETER      :: Spn5MLxb1 = 552
   INTEGER(IntKi), PARAMETER      :: Spn5MLyb1 = 553
   INTEGER(IntKi), PARAMETER      :: Spn5MLzb1 = 554
   INTEGER(IntKi), PARAMETER      :: Spn6MLxb1 = 555
   INTEGER(IntKi), PARAMETER      :: Spn6MLyb1 = 556
   INTEGER(IntKi), PARAMETER      :: Spn6MLzb1 = 557
   INTEGER(IntKi), PARAMETER      :: Spn7MLxb1 = 558
   INTEGER(IntKi), PARAMETER      :: Spn7MLyb1 = 559
   INTEGER(IntKi), PARAMETER      :: Spn7MLzb1 = 560
   INTEGER(IntKi), PARAMETER      :: Spn8MLxb1 = 561
   INTEGER(IntKi), PARAMETER      :: Spn8MLyb1 = 562
   INTEGER(IntKi), PARAMETER      :: Spn8MLzb1 = 563
   INTEGER(IntKi), PARAMETER      :: Spn9MLxb1 = 564
   INTEGER(IntKi), PARAMETER      :: Spn9MLyb1 = 565
   INTEGER(IntKi), PARAMETER      :: Spn9MLzb1 = 566
   INTEGER(IntKi), PARAMETER      :: Spn1FLxb1 = 567
   INTEGER(IntKi), PARAMETER      :: Spn1FLyb1 = 568
   INTEGER(IntKi), PARAMETER      :: Spn1FLzb1 = 569
   INTEGER(IntKi), PARAMETER      :: Spn2FLxb1 = 570
   INTEGER(IntKi), PARAMETER      :: Spn2FLyb1 = 571
   INTEGER(IntKi), PARAMETER      :: Spn2FLzb1 = 572
   INTEGER(IntKi), PARAMETER      :: Spn3FLxb1 = 573
   INTEGER(IntKi), PARAMETER      :: Spn3FLyb1 = 574
   INTEGER(IntKi), PARAMETER      :: Spn3FLzb1 = 575
   INTEGER(IntKi), PARAMETER      :: Spn4FLxb1 = 576
   INTEGER(IntKi), PARAMETER      :: Spn4FLyb1 = 577
   INTEGER(IntKi), PARAMETER      :: Spn4FLzb1 = 578
   INTEGER(IntKi), PARAMETER      :: Spn5FLxb1 = 579
   INTEGER(IntKi), PARAMETER      :: Spn5FLyb1 = 580
   INTEGER(IntKi), PARAMETER      :: Spn5FLzb1 = 581
   INTEGER(IntKi), PARAMETER      :: Spn6FLxb1 = 582
   INTEGER(IntKi), PARAMETER      :: Spn6FLyb1 = 583
   INTEGER(IntKi), PARAMETER      :: Spn6FLzb1 = 584
   INTEGER(IntKi), PARAMETER      :: Spn7FLxb1 = 585
   INTEGER(IntKi), PARAMETER      :: Spn7FLyb1 = 586
   INTEGER(IntKi), PARAMETER      :: Spn7FLzb1 = 587
   INTEGER(IntKi), PARAMETER      :: Spn8FLxb1 = 588
   INTEGER(IntKi), PARAMETER      :: Spn8FLyb1 = 589
   INTEGER(IntKi), PARAMETER      :: Spn8FLzb1 = 590
   INTEGER(IntKi), PARAMETER      :: Spn9FLxb1 = 591
   INTEGER(IntKi), PARAMETER      :: Spn9FLyb1 = 592
   INTEGER(IntKi), PARAMETER      :: Spn9FLzb1 = 593


  ! Blade 2 Local Span Loads:

   INTEGER(IntKi), PARAMETER      :: Spn1MLxb2 = 594
   INTEGER(IntKi), PARAMETER      :: Spn1MLyb2 = 595
   INTEGER(IntKi), PARAMETER      :: Spn1MLzb2 = 596
   INTEGER(IntKi), PARAMETER      :: Spn2MLxb2 = 597
   INTEGER(IntKi), PARAMETER      :: Spn2MLyb2 = 598
   INTEGER(IntKi), PARAMETER      :: Spn2MLzb2 = 599
   INTEGER(IntKi), PARAMETER      :: Spn3MLxb2 = 600
   INTEGER(IntKi), PARAMETER      :: Spn3MLyb2 = 601
   INTEGER(IntKi), PARAMETER      :: Spn3MLzb2 = 602
   INTEGER(IntKi), PARAMETER      :: Spn4MLxb2 = 603
   INTEGER(IntKi), PARAMETER      :: Spn4MLyb2 = 604
   INTEGER(IntKi), PARAMETER      :: Spn4MLzb2 = 605
   INTEGER(IntKi), PARAMETER      :: Spn5MLxb2 = 606
   INTEGER(IntKi), PARAMETER      :: Spn5MLyb2 = 607
   INTEGER(IntKi), PARAMETER      :: Spn5MLzb2 = 608
   INTEGER(IntKi), PARAMETER      :: Spn6MLxb2 = 609
   INTEGER(IntKi), PARAMETER      :: Spn6MLyb2 = 610
   INTEGER(IntKi), PARAMETER      :: Spn6MLzb2 = 611
   INTEGER(IntKi), PARAMETER      :: Spn7MLxb2 = 612
   INTEGER(IntKi), PARAMETER      :: Spn7MLyb2 = 613
   INTEGER(IntKi), PARAMETER      :: Spn7MLzb2 = 614
   INTEGER(IntKi), PARAMETER      :: Spn8MLxb2 = 615
   INTEGER(IntKi), PARAMETER      :: Spn8MLyb2 = 616
   INTEGER(IntKi), PARAMETER      :: Spn8MLzb2 = 617
   INTEGER(IntKi), PARAMETER      :: Spn9MLxb2 = 618
   INTEGER(IntKi), PARAMETER      :: Spn9MLyb2 = 619
   INTEGER(IntKi), PARAMETER      :: Spn9MLzb2 = 620
   INTEGER(IntKi), PARAMETER      :: Spn1FLxb2 = 621
   INTEGER(IntKi), PARAMETER      :: Spn1FLyb2 = 622
   INTEGER(IntKi), PARAMETER      :: Spn1FLzb2 = 623
   INTEGER(IntKi), PARAMETER      :: Spn2FLxb2 = 624
   INTEGER(IntKi), PARAMETER      :: Spn2FLyb2 = 625
   INTEGER(IntKi), PARAMETER      :: Spn2FLzb2 = 626
   INTEGER(IntKi), PARAMETER      :: Spn3FLxb2 = 627
   INTEGER(IntKi), PARAMETER      :: Spn3FLyb2 = 628
   INTEGER(IntKi), PARAMETER      :: Spn3FLzb2 = 629
   INTEGER(IntKi), PARAMETER      :: Spn4FLxb2 = 630
   INTEGER(IntKi), PARAMETER      :: Spn4FLyb2 = 631
   INTEGER(IntKi), PARAMETER      :: Spn4FLzb2 = 632
   INTEGER(IntKi), PARAMETER      :: Spn5FLxb2 = 633
   INTEGER(IntKi), PARAMETER      :: Spn5FLyb2 = 634
   INTEGER(IntKi), PARAMETER      :: Spn5FLzb2 = 635
   INTEGER(IntKi), PARAMETER      :: Spn6FLxb2 = 636
   INTEGER(IntKi), PARAMETER      :: Spn6FLyb2 = 637
   INTEGER(IntKi), PARAMETER      :: Spn6FLzb2 = 638
   INTEGER(IntKi), PARAMETER      :: Spn7FLxb2 = 639
   INTEGER(IntKi), PARAMETER      :: Spn7FLyb2 = 640
   INTEGER(IntKi), PARAMETER      :: Spn7FLzb2 = 641
   INTEGER(IntKi), PARAMETER      :: Spn8FLxb2 = 642
   INTEGER(IntKi), PARAMETER      :: Spn8FLyb2 = 643
   INTEGER(IntKi), PARAMETER      :: Spn8FLzb2 = 644
   INTEGER(IntKi), PARAMETER      :: Spn9FLxb2 = 645
   INTEGER(IntKi), PARAMETER      :: Spn9FLyb2 = 646
   INTEGER(IntKi), PARAMETER      :: Spn9FLzb2 = 647


  ! Blade 3 Local Span Loads:

   INTEGER(IntKi), PARAMETER      :: Spn1MLxb3 = 648
   INTEGER(IntKi), PARAMETER      :: Spn1MLyb3 = 649
   INTEGER(IntKi), PARAMETER      :: Spn1MLzb3 = 650
   INTEGER(IntKi), PARAMETER      :: Spn2MLxb3 = 651
   INTEGER(IntKi), PARAMETER      :: Spn2MLyb3 = 652
   INTEGER(IntKi), PARAMETER      :: Spn2MLzb3 = 653
   INTEGER(IntKi), PARAMETER      :: Spn3MLxb3 = 654
   INTEGER(IntKi), PARAMETER      :: Spn3MLyb3 = 655
   INTEGER(IntKi), PARAMETER      :: Spn3MLzb3 = 656
   INTEGER(IntKi), PARAMETER      :: Spn4MLxb3 = 657
   INTEGER(IntKi), PARAMETER      :: Spn4MLyb3 = 658
   INTEGER(IntKi), PARAMETER      :: Spn4MLzb3 = 659
   INTEGER(IntKi), PARAMETER      :: Spn5MLxb3 = 660
   INTEGER(IntKi), PARAMETER      :: Spn5MLyb3 = 661
   INTEGER(IntKi), PARAMETER      :: Spn5MLzb3 = 662
   INTEGER(IntKi), PARAMETER      :: Spn6MLxb3 = 663
   INTEGER(IntKi), PARAMETER      :: Spn6MLyb3 = 664
   INTEGER(IntKi), PARAMETER      :: Spn6MLzb3 = 665
   INTEGER(IntKi), PARAMETER      :: Spn7MLxb3 = 666
   INTEGER(IntKi), PARAMETER      :: Spn7MLyb3 = 667
   INTEGER(IntKi), PARAMETER      :: Spn7MLzb3 = 668
   INTEGER(IntKi), PARAMETER      :: Spn8MLxb3 = 669
   INTEGER(IntKi), PARAMETER      :: Spn8MLyb3 = 670
   INTEGER(IntKi), PARAMETER      :: Spn8MLzb3 = 671
   INTEGER(IntKi), PARAMETER      :: Spn9MLxb3 = 672
   INTEGER(IntKi), PARAMETER      :: Spn9MLyb3 = 673
   INTEGER(IntKi), PARAMETER      :: Spn9MLzb3 = 674
   INTEGER(IntKi), PARAMETER      :: Spn1FLxb3 = 675
   INTEGER(IntKi), PARAMETER      :: Spn1FLyb3 = 676
   INTEGER(IntKi), PARAMETER      :: Spn1FLzb3 = 677
   INTEGER(IntKi), PARAMETER      :: Spn2FLxb3 = 678
   INTEGER(IntKi), PARAMETER      :: Spn2FLyb3 = 679
   INTEGER(IntKi), PARAMETER      :: Spn2FLzb3 = 680
   INTEGER(IntKi), PARAMETER      :: Spn3FLxb3 = 681
   INTEGER(IntKi), PARAMETER      :: Spn3FLyb3 = 682
   INTEGER(IntKi), PARAMETER      :: Spn3FLzb3 = 683
   INTEGER(IntKi), PARAMETER      :: Spn4FLxb3 = 684
   INTEGER(IntKi), PARAMETER      :: Spn4FLyb3 = 685
   INTEGER(IntKi), PARAMETER      :: Spn4FLzb3 = 686
   INTEGER(IntKi), PARAMETER      :: Spn5FLxb3 = 687
   INTEGER(IntKi), PARAMETER      :: Spn5FLyb3 = 688
   INTEGER(IntKi), PARAMETER      :: Spn5FLzb3 = 689
   INTEGER(IntKi), PARAMETER      :: Spn6FLxb3 = 690
   INTEGER(IntKi), PARAMETER      :: Spn6FLyb3 = 691
   INTEGER(IntKi), PARAMETER      :: Spn6FLzb3 = 692
   INTEGER(IntKi), PARAMETER      :: Spn7FLxb3 = 693
   INTEGER(IntKi), PARAMETER      :: Spn7FLyb3 = 694
   INTEGER(IntKi), PARAMETER      :: Spn7FLzb3 = 695
   INTEGER(IntKi), PARAMETER      :: Spn8FLxb3 = 696
   INTEGER(IntKi), PARAMETER      :: Spn8FLyb3 = 697
   INTEGER(IntKi), PARAMETER      :: Spn8FLzb3 = 698
   INTEGER(IntKi), PARAMETER      :: Spn9FLxb3 = 699
   INTEGER(IntKi), PARAMETER      :: Spn9FLyb3 = 700
   INTEGER(IntKi), PARAMETER      :: Spn9FLzb3 = 701


  ! Hub and Rotor Loads:

   INTEGER(IntKi), PARAMETER      :: LSShftFxa = 702
   INTEGER(IntKi), PARAMETER      :: LSShftFya = 703
   INTEGER(IntKi), PARAMETER      :: LSShftFza = 704
   INTEGER(IntKi), PARAMETER      :: LSShftFys = 705
   INTEGER(IntKi), PARAMETER      :: LSShftFzs = 706
   INTEGER(IntKi), PARAMETER      :: LSShftMxa = 707
   INTEGER(IntKi), PARAMETER      :: LSSTipMya = 708
   INTEGER(IntKi), PARAMETER      :: LSSTipMza = 709
   INTEGER(IntKi), PARAMETER      :: LSSTipMys = 710
   INTEGER(IntKi), PARAMETER      :: LSSTipMzs = 711
   INTEGER(IntKi), PARAMETER      :: CThrstAzm = 712
   INTEGER(IntKi), PARAMETER      :: CThrstRad = 713
   INTEGER(IntKi), PARAMETER      :: RotPwr    = 714
   INTEGER(IntKi), PARAMETER      :: RotCq     = 715
   INTEGER(IntKi), PARAMETER      :: RotCp     = 716
   INTEGER(IntKi), PARAMETER      :: RotCt     = 717


  ! Shaft Strain Gage Loads:

   INTEGER(IntKi), PARAMETER      :: LSSGagMya = 718
   INTEGER(IntKi), PARAMETER      :: LSSGagMza = 719
   INTEGER(IntKi), PARAMETER      :: LSSGagMys = 720
   INTEGER(IntKi), PARAMETER      :: LSSGagMzs = 721


  ! Generator and High-Speed Shaft Loads:

   INTEGER(IntKi), PARAMETER      :: HSShftTq  = 722
   INTEGER(IntKi), PARAMETER      :: HSShftPwr = 723
   INTEGER(IntKi), PARAMETER      :: HSShftCq  = 724
   INTEGER(IntKi), PARAMETER      :: HSShftCp  = 725
   INTEGER(IntKi), PARAMETER      :: GenTq     = 726
   INTEGER(IntKi), PARAMETER      :: GenPwr    = 727
   INTEGER(IntKi), PARAMETER      :: GenCq     = 728
   INTEGER(IntKi), PARAMETER      :: GenCp     = 729
   INTEGER(IntKi), PARAMETER      :: HSSBrTq   = 730


  ! Rotor-Furl Bearing Loads:

   INTEGER(IntKi), PARAMETER      :: RFrlBrM   = 731


  ! Tail-Furl Bearing Loads:

   INTEGER(IntKi), PARAMETER      :: TFrlBrM   = 732


  ! Tail Fin Aerodynamic Loads:

   INTEGER(IntKi), PARAMETER      :: TFinAlpha = 733
   INTEGER(IntKi), PARAMETER      :: TFinCLift = 734
   INTEGER(IntKi), PARAMETER      :: TFinCDrag = 735
   INTEGER(IntKi), PARAMETER      :: TFinDnPrs = 736
   INTEGER(IntKi), PARAMETER      :: TFinCPFx  = 737
   INTEGER(IntKi), PARAMETER      :: TFinCPFy  = 738


  ! Tower-Top / Yaw Bearing Loads:

   INTEGER(IntKi), PARAMETER      :: YawBrFxn  = 739
   INTEGER(IntKi), PARAMETER      :: YawBrFyn  = 740
   INTEGER(IntKi), PARAMETER      :: YawBrFzn  = 741
   INTEGER(IntKi), PARAMETER      :: YawBrFxp  = 742
   INTEGER(IntKi), PARAMETER      :: YawBrFyp  = 743
   INTEGER(IntKi), PARAMETER      :: YawBrMxn  = 744
   INTEGER(IntKi), PARAMETER      :: YawBrMyn  = 745
   INTEGER(IntKi), PARAMETER      :: YawBrMzn  = 746
   INTEGER(IntKi), PARAMETER      :: YawBrMxp  = 747
   INTEGER(IntKi), PARAMETER      :: YawBrMyp  = 748


  ! Tower Base Loads:

   INTEGER(IntKi), PARAMETER      :: TwrBsFxt  = 749
   INTEGER(IntKi), PARAMETER      :: TwrBsFyt  = 750
   INTEGER(IntKi), PARAMETER      :: TwrBsFzt  = 751
   INTEGER(IntKi), PARAMETER      :: TwrBsMxt  = 752
   INTEGER(IntKi), PARAMETER      :: TwrBsMyt  = 753
   INTEGER(IntKi), PARAMETER      :: TwrBsMzt  = 754


  ! Local Tower Loads:

   INTEGER(IntKi), PARAMETER      :: TwHt1MLxt = 755
   INTEGER(IntKi), PARAMETER      :: TwHt1MLyt = 756
   INTEGER(IntKi), PARAMETER      :: TwHt1MLzt = 757
   INTEGER(IntKi), PARAMETER      :: TwHt2MLxt = 758
   INTEGER(IntKi), PARAMETER      :: TwHt2MLyt = 759
   INTEGER(IntKi), PARAMETER      :: TwHt2MLzt = 760
   INTEGER(IntKi), PARAMETER      :: TwHt3MLxt = 761
   INTEGER(IntKi), PARAMETER      :: TwHt3MLyt = 762
   INTEGER(IntKi), PARAMETER      :: TwHt3MLzt = 763
   INTEGER(IntKi), PARAMETER      :: TwHt4MLxt = 764
   INTEGER(IntKi), PARAMETER      :: TwHt4MLyt = 765
   INTEGER(IntKi), PARAMETER      :: TwHt4MLzt = 766
   INTEGER(IntKi), PARAMETER      :: TwHt5MLxt = 767
   INTEGER(IntKi), PARAMETER      :: TwHt5MLyt = 768
   INTEGER(IntKi), PARAMETER      :: TwHt5MLzt = 769
   INTEGER(IntKi), PARAMETER      :: TwHt6MLxt = 770
   INTEGER(IntKi), PARAMETER      :: TwHt6MLyt = 771
   INTEGER(IntKi), PARAMETER      :: TwHt6MLzt = 772
   INTEGER(IntKi), PARAMETER      :: TwHt7MLxt = 773
   INTEGER(IntKi), PARAMETER      :: TwHt7MLyt = 774
   INTEGER(IntKi), PARAMETER      :: TwHt7MLzt = 775
   INTEGER(IntKi), PARAMETER      :: TwHt8MLxt = 776
   INTEGER(IntKi), PARAMETER      :: TwHt8MLyt = 777
   INTEGER(IntKi), PARAMETER      :: TwHt8MLzt = 778
   INTEGER(IntKi), PARAMETER      :: TwHt9MLxt = 779
   INTEGER(IntKi), PARAMETER      :: TwHt9MLyt = 780
   INTEGER(IntKi), PARAMETER      :: TwHt9MLzt = 781
   INTEGER(IntKi), PARAMETER      :: TwHt1FLxt = 782
   INTEGER(IntKi), PARAMETER      :: TwHt1FLyt = 783
   INTEGER(IntKi), PARAMETER      :: TwHt1FLzt = 784
   INTEGER(IntKi), PARAMETER      :: TwHt2FLxt = 785
   INTEGER(IntKi), PARAMETER      :: TwHt2FLyt = 786
   INTEGER(IntKi), PARAMETER      :: TwHt2FLzt = 787
   INTEGER(IntKi), PARAMETER      :: TwHt3FLxt = 788
   INTEGER(IntKi), PARAMETER      :: TwHt3FLyt = 789
   INTEGER(IntKi), PARAMETER      :: TwHt3FLzt = 790
   INTEGER(IntKi), PARAMETER      :: TwHt4FLxt = 791
   INTEGER(IntKi), PARAMETER      :: TwHt4FLyt = 792
   INTEGER(IntKi), PARAMETER      :: TwHt4FLzt = 793
   INTEGER(IntKi), PARAMETER      :: TwHt5FLxt = 794
   INTEGER(IntKi), PARAMETER      :: TwHt5FLyt = 795
   INTEGER(IntKi), PARAMETER      :: TwHt5FLzt = 796
   INTEGER(IntKi), PARAMETER      :: TwHt6FLxt = 797
   INTEGER(IntKi), PARAMETER      :: TwHt6FLyt = 798
   INTEGER(IntKi), PARAMETER      :: TwHt6FLzt = 799
   INTEGER(IntKi), PARAMETER      :: TwHt7FLxt = 800
   INTEGER(IntKi), PARAMETER      :: TwHt7FLyt = 801
   INTEGER(IntKi), PARAMETER      :: TwHt7FLzt = 802
   INTEGER(IntKi), PARAMETER      :: TwHt8FLxt = 803
   INTEGER(IntKi), PARAMETER      :: TwHt8FLyt = 804
   INTEGER(IntKi), PARAMETER      :: TwHt8FLzt = 805
   INTEGER(IntKi), PARAMETER      :: TwHt9FLxt = 806
   INTEGER(IntKi), PARAMETER      :: TwHt9FLyt = 807
   INTEGER(IntKi), PARAMETER      :: TwHt9FLzt = 808


  ! Platform Loads:

   INTEGER(IntKi), PARAMETER      :: PtfmFxt   = 809
   INTEGER(IntKi), PARAMETER      :: PtfmFyt   = 810
   INTEGER(IntKi), PARAMETER      :: PtfmFzt   = 811
   INTEGER(IntKi), PARAMETER      :: PtfmFxi   = 812
   INTEGER(IntKi), PARAMETER      :: PtfmFyi   = 813
   INTEGER(IntKi), PARAMETER      :: PtfmFzi   = 814
   INTEGER(IntKi), PARAMETER      :: PtfmMxt   = 815
   INTEGER(IntKi), PARAMETER      :: PtfmMyt   = 816
   INTEGER(IntKi), PARAMETER      :: PtfmMzt   = 817
   INTEGER(IntKi), PARAMETER      :: PtfmMxi   = 818
   INTEGER(IntKi), PARAMETER      :: PtfmMyi   = 819
   INTEGER(IntKi), PARAMETER      :: PtfmMzi   = 820


  ! Internal Degrees of Freedom:

   INTEGER(IntKi), PARAMETER      :: Q_B1E1    = 821
   INTEGER(IntKi), PARAMETER      :: Q_B2E1    = 822
   INTEGER(IntKi), PARAMETER      :: Q_B3E1    = 823
   INTEGER(IntKi), PARAMETER      :: Q_B1F1    = 824
   INTEGER(IntKi), PARAMETER      :: Q_B2F1    = 825
   INTEGER(IntKi), PARAMETER      :: Q_B3F1    = 826
   INTEGER(IntKi), PARAMETER      :: Q_B1F2    = 827
   INTEGER(IntKi), PARAMETER      :: Q_B2F2    = 828
   INTEGER(IntKi), PARAMETER      :: Q_B3F2    = 829
   INTEGER(IntKi), PARAMETER      :: Q_Teet    = 830
   INTEGER(IntKi), PARAMETER      :: Q_DrTr    = 831
   INTEGER(IntKi), PARAMETER      :: Q_GeAz    = 832
   INTEGER(IntKi), PARAMETER      :: Q_RFrl    = 833
   INTEGER(IntKi), PARAMETER      :: Q_TFrl    = 834
   INTEGER(IntKi), PARAMETER      :: Q_Yaw     = 835
   INTEGER(IntKi), PARAMETER      :: Q_TFA1    = 836
   INTEGER(IntKi), PARAMETER      :: Q_TSS1    = 837
   INTEGER(IntKi), PARAMETER      :: Q_TFA2    = 838
   INTEGER(IntKi), PARAMETER      :: Q_TSS2    = 839
   INTEGER(IntKi), PARAMETER      :: Q_Sg      = 840
   INTEGER(IntKi), PARAMETER      :: Q_Sw      = 841
   INTEGER(IntKi), PARAMETER      :: Q_Hv      = 842
   INTEGER(IntKi), PARAMETER      :: Q_R       = 843
   INTEGER(IntKi), PARAMETER      :: Q_P       = 844
   INTEGER(IntKi), PARAMETER      :: Q_Y       = 845
   INTEGER(IntKi), PARAMETER      :: QD_B1E1   = 846
   INTEGER(IntKi), PARAMETER      :: QD_B2E1   = 847
   INTEGER(IntKi), PARAMETER      :: QD_B3E1   = 848
   INTEGER(IntKi), PARAMETER      :: QD_B1F1   = 849
   INTEGER(IntKi), PARAMETER      :: QD_B2F1   = 850
   INTEGER(IntKi), PARAMETER      :: QD_B3F1   = 851
   INTEGER(IntKi), PARAMETER      :: QD_B1F2   = 852
   INTEGER(IntKi), PARAMETER      :: QD_B2F2   = 853
   INTEGER(IntKi), PARAMETER      :: QD_B3F2   = 854
   INTEGER(IntKi), PARAMETER      :: QD_Teet   = 855
   INTEGER(IntKi), PARAMETER      :: QD_DrTr   = 856
   INTEGER(IntKi), PARAMETER      :: QD_GeAz   = 857
   INTEGER(IntKi), PARAMETER      :: QD_RFrl   = 858
   INTEGER(IntKi), PARAMETER      :: QD_TFrl   = 859
   INTEGER(IntKi), PARAMETER      :: QD_Yaw    = 860
   INTEGER(IntKi), PARAMETER      :: QD_TFA1   = 861
   INTEGER(IntKi), PARAMETER      :: QD_TSS1   = 862
   INTEGER(IntKi), PARAMETER      :: QD_TFA2   = 863
   INTEGER(IntKi), PARAMETER      :: QD_TSS2   = 864
   INTEGER(IntKi), PARAMETER      :: QD_Sg     = 865
   INTEGER(IntKi), PARAMETER      :: QD_Sw     = 866
   INTEGER(IntKi), PARAMETER      :: QD_Hv     = 867
   INTEGER(IntKi), PARAMETER      :: QD_R      = 868
   INTEGER(IntKi), PARAMETER      :: QD_P      = 869
   INTEGER(IntKi), PARAMETER      :: QD_Y      = 870
   INTEGER(IntKi), PARAMETER      :: QD2_B1E1  = 871
   INTEGER(IntKi), PARAMETER      :: QD2_B2E1  = 872
   INTEGER(IntKi), PARAMETER      :: QD2_B3E1  = 873
   INTEGER(IntKi), PARAMETER      :: QD2_B1F1  = 874
   INTEGER(IntKi), PARAMETER      :: QD2_B2F1  = 875
   INTEGER(IntKi), PARAMETER      :: QD2_B3F1  = 876
   INTEGER(IntKi), PARAMETER      :: QD2_B1F2  = 877
   INTEGER(IntKi), PARAMETER      :: QD2_B2F2  = 878
   INTEGER(IntKi), PARAMETER      :: QD2_B3F2  = 879
   INTEGER(IntKi), PARAMETER      :: QD2_Teet  = 880
   INTEGER(IntKi), PARAMETER      :: QD2_DrTr  = 881
   INTEGER(IntKi), PARAMETER      :: QD2_GeAz  = 882
   INTEGER(IntKi), PARAMETER      :: QD2_RFrl  = 883
   INTEGER(IntKi), PARAMETER      :: QD2_TFrl  = 884
   INTEGER(IntKi), PARAMETER      :: QD2_Yaw   = 885
   INTEGER(IntKi), PARAMETER      :: QD2_TFA1  = 886
   INTEGER(IntKi), PARAMETER      :: QD2_TSS1  = 887
   INTEGER(IntKi), PARAMETER      :: QD2_TFA2  = 888
   INTEGER(IntKi), PARAMETER      :: QD2_TSS2  = 889
   INTEGER(IntKi), PARAMETER      :: QD2_Sg    = 890
   INTEGER(IntKi), PARAMETER      :: QD2_Sw    = 891
   INTEGER(IntKi), PARAMETER      :: QD2_Hv    = 892
   INTEGER(IntKi), PARAMETER      :: QD2_R     = 893
   INTEGER(IntKi), PARAMETER      :: QD2_P     = 894
   INTEGER(IntKi), PARAMETER      :: QD2_Y     = 895


     ! The maximum number of output channels which can be output by the code.
   INTEGER(IntKi), PARAMETER      :: MaxOutPts = 895

!End of code generated by Matlab script


INTEGER,  PARAMETER          :: TipDxc( 3)  = (/TipDxc1,  TipDxc2,  TipDxc3/)
INTEGER,  PARAMETER          :: TipDyc( 3)  = (/TipDyc1,  TipDyc2,  TipDyc3/)
INTEGER,  PARAMETER          :: TipDzc( 3)  = (/TipDzc1,  TipDzc2,  TipDzc3/)
INTEGER,  PARAMETER          :: TipDxb( 3)  = (/TipDxb1,  TipDxb2,  TipDxb3/)
INTEGER,  PARAMETER          :: TipDyb( 3)  = (/TipDyb1,  TipDyb2,  TipDyb3/)
INTEGER,  PARAMETER          :: TipALxb(3)  = (/TipALxb1, TipALxb2, TipALxb3/)
INTEGER,  PARAMETER          :: TipALyb(3)  = (/TipALyb1, TipALyb2, TipALyb3/)
INTEGER,  PARAMETER          :: TipALzb(3)  = (/TipALzb1, TipALzb2, TipALzb3/)
INTEGER,  PARAMETER          :: TipRDxb(3)  = (/TipRDxb1, TipRDxb2, TipRDxb3/)
INTEGER,  PARAMETER          :: TipRDyb(3)  = (/TipRDyb1, TipRDyb2, TipRDyb3/)
INTEGER,  PARAMETER          :: TipRDzc(3)  = (/TipRDzc1, TipRDzc2, TipRDzc3/)
INTEGER,  PARAMETER          :: TipClrnc(3) = (/TipClrnc1,TipClrnc2,TipClrnc3/)
INTEGER,  PARAMETER          :: PtchPMzc(3) = (/PtchPMzc1,PtchPMzc2,PtchPMzc3/)

INTEGER,  PARAMETER          :: RootFxc(3) = (/ RootFxc1,RootFxc2,RootFxc3 /)
INTEGER,  PARAMETER          :: RootFyc(3) = (/ RootFyc1,RootFyc2,RootFyc3 /)
INTEGER,  PARAMETER          :: RootFzc(3) = (/ RootFzc1,RootFzc2,RootFzc3 /)
INTEGER,  PARAMETER          :: RootFxb(3) = (/ RootFxb1,RootFxb2,RootFxb3 /)
INTEGER,  PARAMETER          :: RootFyb(3) = (/ RootFyb1,RootFyb2,RootFyb3 /)
INTEGER,  PARAMETER          :: RootMxc(3) = (/ RootMxc1,RootMxc2,RootMxc3 /)
INTEGER,  PARAMETER          :: RootMyc(3) = (/ RootMyc1,RootMyc2,RootMyc3 /)
INTEGER,  PARAMETER          :: RootMzc(3) = (/ RootMzc1,RootMzc2,RootMzc3 /)
INTEGER,  PARAMETER          :: RootMxb(3) = (/ RootMxb1,RootMxb2,RootMxb3 /)
INTEGER,  PARAMETER          :: RootMyb(3) = (/ RootMyb1,RootMyb2,RootMyb3 /)

INTEGER,  PARAMETER          :: SpnALxb(9, 3) = RESHAPE( (/ &
                                    Spn1ALxb1,Spn2ALxb1,Spn3ALxb1,Spn4ALxb1,Spn5ALxb1,Spn6ALxb1,Spn7ALxb1,Spn8ALxb1,Spn9ALxb1, &
                                    Spn1ALxb2,Spn2ALxb2,Spn3ALxb2,Spn4ALxb2,Spn5ALxb2,Spn6ALxb2,Spn7ALxb2,Spn8ALxb2,Spn9ALxb2, &
                                    Spn1ALxb3,Spn2ALxb3,Spn3ALxb3,Spn4ALxb3,Spn5ALxb3,Spn6ALxb3,Spn7ALxb3,Spn8ALxb3,Spn9ALxb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnALyb(9, 3) = RESHAPE( (/ &
                                    Spn1ALyb1,Spn2ALyb1,Spn3ALyb1,Spn4ALyb1,Spn5ALyb1,Spn6ALyb1,Spn7ALyb1,Spn8ALyb1,Spn9ALyb1, &
                                    Spn1ALyb2,Spn2ALyb2,Spn3ALyb2,Spn4ALyb2,Spn5ALyb2,Spn6ALyb2,Spn7ALyb2,Spn8ALyb2,Spn9ALyb2, &
                                    Spn1ALyb3,Spn2ALyb3,Spn3ALyb3,Spn4ALyb3,Spn5ALyb3,Spn6ALyb3,Spn7ALyb3,Spn8ALyb3,Spn9ALyb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnALzb(9, 3) = RESHAPE( (/ &
                                    Spn1ALzb1,Spn2ALzb1,Spn3ALzb1,Spn4ALzb1,Spn5ALzb1,Spn6ALzb1,Spn7ALzb1,Spn8ALzb1,Spn9ALzb1, &
                                    Spn1ALzb2,Spn2ALzb2,Spn3ALzb2,Spn4ALzb2,Spn5ALzb2,Spn6ALzb2,Spn7ALzb2,Spn8ALzb2,Spn9ALzb2, &
                                    Spn1ALzb3,Spn2ALzb3,Spn3ALzb3,Spn4ALzb3,Spn5ALzb3,Spn6ALzb3,Spn7ALzb3,Spn8ALzb3,Spn9ALzb3  &
                                /), (/9, 3/) )

INTEGER,  PARAMETER          :: SpnFLxb(9,3) = RESHAPE( (/ &
                                    Spn1FLxb1,Spn2FLxb1,Spn3FLxb1,Spn4FLxb1,Spn5FLxb1,Spn6FLxb1,Spn7FLxb1,Spn8FLxb1,Spn9FLxb1, &
                                    Spn1FLxb2,Spn2FLxb2,Spn3FLxb2,Spn4FLxb2,Spn5FLxb2,Spn6FLxb2,Spn7FLxb2,Spn8FLxb2,Spn9FLxb2, &
                                    Spn1FLxb3,Spn2FLxb3,Spn3FLxb3,Spn4FLxb3,Spn5FLxb3,Spn6FLxb3,Spn7FLxb3,Spn8FLxb3,Spn9FLxb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnFLyb(9,3) = RESHAPE( (/ &
                                    Spn1FLyb1,Spn2FLyb1,Spn3FLyb1,Spn4FLyb1,Spn5FLyb1,Spn6FLyb1,Spn7FLyb1,Spn8FLyb1,Spn9FLyb1, &
                                    Spn1FLyb2,Spn2FLyb2,Spn3FLyb2,Spn4FLyb2,Spn5FLyb2,Spn6FLyb2,Spn7FLyb2,Spn8FLyb2,Spn9FLyb2, &
                                    Spn1FLyb3,Spn2FLyb3,Spn3FLyb3,Spn4FLyb3,Spn5FLyb3,Spn6FLyb3,Spn7FLyb3,Spn8FLyb3,Spn9FLyb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnFLzb(9,3) = RESHAPE( (/ &
                                    Spn1FLzb1,Spn2FLzb1,Spn3FLzb1,Spn4FLzb1,Spn5FLzb1,Spn6FLzb1,Spn7FLzb1,Spn8FLzb1,Spn9FLzb1, &
                                    Spn1FLzb2,Spn2FLzb2,Spn3FLzb2,Spn4FLzb2,Spn5FLzb2,Spn6FLzb2,Spn7FLzb2,Spn8FLzb2,Spn9FLzb2, &
                                    Spn1FLzb3,Spn2FLzb3,Spn3FLzb3,Spn4FLzb3,Spn5FLzb3,Spn6FLzb3,Spn7FLzb3,Spn8FLzb3,Spn9FLzb3  &
                                /), (/9, 3/) )

INTEGER,  PARAMETER          :: SpnMLxb(9,3) = RESHAPE( (/ &
                                    Spn1MLxb1,Spn2MLxb1,Spn3MLxb1,Spn4MLxb1,Spn5MLxb1,Spn6MLxb1,Spn7MLxb1,Spn8MLxb1,Spn9MLxb1, &
                                    Spn1MLxb2,Spn2MLxb2,Spn3MLxb2,Spn4MLxb2,Spn5MLxb2,Spn6MLxb2,Spn7MLxb2,Spn8MLxb2,Spn9MLxb2, &
                                    Spn1MLxb3,Spn2MLxb3,Spn3MLxb3,Spn4MLxb3,Spn5MLxb3,Spn6MLxb3,Spn7MLxb3,Spn8MLxb3,Spn9MLxb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnMLyb(9,3) = RESHAPE( (/ &
                                    Spn1MLyb1,Spn2MLyb1,Spn3MLyb1,Spn4MLyb1,Spn5MLyb1,Spn6MLyb1,Spn7MLyb1,Spn8MLyb1,Spn9MLyb1, &
                                    Spn1MLyb2,Spn2MLyb2,Spn3MLyb2,Spn4MLyb2,Spn5MLyb2,Spn6MLyb2,Spn7MLyb2,Spn8MLyb2,Spn9MLyb2, &
                                    Spn1MLyb3,Spn2MLyb3,Spn3MLyb3,Spn4MLyb3,Spn5MLyb3,Spn6MLyb3,Spn7MLyb3,Spn8MLyb3,Spn9MLyb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnMLzb(9,3) = RESHAPE( (/ &
                                    Spn1MLzb1,Spn2MLzb1,Spn3MLzb1,Spn4MLzb1,Spn5MLzb1,Spn6MLzb1,Spn7MLzb1,Spn8MLzb1,Spn9MLzb1, &
                                    Spn1MLzb2,Spn2MLzb2,Spn3MLzb2,Spn4MLzb2,Spn5MLzb2,Spn6MLzb2,Spn7MLzb2,Spn8MLzb2,Spn9MLzb2, &
                                    Spn1MLzb3,Spn2MLzb3,Spn3MLzb3,Spn4MLzb3,Spn5MLzb3,Spn6MLzb3,Spn7MLzb3,Spn8MLzb3,Spn9MLzb3  &
                                /), (/9, 3/) )

INTEGER,  PARAMETER          :: SpnTDxb(9,3) = RESHAPE( (/ &
                                    Spn1TDxb1,Spn2TDxb1,Spn3TDxb1,Spn4TDxb1,Spn5TDxb1,Spn6TDxb1,Spn7TDxb1,Spn8TDxb1,Spn9TDxb1, &
                                    Spn1TDxb2,Spn2TDxb2,Spn3TDxb2,Spn4TDxb2,Spn5TDxb2,Spn6TDxb2,Spn7TDxb2,Spn8TDxb2,Spn9TDxb2, &
                                    Spn1TDxb3,Spn2TDxb3,Spn3TDxb3,Spn4TDxb3,Spn5TDxb3,Spn6TDxb3,Spn7TDxb3,Spn8TDxb3,Spn9TDxb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnTDyb(9,3) = RESHAPE( (/ &
                                    Spn1TDyb1,Spn2TDyb1,Spn3TDyb1,Spn4TDyb1,Spn5TDyb1,Spn6TDyb1,Spn7TDyb1,Spn8TDyb1,Spn9TDyb1, &
                                    Spn1TDyb2,Spn2TDyb2,Spn3TDyb2,Spn4TDyb2,Spn5TDyb2,Spn6TDyb2,Spn7TDyb2,Spn8TDyb2,Spn9TDyb2, &
                                    Spn1TDyb3,Spn2TDyb3,Spn3TDyb3,Spn4TDyb3,Spn5TDyb3,Spn6TDyb3,Spn7TDyb3,Spn8TDyb3,Spn9TDyb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnTDzb(9,3) = RESHAPE( (/ &
                                    Spn1TDzb1,Spn2TDzb1,Spn3TDzb1,Spn4TDzb1,Spn5TDzb1,Spn6TDzb1,Spn7TDzb1,Spn8TDzb1,Spn9TDzb1, &
                                    Spn1TDzb2,Spn2TDzb2,Spn3TDzb2,Spn4TDzb2,Spn5TDzb2,Spn6TDzb2,Spn7TDzb2,Spn8TDzb2,Spn9TDzb2, &
                                    Spn1TDzb3,Spn2TDzb3,Spn3TDzb3,Spn4TDzb3,Spn5TDzb3,Spn6TDzb3,Spn7TDzb3,Spn8TDzb3,Spn9TDzb3  &
                                /), (/9, 3/) )

INTEGER,  PARAMETER          :: SpnRDxb(9,3) = RESHAPE( (/ &
                                    Spn1RDxb1,Spn2RDxb1,Spn3RDxb1,Spn4RDxb1,Spn5RDxb1,Spn6RDxb1,Spn7RDxb1,Spn8RDxb1,Spn9RDxb1, &
                                    Spn1RDxb2,Spn2RDxb2,Spn3RDxb2,Spn4RDxb2,Spn5RDxb2,Spn6RDxb2,Spn7RDxb2,Spn8RDxb2,Spn9RDxb2, &
                                    Spn1RDxb3,Spn2RDxb3,Spn3RDxb3,Spn4RDxb3,Spn5RDxb3,Spn6RDxb3,Spn7RDxb3,Spn8RDxb3,Spn9RDxb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnRDyb(9,3) = RESHAPE( (/ &
                                    Spn1RDyb1,Spn2RDyb1,Spn3RDyb1,Spn4RDyb1,Spn5RDyb1,Spn6RDyb1,Spn7RDyb1,Spn8RDyb1,Spn9RDyb1, &
                                    Spn1RDyb2,Spn2RDyb2,Spn3RDyb2,Spn4RDyb2,Spn5RDyb2,Spn6RDyb2,Spn7RDyb2,Spn8RDyb2,Spn9RDyb2, &
                                    Spn1RDyb3,Spn2RDyb3,Spn3RDyb3,Spn4RDyb3,Spn5RDyb3,Spn6RDyb3,Spn7RDyb3,Spn8RDyb3,Spn9RDyb3  &
                                /), (/9, 3/) )
INTEGER,  PARAMETER          :: SpnRDzb(9,3) = RESHAPE( (/ &
                                    Spn1RDzb1,Spn2RDzb1,Spn3RDzb1,Spn4RDzb1,Spn5RDzb1,Spn6RDzb1,Spn7RDzb1,Spn8RDzb1,Spn9RDzb1, &
                                    Spn1RDzb2,Spn2RDzb2,Spn3RDzb2,Spn4RDzb2,Spn5RDzb2,Spn6RDzb2,Spn7RDzb2,Spn8RDzb2,Spn9RDzb2, &
                                    Spn1RDzb3,Spn2RDzb3,Spn3RDzb3,Spn4RDzb3,Spn5RDzb3,Spn6RDzb3,Spn7RDzb3,Spn8RDzb3,Spn9RDzb3  &
                                /), (/9, 3/) )


INTEGER,  PARAMETER          :: TwHtALxt(9) = (/ &
                                    TwHt1ALxt,TwHt2ALxt,TwHt3ALxt,TwHt4ALxt,TwHt5ALxt,TwHt6ALxt,TwHt7ALxt,TwHt8ALxt,TwHt9ALxt /)
INTEGER,  PARAMETER          :: TwHtALyt(9) = (/ &
                                    TwHt1ALyt,TwHt2ALyt,TwHt3ALyt,TwHt4ALyt,TwHt5ALyt,TwHt6ALyt,TwHt7ALyt,TwHt8ALyt,TwHt9ALyt /)
INTEGER,  PARAMETER          :: TwHtALzt(9) = (/ &
                                    TwHt1ALzt,TwHt2ALzt,TwHt3ALzt,TwHt4ALzt,TwHt5ALzt,TwHt6ALzt,TwHt7ALzt,TwHt8ALzt,TwHt9ALzt /)

INTEGER,  PARAMETER          :: TwHtMLxt(9) = (/ &
                                    TwHt1MLxt,TwHt2MLxt,TwHt3MLxt,TwHt4MLxt,TwHt5MLxt,TwHt6MLxt,TwHt7MLxt,TwHt8MLxt,TwHt9MLxt /)
INTEGER,  PARAMETER          :: TwHtMLyt(9) = (/ &
                                    TwHt1MLyt,TwHt2MLyt,TwHt3MLyt,TwHt4MLyt,TwHt5MLyt,TwHt6MLyt,TwHt7MLyt,TwHt8MLyt,TwHt9MLyt /)
INTEGER,  PARAMETER          :: TwHtMLzt(9) = (/ &
                                    TwHt1MLzt,TwHt2MLzt,TwHt3MLzt,TwHt4MLzt,TwHt5MLzt,TwHt6MLzt,TwHt7MLzt,TwHt8MLzt,TwHt9MLzt /)

INTEGER,  PARAMETER          :: TwHtFLxt(9) = (/ &
                                    TwHt1FLxt,TwHt2FLxt,TwHt3FLxt,TwHt4FLxt,TwHt5FLxt,TwHt6FLxt,TwHt7FLxt,TwHt8FLxt,TwHt9FLxt /)
INTEGER,  PARAMETER          :: TwHtFLyt(9) = (/ &
                                    TwHt1FLyt,TwHt2FLyt,TwHt3FLyt,TwHt4FLyt,TwHt5FLyt,TwHt6FLyt,TwHt7FLyt,TwHt8FLyt,TwHt9FLyt /)
INTEGER,  PARAMETER          :: TwHtFLzt(9) = (/ &
                                    TwHt1FLzt,TwHt2FLzt,TwHt3FLzt,TwHt4FLzt,TwHt5FLzt,TwHt6FLzt,TwHt7FLzt,TwHt8FLzt,TwHt9FLzt /)

INTEGER,  PARAMETER          :: TwHtTDxt(9) = (/ &
                                    TwHt1TDxt,TwHt2TDxt,TwHt3TDxt,TwHt4TDxt,TwHt5TDxt,TwHt6TDxt,TwHt7TDxt,TwHt8TDxt,TwHt9TDxt /)
INTEGER,  PARAMETER          :: TwHtTDyt(9) = (/ &
                                    TwHt1TDyt,TwHt2TDyt,TwHt3TDyt,TwHt4TDyt,TwHt5TDyt,TwHt6TDyt,TwHt7TDyt,TwHt8TDyt,TwHt9TDyt /)
INTEGER,  PARAMETER          :: TwHtTDzt(9) = (/ &
                                    TwHt1TDzt,TwHt2TDzt,TwHt3TDzt,TwHt4TDzt,TwHt5TDzt,TwHt6TDzt,TwHt7TDzt,TwHt8TDzt,TwHt9TDzt /)

INTEGER,  PARAMETER          :: TwHtRDxt(9) = (/ &
                                    TwHt1RDxt,TwHt2RDxt,TwHt3RDxt,TwHt4RDxt,TwHt5RDxt,TwHt6RDxt,TwHt7RDxt,TwHt8RDxt,TwHt9RDxt /)
INTEGER,  PARAMETER          :: TwHtRDyt(9) = (/ &
                                    TwHt1RDyt,TwHt2RDyt,TwHt3RDyt,TwHt4RDyt,TwHt5RDyt,TwHt6RDyt,TwHt7RDyt,TwHt8RDyt,TwHt9RDyt /)
INTEGER,  PARAMETER          :: TwHtRDzt(9) = (/ &
                                    TwHt1RDzt,TwHt2RDzt,TwHt3RDzt,TwHt4RDzt,TwHt5RDzt,TwHt6RDzt,TwHt7RDzt,TwHt8RDzt,TwHt9RDzt /)

INTEGER,  PARAMETER          :: TwHtTPxi(9) = (/ &
                                    TwHt1TPxi,TwHt2TPxi,TwHt3TPxi,TwHt4TPxi,TwHt5TPxi,TwHt6TPxi,TwHt7TPxi,TwHt8TPxi,TwHt9TPxi /)
INTEGER,  PARAMETER          :: TwHtTPyi(9) = (/ &
                                    TwHt1TPyi,TwHt2TPyi,TwHt3TPyi,TwHt4TPyi,TwHt5TPyi,TwHt6TPyi,TwHt7TPyi,TwHt8TPyi,TwHt9TPyi /)
INTEGER,  PARAMETER          :: TwHtTPzi(9) = (/ &
                                    TwHt1TPzi,TwHt2TPzi,TwHt3TPzi,TwHt4TPzi,TwHt5TPzi,TwHt6TPzi,TwHt7TPzi,TwHt8TPzi,TwHt9TPzi /)

INTEGER,  PARAMETER          :: TwHtRPxi(9) = (/ &
                                    TwHt1RPxi,TwHt2RPxi,TwHt3RPxi,TwHt4RPxi,TwHt5RPxi,TwHt6RPxi,TwHt7RPxi,TwHt8RPxi,TwHt9RPxi /)
INTEGER,  PARAMETER          :: TwHtRPyi(9) = (/ &
                                    TwHt1RPyi,TwHt2RPyi,TwHt3RPyi,TwHt4RPyi,TwHt5RPyi,TwHt6RPyi,TwHt7RPyi,TwHt8RPyi,TwHt9RPyi /)
INTEGER,  PARAMETER          :: TwHtRPzi(9) = (/ &
                                    TwHt1RPzi,TwHt2RPzi,TwHt3RPzi,TwHt4RPzi,TwHt5RPzi,TwHt6RPzi,TwHt7RPzi,TwHt8RPzi,TwHt9RPzi /)



END MODULE ElastoDyn_Parameters
!**********************************************************************************************************************************
!**********************************************************************************************************************************
! The ElastoDyn.f90, ElastoDyn_Types.f90, and ElastoDyn_Parameters.f90 files make up the ElastoDyn module of the
! FAST Modularization Framework. ElastoDyn_Types is auto-generated based on FAST_Registry.txt.
!
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of ElastoDyn.
!
!    ElastoDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with ElastoDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE ElastoDyn

   USE NWTC_Library

   USE ElastoDyn_Parameters
   USE ElastoDyn_Types


   IMPLICIT NONE

!BJJ REMOVE FOR NOW:   PRIVATE

   TYPE(ProgDesc), PARAMETER  :: ED_Ver = ProgDesc( 'ElastoDyn', 'v1.00.00a-bjj', '31-March-2013' )



      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: ED_Init                           ! Initialization routine
   PUBLIC :: ED_End                            ! Ending routine (includes clean up)

   PUBLIC :: ED_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                               !   continuous states, and updating discrete states
   PUBLIC :: ED_CalcOutput                     ! Routine for computing outputs

   PUBLIC :: ED_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   PUBLIC :: ED_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   PUBLIC :: ED_UpdateDiscState                ! Tight coupling routine for updating discrete states

   !PUBLIC :: ED_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                            !   (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
   !PUBLIC :: ED_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                            !   (Xd), and constraint-state (Z) equations all with respect to the continuous
   !                                            !   states (x)
   !PUBLIC :: ED_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                            !   (Xd), and constraint-state (Z) equations all with respect to the discrete
   !                                            !   states (xd)
   !PUBLIC :: ED_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                            !   (Xd), and constraint-state (Z) equations all with respect to the constraint
   !                                            !   states (z)


CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

   TYPE(ED_InitInputType),       INTENT(IN   )  :: InitInp     ! Input data for initialization routine
   TYPE(ED_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input; input mesh must be defined
   TYPE(ED_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
   TYPE(ED_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
   TYPE(ED_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
   TYPE(ED_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
   TYPE(ED_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states
   TYPE(ED_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                               !   only the output mesh is initialized)
   REAL(DbKi),                   INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                               !   (1) ED_UpdateStates() is called in loose coupling &
                                                               !   (2) ED_UpdateDiscState() is called in tight coupling.
                                                               !   Input is the suggested time from the glue code;
                                                               !   Output is the actual coupling interval that will be used
                                                               !   by the glue code.
   TYPE(ED_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables

   TYPE(ED_InputFile)                           :: InputFileData           ! Data stored in the module's input file
   LOGICAL, PARAMETER                           :: GetAdamsVals = .FALSE.  ! Determines if we should read Adams values and create (update) an Adams model

!bjj: ERROR CHECKING HERE!!!!

      ! Initialize variables for this routine

   ErrStat = ErrID_None
   ErrMsg  = ""
      

      ! Initialize the NWTC Subroutine Library

   CALL NWTC_Init( )

      ! Display the module information

   CALL DispNVD( ED_Ver )

      !............................................................................................
      ! Read the input file and validate the data
      !............................................................................................
   p%RootName = TRIM(InitInp%RootName)//'_'//TRIM(ED_Ver%Name) ! all of the output file names from this module will end with '_ElastoDyn'

   CALL ED_ReadInput( InitInp%InputFile, InitInp%ADInputFile, InputFileData, GetAdamsVals, p%RootName, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) THEN
            !we need a routine to destroy the InputFileData structure....

      CALL WrScr( ErrMsg )
      RETURN
   END IF
   CALL ED_ValidateInput( InputFileData, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
   !we need a routine to destroy the InputFileData structure....
         
      RETURN
   END IF
      
      !............................................................................................
      ! Define parameters here:
      !............................................................................................
   CALL ED_SetParameters( InputFileData, p, ErrStat, ErrMsg )     
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
   !we need a routine to destroy the InputFileData structure....
         
      RETURN
   END IF

   p%DT  = Interval
   

      !............................................................................................
      ! Define initial system states here:
      !............................................................................................               
   xd%DummyDiscState          = 0                                             ! we don't have discrete states
   z%DummyConstrState         = 0                                             ! we don't have constraint states
      
   CALL Init_ContStates( x, p, InputFileData, ErrStat, ErrMsg )               ! initialize the continuous states
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
   !we need a routine to destroy the InputFileData structure....
         
      RETURN
   END IF

      !............................................................................................
      ! Initialize other states:
      !............................................................................................
   CALL Init_OtherStates( OtherState, p, InputFileData, ErrStat, ErrMsg )      ! initialize the other states
      
   !bjj: is this a continuous state?
   CALL AllocAry(OtherState%BlPitch, p%NumBl, 'BlPitch', ErrStat, ErrMsg )
   IF (ErrStat /= ErrID_None) RETURN
   OtherState%BlPitch = InputFileData%BlPitch(1:p%NumBl)   
   
      !............................................................................................
      ! Define initial guess for the system inputs here:
      !............................................................................................

   CALL AllocAry( u%BlPitchCom, p%NumBl, 'BlPitchCom', ErrStat, ErrMsg )
   IF (ErrStat /= ErrID_None) RETURN
   u%BlPitchCom = InputFileData%BlPitch(1:p%NumBl)


      !............................................................................................
      ! Define system output initializations (set up mesh) here:
      !............................................................................................

   ALLOCATE ( y%AllOuts(0:MaxOutPts) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = 'Error allocating memory for the AllOuts array.'
      !we need a routine to destroy the InputFileData structure....
      
      RETURN
   ENDIF
   y%AllOuts = 0.0      

      !............................................................................................
      ! Define initialization-routine output here:
      !............................................................................................

      !InitOut%WriteOutputHdr = (/ 'Time      ', 'Column2   ' /)
      !InitOut%WriteOutputUnt = (/ '(s)',  '(-)'     /)
      !
   ALLOCATE ( y%WriteOutput(0:p%NumOuts) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = 'Error allocating memory for the ElastoDyn WriteOutput array.' 
      !we need a routine to destroy the InputFileData structure....
      RETURN
   ENDIF
      
   InitOut%Ver = ED_Ver
   
   CALL AllocAry(InitOut%BlPitch, p%NumBl, 'BlPitch', ErrStat, ErrMsg )
   IF (ErrStat /= ErrID_None) RETURN
   InitOut%BlPitch = InputFileData%BlPitch(1:p%NumBl)   
   
      
      !............................................................................................
      ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
      !   this module must be called here:
      !............................................................................................

       !Interval = p%DT

      ! Initialize Other State data:
   ! Allocate space for coordinate systems

   CALL Alloc_CoordSys( OtherState%CoordSys, p, ErrStat, ErrMsg )
     
       
       
       ! Destroy the InputFileData structure (deallocate arrays)
       
   CALL ED_DestroyInputFile(InputFileData, ErrStat, ErrMsg )
       
END SUBROUTINE ED_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(ED_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(ED_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(ED_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(ED_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(ED_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(ED_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(ED_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL ED_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL ED_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL ED_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL ED_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL ED_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL ED_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL ED_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE ED_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                    INTENT(IN   ) :: Time        ! Current simulation time in seconds
      TYPE(ED_InputType),            INTENT(IN   ) :: u           ! Inputs at Time
      TYPE(ED_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(ED_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at Time;
                                                                  !   Output: Continuous states at Time + Interval
      TYPE(ED_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at Time;
                                                                  !   Output: Discrete states at Time  + Interval
      TYPE(ED_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at Time;
                                                                  !   Output: Constraint states at Time
      TYPE(ED_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                  INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(ED_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at Time
      TYPE(ED_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                               :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                       :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL ED_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL ED_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL ED_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL ED_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  END IF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL ED_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL ED_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL ED_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in ED_UpdateDiscState(), so ED_CalcOutput(),
         !   ED_CalcContStateDeriv(), and ED_CalcConstrStates() must be called first (see above).

      CALL ED_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL ED_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL ED_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE ED_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
! This SUBROUTINE is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
! NOTE: the descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
! for a complete description of each output parameter.
! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated 
! All of the calculated output channels are placed into the y%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................

   REAL(DbKi),                   INTENT(IN   )  :: t           ! Current simulation time in seconds
   TYPE(ED_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
   TYPE(ED_ParameterType),       INTENT(IN   )  :: p           ! Parameters
   TYPE(ED_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
   TYPE(ED_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
   TYPE(ED_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
   TYPE(ED_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
   TYPE(ED_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                               !   nectivity information does not have to be recalculated)
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables:

   REAL(ReKi)                   :: AngAccEB  (3)                                   ! Angular acceleration of the base plate                                                (body B) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: AngAccER  (3)                                   ! Angular acceleration of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: AngAccEX  (3)                                   ! Angular acceleration of the platform                                                  (body X) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in several expressions.
   REAL(ReKi)                   :: CThrstys                                        ! Estimate of the ys-location of the center of thrust.
   REAL(ReKi)                   :: CThrstzs                                        ! Estimate of the zs-location of the center of thrust.
   REAL(ReKi)                   :: FrcMGagB  (3)                                   ! Total force at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
   REAL(ReKi)                   :: FrcFGagT  (3)                                   ! Total force at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
   REAL(ReKi)                   :: FrcONcRt  (3)                                   ! Total force at the yaw bearing (point O  ) due to the nacelle, generator, and rotor.
   REAL(ReKi)                   :: FrcPRot   (3)                                   ! Total force at the teeter pin  (point P  ) due to the rotor.
   REAL(ReKi)                   :: FrcT0Trb  (3)                                   ! Total force at the base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
   REAL(ReKi)                   :: FZHydro   (3)                                   ! Total platform hydrodynamic force at the platform reference (point Z).
   REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system.
   REAL(ReKi)                   :: LinAccEIMU(3)                                   ! Total linear acceleration of the nacelle IMU (point IMU) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: LinAccEZ  (3)                                   ! Total linear acceleration of the platform refernce (point Z) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: MomBNcRt  (3)                                   ! Total moment at the base plate      (body B) / yaw bearing                           (point O) due to the nacelle, generator, and rotor.
   REAL(ReKi)                   :: MomFGagT  (3)                                   ! Total moment at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
   REAL(ReKi)                   :: MomLPRot  (3)                                   ! Total moment at the low-speed shaft (body L) / teeter pin                            (point P) due to the rotor.
   REAL(ReKi)                   :: MomMGagB  (3)                                   ! Total moment at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
   REAL(ReKi)                   :: MomNGnRt  (3)                                   ! Total moment at the nacelle         (body N) / specified point on rotor-furl axis    (point V) due to the structure that furls with the rotor, generator, and rotor.
   REAL(ReKi)                   :: MomNTail  (3)                                   ! Total moment at the nacelle         (body N) / specified point on  tail-furl axis    (point W) due to the tail.
   REAL(ReKi)                   :: MomX0Trb  (3)                                   ! Total moment at the tower base      (body X) / base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
   REAL(ReKi)                   :: MXHydro   (3)                                   ! Total platform hydrodynamic moment acting at the platform (body X) / platform reference (point Z).
   REAL(ReKi)                   :: rOPO      (3)                                   ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).
   REAL(ReKi)                   :: rOSTip    (3)                                   ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip).
   REAL(ReKi)                   :: rOSTipxn                                        ! Component of rOSTip directed along the xn-axis.
   REAL(ReKi)                   :: rOSTipyn                                        ! Component of rOSTip directed along the yn-axis.
   REAL(ReKi)                   :: rOSTipzn                                        ! Component of rOSTip directed along the zn-axis.
   REAL(ReKi)                   :: rTPT      (3)                                   ! Position vector from the undeflected tower node (point T prime) to the deflected node (point T)
   REAL(ReKi)                   :: rSPS      (3)                                   ! Position vector from the undeflected blade node (point S prime) to the deflected node (point S)
   REAL(ReKi)                   :: rSTipPSTip(3)                                   ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip).
   REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.
   REAL(ReKi)                   :: TmpVec2   (3)                                   ! A temporary vector.


   REAL(ReKi)                   :: LinAccES (p%NumBl,p%TipNode,3)                  ! Total linear acceleration of a point on a   blade (point S) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: LinAccET (p%TwrNodes,3)                         ! Total linear acceleration of a point on the tower (point T) in the inertia frame (body E for earth).
   REAL(ReKi)                   :: FrcS0B   (p%NumBl,3)                            ! Total force at the blade root (point S(0)) due to the blade.
   REAL(ReKi)                   :: FTHydro  (p%TwrNodes,3)                         ! Total hydrodynamic force per unit length acting on the tower at point T.
   REAL(ReKi)                   :: MFHydro  (p%TwrNodes,3)                         ! Total hydrodynamic moment per unit length acting on a tower element (body F) at point T.
   REAL(ReKi)                   :: MomH0B   (p%NumBl,3)                            ! Total moment at the hub (body H) / blade root (point S(0)) due to the blade.


   INTEGER(IntKi)               :: I                                               ! Generic index
   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.

   
   
         ! Initialize some output values
      ErrStat = ErrID_None
      ErrMsg  = ""


   !Array y%AllOuts() is initialized to 0.0 in subroutine ChckOutLst(), so we are not going to reinitialize it here.

   !...............................................................................................................................
   ! Calculate all of the total forces and moments using all of the partial forces and moments calculated in RtHS().  Also,
   !   calculate all of the total angular and linear accelerations using all of the partial accelerations calculated in RtHS().
   !   To do this, first initialize the variables using the portions not associated with the accelerations.  Then add the portions
   !   associated with the accelerations one by one:
   !...............................................................................................................................

   AngAccEB   = OtherState%RtHS%AngAccEBt
   AngAccER   = OtherState%RtHS%AngAccERt
   AngAccEX   = OtherState%RtHS%AngAccEXt
   LinAccEIMU = OtherState%RtHS%LinAccEIMUt
   LinAccEO   = OtherState%RtHS%LinAccEOt
   LinAccEZ   = OtherState%RtHS%LinAccEZt
   FrcONcRt   = OtherState%RtHS%FrcONcRtt
   FrcPRot    = OtherState%RtHS%FrcPRott
   FrcT0Trb   = OtherState%RtHS%FrcT0Trbt
   FZHydro    = OtherState%RtHS%FZHydrot
   MomBNcRt   = OtherState%RtHS%MomBNcRtt
   MomLPRot   = OtherState%RtHS%MomLPRott
   MomNGnRt   = OtherState%RtHS%MomNGnRtt
   MomNTail   = OtherState%RtHS%MomNTailt
   MomX0Trb   = OtherState%RtHS%MomX0Trbt
   MXHydro    = OtherState%RtHS%MXHydrot

   DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
      AngAccEB   = AngAccEB   + OtherState%RtHS%PAngVelEB  (p%DOFs%SrtPS(I),0,:)*OtherState%QD2T(p%DOFs%SrtPS(I))
      AngAccER   = AngAccER   + OtherState%RtHS%PAngVelER  (p%DOFs%SrtPS(I),0,:)*OtherState%QD2T(p%DOFs%SrtPS(I))
      LinAccEIMU = LinAccEIMU + OtherState%RtHS%PLinVelEIMU(p%DOFs%SrtPS(I),0,:)*OtherState%QD2T(p%DOFs%SrtPS(I))
      LinAccEO   = LinAccEO   + OtherState%RtHS%PLinVelEO  (p%DOFs%SrtPS(I),0,:)*OtherState%QD2T(p%DOFs%SrtPS(I))
      FrcONcRt   = FrcONcRt   + OtherState%RtHS%PFrcONcRt  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      FrcPRot    = FrcPRot    + OtherState%RtHS%PFrcPRot   (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      FrcT0Trb   = FrcT0Trb   + OtherState%RtHS%PFrcT0Trb  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      MomBNcRt   = MomBNcRt   + OtherState%RtHS%PMomBNcRt  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      MomLPRot   = MomLPRot   + OtherState%RtHS%PMomLPRot  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      MomNGnRt   = MomNGnRt   + OtherState%RtHS%PMomNGnRt  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      MomNTail   = MomNTail   + OtherState%RtHS%PMomNTail  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
      MomX0Trb   = MomX0Trb   + OtherState%RtHS%PMomX0Trb  (p%DOFs%SrtPS(I),  :)*OtherState%QD2T(p%DOFs%SrtPS(I))
   ENDDO             ! I - All active (enabled) DOFs
   DO I = 1,p%DOFs%NPYE     ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)
      AngAccEX   = AngAccEX   + OtherState%RtHS%PAngVelEX  (p%DOFs%PYE  (I),0,:)*OtherState%QD2T(p%DOFs%PYE  (I))
      LinAccEZ   = LinAccEZ   + OtherState%RtHS%PLinVelEZ  (p%DOFs%PYE  (I),0,:)*OtherState%QD2T(p%DOFs%PYE  (I))
      FZHydro    = FZHydro    + OtherState%RtHS%PFZHydro   (p%DOFs%PYE  (I),  :)*OtherState%QD2T(p%DOFs%PYE  (I))
      MXHydro    = MXHydro    + OtherState%RtHS%PMXHydro   (p%DOFs%PYE  (I),  :)*OtherState%QD2T(p%DOFs%PYE  (I))
   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)



   DO K = 1,p%NumBl ! Loop through all blades

      LinAccES(K,p%TipNode,:) = OtherState%RtHS%LinAccESt(K,p%TipNode,:)
      FrcS0B  (K,          :) = OtherState%RtHS%FrcS0Bt  (K,          :)
      MomH0B  (K,          :) = OtherState%RtHS%MomH0Bt  (K,          :)

      DO I = 1,p%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
         LinAccES(K,p%TipNode,:) = LinAccES(K,p%TipNode,:) + OtherState%RtHS%PLinVelES(K,p%TipNode,p%DOFs%PSE(K,I),0,:)*OtherState%QD2T(p%DOFs%PSE(K,I))
         FrcS0B  (K,          :) = FrcS0B  (K,          :) + OtherState%RtHS%PFrcS0B  (K,          p%DOFs%PSE(K,I),  :)*OtherState%QD2T(p%DOFs%PSE(K,I))
         MomH0B  (K,          :) = MomH0B  (K,          :) + OtherState%RtHS%PMomH0B  (K,          p%DOFs%PSE(K,I),  :)*OtherState%QD2T(p%DOFs%PSE(K,I))
      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

         LinAccES(K,J,:) = OtherState%RtHS%LinAccESt(K,J,:)

         DO I = 1,p%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
            LinAccES(K,J,:) = LinAccES(K,J,:) + OtherState%RtHS%PLinVelES(K,J,p%DOFs%PSE(K,I),0,:)*OtherState%QD2T(p%DOFs%PSE(K,I))
         ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      ENDDO             ! J - Blade nodes / elements

   ENDDO          ! K - All blades

   DO J = 1,p%TwrNodes  ! Loop through the tower nodes / elements

      LinAccET(J,:) = OtherState%RtHS%LinAccETt(J,:)
      FTHydro (J,:) = OtherState%RtHS%FTHydrot (J,:)
      MFHydro (J,:) = OtherState%RtHS%MFHydrot (J,:)

      DO I = 1,p%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
         LinAccET(J,:) = LinAccET(J,:) + OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,:)*OtherState%QD2T(p%DOFs%PTE(I))
         FTHydro (J,:) = FTHydro (J,:) + OtherState%RtHS%PFTHydro (J,p%DOFs%PTE(I),  :)*OtherState%QD2T(p%DOFs%PTE(I))
         MFHydro (J,:) = MFHydro (J,:) + OtherState%RtHS%PMFHydro (J,p%DOFs%PTE(I),  :)*OtherState%QD2T(p%DOFs%PTE(I))
      ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

   ENDDO ! J - Tower nodes / elements


      ! Convert the units of the forces and moments from N and N-m
      !    to kN and kN-m:

   FrcONcRt = 0.001*FrcONcRt
   FrcPRot  = 0.001*FrcPRot
   FrcT0Trb = 0.001*FrcT0Trb
   FZHydro  = 0.001*FZHydro
   MomBNcRt = 0.001*MomBNcRt
   MomLPRot = 0.001*MomLPRot
   MomNGnRt = 0.001*MomNGnRt
   MomNTail = 0.001*MomNTail
   MomX0Trb = 0.001*MomX0Trb
   MXHydro  = 0.001*MXHydro
   FrcS0B   = 0.001*FrcS0B
   MomH0B   = 0.001*MomH0B


   !...............................................................................................................................
   ! set the values in the AllOuts array:
   !...............................................................................................................................
      ! Define the output channel specifying the current simulation time:

   y%AllOuts(  Time) = REAL( t, ReKi )


   !IF ( p_FAST%CompAero )  THEN   ! AeroDyn has been used
   !
   !
   !   ! Wind Motions:
   !
   !   HHWndVec(:) = AD_GetUndisturbedWind( REAL(t, ReKi), (/REAL(0.0, ReKi), REAL(0.0, ReKi), p%FASTHH /), ErrStat )
   !
   !   y%AllOuts(  WindVxi) = HHWndVec(1)
   !   y%AllOuts(  WindVyi) = HHWndVec(2)
   !   y%AllOuts(  WindVzi) = HHWndVec(3)
   !   y%AllOuts( TotWindV) = SQRT(   ( y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi) ) &
   !                                + ( y%AllOuts(  WindVyi)*y%AllOuts(  WindVyi) ) &
   !                                + ( y%AllOuts(  WindVzi)*y%AllOuts(  WindVzi) )   )
   !   y%AllOuts( HorWindV) = SQRT(   ( y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi) ) &
   !                                + ( y%AllOuts(  WindVyi)*y%AllOuts(  WindVyi) )   )
   !   y%AllOuts(HorWndDir) = ATAN2( y%AllOuts(  WindVyi), y%AllOuts(  WindVxi) )*R2D
   !   y%AllOuts(VerWndDir) = ATAN2( y%AllOuts(  WindVzi), y%AllOuts( HorWindV) )*R2D
   !
   !
   !   ! Tail Fin Element Aerodynamics:
   !
   !   !y%AllOuts(TFinAlpha) = TFinAOA*R2D
   !   !y%AllOuts(TFinCLift) = TFinCL
   !   !y%AllOuts(TFinCDrag) = TFinCD
   !   !y%AllOuts(TFinDnPrs) = TFinQ
   !   !y%AllOuts(TFinCPFx ) = TFinKFx*0.001
   !   !y%AllOuts(TFinCPFy ) = TFinKFy*0.001
   !   !
   !
   !ELSE

      y%AllOuts(  WindVxi) = 0
      y%AllOuts(  WindVyi) = 0
      y%AllOuts(  WindVzi) = 0
      y%AllOuts( TotWindV) = 0
      y%AllOuts( HorWindV) = 0
      y%AllOuts(HorWndDir) = 0
      y%AllOuts(VerWndDir) = 0

      ! Tail Fin Element Aerodynamics:

      y%AllOuts(TFinAlpha) = 0
      y%AllOuts(TFinCLift) = 0
      y%AllOuts(TFinCDrag) = 0
      y%AllOuts(TFinDnPrs) = 0
      y%AllOuts(TFinCPFx ) = 0
      y%AllOuts(TFinCPFy ) = 0
      
   !ENDIF



      ! Blade (1-3) Tip Motions:

   DO K = 1,p%NumBl
      rSTipPSTip = OtherState%RtHS%rS0S(K,p%TipNode,:) - p%BldFlexL*OtherState%CoordSys%j3(K,:)  ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip) of blade 1.
      rOSTip     = OtherState%RtHS%rS  (K,p%TipNode,:) - OtherState%RtHS%rO                ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip) of blade 1.
      rOSTipxn   =      DOT_PRODUCT( rOSTip, OtherState%CoordSys%d1 )                ! Component of rOSTip directed along the xn-axis.
      rOSTipyn   = -1.0*DOT_PRODUCT( rOSTip, OtherState%CoordSys%d3 )                ! Component of rOSTip directed along the yn-axis.
      rOSTipzn   =      DOT_PRODUCT( rOSTip, OtherState%CoordSys%d2 )                ! Component of rOSTip directed along the zn-axis.

      y%AllOuts(  TipDxc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i1(K,         :) )
      y%AllOuts(  TipDyc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i2(K,         :) )
      y%AllOuts(  TipDzc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i3(K,         :) )
      y%AllOuts(  TipDxb(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%j1(K,         :) )
      y%AllOuts(  TipDyb(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%j2(K,         :) )
      !JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
      y%AllOuts( TipALxb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n1(K,p%BldNodes,:) )
      y%AllOuts( TipALyb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n2(K,p%BldNodes,:) )
      y%AllOuts( TipALzb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n3(K,p%BldNodes,:) )
      y%AllOuts( TipRDxb(K) ) = DOT_PRODUCT( OtherState%RtHS%AngPosHM(K,p%TipNode,:), OtherState%CoordSys%j1(K,         :) )*R2D
      y%AllOuts( TipRDyb(K) ) = DOT_PRODUCT( OtherState%RtHS%AngPosHM(K,p%TipNode,:), OtherState%CoordSys%j2(K,         :) )*R2D
      ! There is no sense computing AllOuts( TipRDzc(K) ) here since it is always zero for FAST simulation results.
      IF ( rOSTipzn > 0.0 )  THEN   ! Tip of blade K is above the yaw bearing.
         y%AllOuts(TipClrnc(K) ) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn + rOSTipzn*rOSTipzn ) ! Absolute distance from the tower top / yaw bearing to the tip of blade 1.
      ELSE                          ! Tip of blade K is below the yaw bearing.
         y%AllOuts(TipClrnc(K) ) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn                     ) ! Perpendicular distance from the yaw axis / tower centerline to the tip of blade 1.
      ENDIF

   END DO !K

      ! Blade (1-3) Local Span Motions:

   DO K = 1,p%NumBl
      DO I = 1, p%NBlGages

         y%AllOuts( SpnALxb(I,K) ) = DOT_PRODUCT( LinAccES(K,p%BldGagNd(I),:), OtherState%CoordSys%n1(K,p%BldGagNd(I),:) )
         y%AllOuts( SpnALyb(I,K) ) = DOT_PRODUCT( LinAccES(K,p%BldGagNd(I),:), OtherState%CoordSys%n2(K,p%BldGagNd(I),:) )
         y%AllOuts( SpnALzb(I,K) ) = DOT_PRODUCT( LinAccES(K,p%BldGagNd(I),:), OtherState%CoordSys%n3(K,p%BldGagNd(I),:) )

         rSPS                    = OtherState%RtHS%rS0S(K,p%BldGagNd(I),:) - p%RNodes(p%BldGagNd(I))*OtherState%CoordSys%j3(K,:)

         y%AllOuts( SpnTDxb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j1(K,:) )
         y%AllOuts( SpnTDyb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j2(K,:) )
         y%AllOuts( SpnTDzb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j3(K,:) )

         y%AllOuts( SpnRDxb(I,K) ) = DOT_PRODUCT( OtherState%RtHS%AngPosHM(K,p%BldGagNd(I),:), OtherState%CoordSys%j1(K,:) )*R2D
         y%AllOuts( SpnRDyb(I,K) ) = DOT_PRODUCT( OtherState%RtHS%AngPosHM(K,p%BldGagNd(I),:), OtherState%CoordSys%j2(K,:) )*R2D
        !y%AllOuts( SpnRDzb(I,K) ) = DOT_PRODUCT( OtherState%RtHS%AngPosHM(K,p%BldGagNd(I),:), OtherState%CoordSys%j3(K,:) )*R2D           ! this is always zero for FAST

      END DO !I
   END DO !K



      ! Blade Pitch Motions:

   y%AllOuts(PtchPMzc1) = OtherState%BlPitch(1)*R2D
   y%AllOuts(PtchPMzc2) = OtherState%BlPitch(2)*R2D
   IF ( p%NumBl == 3 )  THEN ! 3-blader

      y%AllOuts(PtchPMzc3) = OtherState%BlPitch(3)*R2D

   ELSE  ! 2-blader


      ! Teeter Motions:

      y%AllOuts(  TeetPya) =x%QT  (DOF_Teet)*R2D
      y%AllOuts(  TeetVya) =x%QDT (DOF_Teet)*R2D
      y%AllOuts(  TeetAya) =  OtherState%QD2T(DOF_Teet)*R2D

   ENDIF


      ! Shaft Motions:

   y%AllOuts(LSSTipPxa) = MOD( ( x%QT (DOF_GeAz) + x%QT  (DOF_DrTr) + p%AzimB1Up)*R2D  + 90.0, 360.0 ) !bjj: this used IgnoreMod for linearization
   y%AllOuts(LSSGagPxa) = MOD( ( x%QT (DOF_GeAz)                    + p%AzimB1Up)*R2D  + 90.0, 360.0 ) !bjj: this used IgnoreMod for linearization
   y%AllOuts(   LSSTipVxa) =    ( x%QDT (DOF_GeAz) +  x%QDT (DOF_DrTr) )*RPS2RPM
   y%AllOuts(   LSSTipAxa) =    (   OtherState%QD2T(DOF_GeAz) +    OtherState%QD2T(DOF_DrTr) )*R2D
   y%AllOuts(   LSSGagVxa) =      x%QDT (DOF_GeAz)                      *RPS2RPM
   y%AllOuts(   LSSGagAxa) =        OtherState%QD2T(DOF_GeAz)                      *R2D
   y%AllOuts(     HSShftV) = ABS(p%GBRatio)*y%AllOuts(LSSGagVxa)
   y%AllOuts(     HSShftA) = ABS(p%GBRatio)*y%AllOuts(LSSGagAxa)

   IF ( .NOT. EqualRealNos( y%AllOuts(WindVxi), 0.0_ReKi ) )  THEN  ! .TRUE. if the denominator in the following equation is not zero.
      y%AllOuts(TipSpdRat) =      ( x%QDT (DOF_GeAz) + x%QDT (DOF_DrTr) )*p%AvgNrmTpRd / y%AllOuts(  WindVxi)
   ELSE
      y%AllOuts(TipSpdRat) = 0.0
   ENDIF


      ! Nacelle IMU Motions:

   y%AllOuts(NcIMUTVxs) =      DOT_PRODUCT( OtherState%RtHS%LinVelEIMU, OtherState%CoordSys%c1 )
   y%AllOuts(NcIMUTVys) = -1.0*DOT_PRODUCT( OtherState%RtHS%LinVelEIMU, OtherState%CoordSys%c3 )
   y%AllOuts(NcIMUTVzs) =      DOT_PRODUCT( OtherState%RtHS%LinVelEIMU, OtherState%CoordSys%c2 )
   y%AllOuts(NcIMUTAxs) =      DOT_PRODUCT(                 LinAccEIMU, OtherState%CoordSys%c1 )
   y%AllOuts(NcIMUTAys) = -1.0*DOT_PRODUCT(                 LinAccEIMU, OtherState%CoordSys%c3 )
   y%AllOuts(NcIMUTAzs) =      DOT_PRODUCT(                 LinAccEIMU, OtherState%CoordSys%c2 )
   y%AllOuts(NcIMURVxs) =      DOT_PRODUCT( OtherState%RtHS%AngVelER  , OtherState%CoordSys%c1 )*R2D
   y%AllOuts(NcIMURVys) = -1.0*DOT_PRODUCT( OtherState%RtHS%AngVelER  , OtherState%CoordSys%c3 )*R2D
   y%AllOuts(NcIMURVzs) =      DOT_PRODUCT( OtherState%RtHS%AngVelER  , OtherState%CoordSys%c2 )*R2D
   y%AllOuts(NcIMURAxs) =      DOT_PRODUCT(                 AngAccER  , OtherState%CoordSys%c1 )*R2D
   y%AllOuts(NcIMURAys) = -1.0*DOT_PRODUCT(                 AngAccER  , OtherState%CoordSys%c3 )*R2D
   y%AllOuts(NcIMURAzs) =      DOT_PRODUCT(                 AngAccER  , OtherState%CoordSys%c2 )*R2D


      ! Rotor-Furl Motions:

   y%AllOuts( RotFurlP) = x%QT  (DOF_RFrl)*R2D
   y%AllOuts( RotFurlV) = x%QDT (DOF_RFrl)*R2D
   y%AllOuts( RotFurlA) = OtherState%QD2T(DOF_RFrl)*R2D


      ! Tail-Furl Motions:

   y%AllOuts(TailFurlP) = x%QT  (DOF_TFrl)*R2D
   y%AllOuts(TailFurlV) = x%QDT (DOF_TFrl)*R2D
   y%AllOuts(TailFurlA) = OtherState%QD2T(DOF_TFrl)*R2D


      ! Yaw Motions:

   y%AllOuts(   YawPzn) = x%QT  (DOF_Yaw )*R2D
   y%AllOuts(   YawVzn) = x%QDT (DOF_Yaw )*R2D
   y%AllOuts(   YawAzn) = OtherState%QD2T(DOF_Yaw )*R2D


   ! Tower-Top / Yaw Bearing Motions:

   rOPO     = OtherState%RtHS%rT0O - p%TwrFlexL*OtherState%CoordSys%a2 ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).

   y%AllOuts(YawBrTDxp) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%b1 )
   y%AllOuts(YawBrTDyp) = -DOT_PRODUCT(     rOPO, OtherState%CoordSys%b3 )
   y%AllOuts(YawBrTDzp) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%b2 )
   y%AllOuts(YawBrTDxt) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%a1 )
   y%AllOuts(YawBrTDyt) = -DOT_PRODUCT(     rOPO, OtherState%CoordSys%a3 )
   y%AllOuts(YawBrTDzt) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%a2 )
   y%AllOuts(YawBrTAxp) =  DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b1 )
   y%AllOuts(YawBrTAyp) = -DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b3 )
   y%AllOuts(YawBrTAzp) =  DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b2 )
   y%AllOuts(YawBrRDxt) =  DOT_PRODUCT( OtherState%RtHS%AngPosXB, OtherState%CoordSys%a1 )*R2D
   y%AllOuts(YawBrRDyt) = -DOT_PRODUCT( OtherState%RtHS%AngPosXB, OtherState%CoordSys%a3 )*R2D
   ! There is no sense computing y%AllOuts(YawBrRDzt) here since it is always zero for FAST simulation results.
   y%AllOuts(YawBrRVxp) =  DOT_PRODUCT( OtherState%RtHS%AngVelEB, OtherState%CoordSys%b1 )*R2D
   y%AllOuts(YawBrRVyp) = -DOT_PRODUCT( OtherState%RtHS%AngVelEB, OtherState%CoordSys%b3 )*R2D
   y%AllOuts(YawBrRVzp) =  DOT_PRODUCT( OtherState%RtHS%AngVelEB, OtherState%CoordSys%b2 )*R2D
   y%AllOuts(YawBrRAxp) =  DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b1 )*R2D
   y%AllOuts(YawBrRAyp) = -DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b3 )*R2D
   y%AllOuts(YawBrRAzp) =  DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b2 )*R2D


      ! Local Tower Motions:

   DO I = 1, p%NTwGages

      y%AllOuts( TwHtALxt(I) ) =      DOT_PRODUCT( LinAccET(p%TwrGagNd(I),:), OtherState%CoordSys%t1(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtALyt(I) ) = -1.0*DOT_PRODUCT( LinAccET(p%TwrGagNd(I),:), OtherState%CoordSys%t3(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtALzt(I) ) =      DOT_PRODUCT( LinAccET(p%TwrGagNd(I),:), OtherState%CoordSys%t2(p%TwrGagNd(I),:) )

      rTPT                   = OtherState%RtHS%rT0T(p%TwrGagNd(I),:) - p%HNodes(p%TwrGagNd(I))*OtherState%CoordSys%a2(:)

      y%AllOuts( TwHtTDxt(I) ) =      DOT_PRODUCT( rTPT,     OtherState%CoordSys%a1 )
      y%AllOuts( TwHtTDyt(I) ) = -1.0*DOT_PRODUCT( rTPT,     OtherState%CoordSys%a3 )
      y%AllOuts( TwHtTDzt(I) ) =      DOT_PRODUCT( rTPT,     OtherState%CoordSys%a2 )

      y%AllOuts( TwHtRDxt(I) ) =      DOT_PRODUCT( OtherState%RtHS%AngPosXF(p%TwrGagNd(I),:), OtherState%CoordSys%a1 )*R2D  !why is this zero???
      y%AllOuts( TwHtRDyt(I) ) = -1.0*DOT_PRODUCT( OtherState%RtHS%AngPosXF(p%TwrGagNd(I),:), OtherState%CoordSys%a3 )*R2D
   !   y%AllOuts( TwHtRDzt(I) ) =     DOT_PRODUCT( OtherState%RtHS%AngPosXF(p%TwrGagNd(I),:), OtherState%CoordSys%a2 )*R2D  !this will always be 0 in FAST, so no need to calculate


      y%AllOuts( TwHtTPxi(I) ) =      OtherState%RtHS%rT(p%TwrGagNd(I),1)
      y%AllOuts( TwHtTPyi(I) ) = -1.0*OtherState%RtHS%rT(p%TwrGagNd(I),3)
      y%AllOuts( TwHtTPzi(I) ) =      OtherState%RtHS%rT(p%TwrGagNd(I),2) - p%PtfmRef

      y%AllOuts( TwHtRPxi(I) ) =  OtherState%RtHS%AngPosEF(p%TwrGagNd(I),1)*R2D
      y%AllOuts( TwHtRPyi(I) ) = -OtherState%RtHS%AngPosEF(p%TwrGagNd(I),3)*R2D
      y%AllOuts( TwHtRPzi(I) ) =  OtherState%RtHS%AngPosEF(p%TwrGagNd(I),2)*R2D

   END DO !I

      ! Platform Motions:

   y%AllOuts( PtfmTDxt) =  DOT_PRODUCT(       OtherState%RtHS%rZ, OtherState%CoordSys%a1 )
   y%AllOuts( PtfmTDyt) = -DOT_PRODUCT(       OtherState%RtHS%rZ, OtherState%CoordSys%a3 )
   y%AllOuts( PtfmTDzt) =  DOT_PRODUCT(       OtherState%RtHS%rZ, OtherState%CoordSys%a2 )
   y%AllOuts( PtfmTDxi) = x%QT  (DOF_Sg )
   y%AllOuts( PtfmTDyi) = x%QT  (DOF_Sw )
   y%AllOuts( PtfmTDzi) = x%QT  (DOF_Hv )
   y%AllOuts( PtfmTVxt) =  DOT_PRODUCT( OtherState%RtHS%LinVelEZ, OtherState%CoordSys%a1 )
   y%AllOuts( PtfmTVyt) = -DOT_PRODUCT( OtherState%RtHS%LinVelEZ, OtherState%CoordSys%a3 )
   y%AllOuts( PtfmTVzt) =  DOT_PRODUCT( OtherState%RtHS%LinVelEZ, OtherState%CoordSys%a2 )
   y%AllOuts( PtfmTVxi) = x%QDT (DOF_Sg )
   y%AllOuts( PtfmTVyi) = x%QDT (DOF_Sw )
   y%AllOuts( PtfmTVzi) = x%QDT (DOF_Hv )
   y%AllOuts( PtfmTAxt) =  DOT_PRODUCT(                 LinAccEZ, OtherState%CoordSys%a1 )
   y%AllOuts( PtfmTAyt) = -DOT_PRODUCT(                 LinAccEZ, OtherState%CoordSys%a3 )
   y%AllOuts( PtfmTAzt) =  DOT_PRODUCT(                 LinAccEZ, OtherState%CoordSys%a2 )
   y%AllOuts( PtfmTAxi) = OtherState%QD2T(DOF_Sg  )
   y%AllOuts( PtfmTAyi) = OtherState%QD2T(DOF_Sw  )
   y%AllOuts( PtfmTAzi) = OtherState%QD2T(DOF_Hv  )
   y%AllOuts( PtfmRDxi) = x%QT  (DOF_R )*R2D
   y%AllOuts( PtfmRDyi) = x%QT  (DOF_P )*R2D
   y%AllOuts( PtfmRDzi) = x%QT  (DOF_Y )*R2D
   y%AllOuts( PtfmRVxt) =  DOT_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%CoordSys%a1 )*R2D
   y%AllOuts( PtfmRVyt) = -DOT_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%CoordSys%a3 )*R2D
   y%AllOuts( PtfmRVzt) =  DOT_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%CoordSys%a2 )*R2D
   y%AllOuts( PtfmRVxi) = x%QDT (DOF_R )*R2D
   y%AllOuts( PtfmRVyi) = x%QDT (DOF_P )*R2D
   y%AllOuts( PtfmRVzi) = x%QDT (DOF_Y )*R2D
   y%AllOuts( PtfmRAxt) =  DOT_PRODUCT(                 AngAccEX, OtherState%CoordSys%a1 )*R2D
   y%AllOuts( PtfmRAyt) = -DOT_PRODUCT(                 AngAccEX, OtherState%CoordSys%a3 )*R2D
   y%AllOuts( PtfmRAzt) =  DOT_PRODUCT(                 AngAccEX, OtherState%CoordSys%a2 )*R2D
   y%AllOuts( PtfmRAxi) = OtherState%QD2T(DOF_R )*R2D
   y%AllOuts( PtfmRAyi) = OtherState%QD2T(DOF_P )*R2D
   y%AllOuts( PtfmRAzi) = OtherState%QD2T(DOF_Y )*R2D


      ! Nacelle Yaw Error Estimate:

!IF ( p_FAST%CompAero )  THEN   ! AeroDyn has been used  !bjj - removed this
   y%AllOuts(NacYawErr) = y%AllOuts(HorWndDir) - y%AllOuts(YawPzn) - y%AllOuts(YawBrRDzt) - y%AllOuts(PtfmRDzi)




      ! Blade Root Loads:

   DO K=1,p%NumBl
      y%AllOuts( RootFxc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i1(K,:) )
      y%AllOuts( RootFyc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i2(K,:) )
      y%AllOuts( RootFzc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i3(K,:) )
      y%AllOuts( RootFxb(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%j1(K,:) )
      y%AllOuts( RootFyb(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%j2(K,:) )
      y%AllOuts( RootMxc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i1(K,:) )
      y%AllOuts( RootMyc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i2(K,:) )
      y%AllOuts( RootMzc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i3(K,:) )
      y%AllOuts( RootMxb(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%j1(K,:) )
      y%AllOuts( RootMyb(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%j2(K,:) )
   END DO !K


      ! Blade Local Span Loads:

   DO K = 1,p%NumBl
      DO I = 1,p%NBlGages

      ! Initialize FrcMGagB and MomMGagB using the tip brake effects:

         FrcMGagB = OtherState%RtHS%FSTipDrag(K,:) - p%TipMass(K)*( p%Gravity*OtherState%CoordSys%z2 + LinAccES(K,p%TipNode,:) )
         MomMGagB = CROSS_PRODUCT( OtherState%RtHS%rS0S(K,p%TipNode,:) - OtherState%RtHS%rS0S(K,p%BldGagNd(I),:), FrcMGagB )

      ! Integrate to find FrcMGagB and MomMGagB using all of the nodes / elements above the current strain gage location:
         DO J = ( p%BldGagNd(I) + 1 ),p%BldNodes ! Loop through blade nodes / elements above strain gage node

            TmpVec2  = OtherState%RtHS%FSAero(K,J,:) - p%MassB(K,J)*( p%Gravity*OtherState%CoordSys%z2 + LinAccES(K,J,:) )  ! Portion of FrcMGagB associated with element J
            FrcMGagB = FrcMGagB + TmpVec2*p%DRNodes(J)

            TmpVec = CROSS_PRODUCT( OtherState%RtHS%rS0S(K,J,:) - OtherState%RtHS%rS0S(K,p%BldGagNd(I),:), TmpVec2 )           ! Portion of MomMGagB associated with element J
            MomMGagB = MomMGagB + ( TmpVec + OtherState%RtHS%MMAero(K,J,:) )*p%DRNodes(J)

         ENDDO ! J - Blade nodes / elements above strain gage node

      ! Add the effects of 1/2 the strain gage element:
      ! NOTE: for the radius in this calculation, assume that there is no
      !   shortening effect (due to blade bending) within the element.  Thus,
      !   the moment arm for the force is 1/4 of p%DRNodes() and the element
      !   length is 1/2 of p%DRNodes().

         TmpVec2  = OtherState%RtHS%FSAero(K,p%BldGagNd(I),:) - p%MassB(K,p%BldGagNd(I))* ( p%Gravity*OtherState%CoordSys%z2 + LinAccES(K,p%BldGagNd(I),:) ) ! Portion of FrcMGagB associated with 1/2 of the strain gage element
         FrcMGagB = FrcMGagB + TmpVec2 * 0.5 * p%DRNodes(p%BldGagNd(I))                                                    ! Portion of FrcMGagB associated with 1/2 of the strain gage element
         FrcMGagB = 0.001*FrcMGagB           ! Convert the local force to kN


         TmpVec = CROSS_PRODUCT( ( 0.25*p%DRNodes(p%BldGagNd(I)) )*OtherState%CoordSys%j3(K,:), TmpVec2 )                              ! Portion of MomMGagB associated with 1/2 of the strain gage element

         MomMGagB = MomMGagB + ( TmpVec + OtherState%RtHS%MMAero(K,p%BldGagNd(I),:) )* ( 0.5 *p%DRNodes(p%BldGagNd(I)) )
         MomMGagB = 0.001*MomMGagB           ! Convert the local moment to kN-m


         y%AllOuts(SpnFLxb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n1(K,p%BldGagNd(I),:) )
         y%AllOuts(SpnFLyb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n2(K,p%BldGagNd(I),:) )
         y%AllOuts(SpnFLzb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n3(K,p%BldGagNd(I),:) )

         y%AllOuts(SpnMLxb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n1(K,p%BldGagNd(I),:) )
         y%AllOuts(SpnMLyb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n2(K,p%BldGagNd(I),:) )
         y%AllOuts(SpnMLzb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n3(K,p%BldGagNd(I),:) )
      END DO ! I
   END DO ! K



      ! Hub and Rotor Loads:

   ComDenom = 0.5*p%AirDens*p%ProjArea*y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi)   ! Common denominator used in several expressions

   y%AllOuts(LSShftFxa) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e1 )
   y%AllOuts(LSShftFya) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e2 )
   y%AllOuts(LSShftFza) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e3 )
   y%AllOuts(LSShftFys) = -DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%c3 )
   y%AllOuts(LSShftFzs) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%c2 )
   y%AllOuts(LSShftMxa) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e1 )
   y%AllOuts(LSSTipMya) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e2 )
   y%AllOuts(LSSTipMza) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e3 )
   y%AllOuts(LSSTipMys) = -DOT_PRODUCT( MomLPRot, OtherState%CoordSys%c3 )
   y%AllOuts(LSSTipMzs) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%c2 )

   IF ( .NOT. EqualRealNos( y%AllOuts(LSShftFxa), 0.0_ReKi ) )   THEN ! .TRUE. if the denominator in the following equations is not zero.

      CThrstys = -y%AllOuts(LSSTipMzs)/y%AllOuts(LSShftFxa)  ! Estimate of the ys-location of the center of thrust
      CThrstzs =  y%AllOuts(LSSTipMys)/y%AllOuts(LSShftFxa)  ! Estimate of the zs-location of the center of thrust

      y%AllOuts(CThrstAzm) = MOD( ( ATAN2( -CThrstzs, -CThrstys ) + p%AzimB1Up )*R2D + 360.0 + 90.0, 360.0 )  !bjj: IgnoreMod was used for linearization... perhaps these outputs should not use the MOD function; only WriteOutputs should have that...
      y%AllOuts(CThrstRad) = MIN( 1.0, SQRT( CThrstys*CThrstys + CThrstzs*CThrstzs )/p%AvgNrmTpRd )

   ELSE

      y%AllOuts(CThrstAzm) = 0.0
      y%AllOuts(CThrstRad) = 0.0

   ENDIF

   y%AllOuts(   RotPwr) = ( x%QDT(DOF_GeAz) + x%QDT(DOF_DrTr) )*y%AllOuts(LSShftMxa)
      
   IF ( .NOT. EqualRealNos( ComDenom, 0.0_ReKi ) )  THEN   ! .TRUE. if the denominator in the following equations is not zero.
   
      y%AllOuts( RotCq) = 1000.0*y%AllOuts(LSShftMxa) / ( ComDenom*p%TipRad )
      y%AllOuts( RotCp) = 1000.0*y%AllOuts(   RotPwr) / ( ComDenom*y%AllOuts(  WindVxi) )
      y%AllOuts( RotCt) = 1000.0*y%AllOuts(LSShftFxa) /   ComDenom
   
   ELSE
   
      y%AllOuts( RotCq) = 0.0
      y%AllOuts( RotCp) = 0.0
      y%AllOuts( RotCt) = 0.0

   ENDIF


      ! Shaft Strain Gage Loads:

   y%AllOuts(LSSGagMya) = y%AllOuts(LSSTipMya) + p%ShftGagL*y%AllOuts(LSShftFza)
   y%AllOuts(LSSGagMza) = y%AllOuts(LSSTipMza) - p%ShftGagL*y%AllOuts(LSShftFya)
   y%AllOuts(LSSGagMys) = y%AllOuts(LSSTipMys) + p%ShftGagL*y%AllOuts(LSShftFzs)
   y%AllOuts(LSSGagMzs) = y%AllOuts(LSSTipMzs) - p%ShftGagL*y%AllOuts(LSShftFys)


      ! Generator and High-Speed Shaft Loads:

   y%AllOuts( HSShftTq) = y%AllOuts(LSShftMxa)*OtherState%RtHS%GBoxEffFac/ABS(p%GBRatio)
   y%AllOuts(HSShftPwr) = y%AllOuts( HSShftTq)*ABS(p%GBRatio)*x%QDT(DOF_GeAz)
   !y%AllOuts(  HSSBrTq) = 0.001*HSSBrTrq
   !y%AllOuts(    GenTq) = 0.001*GenTrq
   !y%AllOuts(   GenPwr) = 0.001*ElecPwr

   y%AllOuts(  HSSBrTq) = 0
   y%AllOuts(    GenTq) = 0
   y%AllOuts(   GenPwr) = 0
   
   
   IF ( .NOT. EqualRealNos( ComDenom, 0.0_ReKi ) )  THEN  ! .TRUE. if the denominator in the following equations is not zero (ComDenom is the same as it is calculated above).

      y%AllOuts( HSShftCq) = 1000.0*y%AllOuts( HSShftTq) / ( ComDenom*p%TipRad )
      y%AllOuts( HSShftCp) = 1000.0*y%AllOuts(HSShftPwr) / ( ComDenom*y%AllOuts(  WindVxi) )
      y%AllOuts(    GenCq) = 1000.0*y%AllOuts(    GenTq) / ( ComDenom*p%TipRad )
      y%AllOuts(    GenCp) = 1000.0*y%AllOuts(   GenPwr) / ( ComDenom*y%AllOuts(  WindVxi) )

   ELSE

      y%AllOuts( HSShftCq) = 0.0
      y%AllOuts( HSShftCp) = 0.0
      y%AllOuts(    GenCq) = 0.0
      y%AllOuts(    GenCp) = 0.0

   ENDIF


      ! Rotor-Furl Axis Loads:

   y%AllOuts(RFrlBrM  ) =  DOT_PRODUCT( MomNGnRt, OtherState%CoordSys%rfa )


      ! Tail-Furl Axis Loads:

   y%AllOuts(TFrlBrM  ) =  DOT_PRODUCT( MomNTail, OtherState%CoordSys%tfa )


      ! Tower-Top / Yaw Bearing Loads:

   y%AllOuts( YawBrFxn) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d1 )
   y%AllOuts( YawBrFyn) = -DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d3 )
   y%AllOuts( YawBrFzn) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d2 )
   y%AllOuts( YawBrFxp) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%b1 )
   y%AllOuts( YawBrFyp) = -DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%b3 )
   y%AllOuts( YawBrMxn) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d1 )
   y%AllOuts( YawBrMyn) = -DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d3 )
   y%AllOuts( YawBrMzn) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d2 )
   y%AllOuts( YawBrMxp) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%b1 )
   y%AllOuts( YawBrMyp) = -DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%b3 )


      ! Tower Base Loads:

   y%AllOuts( TwrBsFxt) =  DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a1 )
   y%AllOuts( TwrBsFyt) = -DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a3 )
   y%AllOuts( TwrBsFzt) =  DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a2 )
   y%AllOuts( TwrBsMxt) =  DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a1 )
   y%AllOuts( TwrBsMyt) = -DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a3 )
   y%AllOuts( TwrBsMzt) =  DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a2 )


      ! Local Tower Loads:

   FrcONcRt = 1000.0*FrcONcRt ! Convert the units of these forces and moments
   MomBNcRt = 1000.0*MomBNcRt ! from kN and kN-m back to N and N-m, respectively.

   DO I=1,p%NTwGages

      ! Initialize FrcFGagT and MomFGagT using the tower-top and yaw bearing mass effects:
      FrcFGagT = FrcONcRt - p%YawBrMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEO )
      MomFGagT = CROSS_PRODUCT( OtherState%RtHS%rZO - OtherState%RtHS%rZT(p%TwrGagNd(I),:), FrcFGagT )
      MomFGagT = MomFGagT + MomBNcRt

      ! Integrate to find FrcFGagT and MomFGagT using all of the nodes / elements above the current strain gage location:
      DO J = ( p%TwrGagNd(I) + 1 ),p%TwrNodes ! Loop through tower nodes / elements above strain gage node
         TmpVec2  = OtherState%RtHS%FTAero(J,:) + FTHydro(J,:) - p%MassT(J)*( p%Gravity*OtherState%CoordSys%z2 + LinAccET(J,:) )           ! Portion of FrcFGagT associated with element J
         FrcFGagT = FrcFGagT + TmpVec2*p%DHNodes(J)

         TmpVec = CROSS_PRODUCT( OtherState%RtHS%rZT(J,:) - OtherState%RtHS%rZT(p%TwrGagNd(I),:), TmpVec2 )                          ! Portion of MomFGagT associated with element J
         MomFGagT = MomFGagT + ( TmpVec + OtherState%RtHS%MFAero(J,:) + MFHydro(J,:) )*p%DHNodes(J)
      ENDDO ! J -Tower nodes / elements above strain gage node

      ! Add the effects of 1/2 the strain gage element:
      ! NOTE: for the radius in this calculation, assume that there is no shortening
      !   effect (due to tower bending) within the element.  Thus, the moment arm
      !   for the force is 1/4 of DHNodes() and the element length is 1/2 of DHNodes().

      TmpVec2  = OtherState%RtHS%FTAero(p%TwrGagNd(I),:) + FTHydro(p%TwrGagNd(I),:) - p%MassT(p%TwrGagNd(I))*( p%Gravity*OtherState%CoordSys%z2 + LinAccET(p%TwrGagNd(I),:))

      FrcFGagT = FrcFGagT + TmpVec2 * 0.5 * p%DHNodes(p%TwrGagNd(I))
      FrcFGagT = 0.001*FrcFGagT  ! Convert the local force to kN

      TmpVec = CROSS_PRODUCT( ( 0.25*p%DHNodes( p%TwrGagNd(I)) )*OtherState%CoordSys%a2, TmpVec2 )              ! Portion of MomFGagT associated with 1/2 of the strain gage element
      TmpVec   = TmpVec   + OtherState%RtHS%MFAero(p%TwrGagNd(I),:) + MFHydro(p%TwrGagNd(I),:)
      MomFGagT = MomFGagT + TmpVec * 0.5 * p%DHNodes(p%TwrGagNd(I))
      MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

      y%AllOuts( TwHtFLxt(I) ) =     DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t1(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtFLyt(I) ) = -1.*DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t3(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtFLzt(I) ) =     DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t2(p%TwrGagNd(I),:) )

      y%AllOuts( TwHtMLxt(I) ) =     DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t1(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtMLyt(I) ) = -1.*DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t3(p%TwrGagNd(I),:) )
      y%AllOuts( TwHtMLzt(I) ) =     DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t2(p%TwrGagNd(I),:) )

   END DO


      ! Platform Loads:

   y%AllOuts(  PtfmFxt) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%a1 )
   y%AllOuts(  PtfmFyt) = -DOT_PRODUCT( FZHydro, OtherState%CoordSys%a3 )
   y%AllOuts(  PtfmFzt) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%a2 )
   y%AllOuts(  PtfmFxi) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%z1 )
   y%AllOuts(  PtfmFyi) = -DOT_PRODUCT( FZHydro, OtherState%CoordSys%z3 )
   y%AllOuts(  PtfmFzi) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%z2 )
   y%AllOuts(  PtfmMxt) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%a1 )
   y%AllOuts(  PtfmMyt) = -DOT_PRODUCT( MXHydro, OtherState%CoordSys%a3 )
   y%AllOuts(  PtfmMzt) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%a2 )
   y%AllOuts(  PtfmMxi) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%z1 )
   y%AllOuts(  PtfmMyi) = -DOT_PRODUCT( MXHydro, OtherState%CoordSys%z3 )
   y%AllOuts(  PtfmMzi) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%z2 )


      ! Internal p%DOFs outputs:

   y%AllOuts( Q_B1E1   ) = x%QT(   DOF_BE(1,1) )
   y%AllOuts( Q_B2E1   ) = x%QT(   DOF_BE(2,1) )
   y%AllOuts( Q_B1F1   ) = x%QT(   DOF_BF(1,1) )
   y%AllOuts( Q_B2F1   ) = x%QT(   DOF_BF(2,1) )
   y%AllOuts( Q_B1F2   ) = x%QT(   DOF_BF(1,2) )
   y%AllOuts( Q_B2F2   ) = x%QT(   DOF_BF(2,2) )
   y%AllOuts( Q_DrTr   ) = x%QT(   DOF_DrTr    )
   y%AllOuts( Q_GeAz   ) = x%QT(   DOF_GeAz    )
   y%AllOuts( Q_RFrl   ) = x%QT(   DOF_RFrl    )
   y%AllOuts( Q_TFrl   ) = x%QT(   DOF_TFrl    )
   y%AllOuts( Q_Yaw    ) = x%QT(   DOF_Yaw     )
   y%AllOuts( Q_TFA1   ) = x%QT(   DOF_TFA1    )
   y%AllOuts( Q_TSS1   ) = x%QT(   DOF_TSS1    )
   y%AllOuts( Q_TFA2   ) = x%QT(   DOF_TFA2    )
   y%AllOuts( Q_TSS2   ) = x%QT(   DOF_TSS2    )
   y%AllOuts( Q_Sg     ) = x%QT(   DOF_Sg      )
   y%AllOuts( Q_Sw     ) = x%QT(   DOF_Sw      )
   y%AllOuts( Q_Hv     ) = x%QT(   DOF_Hv      )
   y%AllOuts( Q_R      ) = x%QT(   DOF_R       )
   y%AllOuts( Q_P      ) = x%QT(   DOF_P       )
   y%AllOuts( Q_Y      ) = x%QT(   DOF_Y       )

   y%AllOuts( QD_B1E1  ) = x%QDT(  DOF_BE(1,1) )
   y%AllOuts( QD_B2E1  ) = x%QDT(  DOF_BE(2,1) )
   y%AllOuts( QD_B1F1  ) = x%QDT(  DOF_BF(1,1) )
   y%AllOuts( QD_B2F1  ) = x%QDT(  DOF_BF(2,1) )
   y%AllOuts( QD_B1F2  ) = x%QDT(  DOF_BF(1,2) )
   y%AllOuts( QD_B2F2  ) = x%QDT(  DOF_BF(2,2) )
   y%AllOuts( QD_DrTr  ) = x%QDT(  DOF_DrTr    )
   y%AllOuts( QD_GeAz  ) = x%QDT(  DOF_GeAz    )
   y%AllOuts( QD_RFrl  ) = x%QDT(  DOF_RFrl    )
   y%AllOuts( QD_TFrl  ) = x%QDT(  DOF_TFrl    )
   y%AllOuts( QD_Yaw   ) = x%QDT(  DOF_Yaw     )
   y%AllOuts( QD_TFA1  ) = x%QDT(  DOF_TFA1    )
   y%AllOuts( QD_TSS1  ) = x%QDT(  DOF_TSS1    )
   y%AllOuts( QD_TFA2  ) = x%QDT(  DOF_TFA2    )
   y%AllOuts( QD_TSS2  ) = x%QDT(  DOF_TSS2    )
   y%AllOuts( QD_Sg    ) = x%QDT(  DOF_Sg      )
   y%AllOuts( QD_Sw    ) = x%QDT(  DOF_Sw      )
   y%AllOuts( QD_Hv    ) = x%QDT(  DOF_Hv      )
   y%AllOuts( QD_R     ) = x%QDT(  DOF_R       )
   y%AllOuts( QD_P     ) = x%QDT(  DOF_P       )
   y%AllOuts( QD_Y     ) = x%QDT(  DOF_Y       )   

   y%AllOuts( QD2_B1E1 ) = OtherState%QD2T( DOF_BE(1,1) )
   y%AllOuts( QD2_B2E1 ) = OtherState%QD2T( DOF_BE(2,1) )
   y%AllOuts( QD2_B1F1 ) = OtherState%QD2T( DOF_BF(1,1) )
   y%AllOuts( QD2_B2F1 ) = OtherState%QD2T( DOF_BF(2,1) )
   y%AllOuts( QD2_B1F2 ) = OtherState%QD2T( DOF_BF(1,2) )
   y%AllOuts( QD2_B2F2 ) = OtherState%QD2T( DOF_BF(2,2) )
   y%AllOuts( QD2_DrTr ) = OtherState%QD2T( DOF_DrTr    )
   y%AllOuts( QD2_GeAz ) = OtherState%QD2T( DOF_GeAz    )
   y%AllOuts( QD2_RFrl ) = OtherState%QD2T( DOF_RFrl    )
   y%AllOuts( QD2_TFrl ) = OtherState%QD2T( DOF_TFrl    )
   y%AllOuts( QD2_Yaw  ) = OtherState%QD2T( DOF_Yaw     )
   y%AllOuts( QD2_TFA1 ) = OtherState%QD2T( DOF_TFA1    )
   y%AllOuts( QD2_TSS1 ) = OtherState%QD2T( DOF_TSS1    )
   y%AllOuts( QD2_TFA2 ) = OtherState%QD2T( DOF_TFA2    )
   y%AllOuts( QD2_TSS2 ) = OtherState%QD2T( DOF_TSS2    )
   y%AllOuts( QD2_Sg   ) = OtherState%QD2T( DOF_Sg      )
   y%AllOuts( QD2_Sw   ) = OtherState%QD2T( DOF_Sw      )
   y%AllOuts( QD2_Hv   ) = OtherState%QD2T( DOF_Hv      )
   y%AllOuts( QD2_R    ) = OtherState%QD2T( DOF_R       )
   y%AllOuts( QD2_P    ) = OtherState%QD2T( DOF_P       )
   y%AllOuts( QD2_Y    ) = OtherState%QD2T( DOF_Y       )

      
   IF ( p%NumBl > 2 ) THEN
      y%AllOuts( Q_B3E1   ) = x%QT(   DOF_BE(3,1) )
      y%AllOuts( Q_B3F1   ) = x%QT(   DOF_BF(3,1) )
      y%AllOuts( Q_B3F2   ) = x%QT(   DOF_BF(3,2) )

      y%AllOuts( QD_B3E1  ) = x%QDT(  DOF_BE(3,1) )
      y%AllOuts( QD_B3F1  ) = x%QDT(  DOF_BF(3,1) )
      y%AllOuts( QD_B3F2  ) = x%QDT(  DOF_BF(3,2) )

      y%AllOuts( QD2_B3E1 ) = OtherState%QD2T( DOF_BE(3,1) )
      y%AllOuts( QD2_B3F1 ) = OtherState%QD2T( DOF_BF(3,1) )
      y%AllOuts( QD2_B3F2 ) = OtherState%QD2T( DOF_BF(3,2) )
   ELSE
      y%AllOuts( Q_Teet   ) = x%QT(            DOF_Teet    )
      y%AllOuts( QD_Teet  ) = x%QDT(           DOF_Teet    )
      y%AllOuts( QD2_Teet ) = OtherState%QD2T( DOF_Teet    )
   END IF
      
   !...............................................................................................................................   
   ! Place the selected output channels into the WriteOutput(:) array with the proper sign:
   !...............................................................................................................................   

   DO I = 0,p%NumOuts  ! Loop through all selected output channels

      y%WriteOutput(I) = p%OutParam(I)%SignM * y%AllOuts( p%OutParam(I)%Indx )

   ENDDO             ! I - All selected output channels



   RETURN

END SUBROUTINE ED_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                   INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(ED_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(ED_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(ED_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(ED_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(ED_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(ED_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(ED_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

!      dxdt%DummyContState = 0


END SUBROUTINE ED_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                   INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(ED_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(ED_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(ED_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(ED_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                  !   Output: Discrete states at Time + Interval
      TYPE(ED_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(ED_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE ED_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                   INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(ED_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(ED_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(ED_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(ED_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(ED_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(ED_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(ED_ConstraintStateType), INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                  !     the input values described above
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE ED_CalcConstrStateResidual
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! WE ARE NOT YET IMPLEMENTING THE JACOBIANS...

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_ReadInput( InputFileName, MeshFile, InputFileData, ReadAdmVals, OutFileRoot, ErrStat, ErrMsg )
! This subroutine reads the input file and stores all the data in the ED_InputFile structure.
! It does not perform data validation.
!..................................................................................................................................

      ! Passed variables

   CHARACTER(*), INTENT(IN)               :: InputFileName  ! Name of the input file
   CHARACTER(*), INTENT(IN)               :: MeshFile       ! File that contains the blade mesh information (AeroDyn input file for now) -- later this info will be defined in one of the ED input files.
   CHARACTER(*), INTENT(IN)               :: OutFileRoot    ! The rootname of all the output files written by this routine.

   !BJJ MODIFIED HERE ONLY FOR TESTING:
!   TYPE(ED_InputFile),   INTENT(OUT)      :: InputFileData  ! Data stored in the module's input file
   TYPE(ED_InputFile),   INTENT(inOUT)      :: InputFileData  ! Data stored in the module's input file

   INTEGER(IntKi),       INTENT(OUT)      :: ErrStat        ! The error status code
   LOGICAL,              INTENT(IN)       :: ReadAdmVals    ! Determines if we should read the Adams-only values
   CHARACTER(*),         INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! local variables

   INTEGER(IntKi)                         :: UnIn           ! Unit number for the input file
   INTEGER(IntKi)                         :: UnEcho         ! Unit number for the echo file
   INTEGER(IntKi)                         :: ErrStat2       ! The error status code
   CHARACTER(LEN(ErrMsg))                 :: ErrMsg2        ! The error message, if an error occurred

   CHARACTER(1024)                        :: BldFile(MaxBl) ! File that contains the blade information (specified in the primary input file)
   CHARACTER(1024)                        :: FurlFile       ! File that contains the furl information (specified in the primary input file)
   CHARACTER(1024)                        :: TwrFile        ! File that contains the tower information (specified in the primary input file)
   
      ! initialize values: 
   
   ErrStat = ErrID_None
   ErrMsg  = ''

   
      ! get the primary/platform input-file data
      ! sets UnEcho, BldFile, FurlFile, TwrFile
   
   CALL ReadPrimaryFile( InputFileName, InputFileData, BldFile, FurlFile, TwrFile, OutFileRoot, UnEcho, ErrStat2, ErrMsg2 )
      CALL CheckError(ErrStat2,ErrMsg2)
      IF ( ErrStat >= AbortErrLev ) RETURN
      

      ! get the furling input-file data
   InputFileData%Furling = .FALSE.              ! Furling is not supported in this version of ElastoDyn
      
   IF ( InputFileData%Furling )  THEN   
      CALL ReadFurlFile( FurlFile, InputFileData, UnEcho, ErrStat2, ErrMsg2 )
         CALL CheckError(ErrStat2,ErrMsg2)
         IF ( ErrStat >= AbortErrLev ) RETURN
   ELSE   ! initialize all of the data that would be read by ReadFurlFile()       
      InputFileData%RFrlDOF   = .FALSE.
      InputFileData%TFrlDOF   = .FALSE.
      InputFileData%RotFurl   = 0.0_ReKi  ! Radians
      InputFileData%TailFurl  = 0.0_ReKi
      InputFileData%Yaw2Shft  = 0.0
      InputFileData%ShftSkew  = 0.0
      InputFileData%RFrlCMxn  = 0.0
      InputFileData%RFrlCMyn  = 0.0
      InputFileData%RFrlCMzn  = 0.0
      InputFileData%BoomCMxn  = 0.0
      InputFileData%BoomCMyn  = 0.0
      InputFileData%BoomCMzn  = 0.0
      InputFileData%TFinCMxn  = 0.0
      InputFileData%TFinCMyn  = 0.0
      InputFileData%TFinCMzn  = 0.0
      InputFileData%TFinCPxn  = 0.0
      InputFileData%TFinCPyn  = 0.0
      InputFileData%TFinCPzn  = 0.0
      InputFileData%TFinSkew  = 0.0
      InputFileData%TFinTilt  = 0.0
      InputFileData%TFinBank  = 0.0
      InputFileData%RFrlPntxn = 0.0
      InputFileData%RFrlPntyn = 0.0
      InputFileData%RFrlPntzn = 0.0
      InputFileData%RFrlSkew  = 0.0
      InputFileData%RFrlTilt  = 0.0
      InputFileData%TFrlPntxn = 0.0
      InputFileData%TFrlPntyn = 0.0
      InputFileData%TFrlPntzn = 0.0
      InputFileData%TFrlSkew  = 0.0
      InputFileData%TFrlTilt  = 0.0
      InputFileData%RFrlMass  = 0.0
      InputFileData%BoomMass  = 0.0
      InputFileData%TFinMass  = 0.0
      InputFileData%RFrlIner  = 0.0
      InputFileData%TFrlIner  = 0.0
      InputFileData%RFrlMod   = 0
      InputFileData%RFrlSpr   = 0.0
      InputFileData%RFrlDmp   = 0.0
      InputFileData%RFrlCDmp  = 0.0
      InputFileData%RFrlUSSP  = 0.0
      InputFileData%RFrlDSSP  = 0.0
      InputFileData%RFrlUSSpr = 0.0
      InputFileData%RFrlDSSpr = 0.0
      InputFileData%RFrlUSDP  = 0.0
      InputFileData%RFrlDSDP  = 0.0
      InputFileData%RFrlUSDmp = 0.0
      InputFileData%RFrlDSDmp = 0.0
      InputFileData%TFrlMod   = 0
      InputFileData%TFrlSpr   = 0.0
      InputFileData%TFrlDmp   = 0.0
      InputFileData%TFrlCDmp  = 0.0
      InputFileData%TFrlUSSP  = 0.0
      InputFileData%TFrlDSSP  = 0.0
      InputFileData%TFrlUSSpr = 0.0
      InputFileData%TFrlDSSpr = 0.0
      InputFileData%TFrlUSDP  = 0.0
      InputFileData%TFrlDSDP  = 0.0
      InputFileData%TFrlUSDmp = 0.0
      InputFileData%TFrlDSDmp = 0.0
   END IF
   
   
      ! get the blade input-file data (from blade and mesh files)

   CALL ReadBladeInputs ( BldFile, MeshFile, ReadAdmVals, InputFileData, UnEcho, ErrStat2, ErrMsg2 )
      CALL CheckError(ErrStat2,ErrMsg2)
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      
      ! get the tower input-file data
      
   CALL ReadTowerFile( TwrFile, InputFileData, ReadAdmVals, UnEcho,  ErrStat2, ErrMsg2 )
      CALL CheckError(ErrStat2,ErrMsg2)
      IF ( ErrStat >= AbortErrLev ) RETURN
   
      
      ! close the echo file (if opened)
      
   IF ( UnEcho > 0 ) CLOSE( UnEcho )
        

CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            IF ( UnEcho > 0 ) CLOSE( UnEcho )
         END IF

      END IF


   END SUBROUTINE CheckError     

END SUBROUTINE ED_ReadInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_ValidateInput( InputFileData, ErrStat, ErrMsg )
! This subroutine validates the input file data
!..................................................................................................................................

   TYPE(ED_InputFile),       INTENT(IN)       :: InputFileData       ! Data stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat             ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg              ! The error message, if an error occurred

      ! Local variables:
   INTEGER(IntKi)                             :: I                   ! Loop counter
   INTEGER(IntKi)                             :: K                   ! Blade number
   INTEGER(IntKi)                             :: ErrStat2            ! Temporary error ID
   LOGICAL                                    :: ReadAdmVals         ! determines if an Adams model will be created (do we read/check all the inputs?)
   LOGICAL                                    :: ReadFile            ! determines if an input file for a blade is the same as the file for the previous blade
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2             ! Temporary message describing error


      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''


  
      ! validate the primary input data
   CALL ValidatePrimaryData( InputFileData, ErrStat2, ErrMsg2 )   
      CALL CheckError( ErrStat2, ErrMsg2 )
   
      
      ! validate the furling input data   
   CALL ValidateFurlData ( InputFileData, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      
   
      ! validate the blade input data
   DO K = 1,InputFileData%NumBl         
      CALL ValidateBladeData ( InputFileData%InpBl(K), ErrStat2, ErrMsg2 )
         CALL CheckError( ErrStat2, ' Errors in blade '//TRIM(Num2LStr(K))//' input data: '//NewLine//TRIM(ErrMsg2) )
   END DO
   
      
      ! validate the tower input data
   CALL ValidateTowerData ( InputFileData, ErrStat, ErrMsg )
      CALL CheckError( ErrStat2, ErrMsg2 )
  
   
      ! validate the Output parameters:
  ! CALL ChckOutLst( InputFileData%OutList, p, ErrStat, ErrMsg )
      
      
   


CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

      END IF


   END SUBROUTINE CheckError     
END SUBROUTINE ED_ValidateInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_SetParameters( InputFileData, p, ErrStat, ErrMsg )
! This subroutine sets the parameters, based on the data stored in InputFileData
!..................................................................................................................................

   TYPE(ED_InputFile),       INTENT(IN)       :: InputFileData  ! Data stored in the module's input file
   TYPE(ED_ParameterType),   INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! Local variables
   INTEGER(IntKi)                             :: K              ! Loop counter (for blades)
   INTEGER(IntKi)                             :: ErrStat2       ! Temporary error ID
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2        ! Temporary message describing error

      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''

      
   
      ! Set parameters from primary input file        
   CALL SetPrimaryParameters( p, InputFileData, ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
         
   p%DT24 = p%DT/24.0_DbKi    ! Time-step parameter needed for Solver().
      
   
      ! Set furling parameters      
   CALL SetFurlParameters( p, InputFileData, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   

      ! Set blade parameters      
   CALL SetBladeParameters( p, InputFileData%InpBl, InputFileData%InpBlMesh, ErrStat2, ErrMsg2 )      
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      
      ! Set tower parameters      
   CALL SetTowerParameters( p, InputFileData, ErrStat2, ErrMsg2 ) ! It requires p%TwrFlexL, and p%TwrNodes to be set first.
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
      
      ! Set the remaining (calculuated) parameters (basically the former Coeff routine)
   CALL SetOtherParameters( p, InputFileData, ErrStat2, ErrMsg2 ) ! requires MANY things to be set first
      
   

CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
         END IF

      END IF


   END SUBROUTINE CheckError

END SUBROUTINE ED_SetParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_DOFparameters( InputFileData, p, ErrStat, ErrMsg )
! This subroutine initializes the ActiveDOF data type as well as the variables related to DOFs, including p%NAug and p%NDOF.
! It assumes that p%NumBl is set.
!..................................................................................................................................

!   TYPE(ED_ActiveDOFs),      INTENT(INOUT)    :: DOFs           ! ActiveDOF data
   TYPE(ED_InputFile),       INTENT(IN)       :: InputFileData  ! Data stored in the module's input file
   TYPE(ED_ParameterType),   INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! Local variables
   INTEGER(IntKi)                             :: K              ! Loop counter (for blades)

      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''


   IF ( p%NumBl == 2 )  THEN
      p%NDOF = 22
   ELSE
      p%NDOF = 24
   ENDIF
   
   p%NAug = p%NDOF + 1

   ! ...........................................................................................................................
   ! allocate and set DOF_Flag and DOF_Desc   
   ! ...........................................................................................................................
   CALL AllocAry( p%DOF_Flag, p%NDOF,   'DOF_Flag',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%DOF_Desc, p%NDOF,   'DOF_Desc',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
   
   IF ( p%NumBl == 2 )  THEN ! the 3rd blade overwrites the DOF_Teet position of the array, so don't use an "ELSE" for this statement
      p%DOF_Flag(DOF_Teet) = InputFileData%TeetDOF   
      p%DOF_Desc(DOF_Teet) = 'Hub teetering DOF (internal DOF index = DOF_Teet)'  
   END IF ! 
   

   DO K = 1,p%NumBl   
      p%DOF_Flag( DOF_BF(K,1) ) = InputFileData%FlapDOF1      
      p%DOF_Desc( DOF_BF(K,1) ) = '1st flapwise bending-mode DOF of blade '//TRIM(Num2LStr( K ))// &
                                  ' (internal DOF index = DOF_BF('         //TRIM(Num2LStr( K ))//',1))'

      p%DOF_Flag( DOF_BE(K,1) ) = InputFileData%EdgeDOF
      p%DOF_Desc( DOF_BE(K,1) ) = '1st edgewise bending-mode DOF of blade '//TRIM(Num2LStr( K ))// &
                                  ' (internal DOF index = DOF_BE('         //TRIM(Num2LStr( K ))//',1))'

      p%DOF_Flag( DOF_BF(K,2) ) = InputFileData%FlapDOF2
      p%DOF_Desc( DOF_BF(K,2) ) = '2nd flapwise bending-mode DOF of blade '//TRIM(Num2LStr( K ))// &
                                  ' (internal DOF index = DOF_BF('         //TRIM(Num2LStr( K ))//',2))'
   ENDDO          ! K - All blades   
       
   p%DOF_Flag(DOF_DrTr) = InputFileData%DrTrDOF
   p%DOF_Desc(DOF_DrTr) = 'Drivetrain rotational-flexibility DOF (internal DOF index = DOF_DrTr)'
   p%DOF_Flag(DOF_GeAz) = InputFileData%GenDOF
   p%DOF_Desc(DOF_GeAz) = 'Variable speed generator DOF (internal DOF index = DOF_GeAz)'
   p%DOF_Flag(DOF_RFrl) = InputFileData%RFrlDOF
   p%DOF_Desc(DOF_RFrl) = 'Rotor-furl DOF (internal DOF index = DOF_RFrl)'
   p%DOF_Flag(DOF_TFrl) = InputFileData%TFrlDOF
   p%DOF_Desc(DOF_TFrl) = 'Tail-furl DOF (internal DOF index = DOF_TFrl)'
   p%DOF_Flag(DOF_Yaw ) = InputFileData%YawDOF
   p%DOF_Desc(DOF_Yaw ) = 'Nacelle yaw DOF (internal DOF index = DOF_Yaw)'
   p%DOF_Flag(DOF_TFA1) = InputFileData%TwFADOF1
   p%DOF_Desc(DOF_TFA1) = '1st tower fore-aft bending mode DOF (internal DOF index = DOF_TFA1)'
   p%DOF_Flag(DOF_TSS1) = InputFileData%TwSSDOF1
   p%DOF_Desc(DOF_TSS1) = '1st tower side-to-side bending mode DOF (internal DOF index = DOF_TSS1)'
   p%DOF_Flag(DOF_TFA2) = InputFileData%TwFADOF2
   p%DOF_Desc(DOF_TFA2) = '2nd tower fore-aft bending mode DOF (internal DOF index = DOF_TFA2)'
   p%DOF_Flag(DOF_TSS2) = InputFileData%TwSSDOF2
   p%DOF_Desc(DOF_TSS2) = '2nd tower side-to-side bending mode DOF (internal DOF index = DOF_TSS2)'
   p%DOF_Flag(DOF_Sg  ) = InputFileData%PtfmSgDOF
   p%DOF_Desc(DOF_Sg  ) = 'Platform horizontal surge translation DOF (internal DOF index = DOF_Sg)'
   p%DOF_Flag(DOF_Sw  ) = InputFileData%PtfmSwDOF
   p%DOF_Desc(DOF_Sw  ) = 'Platform horizontal sway translation DOF (internal DOF index = DOF_Sw)'
   p%DOF_Flag(DOF_Hv  ) = InputFileData%PtfmHvDOF
   p%DOF_Desc(DOF_Hv  ) = 'Platform vertical heave translation DOF (internal DOF index = DOF_Hv)'
   p%DOF_Flag(DOF_R   ) = InputFileData%PtfmRDOF
   p%DOF_Desc(DOF_R   ) = 'Platform roll tilt rotation DOF (internal DOF index = DOF_R)'
   p%DOF_Flag(DOF_P   ) = InputFileData%PtfmPDOF
   p%DOF_Desc(DOF_P   ) = 'Platform pitch tilt rotation DOF (internal DOF index = DOF_P)'
   p%DOF_Flag(DOF_Y   ) = InputFileData%PtfmYDOF
   p%DOF_Desc(DOF_Y   ) = 'Platform yaw rotation DOF (internal DOF index = DOF_Y)'
      
   ! ...........................................................................................................................
   ! allocate the arrays stored in the p%DOFs structure:
   ! ...........................................................................................................................

      ! BJJ: note that this method will cause an error if allocating data that has already been allocated...

   ALLOCATE ( p%DOFs%NPSBE(p%NumBl), p%DOFs%NPSE(p%NumBl),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs NPSBE and NPSE arrays.' )
      RETURN
   ENDIF


   ALLOCATE ( p%DOFs%PCE(p%NDOF), p%DOFs%PDE(p%NDOF), p%DOFs%PIE(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PCE, PDE, and PIE arrays.' )
      RETURN
   ENDIF


   ALLOCATE (  p%DOFs%PTTE(p%NDOF), p%DOFs%PTE(p%NDOF), p%DOFs%PS(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PTTE, PTE, and PS arrays.' )
      RETURN
   ENDIF


   ALLOCATE ( p%DOFs%PUE(p%NDOF), p%DOFs%PYE(p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PUE and PYE arrays.' )
      RETURN
   ENDIF


!bjj was   ALLOCATE ( p%DOFs%PSBE(p%NumBl,3), p%DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   ALLOCATE ( p%DOFs%PSBE(p%NumBl,(NumBE+NumBF)), p%DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PSBE and PSE arrays.' )
      RETURN
   ENDIF


   ALLOCATE ( p%DOFs%SrtPS(p%NDOF), p%DOFs%SrtPSNAUG(p%NAug),  p%DOFs%Diag(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs SrtPS, SrtPSNAUG, and Diag arrays.' )
      RETURN
   ENDIF


   !...............................................................................................................................
   ! Allocate and Initialize arrays for DOFS that contribute to the angular velocity of the hub and blade elements
   !...............................................................................................................................
   ! Define arrays of DOF indices (pointers) that contribute to the angular 
   !   velocities of each rigid body of the wind turbine in the inertia frame:
   ! NOTE: We must include ALL of the appropriate DOF indices in these arrays,
   !       not just the indices of the enabled DOFs, since disabling a DOF only
   !       implies that each DOF acceleration is zero--it does not imply
   !       that each DOF velocity is zero (for example, consider disabled
   !       generator DOF, which still spins at constant speed).
      
   

   IF ( p%NumBl == 2 )  THEN ! 2-blader
      p%NPH = 12                         ! Number of DOFs that contribute to the angular velocity of the hub            (body H) in the inertia frame.
      p%NPM = 15                         ! Number of DOFs that contribute to the angular velocity of the blade elements (body M) in the inertia frame.
   ELSE                    ! 3-blader
      p%NPH = 11                         ! Number of DOFs that contribute to the angular velocity of the hub            (body H) in the inertia frame.
      p%NPM = 14                         ! Number of DOFs that contribute to the angular velocity of the blade elements (body M) in the inertia frame.
   ENDIF


   ALLOCATE ( p%PH(p%NPH),  p%PM(p%NumBl,p%NPM), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveDOFs PH and PM arrays.' )
      RETURN
   ENDIF

      ! Array of DOF indices (pointers) that contribute to the angular velocity of the hub (body H) in the inertia frame:
   p%PH(1:11) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /)

   IF ( p%NumBl == 2 )  THEN ! 2-blader (add DOF_Teet to the arrays)

      p%PH(12) = DOF_Teet

         ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements (body M) in the inertia frame:
      DO K = 1,p%NumBl ! Loop through all blades
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, &
                        DOF_Teet,  DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)          /)
      ENDDO          ! K - All blades

   ELSE  ! 3-blader

         ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements (body M) in the inertia frame:
      DO K = 1,p%NumBl ! Loop through all blades
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, &
                                   DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)         /)
      ENDDO          ! K - All blades

   ENDIF


   !...............................................................................................................................
   ! Calculate the number of active (enabled) DOFs in the model, p%DOFs%NActvDOF:
   !...............................................................................................................................
   CALL SetEnabledDOFIndexArrays( p )

   RETURN
   
   
CONTAINS
   !............................................................................................................................
   SUBROUTINE ExitThisRoutine(ErrID,Msg)
   ! This subroutine cleans up all the allocatable arrays, closes the file, and sets the error status/message
   !............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error ID (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)

         ! Set error status/message

      ErrStat = ErrID
      ErrMsg  = Msg
      IF ( ErrStat /= ErrID_None ) THEN
         ErrMsg = 'Error in Init_DOFparameters: '//TRIM(ErrMsg)
      END IF


   END SUBROUTINE ExitThisRoutine

END SUBROUTINE Init_DOFparameters
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION SHP(Fract, FlexL, ModShpAry, Deriv, ErrStat, ErrMsg)
! SHP calculates the Derive-derivative of the shape function ModShpAry at Fract.
! NOTE: This function only works for Deriv = 0, 1, or 2.
!..................................................................................................................................

      ! Passed variables:

   REAL(ReKi),     INTENT(IN )    :: FlexL                     ! Length of flexible beam, (m)
   REAL(ReKi),     INTENT(IN )    :: Fract                     ! Fractional distance along flexible beam, 0<=Frac<=1
   REAL(ReKi),     INTENT(IN )    :: ModShpAry(:)              ! Array holding mode shape coefficients (2:PolyOrd)
   REAL(ReKi)                     :: SHP                       ! The shape function returned by this function.

   INTEGER(IntKi), INTENT(IN )    :: Deriv                     ! Which derivative to compute Deriv = 0 (regular function SHP), 1 (D(SHP)/DZ), 2 (D2(SHP)/DZ2)
   INTEGER(IntKi), INTENT(OUT)    :: ErrStat                   ! A error level that indicates if/what error occurred
   CHARACTER(*),   INTENT(OUT)    :: ErrMsg                    ! A message indicating the error if one occurred


      ! Local variables:

   INTEGER(IntKi)                 :: CoefTmp                   ! Temporary coefficient
   INTEGER(IntKi)                 :: I                         ! Counts through polynomial array.
   INTEGER(IntKi)                 :: J                         ! I+1
   INTEGER(IntKi)                 :: Swtch(0:2)                ! Corresponds to which derivative to compute.  Sets all portions of the coefficient = 0 except those that are relevant.


   IF ( Deriv < 0 .OR. Deriv > 2 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Function SHP input Deriv='//TRIM(Num2LStr(Deriv))//' is invalid. Deriv must be 0, 1, or 2.'
      RETURN
   ELSEIF ( Fract < 0.0_ReKi .OR. Fract > 1.0_ReKi ) THEN
      ErrStat = ErrID_Warn
      ErrMsg  = 'Function SHP input Fract='//TRIM(Num2LStr(Fract))//' does not meet the condition 0<=Fract<=1.'
   ELSE
      ErrStat = ErrID_None
   END IF

   Swtch        = 0 ! Initialize Swtch(:) to 0
   Swtch(Deriv) = 1
   SHP          = 0.0

   DO I = 1,SIZE(ModShpAry,DIM=1,KIND=IntKi) ! =2,PolyOrd
      J = I + 1
      CoefTmp = Swtch(0) + Swtch(1)*J + Swtch(2)*I*J

      IF ( (J == 2) .AND. (Deriv == 2) ) THEN !bjj this could be removed as Fract**0 = 1 (0**0 = 1 in Fortran)
         SHP =       ModShpAry(I)*CoefTmp                         /( FlexL**Deriv )
      ELSE
         SHP = SHP + ModShpAry(I)*CoefTmp*( Fract**( J - Deriv ) )/( FlexL**Deriv )
      ENDIF
   ENDDO !I

   RETURN

END FUNCTION SHP
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_CoordSys( CoordSys, p, ErrStat, ErrMsg )
! This subroutine allocates the coordinate systems in the ED_CoordSys type.
!..................................................................................................................................

IMPLICIT NONE

   ! passed arguments

TYPE(ED_CoordSys),        INTENT(OUT) :: CoordSys       ! The coordinate systems, with arrays to be allocated
TYPE(ED_ParameterType),   INTENT(IN)  :: p              ! Parameters of the structural dynamics module

INTEGER(IntKi),           INTENT(OUT) :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT) :: ErrMsg         ! Err msg


   ! local variables

CHARACTER(200), PARAMETER        :: ErrTxt = 'coordinate system arrays in SUBROUTINE Alloc_CoordSys.'


   ! Initialize ErrStat and ErrMsg

ErrStat = ErrID_None
ErrMsg  = ""


  ! Allocate coordinate system arrays:

ALLOCATE ( CoordSys%i1(p%NumBl,3), CoordSys%i2(p%NumBl,3), CoordSys%i3(p%NumBl,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the i1, i2, and i3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%j1(p%NumBl,3), CoordSys%j2(p%NumBl,3), CoordSys%j3(p%NumBl,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the j1, j2, and j3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%m1(p%NumBl,p%BldNodes,3), CoordSys%m2(p%NumBl,p%BldNodes,3), &
           CoordSys%m3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the m1, m2, and m3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%n1(p%NumBl,p%BldNodes,3), CoordSys%n2(p%NumBl,p%BldNodes,3), &
           CoordSys%n3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the n1, n2, and n3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%t1(p%TwrNodes,3), CoordSys%t2(p%TwrNodes,3), CoordSys%t3(p%TwrNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the t1, t2, and t3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%te1(p%NumBl,p%BldNodes,3), CoordSys%te2(p%NumBl,p%BldNodes,3), &
           CoordSys%te3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the te1, te2, and te3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


RETURN
END SUBROUTINE Alloc_CoordSys
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_BladeMeshInputProperties( BladeKInputFileMesh, ErrStat, ErrMsg )
! This routine allocates arrays for the blade mesh properties from the input file
!..................................................................................................................................

   TYPE(ED_BladeMeshInputData),   INTENT(INOUT)  :: BladeKInputFileMesh      ! Data for Blade K stored in the module's input file
   INTEGER(IntKi),                INTENT(OUT)    :: ErrStat                  ! Error status
   CHARACTER(*),                  INTENT(OUT)    :: ErrMsg                   ! Err msg


   IF ( BladeKInputFileMesh%BldNodes < 1 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating arrays for blade mesh input properties: BldNodes must be at least 1.'
      RETURN
   END IF

   CALL AllocAry  ( BladeKInputFileMesh%RNodes,   BladeKInputFileMesh%BldNodes, 'RNodes'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileMesh%AeroTwst, BladeKInputFileMesh%BldNodes, 'AeroTwst', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileMesh%Chord,    BladeKInputFileMesh%BldNodes, 'Chord'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN       
   

END SUBROUTINE Alloc_BladeMeshInputProperties
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_BladeInputProperties( BladeKInputFileData, AllocAdams, ErrStat, ErrMsg )
! This routine allocates arrays for the blade properties from the input file
!..................................................................................................................................

   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData      ! Data for Blade K stored in the module's input file
   LOGICAL,                  INTENT(IN)     :: AllocAdams               ! Logical to determine if we should allocate the arrays only used for Adams
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                  ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                   ! Err message


   IF ( BladeKInputFileData%NBlInpSt < 1 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating arrays for blade input properties: NBlInpSt must be at least 1.'
      RETURN
   END IF


      ! Allocate the arrays.

   CALL AllocAry  ( BladeKInputFileData%BlFract,  BladeKInputFileData%NBlInpSt, 'BlFract'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%AerCen,   BladeKInputFileData%NBlInpSt, 'AerCen'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%StrcTwst, BladeKInputFileData%NBlInpSt, 'StrcTwst' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BMassDen, BladeKInputFileData%NBlInpSt, 'BMassDen' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%FlpStff,  BladeKInputFileData%NBlInpSt, 'FlpStff'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EdgStff,  BladeKInputFileData%NBlInpSt, 'EdgStff'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


   IF ( AllocAdams ) THEN
      CALL AllocAry  ( BladeKInputFileData%GJStff,   BladeKInputFileData%NBlInpSt, 'GJStff'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%EAStff,   BladeKInputFileData%NBlInpSt, 'EAStff'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%Alpha,    BladeKInputFileData%NBlInpSt, 'Alpha'    , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%FlpIner,  BladeKInputFileData%NBlInpSt, 'FlpIner'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%EdgIner,  BladeKInputFileData%NBlInpSt, 'EdgIner'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%PrecrvRef,BladeKInputFileData%NBlInpSt, 'PrecrvRef', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%PreswpRef,BladeKInputFileData%NBlInpSt, 'PreswpRef', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%FlpcgOf,  BladeKInputFileData%NBlInpSt, 'FlpcgOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%EdgcgOf,  BladeKInputFileData%NBlInpSt, 'EdgcgOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%FlpEAOf,  BladeKInputFileData%NBlInpSt, 'FlpEAOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( BladeKInputFileData%EdgEAOf,  BladeKInputFileData%NBlInpSt, 'EdgEAOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN            
   END IF
   
   
      ! BJJ: note that these used to be allocated 2:PolyOrd  :

   CALL AllocAry  ( BladeKInputFileData%BldFl1Sh,  PolyOrd-1, 'BldFl1Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldFl2Sh,  PolyOrd-1, 'BldFl2Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldEdgSh,  PolyOrd-1, 'BldEdgSh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


END SUBROUTINE Alloc_BladeInputProperties
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateBladeData ( BladeKInputFileData, ErrStat, ErrMsg )
! This routine checks the blade file input data for errors
!..................................................................................................................................
   TYPE(BladeInputData),     INTENT(IN)     :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

      ! local variables
   INTEGER                                  :: I                                   ! Loop counter
   INTEGER(IntKi)                           :: ErrStat2                            ! Error status
   CHARACTER(LEN(ErrMsg))                   :: ErrMsg2                             ! Temporary error message


   ErrStat = ErrID_None
   ErrMsg= ''


      ! Check that BlFract goes from 0.0 to 1.0 in increasing order:

   IF ( .NOT. EqualRealNos( BladeKInputFileData%BlFract(1), 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract(1) must be 0.0.'
   END IF

   IF ( BladeKInputFileData%NBlInpSt /= 1 .AND. &
      .NOT. EqualRealNos( BladeKInputFileData%BlFract(BladeKInputFileData%NBlInpSt), 1.0_ReKi )  ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract('//TRIM( Num2LStr( BladeKInputFileData%NBlInpSt ) )//') must be 1.0.'
   END IF

   DO I = 2,BladeKInputFileData%NBlInpSt
      IF ( BladeKInputFileData%BlFract(I) <= BladeKInputFileData%BlFract(I-1) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract('//TRIM( Num2LStr( I ) )//') must be greater than BlFract('&
                                                      //TRIM( Num2LStr(I-1) )//').'

      ENDIF
   END DO


   DO I = 1,BladeKInputFileData%NBlInpSt

         ! Check that AerCen is contained in [0.0, 1.0]:
      IF ( ( BladeKInputFileData%AerCen(I) ) < 0.0_ReKi .OR. ( BladeKInputFileData%AerCen(I) > 1.0_ReKi ) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  AerCen('//TRIM( Num2LStr( I ) )//') must be between 0 and 1 (inclusive).'
      END IF

         ! Check that StrcTwst is contained in (-pi,pi] radians ( i.e., (-180.0, 180.0] degrees):
      IF ( ( BladeKInputFileData%StrcTwst(I) <= -pi ) .OR. ( BladeKInputFileData%StrcTwst(I) > pi ) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  StrcTwst('//TRIM( Num2LStr( I ) ) // &
                     ') must be greater than -180 and less than or equal to 180.'
      END IF

         ! Check that BMassDen is contained in (0.0, inf):
      IF ( BladeKInputFileData%BMassDen(I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  BMassDen('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF

         ! Check that FlpStff is contained in (0.0, inf):
      IF ( BladeKInputFileData%FlpStff (I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlpStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF

         ! Check that EdgStff is contained in (0.0, inf):
      IF ( BladeKInputFileData%EdgStff (I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  EdgStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF

   END DO


      ! Check values for Adams input
      
   IF ( ALLOCATED(BladeKInputFileData%GJStff) ) THEN  ! We assume that if GJStff is allocated, we are using ADAMS inputs

         ! The reference axis must be coincident with the pitch axis at the blade root (I == 1):
      IF ( .NOT. EqualRealNos( BladeKInputFileData%PrecrvRef(1), 0.0_ReKi ) .OR. &
            .NOT. EqualRealNos( BladeKInputFileData%PreswpRef(1), 0.0_ReKi )      )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  Both PrecrvRef(1) and PreswpRef(1) must be zero '//&
                            '(the reference axis must be coincident with the pitch axis at the blade root).'
      END IF


      DO I = 1,BladeKInputFileData%NBlInpSt

            ! Check that GJStff is contained in (0.0, inf):
         IF ( BladeKInputFileData%GJStff(I) <= 0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  GJStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
         END IF

            ! Check that EAStff is contained in (0.0, inf):
         IF ( BladeKInputFileData%EAStff(I) <= 0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  EAStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
         END IF

            ! Check that Alpha is contained in (-1.0, 1):
         IF ( ( BladeKInputFileData%Alpha(I) <= -1.0_ReKi ) .OR. ( BladeKInputFileData%Alpha(I) >= 1.0_ReKi ) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  Alpha('//TRIM( Num2LStr( I ) )//') (the blade flap/twist'// &
                         ' coupling coefficient) must be between -1 and 1 (exclusive).'
         END IF

            ! Check that FlpIner is contained in [0.0, inf):
         IF ( BladeKInputFileData%FlpIner(I) <  0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlpIner('//TRIM( Num2LStr( I ) )//') must not be less than zero.'
         END IF

            ! Check that EdgIner is contained in [0.0, inf):
         IF ( BladeKInputFileData%EdgIner(I) <  0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  EdgIner('//TRIM( Num2LStr( I ) )//') must not be less than zero.'
         END IF

            ! Check that PrecrvRef is 0.0 for Adams models:
         IF ( .NOT. EqualRealNos( BladeKInputFileData%PrecrvRef(I), 0.0_ReKi) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  PrecrvRef('//TRIM( Num2LStr( I ) )//') must be zero for Adams models.'
         END IF

            ! Check that GJStff is contained in (0.0, inf):
         IF ( .NOT. EqualRealNos( BladeKInputFileData%PreswpRef(I), 0.0_ReKi) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  PreswpRef('//TRIM( Num2LStr( I ) )//') must be zero for Adams models.'
         END IF

      END DO

   END IF  ! check for Adams models


      ! Check that the blade damping is not negative:

   IF ( ANY( BladeKInputFileData%BldFlDmp < 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BldFlDmp must not be negative.'
   END IF

   IF ( ANY( BladeKInputFileData%BldEdDmp < 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BldEdDmp must not be negative.'
   END IF


      ! Check that the stiffness tuner isn't negative:

   IF ( ANY( BladeKInputFileData%FlStTunr <= 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlStTunr must be greater than zero.'
   END IF



      ! Check that the mode shape coefficients are valid:

   CALL ValidateModeShapeCoeffs(BladeKInputFileData%BldFl1Sh, 'blade flap mode 1', ErrStat2, ErrMsg2 )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF

   CALL ValidateModeShapeCoeffs(BladeKInputFileData%BldFl2Sh, 'blade flap mode 2', ErrStat2, ErrMsg2 )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF

   CALL ValidateModeShapeCoeffs(BladeKInputFileData%BldEdgSh, 'blade edge', ErrStat2, ErrMsg2 )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF


END SUBROUTINE ValidateBladeData
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateModeShapeCoeffs( Coeffs, ShpDesc, ErrStat, ErrMsg )
! This routine checks that the mode shape coefficients add to 1.0, within numerical tolerance.
!..................................................................................................................................
   REAL(ReKi),               INTENT(IN )    :: Coeffs(:)                           ! Mode shape coefficients
   CHARACTER(*),             INTENT(IN)     :: ShpDesc                             ! Description of the mode shape for the error message
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

      ! local variables
   REAL(ReKi)                               :: Displ                               ! Blade tip/tower top displacement for a mode shape


      ! Check that the mode shape coefficients add to 1.0:

   Displ = SUM( Coeffs )
! bjj this new check seems to be a bit too restrictive for the input data:
!   IF ( .NOT. EqualRealNos( Displ, 1.0_ReKi ) ) THEN
   IF ( ABS( Displ - 1.0_ReKi ) > 0.001_ReKi ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = '  Mode shape coefficients for '//TRIM(ShpDesc)//' must add to 1.0.'
   ELSE
      ErrStat = ErrID_None
      ErrMsg  = ''
   END IF


END SUBROUTINE ValidateModeShapeCoeffs
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetBladeParameters( p, BladeInData, BladeMeshData, ErrStat, ErrMsg )
! This takes the blade input file data and sets the corresponding blade parameters, performing linear interpolation of the
! input data to the specified blade mesh.
! This routine assumes p%HubRad and p%BldFlexL are already set.
!..................................................................................................................................

   TYPE(ED_ParameterType),        INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   TYPE(BladeInputData),          INTENT(IN)     :: BladeInData(:)                      ! Program input data for all blades
   TYPE(ED_BladeMeshInputData),   INTENT(IN)     :: BladeMeshData(:)                    ! Program input mesh data for all blades
   INTEGER(IntKi),                INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),                  INTENT(OUT)    :: ErrMsg                              ! Error message

      ! Local variables:
   REAL(ReKi)                                    :: x                                   ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                               :: K                                   ! Blade number
   INTEGER(IntKi )                               :: J                                   ! Index for the node arrays
   INTEGER(IntKi)                                :: InterpInd                           ! Index for the interpolation routine
   LOGICAL                                       :: SetAdmVals                          ! Logical to determine if Adams inputs should be set

      ! initialize variables
   ErrStat = ErrID_None
   ErrMsg  = ''

   SetAdmVals = ALLOCATED( BladeInData(1)%GJStff )
      
   ! ..............................................................................................................................   
   ! Set the blade discretization information here:
   ! ..............................................................................................................................   
  
   DO K=1,1 ! we're going to assume the discretization is the same for all blades 

      p%BldNodes = BladeMeshData(K)%BldNodes 
   
      p%TipNode  = p%BldNodes + 1    ! The index for the blade tip and tower top nodes
   END DO

      ! .......... Allocate arrays for the blade parameters being set in this routine ..........:
      
   CALL Alloc_BladeParameters( p, SetAdmVals, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

   
   DO K=1,1 ! we're going to assume the discretization is the same for all blades 

      p%RNodes   = BladeMeshData(K)%RNodes - p%HubRad   ! Radius to blade analysis nodes relative to root ( 0 < RNodes(:) < p%BldFlexL ) (Convert RNodes to be relative to the hub)

      p%DRNodes(1) = 2.0*p%RNodes(1)
      DO J = 2,p%BldNodes
         p%DRNodes(J) = 2.0*( p%RNodes(J) - p%RNodes(J-1) ) - p%DRNodes(J-1)
      END DO
   
      p%Chord     = BladeMeshData(K)%Chord
      p%AeroTwst  = BladeMeshData(K)%AeroTwst
      p%CAeroTwst = COS(p%AeroTwst)
      p%SAeroTwst = SIN(p%AeroTwst)
      
   END DO
      
   
   ! ..............................................................................................................................   
   ! Interpolate the blade properties to this discretization:
   ! ..............................................................................................................................   
   
   ! Array definitions:

   !    Input      Interp    Description
   !    -----      ------    -----------
   !    BlFract    RNodesNorm Fractional radius (0 at root, 1 at tip)
   !    AerCen     AeroCent   Aerodynamic center (0 at LE, 1 at TE)
   !    StrcTwst   ThetaS     Structural twist
   !    BMassDen   MassB      Lineal mass density
   !    FlpStff    StiffBF    Flapwise stiffness
   !    EdgStff    StiffBE    Edgewise stiffness
   !    GJStff     StiffBGJ   Blade torsional stiffness
   !    EAStff     StiffBEA   Blade extensional stiffness
   !    Alpha      BAlpha     Blade flap/twist coupling coefficient
   !    FlpIner    InerBFlp   Blade flap (about local structural yb-axis) mass inertia per unit length
   !    EdgIner    InerBEdg   Blade edge (about local structural xb-axis) mass inertia per unit length
   !    PrecrvRef  RefAxisxb  Blade offset for defining the reference axis from the pitch axis for precurved blades (along xb-axis)
   !    PreswpRef  RefAxisyb  Blade offset for defining the reference axis from the pitch axis for preswept  blades (along yb-axis)
   !    FlpcgOf    cgOffBFlp  Blade flap mass cg offset
   !    EdgcgOf    cgOffBEdg  Blade edge mass cg offset
   !    FlpEAOf    EAOffBFlp  Blade flap elastic axis offset
   !    EdgEAOf    EAOffBEdg  Blade edge elastic axis offset


      ! Define RNodesNorm() which is common to all the blades:

   p%RNodesNorm = p%RNodes/p%BldFlexL  ! Normalized radius to analysis nodes relative to hub ( 0 < RNodesNorm(:) < 1 )


      ! Perform a linear interpolation of the input data to map to the meshed data for simulation:

   DO K=1,p%NumBl
      InterpInd = 1

      DO J=1,p%BldNodes

            ! Get the index into BlFract for all of the arrays, using the NWTC Subroutine Library
         p%AeroCent(K,J) = InterpStp( p%RNodesNorm(J), BladeInData(K)%BlFract, BladeInData(K)%AerCen, &
                                      InterpInd, BladeInData(K)%NBlInpSt )


            ! The remaining arrays will have the same x value for the linear interpolation,
            ! so we'll do it manually (with a local subroutine) instead of calling the InterpStp routine again
         IF ( BladeInData(K)%NBlInpSt < 2 ) THEN
            x         = 1.0
            InterpInd = 0
         ELSE
            x = ( p%RNodesNorm(J)                     - BladeInData(K)%BlFract(InterpInd) ) / &
                ( BladeInData(K)%BlFract(InterpInd+1) - BladeInData(K)%BlFract(InterpInd) )
         END IF

         p%ThetaS  (K,J) = InterpAry( x, BladeInData(K)%StrcTwst, InterpInd )
         p%MassB   (K,J) = InterpAry( x, BladeInData(K)%BMassDen, InterpInd )
         p%StiffBF (K,J) = InterpAry( x, BladeInData(K)%FlpStff , InterpInd )
         p%StiffBE (K,J) = InterpAry( x, BladeInData(K)%EdgStff , InterpInd )

         IF ( SetAdmVals ) THEN
            p%StiffBGJ (K,J) = InterpAry( x, BladeInData(K)%GJStff   , InterpInd )
            p%StiffBEA (K,J) = InterpAry( x, BladeInData(K)%EAStff   , InterpInd )
            p%BAlpha   (K,J) = InterpAry( x, BladeInData(K)%Alpha    , InterpInd )
            p%InerBFlp (K,J) = InterpAry( x, BladeInData(K)%FlpIner  , InterpInd )
            p%InerBEdg (K,J) = InterpAry( x, BladeInData(K)%EdgIner  , InterpInd )
            p%RefAxisxb(K,J) = InterpAry( x, BladeInData(K)%PrecrvRef, InterpInd )
            p%RefAxisyb(K,J) = InterpAry( x, BladeInData(K)%PreswpRef, InterpInd )
            p%cgOffBFlp(K,J) = InterpAry( x, BladeInData(K)%FlpcgOf  , InterpInd )
            p%cgOffBEdg(K,J) = InterpAry( x, BladeInData(K)%EdgcgOf  , InterpInd )
            p%EAOffBFlp(K,J) = InterpAry( x, BladeInData(K)%FlpEAOf  , InterpInd )
            p%EAOffBEdg(K,J) = InterpAry( x, BladeInData(K)%EdgEAOf  , InterpInd )
         END IF


         
      END DO ! J (Blade nodes)

      IF ( SetAdmVals ) THEN
            ! Set the valus for the tip node
         p%RefAxisxb(K,p%TipNode) = BladeInData(K)%PrecrvRef( BladeInData(K)%NBlInpSt )
         p%RefAxisyb(K,p%TipNode) = BladeInData(K)%PreswpRef( BladeInData(K)%NBlInpSt )
      END IF


         ! Set the blade damping and stiffness tuner
      p%BldFDamp(K,:) = BladeInData(K)%BldFlDmp
      p%BldEDamp(K,:) = BladeInData(K)%BldEdDmp
      p%FStTunr (K,:) = BladeInData(K)%FlStTunr



         ! Set the mode shape arrays
      p%BldEdgSh(:,K) = BladeInData(K)%BldEdgSh
      p%BldFl1Sh(:,K) = BladeInData(K)%BldFl1Sh
      p%BldFl2Sh(:,K) = BladeInData(K)%BldFl2Sh


   END DO ! ( Blades )

   p%CThetaS = COS(p%ThetaS)
   p%SThetaS = SIN(p%ThetaS)


RETURN


CONTAINS
!..................................................................................................................................
   FUNCTION InterpAry( x, YAry, Ind )
      ! This subroutine is used to interpolate the arrays more efficiently (all arrays have the same X value)
      ! See InterpStpReal() for comparison. This assumes we already know Ind and that
      ! x = ( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) )


      REAL(ReKi),      INTENT(IN) :: x                ! the relative distance between Ind and Ind+ 1
      REAL(ReKi),      INTENT(IN) :: YAry (:)         ! Array of Y values to be interpolated.
      INTEGER(IntKi) , INTENT(IN) :: Ind              ! the index into the array

      REAL(ReKi)                  :: InterpAry        ! the value calculated in this function

      IF ( Ind >= SIZE(YAry) ) THEN
         InterpAry = YAry( SIZE(YAry) )
      ELSE
         InterpAry = ( YAry(Ind+1) - YAry(Ind) ) * x  + YAry(Ind)
      END IF

   END FUNCTION InterpAry
!..................................................................................................................................
END SUBROUTINE SetBladeParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_BladeParameters( p, AllocAdams, ErrStat, ErrMsg )
! This routine allocates arrays for the blade parameters.
!..................................................................................................................................

   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   LOGICAL,                  INTENT(IN)     :: AllocAdams                          ! Logical to determine if Adams inputs should be allocated
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Err msg

   
      ! Allocate arrays to hold the blade analysis nodes.
   CALL AllocAry  ( p%RNodes,             p%BldNodes, 'RNodes'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%DRNodes,            p%BldNodes, 'DRNodes'  , ErrStat, ErrMsg )
   
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%Chord,              p%BldNodes, 'Chord'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%AeroTwst,           p%BldNodes, 'AeroTwst' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%CAeroTwst,          p%BldNodes, 'CAeroTwst', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%SAeroTwst,          p%BldNodes, 'SAeroTwst', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
   
      ! Allocate arrays to hold blade data at the analysis nodes.
   CALL AllocAry  ( p%RNodesNorm,              p%BldNodes, 'RNodesNorm' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%AeroCent,    p%NumBl,    p%BldNodes, 'AeroCent'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%ThetaS,      p%NumBl,    p%BldNodes, 'ThetaS'     , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%CThetaS,     p%NumBl,    p%BldNodes, 'CThetaS'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%SThetaS,     p%NumBl,    p%BldNodes, 'SThetaS'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%MassB,       p%NumBl,    p%BldNodes, 'MassB'      , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%StiffBF,     p%NumBl,    p%BldNodes, 'StiffBF'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%StiffBE,     p%NumBl,    p%BldNodes, 'StiffBE'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


   IF ( AllocAdams ) THEN
      CALL AllocAry  ( p%StiffBGJ,    p%NumBl,    p%BldNodes, 'StiffBGJ'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%StiffBEA,    p%NumBl,    p%BldNodes, 'StiffBEA'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%BAlpha,      p%NumBl,    p%BldNodes, 'BAlpha'     , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%InerBFlp,    p%NumBl,    p%BldNodes, 'InerBFlp'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%InerBEdg,    p%NumBl,    p%BldNodes, 'InerBEdg'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%RefAxisxb,   p%NumBl,    p%TipNode,  'RefAxisxb'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%RefAxisyb,   p%NumBl,    p%TipNode,  'RefAxisyb'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%cgOffBFlp,   p%NumBl,    p%BldNodes, 'cgOffBFlp'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%cgOffBEdg,   p%NumBl,    p%BldNodes, 'cgOffBEdg'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%EAOffBFlp,   p%NumBl,    p%BldNodes, 'EAOffBFlp'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%EAOffBEdg,   p%NumBl,    p%BldNodes, 'EAOffBEdg'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   END IF

   CALL AllocAry  ( p%BldEDamp,    p%NumBl,    NumBE,      'BldEDamp'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%BldFDamp,    p%NumBl,    NumBF,      'BldFDamp'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%FStTunr,     p%NumBl,    NumBF,      'FStTunr'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


         ! Allocate space for the mode shape arrays:

   ALLOCATE( p%BldEdgSh(2:PolyOrd,p%NumBl), p%BldFl1Sh(2:PolyOrd,p%NumBl), p%BldFl2Sh(2:PolyOrd,p%NumBl), STAT = ErrStat )
   IF ( ErrStat /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = ' Error allocating BldEdgSh, BldFl1Sh, and BldFl2Sh arrays.'
      RETURN
   END IF


END SUBROUTINE Alloc_BladeParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateTowerData ( InputFileData, ErrStat, ErrMsg )
! This routine checks the tower file input data for errors
!..................................................................................................................................
   TYPE(ED_InputFile),     INTENT(IN   )    :: InputFileData                       ! Data stored in the module's input file
   INTEGER(IntKi),         INTENT(  OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),           INTENT(  OUT)    :: ErrMsg                              ! Error message

      ! local variables
   INTEGER                                  :: I                                   ! Loop counter
   INTEGER(IntKi)                           :: ErrStat2                            ! Error status
   CHARACTER(LEN(ErrMsg))                   :: ErrMsg2                             ! Temporary error message


   ErrStat = ErrID_None
   ErrMsg= ''



      ! Check that HtFract goes from 0.0 to 1.0 in increasing order:

   IF ( .NOT. EqualRealNos( InputFileData%HtFract(1), 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  HtFract(1) must be 0.0.'
   END IF

   IF ( InputFileData%NTwInpSt /= 1 .AND. &
      .NOT. EqualRealNos( InputFileData%HtFract(InputFileData%NTwInpSt), 1.0_ReKi )  ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  HtFract('//TRIM( Num2LStr( InputFileData%NTwInpSt ) )//') must be 1.0.'
   END IF

   DO I = 2,InputFileData%NTwInpSt
      IF ( InputFileData%HtFract(I) <= InputFileData%HtFract(I-1) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  HtFract('//TRIM( Num2LStr( I ) )//') must be greater than BlFract('&
                                                      //TRIM( Num2LStr(I-1) )//').'

      ENDIF
   END DO


      ! Check the input arrays:

   DO I = 1,InputFileData%NTwInpSt
      IF ( InputFileData%TMassDen(I) <= 0.0_ReKi ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  TMassDen('//TRIM(Num2LStr( I ))//') must be greater than zero.'
      END IF

      IF ( InputFileData%TwFAStif(I) <= 0.0_ReKi ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwFAStif('//TRIM(Num2LStr( I ))//') must be greater than zero.'
      END IF

      IF ( InputFileData%TwSSStif(I) <= 0.0_ReKi ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwSSStif('//TRIM(Num2LStr( I ))//') must be greater than zero.'
      END IF
   END DO

      ! Check Adams inputs
      
   IF ( ALLOCATED( InputFileData%TwGJStif ) ) THEN ! Assume that all of the Adams tower data is allocated

      DO I = 1,InputFileData%NTwInpSt
         IF ( InputFileData%TwGJStif(I) <= 0.0_ReKi ) THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwGJStif('//TRIM(Num2LStr( I ))//') must be greater than zero.'
         END IF

         IF ( InputFileData%TwEAStif(I) <= 0.0_ReKi ) THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwEAStif('//TRIM(Num2LStr( I ))//') must be greater than zero.'
         END IF

         IF ( InputFileData%TwFAIner(I) <= 0.0_ReKi ) THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwFAIner('//TRIM(Num2LStr( I ))//') must be greater than zero.'
         END IF

         IF ( InputFileData%TwSSIner(I) <= 0.0_ReKi ) THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwSSIner('//TRIM(Num2LStr( I ))//') must be greater than zero.'
         END IF
      END DO

   END IF ! Check items for Adams



      ! Check that the tower damping (TwrFADmp) is contained in the range [0, 100]:

   IF ( ANY( InputFileData%TwrFADmp < 0.0_ReKi ) .OR. ANY( InputFileData%TwrFADmp > 100.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwrFADmp must be between 0 and 100 (inclusive).'
   END IF

   IF ( ANY( InputFileData%TwrSSDmp < 0.0_ReKi ) .OR. ANY( InputFileData%TwrSSDmp > 100.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  TwrSSDmp must be between 0 and 100 (inclusive).'
   END IF


      ! Check that the tower tuners are positive numbers:

   IF ( ANY( InputFileData%FAStTunr <= 0.0_ReKi )  ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  FAStTunr must be greater than zero.'
   END IF

   IF ( ANY( InputFileData%SSStTunr <= 0.0_ReKi )  ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  SSStTunr must be greater than zero.'
   END IF



      ! Validate the mode shape coefficients:

   CALL ValidateModeShapeCoeffs( InputFileData%TwFAM1Sh, 'tower fore-aft mode 1', ErrStat, ErrMsg )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF

   CALL ValidateModeShapeCoeffs( InputFileData%TwFAM2Sh, 'tower fore-aft mode 2', ErrStat, ErrMsg )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF

   CALL ValidateModeShapeCoeffs( InputFileData%TwSSM1Sh, 'tower side-to-side mode 1', ErrStat, ErrMsg )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF

   CALL ValidateModeShapeCoeffs( InputFileData%TwSSM2Sh, 'tower side-to-side mode 2', ErrStat, ErrMsg )
   IF ( ErrStat2 /= ErrID_None ) THEN
      ErrStat = MAX(ErrStat2, ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//TRIM(ErrMsg2)
   END IF


END SUBROUTINE ValidateTowerData
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_TowerInputProperties( InputFileData, AllocAdams, ErrStat, ErrMsg )
! This routine allocates arrays for the tower properties from the input file
!..................................................................................................................................

   TYPE(ED_InputFile),       INTENT(INOUT)  :: InputFileData      ! All the data in the ElastoDyn input file
   LOGICAL,                  INTENT(IN)     :: AllocAdams         ! Determines if the columns for Adams data will be read
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat            ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg             ! Error message


   IF ( InputFileData%NTwInpSt < 1 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating arrays for tower input properties: NTwInpSt must be at least 1.'
      RETURN
   END IF


      ! Allocate the arrays.

   CALL AllocAry  ( InputFileData%HtFract,   InputFileData%NTwInpSt, 'HtFract'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TMassDen,  InputFileData%NTwInpSt, 'TMassDen'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TwFAStif,  InputFileData%NTwInpSt, 'TwFAStif'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TwSSStif,  InputFileData%NTwInpSt, 'TwSSStif'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

   IF ( AllocAdams ) THEN
      CALL AllocAry  ( InputFileData%TwGJStif,  InputFileData%NTwInpSt, 'TwGJStif'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( InputFileData%TwEAStif,  InputFileData%NTwInpSt, 'TwEAStif'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( InputFileData%TwFAIner,  InputFileData%NTwInpSt, 'TwFAIner'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( InputFileData%TwSSIner,  InputFileData%NTwInpSt, 'TwSSIner'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( InputFileData%TwFAcgOf,  InputFileData%NTwInpSt, 'TwFAcgOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( InputFileData%TwSScgOf,  InputFileData%NTwInpSt, 'TwSScgOf'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   END IF
      

      ! BJJ: note that these used to be allocated 2:PolyOrd  :
   CALL AllocAry  ( InputFileData%TwFAM1Sh,  PolyOrd-1, 'TwFAM1Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TwFAM2Sh,  PolyOrd-1, 'TwFAM2Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TwSSM1Sh,  PolyOrd-1, 'TwSSM1Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( InputFileData%TwSSM2Sh,  PolyOrd-1, 'TwSSM2Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


END SUBROUTINE Alloc_TowerInputProperties
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_TowerParameters( p, AllocAdams, ErrStat, ErrMsg )
! This routine allocates arrays for the tower parameters.
!..................................................................................................................................

   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   LOGICAL,                  INTENT(IN)     :: AllocAdams                          ! Logical to determine if Adams inputs should be allocated
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Err msg




      ! Allocate arrays to hold tower data at the analysis nodes.
   CALL AllocAry  ( p%HNodesNorm,    p%TwrNodes, 'HNodesNorm', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%HNodes,        p%TwrNodes, 'HNodes'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%DHNodes,       p%TwrNodes, 'DHNodes'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%MassT,         p%TwrNodes, 'MassT'     , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%StiffTFA,      p%TwrNodes, 'StiffTFA'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%StiffTSS,      p%TwrNodes, 'StiffTSS'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

   IF ( AllocAdams ) THEN
      CALL AllocAry  ( p%StiffTGJ,      p%TwrNodes, 'StiffTGJ'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%StiffTEA,      p%TwrNodes, 'StiffTEA'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%InerTFA,       p%TwrNodes, 'InerTFA'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%InerTSS,       p%TwrNodes, 'InerTSS'   , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%cgOffTFA,      p%TwrNodes, 'cgOffTFA'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
      CALL AllocAry  ( p%cgOffTSS,      p%TwrNodes, 'cgOffTSS'  , ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   END IF

   !   ! these are for HydroDyn?
   !CALL AllocAry  ( p%DiamT,         p%TwrNodes, 'DiamT'     , ErrStat, ErrMsg )
   !IF ( ErrStat /= ErrID_None ) RETURN
   !CALL AllocAry  ( p%CAT,           p%TwrNodes, 'CAT'       , ErrStat, ErrMsg )
   !IF ( ErrStat /= ErrID_None ) RETURN
   !CALL AllocAry  ( p%CDT,           p%TwrNodes, 'CDT'       , ErrStat, ErrMsg )
   !IF ( ErrStat /= ErrID_None ) RETURN



   !      ! Allocate space for the mode shape arrays:
   !
   !ALLOCATE( p%BldEdgSh(2:PolyOrd,p%NumBl), p%BldFl1Sh(2:PolyOrd,p%NumBl), p%BldFl2Sh(2:PolyOrd,p%NumBl), STAT = ErrStat )
   !IF ( ErrStat /= 0 ) THEN
   !   ErrStat = ErrID_Fatal
   !   ErrMsg  = ' Error allocating BldEdgSh, BldFl1Sh, and BldFl2Sh arrays.'
   !   RETURN
   !END IF



END SUBROUTINE Alloc_TowerParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetOtherParameters( p, InputFileData, ErrStat, ErrMsg )
! This routine sets the remaining parameters (replacing the former FAST Initialize routine), first allocating necessary arrays. 
! It requires p%NDOF, p%NumBl, p%TTopNode, p%TipNode to be set before calling this routine.
!..................................................................................................................................

   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                            ! The parameters of the structural dynamics module
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Err msg
 
   

   
      ! Allocate the arrays needed in the Coeff routine:
      
   CALL AllocAry( p%AxRedTFA, 2,       2_IntKi, p%TTopNode,         'AxRedTFA',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%AxRedTSS, 2,       2_IntKi, p%TTopNode,         'AxRedTSS',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%AxRedBld, p%NumBl, 3_IntKi, 3_IntKi, p%TipNode, 'AxRedBld',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%BldCG,    p%NumBl,                              'BldCG',     ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%KBF,      p%NumBl, 2_IntKi, 2_IntKi,            'KBF',       ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%KBE,      p%NumBl, 1_IntKi, 1_IntKi,            'KBE',       ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%CBF,      p%NumBl, 2_IntKi, 2_IntKi,            'CBF',       ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%CBE,      p%NumBl, 1_IntKi, 1_IntKi,            'CBE',       ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%SecondMom,p%NumBl,                              'SecondMom', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%FirstMom, p%NumBl,                              'FirstMom',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%FreqBE,   p%NumBl, NumBE, 3_IntKi,              'FreqBE',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%FreqBF,   p%NumBl, NumBF, 3_IntKi,              'FreqBF',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%BldMass,  p%NumBl,                              'BldMass',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
!bjj remove these two:   (we're going to get rid of these variables)
  CALL AllocAry( p%rSAerCenn1,p%NumBl,p%BldNodes,  'rSAerCenn1',  ErrStat, ErrMsg )
  IF ( ErrStat /= ErrID_None ) RETURN
  CALL AllocAry( p%rSAerCenn2,p%NumBl,p%BldNodes,  'rSAerCenn2',  ErrStat, ErrMsg )
  IF ( ErrStat /= ErrID_None ) RETURN
   
  
   ALLOCATE ( p%TwrFASF(2,p%TTopNode,0:2) , STAT=ErrStat )
   IF ( ErrStat /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating TwrFASF array.'
      RETURN
   END IF

   ALLOCATE ( p%TwrSSSF(2,p%TTopNode,0:2) , STAT=ErrStat   )
   IF ( ErrStat /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating TwrSSSF array.'
      RETURN
   END IF

   ALLOCATE ( p%TwistedSF(p%NumBl,2,3,p%TipNode,0:2) , STAT=ErrStat )
   IF ( ErrStat /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating TwistedSF array.'
      RETURN
   END IF
   
   
   CALL Coeff(p, InputFileData, ErrStat, ErrMsg)   
   IF ( ErrStat /= ErrID_None ) RETURN
  
   
END SUBROUTINE SetOtherParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_OtherState( OtherState, p, ErrStat, ErrMsg  )
! This routine allocates arrays in the RtHndSide data structure.
! It requires p%TwrNodes, p%NumBl, p%TipNode, p%NDOF, p%BldNodes to be set before calling this routine.
!..................................................................................................................................

   TYPE(ED_OtherStateType),  INTENT(INOUT)  :: OtherState                   ! other/optimization states
   TYPE(ED_ParameterType),   INTENT(IN)     :: p                            ! Parameters of the structural dynamics module
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message

  
   CALL Alloc_RtHS( OtherState%RtHS, p, ErrStat, ErrMsg  )
   IF (ErrStat /= ErrID_None ) RETURN
     
   CALL AllocAry( OtherState%QD2T, p%NDOF,   'OtherState%QD2T',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
      ! for loose coupling:
   CALL AllocAry( OtherState%IC,  NMX,   'IC',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   
!bjj: we probably should make these a Continuous State type:   
   CALL AllocAry( OtherState%Q,    p%NDOF, NMX,   'OtherState%Q',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( OtherState%QD,   p%NDOF, NMX,   'OtherState%QD',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( OtherState%QD2,  p%NDOF, NMX,   'OtherState%QD2',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
  
   
END SUBROUTINE Alloc_OtherState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_RtHS( RtHS, p, ErrStat, ErrMsg  )
! This routine allocates arrays in the RtHndSide data structure.
! It requires p%TwrNodes, p%NumBl, p%TipNode, p%NDOF, p%BldNodes to be set before calling this routine.
!..................................................................................................................................

   TYPE(ED_RtHndSide),       INTENT(INOUT)  :: RtHS                         ! RtHndSide data type
   TYPE(ED_ParameterType),   INTENT(IN)     :: p                            ! Parameters of the structural dynamics module
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message

   ! local variables:
   INTEGER(IntKi),   PARAMETER              :: Dims = 3                     ! The position arrays all must be allocated with a dimension for X,Y,and Z

      
   CALL AllocAry( RtHS%AngPosEF, p%TwrNodes,         Dims, 'AngPosEF',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%AngPosXF, p%TwrNodes,         Dims, 'AngPosXF',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%LinAccESt,p%NumBl, p%TipNode, Dims, 'LinAccESt', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%LinAccETt,p%TwrNodes,         Dims, 'LinAccETt', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%PFrcS0B,  p%NumBl,p%NDOF,     Dims, 'PFrcS0B',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%FrcS0Bt,  p%NumBl,            Dims, 'FrcS0Bt',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%PMomH0B,  p%NumBl, p%NDOF,    Dims, 'PMomH0B',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%MomH0Bt,  p%NumBl,            Dims, 'MomH0Bt',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%PFrcPRot,  p%NDOF,            Dims, 'PFrcPRot',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%PMomLPRot, p%NDOF,            Dims, 'PMomLPRot', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( RtHS%PMomNGnRt, p%NDOF,            Dims, 'PMomNGnRt', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PMomNTail, p%NDOF,            Dims, 'PMomNTail', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PFrcONcRt, p%NDOF,            Dims, 'PFrcONcRt', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PMomBNcRt, p%NDOF,            Dims, 'PMomBNcRt', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PFrcT0Trb, p%NDOF,            Dims, 'PFrcT0Trb', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PMomX0Trb, p%NDOF,            Dims, 'PMomX0Trb', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%FSAero,    p%NumBl,p%BldNodes,Dims, 'FSAero',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%MMAero,    p%NumBl,p%BldNodes,Dims, 'MMAero',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%FSTipDrag, p%NumBl,           Dims, 'FSTipDrag', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%rS,        p%NumBl,p%TipNode, Dims, 'rS',        ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%rS0S,      p%NumBl,p%TipNode, Dims, 'rS0S',      ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%AngPosHM,  p%NumBl,p%TipNode, Dims, 'AngPosHM',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%FTAero,    p%TwrNodes,        Dims, 'FTAero',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%MFAero,    p%TwrNodes,        Dims, 'MFAero',    ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PFTHydro,  p%TwrNodes, p%NDOF,Dims, 'PFTHydro',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%PMFHydro,  p%TwrNodes, p%NDOF,Dims, 'PMFHydro',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%FTHydrot,  p%TwrNodes,        Dims, 'FTHydrot',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%MFHydrot,  p%TwrNodes,        Dims, 'MFHydrot',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%rZT,       p%TwrNodes,        Dims, 'rZT',       ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%rT,        p%TwrNodes,        Dims, 'rT',        ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   CALL AllocAry( RtHS%rT0T,      p%TwrNodes,        Dims, 'rT0T',      ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
   

      ! These are allocated to start numbering a dimension with 0 instead of 1:
   ALLOCATE ( RtHS%PAngVelEB(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PAngVelEB array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PAngVelER(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PAngVelER array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PAngVelEX(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PAngVelEX array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PLinVelEIMU(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PLinVelEIMU array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PLinVelEO(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PLinVelEO array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PLinVelES(p%NumBl,p%TipNode,p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PLinVelES array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PLinVelET(p%TwrNodes,p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PLinVelET array.' 
      RETURN
   ENDIF

   ALLOCATE ( RtHS%PLinVelEZ(p%NDOF,0:1,Dims) , STAT=ErrStat )
   IF ( ErrStat /= 0_IntKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating memory for the PLinVelEZ array.' 
      RETURN
   ENDIF
   
END SUBROUTINE Alloc_RtHS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetTowerParameters( p, InputFileData, ErrStat, ErrMsg  )
! This takes the tower input file data and sets the corresponding tower parameters, performing linear interpolation of the
! input data to the specified tower mesh.
! It requires p%TwrFlexL, and p%TwrNodes to be set first.
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables

   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                            ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message

      ! Local variables:

   REAL(ReKi)                               :: x                            ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                          :: J                            ! Index for the node arrays
   INTEGER(IntKi)                           :: InterpInd                    ! Index for the interpolation routine
   LOGICAL                                  :: SetAdmVals                   ! Logical to determine if Adams inputs should be set


      ! Initialize data
   ErrStat   = ErrID_None
   ErrMsg    = ''
   SetAdmVals = ALLOCATED( InputFileData%TwGJStif )

   CALL Alloc_TowerParameters( p, SetAdmVals, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN


   !...............................................................................................................................
   ! Define the tower discretization arrays:
   !...............................................................................................................................

      ! DHNodes (Let's use constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--
      !          this will be a future input!):
   p%DHNodes = p%TwrFlexL/p%TwrNodes

      ! HNodes:
   p%HNodes(1) = 0.5*p%DHNodes(1)
   DO J=2,p%TwrNodes
      p%HNodes(J) = p%HNodes( J - 1 ) + 0.5*( p%DHNodes(J) + p%DHNodes( J - 1 ) )
   END DO

      ! HNodesNorm:
   p%HNodesNorm = p%HNodes/p%TwrFlexL


   !...............................................................................................................................
   ! Interpolate the input data to the tower discretization
   !...............................................................................................................................
   ! Array definitions:

   !    Input      Interp    Description
   !    -----      ------    -----------
   !    HtFract    HNodesNorm Fractional height (0 at top of rigid section, 1 at tower top)
   !    TMassDen   MassT      Lineal mass density
   !    TwFAStif   StiffTFA   Tower fore-aft stiffness
   !    TwSSStif   StiffTSS   Tower side-to-side stiffness
   !    TwGJStif   StiffTGJ   Tower torsional stiffness
   !    TwEAStif   StiffTEA   Tower extensional stiffness
   !    TwFAIner   InerTFA    Tower fore-aft (about yt-axis) mass inertia per unit length
   !    TwSSIner   InerTSS    Tower side-to-side (about xt-axis) mass inertia per unit length
   !    TwFAcgOf   cgOffTFA   Tower fore-aft mass cg offset
   !    TwSScgOf   cgOffTSS   Tower side-to-side mass cg offset

   InterpInd = 1


   DO J=1,p%TwrNodes

         ! Get the index into HtFract for all of the arrays, using the NWTC Subroutine Library
      p%MassT     (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TMassDen, InterpInd, InputFileData%NTwInpSt )
      p%StiffTFA  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwFAStif, InterpInd, InputFileData%NTwInpSt )
      p%StiffTSS  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwSSStif, InterpInd, InputFileData%NTwInpSt )
   END DO ! J
   
   
   IF ( SetAdmVals )  THEN          ! An ADAMS model will be created; thus, read in all the cols.
      DO J=1,p%TwrNodes      
         p%StiffTGJ  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwGJStif, InterpInd, InputFileData%NTwInpSt )
         p%StiffTEA  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwEAStif, InterpInd, InputFileData%NTwInpSt )
         p%InerTFA   (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwFAIner, InterpInd, InputFileData%NTwInpSt )
         p%InerTSS   (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwSSIner, InterpInd, InputFileData%NTwInpSt )
         p%cgOffTFA  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwFAcgOf, InterpInd, InputFileData%NTwInpSt )
         p%cgOffTSS  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwSScgOf, InterpInd, InputFileData%NTwInpSt )
      END DO ! J
   END IF


   !...............................................................................................................................
   ! Set other tower parameters:
   !...............................................................................................................................

   p%TTopNode = p%TwrNodes + 1 

   !   ! these are for HydroDyn ?
   !p%DiamT(:) = InputFileData%TwrDiam
   !p%CAT(:)   = InputFileData%TwrCA
   !p%CDT(:)   = InputFileData%TwrCD
   !

RETURN


CONTAINS
!..................................................................................................................................
   FUNCTION InterpAry( x, YAry, Ind )
      ! This subroutine is used to interpolate the arrays more efficiently (all arrays have the same X value)
      ! See InterpStpReal() for comparison. This assumes we already know Ind and that
      ! x = ( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) )


      REAL(ReKi),      INTENT(IN) :: x                ! the relative distance between Ind and Ind+ 1
      REAL(ReKi),      INTENT(IN) :: YAry (:)         ! Array of Y values to be interpolated.
      INTEGER(IntKi) , INTENT(IN) :: Ind              ! the index into the array

      REAL(ReKi)                  :: InterpAry        ! the value calculated in this function

      InterpAry = ( YAry(Ind+1) - YAry(Ind) ) * x  + YAry(Ind)

   END FUNCTION InterpAry

END SUBROUTINE SetTowerParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateFurlData( InputFileData, ErrStat, ErrMsg )
! This routine validates the furling inputs.
!..................................................................................................................................

      ! Passed variables:

   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                       ! All the data in the ElastoDyn input file

   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

      ! Local variables:
   CHARACTER(1024)                          :: TmpMsg                              ! a temporary message (so I don't have to keep typing the same error message)
   REAL(ReKi)                               :: SmallAngleLimit_Rad                 ! Largest input angle considered "small" (check in input file), radians
   

      ! Initialize error status and angle limit defined locally (in correct units)
      
   ErrStat = ErrID_None
   ErrMsg  = ''
   SmallAngleLimit_Rad = SmallAngleLimit_Deg*D2R                                 

   
      ! note that all angles are assumed to be in radians here:

      ! Check that angles are in the range (-pi, pi] radians (i.e., (-180, 180] degrees ):
      ! NOTE: these are local subroutines, with ErrStat and ErrMsg INTENT(INOUT)

   CALL CheckAngle180Range( InputFileData%RotFurl,  'RotFurl',  ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TailFurl, 'TailFurl', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TFinSkew, 'TFinSkew', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TFinBank, 'TFinBank', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%RFrlSkew, 'RFrlSkew', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TFrlSkew, 'TFrlSkew', ErrStat, ErrMsg )

   CALL CheckAngle180Range( InputFileData%RFrlUSSP, 'RFrlUSSP', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TFrlUSSP, 'TFrlUSSP', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%RFrlUSDP, 'RFrlUSDP', ErrStat, ErrMsg )
   CALL CheckAngle180Range( InputFileData%TFrlUSDP, 'TFrlUSDP', ErrStat, ErrMsg )

   CALL CheckAngle180Range( InputFileData%RFrlDSSP, 'RFrlDSSP', ErrStat, ErrMsg )
   IF ( InputFileData%RFrlDSSP > InputFileData%RFrlUSSP ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  RFrlDSSP must not be larger than RFrlUSSP'
   END IF

   CALL CheckAngle180Range( InputFileData%RFrlDSDP, 'RFrlDSDP', ErrStat, ErrMsg )
   IF ( InputFileData%RFrlDSDP > InputFileData%RFrlUSDP ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  RFrlDSDP must not be larger than RFrlUSDP'
   END IF

   CALL CheckAngle180Range( InputFileData%TFrlDSSP, 'TFrlDSSP', ErrStat, ErrMsg )
   IF ( InputFileData%TFrlDSSP > InputFileData%TFrlUSSP ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  TFrlDSSP must not be larger than TFrlUSSP'
   END IF

   CALL CheckAngle180Range( InputFileData%TFrlDSDP, 'TFrlDSDP', ErrStat, ErrMsg )
   IF ( InputFileData%TFrlDSDP > InputFileData%TFrlUSDP ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  TFrlDSDP must not be larger than TFrlUSDP'
   END IF


      ! Check that tilt angles are in the range [-pi/2, pi/2] radians (i.e., [-90, 90] degrees ):

   CALL CheckAngle90Range( InputFileData%TFinTilt, 'TFinTilt', ErrStat, ErrMsg )
   CALL CheckAngle90Range( InputFileData%RFrlTilt, 'RFrlTilt', ErrStat, ErrMsg )
   CALL CheckAngle90Range( InputFileData%TFrlTilt, 'TFrlTilt', ErrStat, ErrMsg )


      ! Check for violations of the small-angle assumption (15-degree limit, using radians):

   IF ( ABS( InputFileData%ShftSkew ) > SmallAngleLimit_Rad )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  ShftSkew should only be used to skew the shaft a few degrees away from the zero-yaw ' &
                //'position and must not be used as a replacement for the yaw angle. '&
                //'ShftSkew must be between -'//TRIM(Num2LStr(SmallAngleLimit_Rad))//' and ' &
                                              //TRIM(Num2LStr(SmallAngleLimit_Rad))//' radians.'
   END IF


      ! Warn if tail is defined upwind of the tower:

   IF ( InputFileData%BoomCMxn < 0.0_ReKi )  THEN   ! Print out warning when tail boom CM defined upwind of the tower.
      ErrStat = MAX(ErrID_Warn,ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  WARNING: Tail boom CM is defined upwind of the tower (BoomCMxn < 0).'
   ENDIF

   IF ( InputFileData%TFinCMxn < 0.0_ReKi )  THEN   ! Print out warning when tail fin CM defined upwind of the tower.
      ErrStat = MAX(ErrID_Warn,ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  WARNING: Tail fin CM is defined upwind of the tower (TFinCMxn < 0).'
   ENDIF

   IF ( InputFileData%TFinCPxn < 0.0_ReKi )  THEN   ! Print out warning when tail fin CP defined upwind of the tower.
      ErrStat = MAX(ErrID_Warn,ErrStat)
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  WARNING: Tail fin CP is defined upwind of the tower (TFinCPxn < 0).'
   ENDIF


      ! Check that mass, inertias, damping, etc. values aren't negative:

   CALL CheckNegative( InputFileData%RFrlMass, 'RFrlMass', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%BoomMass, 'BoomMass', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFinMass, 'TFinMass', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlIner, 'RFrlIner', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlIner, 'TFrlIner', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlSpr,  'RFrlSpr',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlDmp,  'RFrlDmp',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlCDmp, 'RFrlCDmp', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlUSSpr,'RFrlUSSpr',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlDSSpr,'RFrlDSSpr',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlUSDmp,'RFrlUSDmp',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RFrlDSDmp,'RFrlDSDmp',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlSpr,  'TFrlSpr',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlDmp,  'TFrlDmp',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlCDmp, 'TFrlCDmp', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlUSSpr,'TFrlUSSpr',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlDSSpr,'TFrlDSSpr',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlUSDmp,'TFrlUSDmp',ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TFrlDSDmp,'TFrlDSDmp',ErrStat, ErrMsg )


      ! Check that furling models are valid:

   IF ( InputFileData%TFrlMod < 0 .OR. InputFileData%TFrlMod > 2 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  TFrlMod must be 0, 1, or 2.'
   END IF

   IF ( InputFileData%RFrlMod < 0 .OR. InputFileData%RFrlMod > 2 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  RFrlMod must be 0, 1, or 2.'
   END IF


   !   ! bjj: THESE ARE checks for tail fin aerodynamics, which should be in aerodyn, in my opinion
   !CALL CheckNegative( TFinArea,               'TFinArea',ErrStat, ErrMsg )
   !
   !IF ( TFinMod < 0 .OR. TFinMod > 2 )  THEN
   !   ErrStat = ErrID_Fatal
   !   ErrMsg  = TRIM(ErrMsg)//NewLine//'  TFinMod must be 0, 1, or 2.'
   !END IF


   RETURN

CONTAINS
   !-------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE CheckAngle180Range( Var, VarDesc, ErrStat, ErrMsg )
   ! This routine checks that an angle is in the range (-pi, pi] radians. If not, ErrStat = ErrID_Fatal
   ! Note that all values are assumed to be in radians, even if read in degrees (-180 deg, 180 deg]
   !...............................................................................................................................
   REAL(ReKi),     INTENT(IN)    :: Var         ! Variable to check
   CHARACTER(*),   INTENT(IN)    :: VarDesc     ! Description of variable (used in error message)
   INTEGER(IntKi), INTENT(INOUT) :: ErrStat     ! Error status to update if Var is not in specified range
   CHARACTER(*),   INTENT(INOUT) :: ErrMsg      ! Error message to update if Var is not in specified range

   
      IF ( ( Var <= -pi ) .OR. ( Var > pi ) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine// &
                   '  '//TRIM(VarDesc)//' must be greater than -pi radians and less than or equal to pi radians '// &
                                        '(i.e., in the range (-180, 180] degrees).'
      END IF

   END SUBROUTINE CheckAngle180Range
   !-------------------------------------------------------------------------------------------------------------------------------
 END SUBROUTINE ValidateFurlData
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetFurlParameters( p, InputFileData, ErrStat, ErrMsg  )
! This takes the furling input file data and sets the corresponding furling parameters.
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables

   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                            ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message

      ! Local variables:

   REAL(ReKi)                               :: x                            ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                          :: J                            ! Index for the node arrays
   INTEGER(IntKi)                           :: InterpInd                    ! Index for the interpolation routine


      ! Initialize error data
   ErrStat = ErrID_None
   ErrMsg  = ''
   
   
      ! Direct copy of InputFileData to parameters
      
   p%RFrlMass = InputFileData%RFrlMass
   p%BoomMass = InputFileData%BoomMass
   p%TFinMass = InputFileData%TFinMass   
   p%TFrlIner = InputFileData%TFrlIner   
   
   p%RFrlMod  = InputFileData%RFrlMod
   p%TFrlMod  = InputFileData%TFrlMod
   
   p%RFrlSpr  = InputFileData%RFrlSpr
   p%RFrlDmp  = InputFileData%RFrlDmp
   p%RFrlCDmp = InputFileData%RFrlCDmp
   p%RFrlUSSP = InputFileData%RFrlUSSP
   p%RFrlDSSP = InputFileData%RFrlDSSP
   p%RFrlDSSpr= InputFileData%RFrlDSSpr
   p%RFrlUSSpr= InputFileData%RFrlUSSpr
   p%RFrlUSDP = InputFileData%RFrlUSDP
   p%RFrlDSDP = InputFileData%RFrlDSDP
   p%RFrlUSDmp= InputFileData%RFrlUSDmp
   p%RFrlDSDmp= InputFileData%RFrlDSDmp
      
   p%TFrlSpr  = InputFileData%TFrlSpr
   p%TFrlDmp  = InputFileData%TFrlDmp
   p%TFrlCDmp = InputFileData%TFrlCDmp
   p%TFrlUSSP = InputFileData%TFrlUSSP
   p%TFrlDSSP = InputFileData%TFrlDSSP
   p%TFrlUSSpr= InputFileData%TFrlUSSpr
   p%TFrlDSSpr= InputFileData%TFrlDSSpr
   p%TFrlUSDP = InputFileData%TFrlUSDP
   p%TFrlDSDP = InputFileData%TFrlDSDP
   p%TFrlUSDmp= InputFileData%TFrlUSDmp
   p%TFrlDSDmp= InputFileData%TFrlDSDmp
   
   p%RFrlPntxn = InputFileData%RFrlPntxn
   p%RFrlPntyn = InputFileData%RFrlPntyn
   p%RFrlPntzn = InputFileData%RFrlPntzn
   
   p%TFrlPntxn = InputFileData%TFrlPntxn
   p%TFrlPntyn = InputFileData%TFrlPntyn
   p%TFrlPntzn = InputFileData%TFrlPntzn


      ! Store sine/cosine values instead of some input angles:
   
   p%CShftSkew = COS( InputFileData%ShftSkew )
   p%SShftSkew = SIN( InputFileData%ShftSkew )

   p%CTFinSkew = COS( InputFileData%TFinSkew )
   p%STFinSkew = SIN( InputFileData%TFinSkew )
   p%CTFinTilt = COS( InputFileData%TFinTilt )
   p%STFinTilt = SIN( InputFileData%TFinTilt )
   p%CTFinBank = COS( InputFileData%TFinBank )
   p%STFinBank = SIN( InputFileData%TFinBank )

   p%CRFrlSkew = COS( InputFileData%RFrlSkew )
   p%SRFrlSkew = SIN( InputFileData%RFrlSkew )
   p%CRFrlTilt = COS( InputFileData%RFrlTilt )
   p%SRFrlTilt = SIN( InputFileData%RFrlTilt )

   p%CTFrlSkew = COS( InputFileData%TFrlSkew )
   p%STFrlSkew = SIN( InputFileData%TFrlSkew )
   p%CTFrlTilt = COS( InputFileData%TFrlTilt )
   p%STFrlTilt = SIN( InputFileData%TFrlTilt )

   
      ! Common multiplications of sines and cosines:

   p%CRFrlSkw2 = p%CRFrlSkew**2
   p%SRFrlSkw2 = p%SRFrlSkew**2
   p%CSRFrlSkw = p%CRFrlSkew*p%SRFrlSkew
   p%CRFrlTlt2 = p%CRFrlTilt**2
   p%SRFrlTlt2 = p%SRFrlTilt**2
   p%CSRFrlTlt = p%CRFrlTilt*p%SRFrlTilt

   p%CTFrlSkw2 = p%CTFrlSkew**2
   p%STFrlSkw2 = p%STFrlSkew**2
   p%CSTFrlSkw = p%CTFrlSkew*p%STfrlSkew
   p%CTFrlTlt2 = p%CTFrlTilt**2
   p%STFrlTlt2 = p%STFrlTilt**2
   p%CSTFrlTlt = p%CTFrlTilt*p%STFrlTilt


      ! Calculate some positions:   
   
   p%rWIxn     = InputFileData%BoomCMxn - p%TFrlPntxn
   p%rWIyn     = InputFileData%BoomCMyn - p%TFrlPntyn
   p%rWIzn     = InputFileData%BoomCMzn - p%TFrlPntzn

   p%rWJxn     = InputFileData%TFinCMxn - p%TFrlPntxn
   p%rWJyn     = InputFileData%TFinCMyn - p%TFrlPntyn
   p%rWJzn     = InputFileData%TFinCMzn - p%TFrlPntzn

   p%rWKxn     = InputFileData%TFinCPxn - p%TFrlPntxn
   p%rWKyn     = InputFileData%TFinCPyn - p%TFrlPntyn
   p%rWKzn     = InputFileData%TFinCPzn - p%TFrlPntzn

   p%rVDxn     = InputFileData%RFrlCMxn - p%RFrlPntxn
   p%rVDyn     = InputFileData%RFrlCMyn - p%RFrlPntyn
   p%rVDzn     = InputFileData%RFrlCMzn - p%RFrlPntzn

   p%rVPxn     =        0.0_ReKi        - p%RFrlPntxn
   p%rVPyn     = InputFileData%Yaw2Shft - p%RFrlPntyn   
   
   
      ! Note: These positions are also used for non-furling machines:
      
   p%rVPzn     = InputFileData%Twr2Shft - p%RFrlPntzn 
   p%rVIMUxn   = InputFileData%NcIMUxn  - p%RFrlPntxn 
   p%rVIMUyn   = InputFileData%NcIMUyn  - p%RFrlPntyn 
   p%rVIMUzn   = InputFileData%NcIMUzn  - p%RFrlPntzn   
   
END SUBROUTINE SetFurlParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetPrimaryParameters( p, InputFileData, ErrStat, ErrMsg  )
! This takes the primary input file data and sets the corresponding parameters.
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables

   TYPE(ED_ParameterType),   INTENT(INOUT)  :: p                            ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message


      ! Initialize error data
   ErrStat = ErrID_None
   ErrMsg  = ''

   !p%Twr2Shft  = InputFileData%Twr2Shft
   !p%HubIner   = InputFileData%HubIner
   !p%NacYIner  = InputFileData%NacYIner

   !...............................................................................................................................
   ! Direct copy of variables:
   !...............................................................................................................................     
   p%NumBl     = InputFileData%NumBl
   p%TipRad    = InputFileData%TipRad
   p%HubRad    = InputFileData%HubRad

   p%TwrNodes  = InputFileData%TwrNodes
   
   p%DT        = InputFileData%DT
   p%Gravity   = InputFileData%Gravity
   p%OverHang  = InputFileData%OverHang
   p%ShftGagL  = InputFileData%ShftGagL
   p%TowerHt   = InputFileData%TowerHt
   p%TwrRBHt   = InputFileData%TwrRBHt
   p%TwrDraft  = InputFileData%TwrDraft
   p%PtfmRef   = InputFileData%PtfmRef
   p%TipMass   = InputFileData%TipMass
   p%HubMass   = InputFileData%HubMass
   p%GenIner   = InputFileData%GenIner
   p%NacMass   = InputFileData%NacMass
   p%YawBrMass = InputFileData%YawBrMass
   p%PtfmMass  = InputFileData%PtfmMass
   p%PtfmRIner = InputFileData%PtfmRIner
   p%PtfmPIner = InputFileData%PtfmPIner
   p%PtfmYIner = InputFileData%PtfmYIner
   p%GBoxEff   = InputFileData%GBoxEff
   p%GBRatio   = InputFileData%GBRatio
   p%DTTorSpr  = InputFileData%DTTorSpr
   p%DTTorDmp  = InputFileData%DTTorDmp 
     
   
   p%NTwGages  = InputFileData%NTwGages
   p%TwrGagNd  = InputFileData%TwrGagNd
   p%NBlGages  = InputFileData%NBlGages
   p%BldGagNd  = InputFileData%BldGagNd
   !p%OutFile   = InputFileData%OutFile
   !p%OutFileFmt= InputFileData%OutFileFmt !wrbinoutput, wrtxtoutput???
   p%OutFmt    = InputFileData%OutFmt 
   p%Tstart    = InputFileData%Tstart
   !p%DecFact   = InputFileData%DecFact
   p%NumOuts   = InputFileData%NumOuts
   
   IF ( p%NumBl == 2 ) THEN
      p%UndSling = InputFileData%UndSling    
      p%TeetMod  = InputFileData%TeetMod
      p%TeetDmpP = InputFileData%TeetDmpP
      p%TeetDmp  = InputFileData%TeetDmp
      p%TeetCDmp = InputFileData%TeetCDmp
      p%TeetSStP = InputFileData%TeetSStP
      p%TeetHStP = InputFileData%TeetHStP
      p%TeetSSSp = InputFileData%TeetSSSp
      p%TeetHSSp = InputFileData%TeetHSSp
   ELSE ! Three-bladed turbines don't use these parameters, so set them to zero.
      p%UndSling = 0.0         
      p%TeetMod  = 0
      p%TeetDmpP = 0.0
      p%TeetDmp  = 0.0
      p%TeetCDmp = 0.0
      p%TeetSStP = 0.0      
      p%TeetHStP = 0.0
      p%TeetSSSp = 0.0
      p%TeetHSSp = 0.0
   END IF
   
   
      ! initialize all of the DOF parameters:
   CALL Init_DOFparameters( InputFileData, p, ErrStat, ErrMsg ) !sets p%NDOF and p%NAug
      
      ! Set parameters for output channels:
   CALL SetOutParam(InputFileData%OutList, p, ErrStat, ErrMsg ) ! requires: p%NumOuts, p%NumBl, p%NBlGages, p%NTwGages; sets: p%OutParam.

   IF ( InputFileData%TabDelim ) THEN
      p%Delim = TAB
   ELSE
      p%Delim = ' '
   END IF 
   
   IF ( InputFileData%TabDelim ) THEN
      p%Delim = TAB
   ELSE
      p%Delim = ' '
   END IF 
   
   !...............................................................................................................................
   ! Calculate some indirect inputs:
   !...............................................................................................................................
   p%TwoPiNB   = TwoPi/p%NumBl                                                     ! 2*Pi/NumBl is used in RtHS().
   
   p%rZT0zt    = p%TwrRBHt + p%PtfmRef  - p%TwrDraft                               ! zt-component of position vector rZT0.
   p%RefTwrHt  = p%TowerHt + p%PtfmRef                                             ! Vertical distance between ElastoDyn's undisplaced tower height (variable TowerHt) and ElastoDyn's inertia frame reference point (variable PtfmRef).
   p%TwrFlexL  = p%TowerHt + p%TwrDraft - p%TwrRBHt                                ! Height / length of the flexible portion of the tower.
   p%BldFlexL  = p%TipRad               - p%HubRad                                 ! Length of the flexible portion of the blade.
   
   p%rZYzt     = p%PtfmRef  - InputFileData%PtfmCM

   !...............................................................................................................................
   ! set cosine and sine of Precone and Delta3 angles:
   !...............................................................................................................................
   CALL AllocAry( p%CosPreC,  p%NumBl,                              'CosPreC',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( p%SinPreC,  p%NumBl,                              'SinPreC',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
   p%CosPreC  = COS( InputFileData%Precone(1:p%NumBl) )
   p%SinPreC  = SIN( InputFileData%Precone(1:p%NumBl) )
   p%CosDel3  = COS( InputFileData%Delta3 )
   p%SinDel3  = SIN( InputFileData%Delta3 )   
      
   !...............................................................................................................................

      ! Calculate the average tip radius normal to the shaft (AvgNrmTpRd)
      !   and the swept area of the rotor (ProjArea):

   p%AvgNrmTpRd = p%TipRad*SUM(p%CosPreC)/p%NumBl     ! Average tip radius normal to the saft.
   p%ProjArea   = pi*( p%AvgNrmTpRd**2 )              ! Swept area of the rotor projected onto the rotor plane (the plane normal to the low-speed shaft).

   p%RotSpeed  = InputFileData%RotSpeed               ! Rotor speed in rad/sec.
   p%CShftTilt = COS( InputFileData%ShftTilt )
   p%SShftTilt = SIN( InputFileData%ShftTilt )
   
   p%FASTHH    = p%TowerHt + InputFileData%Twr2Shft + p%OverHang*p%SShftTilt

   
      ! Direct copy of InputFileData to parameters
      
   !p%FlapDOF1  = InputFileData%FlapDOF1
   !p%FlapDOF2  = InputFileData%FlapDOF2
   !p%EdgeDOF   = InputFileData%EdgeDOF
   !p%TeetDOF   = InputFileData%TeetDOF
   !p%DrTrDOF   = InputFileData%DrTrDOF
   !p%GenDOF    = InputFileData%GenDOF
   !p%YawDOF    = InputFileData%YawDOF
   !p%TwFADOF1  = InputFileData%TwFADOF1
   !p%TwFADOF2  = InputFileData%TwFADOF2
   !p%TwSSDOF1  = InputFileData%TwSSDOF1
   !p%TwSSDOF2  = InputFileData%TwSSDOF2
   !p%PtfmSgDOF = InputFileData%PtfmSgDOF
   !p%PtfmSwDOF = InputFileData%PtfmSwDOF
   !p%PtfmHvDOF = InputFileData%PtfmHvDOF
   !p%PtfmRDOF  = InputFileData%PtfmRDOF
   !p%PtfmPDOF  = InputFileData%PtfmPDOF
   !p%PtfmYDOF  = InputFileData%PtfmYDOF
   !p%Azimuth   = InputFileData%Azimuth
   p%RotSpeed  = InputFileData%RotSpeed
   !p%TTDspFA   = InputFileData%TTDspFA
   !p%TTDspSS   = InputFileData%TTDspSS
   !p%PtfmSurge = InputFileData%PtfmSurge
   !p%PtfmSway  = InputFileData%PtfmSway
   !p%PtfmHeave = InputFileData%PtfmHeave
   !p%PtfmRoll  = InputFileData%PtfmRoll
   !p%PtfmPitch = InputFileData%PtfmPitch
   !p%PtfmYaw   = InputFileData%PtfmYaw   
   p%HubCM     = InputFileData%HubCM
   p%AzimB1Up  = InputFileData%AzimB1Up
   
   p%NacCMxn   = InputFileData%NacCMxn
   p%NacCMyn   = InputFileData%NacCMyn
   p%NacCMzn   = InputFileData%NacCMzn
   !p%NcIMUxn   = InputFileData%NcIMUxn
   !p%NcIMUyn   = InputFileData%NcIMUyn
   !p%NcIMUzn   = InputFileData%NcIMUzn
         
   
   ! plus everything else from FAST_Initialize
   
   
      
END SUBROUTINE SetPrimaryParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadBladeInputs ( BldFile, MeshFile, ReadAdmVals, InputFileData, UnEc, ErrStat, ErrMsg )
! This routine reads the data from the blade and mesh inputs files.
! This routines assumes that InputFileData%NumBl has already been set.
!..................................................................................................................................
   

   IMPLICIT                        NONE


      ! Passed variables:

!   TYPE(ED_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),     INTENT(INOUT)  :: InputFileData                       ! Input file data Data for Blade K stored in the module's input file
   CHARACTER(*),           INTENT(IN)     :: BldFile(:)                          ! The array of file names containing blade information
   CHARACTER(*),           INTENT(IN)     :: MeshFile                            ! The file names containing blade mesh information (for now, the aerodyn primary file)
   INTEGER(IntKi),         INTENT(IN)     :: UnEc                                ! I/O unit for echo file. If present and > 0, write to UnEc

   INTEGER(IntKi),         INTENT(OUT)    :: ErrStat                             ! The error ID
   CHARACTER(*),           INTENT(OUT)    :: ErrMsg                              ! Message describing error
   LOGICAL,                INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file


      ! Local variables:
   INTEGER(IntKi)                         :: K                                   ! Blade number
   INTEGER(IntKi)                         :: ErrStat2                            ! Temporary error ID
   LOGICAL                                :: ReadFile                            ! determines if an input file for a blade is the same as the file for the previous blade
   CHARACTER(LEN(ErrMsg))                 :: ErrMsg2                             ! Temporary message describing error


      ! Initialize variables
   ErrStat = ErrID_None
   ErrMsg  = ''
      
   
      ! Allocate space for the input file data
   ALLOCATE( InputFileData%InpBlMesh( 1_IntKi ), STAT=ErrStat2 )              ! for now, we're assuming the discretization is the same on all blades
   IF ( ErrStat2 /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating InpBl array'
      RETURN
   END IF  
   
   ALLOCATE( InputFileData%InpBl( InputFileData%NumBl ), STAT=ErrStat2 )
   IF ( ErrStat2 /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating InpBl array'
      RETURN
   END IF
   
   
   
      ! Get the blade discretization here:   
   CALL ReadBladeMeshFile( InputFileData%InpBlMesh(1), MeshFile, UnEc, ErrStat2, ErrMsg2 )
      CALL CheckError(ErrStat2,ErrMsg2)
      IF ( ErrStat >= AbortErrLev ) RETURN
         
   
      ! Read the input file(s) for all of the blades:
   ReadFile = .TRUE.
   DO K = 1,InputFileData%NumBl   
   
      IF ( ReadFile ) THEN
      
            ! Add a separator to the echo file if appropriate.

         IF ( UnEc > 0 )  THEN
            WRITE (UnEc,'(//,A,/)')  'Blade '//TRIM( Num2LStr( K ) )//' input data from file "'//TRIM( BldFile(K) )//'":'
         END IF 
         
         CALL ReadBladeFile( BldFile(K), InputFileData%InpBl(K), ReadAdmVals, UnEc, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,' Errors reading blade '//TRIM(Num2LStr(K))//' input file ('//TRIM(BldFile(K))//'): '&
                                    //NewLine//TRIM(ErrMsg2))
            IF ( ErrStat >= AbortErrLev ) RETURN
      
      ELSE 
              
         CALL ED_CopyBladeInputData( InputFileData%InpBl(K-1), InputFileData%InpBl(K), MESH_UPDATECOPY, ErrStat2, ErrMsg2 )  
            CALL CheckError(ErrStat2,' Errors copying blade '//TRIM(Num2LStr(K-1))//' input file data: '//NewLine//TRIM(ErrMsg2))
            IF ( ErrStat >= AbortErrLev ) RETURN
               ! bjj: we could just read the file again...            

      END IF      

         ! If the next file is the same as this one, don't read it again:
         
      IF ( K /= InputFileData%NumBl ) ReadFile = BldFile(K) /= BldFile( K + 1 )
    
   END DO
         

   RETURN
   
CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            
         END IF

      END IF


   END SUBROUTINE CheckError   
   
END SUBROUTINE ReadBladeInputs
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadBladeFile ( BldFile, BladeKInputFileData, ReadAdmVals, UnEc, ErrStat, ErrMsg )
! This routine reads a blade input file.
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables:

   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   CHARACTER(*),             INTENT(IN)     :: BldFile                             ! Name of the blade input file data
   LOGICAL,                  INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file
   INTEGER(IntKi),           INTENT(IN)     :: UnEc                                ! I/O unit for echo file. If present and > 0, write to UnEc

   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message


      ! Local variables:

   REAL(ReKi)                   :: AdjBlMs                                         ! Factor to adjust blade mass density.
   REAL(ReKi)                   :: AdjEdSt                                         ! Factor to adjust edge stiffness.
   REAL(ReKi)                   :: AdjFlSt                                         ! Factor to adjust flap stiffness.

   REAL(ReKi)                   :: TmpRAry(17)                                     ! Temporary variable to read table from file (up to 17 columns)

   INTEGER(IntKi)               :: I                                               ! A generic DO index.
   INTEGER( IntKi )             :: UnIn                                            ! Unit number for reading file
   INTEGER( IntKi )             :: NInputCols                                      ! Number of columns to be read from the file
   INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error status
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                         ! Temporary Err msg



   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the input file for blade K.

   CALL OpenFInpFile ( UnIn, BldFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- HEADER -------------------------------------------------------

      ! Ship the header.

   CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 1', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 2', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 3', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !  -------------- BLADE PARAMETERS ---------------------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, BldFile, 'blade parameters', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NBlInpSt - Number of blade input stations.

   CALL ReadVar ( UnIn, BldFile, BladeKInputFileData%NBlInpSt, 'NBlInpSt', 'Number of blade input stations', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! .......... Allocate the arrays based on this NBlInpSt input ..........
   CALL Alloc_BladeInputProperties( BladeKInputFileData, ReadAdmVals, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! BldFlDmp - Blade structural damping ratios in flapwise direction.

   CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldFlDmp, SIZE(BladeKInputFileData%BldFlDmp), 'BldFlDmp', &
                                       'Blade structural damping ratios in flapwise direction', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! BldEdDmp - Blade structural damping ratios in edgewise direction.

   CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldEdDmp, SIZE(BladeKInputFileData%BldEdDmp), 'BldEdDmp', &
                                       'Blade structural damping ratios in edgewise direction', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !  -------------- BLADE ADJUSTMENT FACTORS -------------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, BldFile, 'blade adjustment factors', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! FlStTunr(1) - Blade flapwise modal stiffness tuners.

   CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%FlStTunr, SIZE(BladeKInputFileData%FlStTunr), 'FlStTunr', &
                                                  'Blade flapwise modal stiffness tuners', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjBlMs - Factor to adjust blade mass density.

   CALL ReadVar ( UnIn, BldFile, AdjBlMs, 'AdjBlMs', 'Factor to adjust blade mass density', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjFlSt - Factor to adjust blade flap stiffness.

   CALL ReadVar ( UnIn, BldFile, AdjFlSt, 'AdjFlSt', 'Factor to adjust blade flap stiffness', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjEdSt - Factor to adjust blade edge stiffness.

   CALL ReadVar ( UnIn, BldFile, AdjEdSt, 'AdjEdSt', 'Factor to adjust blade edge stiffness', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Check the locally-defined adjustment factors: AdjBlMs, AdjFlSt, AdjEdSt

      IF ( AdjBlMs <= 0.0_ReKi ) THEN
         CALL CheckError( ErrID_Warn, ' AdjBlMs must be greater than zero.' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( AdjFlSt <= 0.0_ReKi ) THEN
         CALL CheckError( ErrID_Warn, ' AdjFlSt must be greater than zero.' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( AdjEdSt <= 0.0_ReKi ) THEN
         CALL CheckError( ErrID_Warn, ' AdjEdSt must be greater than zero.' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF


   !  -------------- DISTRIBUTED BLADE PROPERTIES ---------------------------------


      ! Skip the comment lines.

   CALL ReadCom ( UnIn, BldFile, 'distributed blade parameters'     , ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter names', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter units', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! Read the table.

   IF ( ReadAdmVals ) THEN
      NInputCols = 17
   ELSE
      NInputCols = 6
   END IF


   DO I=1,BladeKInputFileData%NBlInpSt

      CALL ReadAry( UnIn, BldFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Blade input station table', &
                    ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN

      BladeKInputFileData%BlFract( I) = TmpRAry(1)
      BladeKInputFileData%AerCen(  I) = TmpRAry(2)
      BladeKInputFileData%StrcTwst(I) = TmpRAry(3)*D2R      ! Input in degrees; converted to radians here
      BladeKInputFileData%BMassDen(I) = TmpRAry(4)*AdjBlMs  ! Apply the correction factors to the elemental data.
      BladeKInputFileData%FlpStff( I) = TmpRAry(5)*AdjFlSt  ! Apply the correction factors to the elemental data.
      BladeKInputFileData%EdgStff( I) = TmpRAry(6)*AdjEdSt  ! Apply the correction factors to the elemental data.

      IF ( NInputCols > 6 ) THEN
         BladeKInputFileData%GJStff(   I) = TmpRAry( 7)
         BladeKInputFileData%EAStff(   I) = TmpRAry( 8)
         BladeKInputFileData%Alpha(    I) = TmpRAry( 9)
         BladeKInputFileData%FlpIner(  I) = TmpRAry(10)
         BladeKInputFileData%EdgIner(  I) = TmpRAry(11)
         BladeKInputFileData%PrecrvRef(I) = TmpRAry(12)
         BladeKInputFileData%PreswpRef(I) = TmpRAry(13)
         BladeKInputFileData%FlpcgOf(  I) = TmpRAry(14)
         BladeKInputFileData%EdgcgOf(  I) = TmpRAry(15)
         BladeKInputFileData%FlpEAOf(  I) = TmpRAry(16)
         BladeKInputFileData%EdgEAOf(  I) = TmpRAry(17)
      END IF
   ENDDO ! I



   !  -------------- BLADE MODE SHAPES --------------------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, BldFile, 'blade mode shapes', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BldFl1Sh - Blade-flap mode-1 shape coefficients.
   CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl1Sh, SIZE(BladeKInputFileData%BldFl1Sh), 'BldFl1Sh', &
                           'Blade-flap mode-1 shape coefficients', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BldFl2Sh - Blade-flap mode-2 shape coefficients.

   CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl2Sh, SIZE(BladeKInputFileData%BldFl2Sh), 'BldFl2Sh', &
                    'Blade-flap mode-2 shape coefficients', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BldEdgSh - Blade-edge mode shape coefficients.

   CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldEdgSh, SIZE(BladeKInputFileData%BldEdgSh), 'BldEdgSh', &
                     'Blade-edge mode shape coefficients', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



   !  -------------- END OF FILE --------------------------------------------

      ! Close the blade file.

   CLOSE ( UnIn )
   RETURN


CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF

      END IF


   END SUBROUTINE CheckError

END SUBROUTINE ReadBladeFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadBladeMeshFile( BladeKInputFileMesh, MeshFile, UnEc, ErrStat, ErrMsg )
! This routine reads in the AeroDyn v13.00.00 input file to get the
!   blade discretization used in the structural dynamics module.
!..................................................................................................................................


   IMPLICIT                        NONE

      ! Passed variables

   TYPE(ED_BladeMeshInputData),   INTENT(INOUT)  :: BladeKInputFileMesh                 ! All the data in the ElastoDyn input file
   CHARACTER(*),                  INTENT(IN)     :: MeshFile                            ! Name of the AeroDyn input file data (for mesh)

   INTEGER(IntKi),                INTENT(IN)     :: UnEc                                ! I/O unit for echo file. If present and > 0, write to UnEc
   INTEGER(IntKi),                INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),                  INTENT(OUT)    :: ErrMsg                              ! Error message

      ! Local variables:
   INTEGER(IntKi), PARAMETER    :: NInputCols = 4                                       ! Number of input columns to be read from the file
   REAL(ReKi)                   :: TmpRAry(NInputCols)                                  ! Temporary variable to read table from file
   INTEGER(IntKi)               :: I                                                    ! loop counter
   INTEGER(IntKi)               :: NumFoil                                              ! number of airfoil lines to read
   INTEGER(IntKi)               :: UnIn                                                 ! Unit number for reading file
     
   INTEGER(IntKi)               :: ErrStat2                                             ! Temporary Error status
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                              ! Temporary Err msg



      ! Get an available unit number for the file.

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the AeroDyn input file.

   CALL OpenFInpFile ( UnIn, MeshFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Add a separator to the echo file if appropriate.

   IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Mesh input data from (AeroDyn input) file "'//TRIM( MeshFile )//'":'
 

   !  -------------- HEADER -------------------------------------------------------
   ! BJJ: This file is AeroDyn's input file. Until we decide on a format for the 
   ! structural dynamics input, we will get this information from AeroDyn like we
   ! used to.

   DO I = 1,17
      CALL ReadCom ( UnIn, MeshFile, 'AeroDyn input (for structural dynamics mesh)', ErrStat2, ErrMsg2  )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   END DO

   CALL ReadVar ( UnIn, MeshFile, NumFoil, 'NumFoil', &
                  'Number of airfoil lines to skip in AeroDyn input (for structural dynamics mesh)', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
   DO I = 1,NumFoil
      CALL ReadCom ( UnIn, MeshFile, 'AeroDyn input (for structural dynamics mesh)', ErrStat2, ErrMsg2  )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   END DO

      
  !  -------------- Blade Mesh Data --------------------------------------------------
 
      ! Read in the number of blade elements
   CALL ReadVar( UnIn, MeshFile, BladeKInputFileMesh%BldNodes, 'BldNodes', 'Number of blade elements', ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Allocate the arrays to store input 
   CALL Alloc_BladeMeshInputProperties( BladeKInputFileMesh, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
      ! Read comment line for the element table
   CALL ReadCom( UnIn, MeshFile, 'Blade element table headers', ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
   DO I = 1, BladeKInputFileMesh%BldNodes
   
      CALL ReadAry( UnIn, MeshFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Blade element input table', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN

         BladeKInputFileMesh%RNodes(  I) = TmpRAry(1)
         BladeKInputFileMesh%AeroTwst(I) = TmpRAry(2)*D2R  !Convert input file data (degrees) to radians
         BladeKInputFileMesh%Chord(   I) = TmpRAry(4)
      
   END DO
   
      !bjj: move this to a validation routine:
   IF ( ANY( BladeKInputFileMesh%Chord < 0.0_ReKi ) ) THEN
      CALL CheckError( ErrID_Fatal, 'Chord length must be larger than 0 meters.' )
      RETURN
   END IF
   
   
      ! Close the input file:

   CLOSE ( UnIn )
   RETURN


CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................

END SUBROUTINE ReadBladeMeshFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadFurlFile( FurlFile, InputFileData, UnEc, ErrStat, ErrMsg  )
! This routine reads the furling file input and converts units as appropriate.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables:

   TYPE(ED_InputFile),       INTENT(INOUT)  :: InputFileData                       ! All the data in the ElastoDyn input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   INTEGER(IntKi),           INTENT(IN)     :: UnEc                                ! I/O unit for echo file. If present and > 0, write to UnEc
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message
   CHARACTER(*),             INTENT(IN)     :: FurlFile                            ! Name of the furling input file data

      ! Local variables:

   INTEGER(IntKi)               :: UnIn                                            ! Unit number for reading file
   INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error status
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                         ! Temporary Err msg


      ! Get an available unit number for the file.

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the furling input file.

   CALL OpenFInpFile ( UnIn, FurlFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Add a separator to the echo file if appropriate.

   IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Furling input data from file "'//TRIM( FurlFile )//'":'


   !  -------------- FILE HEADER ---------------------------------------------------

   CALL ReadCom ( UnIn, FurlFile, 'unused tower furling header line 1', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, FurlFile, 'unused tower furling header line 2', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, FurlFile, 'unused tower furling header line 3', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- Furling FEATURE SWITCHES  --------------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'degree of freedom switches (cont)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! RFrlDOF - Rotor-furl DOF.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDOF, 'RFrlDOF', 'Rotor-furl DOF (flag)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlDOF - Tail-furl DOF.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDOF, 'TFrlDOF', 'Tail-furl DOF (flag)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- Furling INITIAL CONDITIONS ------------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'initial conditions (cont)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RotFurl - Initial or fixed rotor-furl angle (read in degrees, converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RotFurl, 'RotFurl', 'Initial or fixed rotor-furl angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      InputFileData%RotFurl   = InputFileData%RotFurl*D2R


      ! TailFurl - Initial or fixed tail-furl angle (read in degrees, converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TailFurl, 'TailFurl', 'Initial or fixed tail-furl angle (deg)',  &
                  ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      InputFileData%TailFurl  = InputFileData%TailFurl*D2R
      

   !  -------------- TURBINE CONFIGURATION (CONT) ---------------------------------


      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'turbine configuration (cont)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Yaw2Shft - Lateral distance from yaw axis to rotor shaft.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%Yaw2Shft, 'Yaw2Shft',  &
                  'Lateral distance from yaw axis to rotor shaft (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! ShftSkew - Rotor shaft skew angle (read in degrees and converted to radians here).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%ShftSkew, 'ShftSkew', 'Rotor shaft skew angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%ShftSkew  = InputFileData%ShftSkew *D2R
      

      ! RFrlCMxn - Downwind distance from tower-top to CM of structure that furls with the rotor (not including rotor).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlCMxn, 'RFrlCMxn',  &
                  'Downwind distance from tower-top to rotor-furl CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlCMyn - Lateral  distance from tower-top to CM of structure that furls with the rotor (not including rotor).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlCMyn, 'RFrlCMyn',  &
                  'Lateral  distance from tower-top to rotor-furl CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlCMzn - Vertical distance from tower-top to CM of structure that furls with the rotor (not including rotor).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlCMzn, 'RFrlCMzn',  &
                  'Vertical distance from tower-top to rotor-furl CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BoomCMxn - Downwind distance from tower-top to tail boom CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%BoomCMxn, 'BoomCMxn',  &
                  'Downwind distance from tower-top to tail boom CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BoomCMyn - Lateral  distance from tower-top to tail boom CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%BoomCMyn, 'BoomCMyn',  &
                  'Lateral  distance from tower-top to tail boom CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! BoomCMzn - Vertical distance from tower-top to tail boom CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%BoomCMzn, 'BoomCMzn', &
                   'Vertical distance from tower-top to tail boom CM (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCMxn - Downwind distance from tower-top to tail fin CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCMxn, 'TFinCMxn', &
                   'Downwind distance from tower-top to tail fin CM (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCMyn - Lateral  distance from tower-top to tail fin CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCMyn, 'TFinCMyn', &
                   'Lateral  distance from tower-top to tail fin CM (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCMzn - Vertical distance from tower-top to tail fin CM.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCMzn, 'TFinCMzn', &
                   'Vertical distance from tower-top to tail fin CM (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCPxn - Downwind distance from tower-top to tail fin CP.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCPxn, 'TFinCPxn', &
                  'Downwind distance from tower-top to tail fin CP (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCPyn - Lateral  distance from tower-top to tail fin CP.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCPyn, 'TFinCPyn', &
                  'Lateral  distance from tower-top to tail fin CP (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinCPzn - Vertical distance from tower-top to tail fin CP.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinCPzn, 'TFinCPzn', &
                  'Vertical distance from tower-top to tail fin CP (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinSkew - Tail fin chordline skew angle (read in degrees, converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinSkew, 'TFinSkew', 'Tail fin chordline skew angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFinSkew  = InputFileData%TFinSkew*D2R


      ! TFinTilt - Tail fin chordline tilt angle (read in degrees, converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinTilt, 'TFinTilt', 'Tail fin chordline tilt angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFinTilt  = InputFileData%TFinTilt *D2R
      

      ! TFinBank - Tail fin planform  bank angle (read in degrees, converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinBank, 'TFinBank', 'Tail fin planform  bank angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      InputFileData%TFinBank  = InputFileData%TFinBank *D2R


      ! RFrlPntxn - Downwind distance from tower-top to arbitrary point on rotor-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlPntxn, 'RFrlPntxn', &
                  'Downwind distance from tower-top to arbitrary point on rotor-furl axis (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlPntyn - Lateral  distance from tower-top to arbitrary point on rotor-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlPntyn, 'RFrlPntyn', &
                  'Lateral  distance from tower-top to arbitrary point on rotor-furl axis (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlPntzn - Vertical distance from tower-top to arbitrary point on rotor-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlPntzn, 'RFrlPntzn', &
                  'Vertical distance from tower-top to arbitrary point on rotor-furl axis (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlSkew - Rotor-furl axis skew angle (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlSkew, 'RFrlSkew', 'Rotor-furl axis skew angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlSkew  = InputFileData%RFrlSkew*D2R

      
      ! RFrlTilt - Rotor-furl axis tilt angle (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlTilt, 'RFrlTilt', 'Rotor-furl axis tilt angle (deg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlTilt  = InputFileData%RFrlTilt*D2R

      
      ! TFrlPntxn - Downwind distance from tower-top to arbitrary point on tail-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlPntxn, 'TFrlPntxn', &
                  'Downwind distance from tower-top to arbitrary point on tail-furl axis (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlPntyn - Lateral  distance from tower-top to arbitrary point on tail-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlPntyn, 'TFrlPntyn', &
                  'Lateral  distance from tower-top to arbitrary point on tail-furl axis (m)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlPntzn - Vertical distance from tower-top to arbitrary point on tail-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlPntzn, 'TFrlPntzn', &
                  'Vertical distance from tower-top to arbitrary point on tail-furl axis (m)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlSkew - Tail-furl axis skew angle (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlSkew, 'TFrlSkew', 'Tail-furl axis skew angle (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFrlSkew  = InputFileData%TFrlSkew *D2R

      
      ! TFrlTilt - Tail-furl axis tilt angle (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlTilt, 'TFrlTilt', 'Tail-furl axis tilt angle (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      InputFileData%TFrlTilt  = InputFileData%TFrlTilt *D2R


   !  -------------- MASS AND INERTIA (CONT) --------------------------------------

      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'mass and inertia (cont)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! RFrlMass - Mass of structure that furls with the rotor (not including rotor).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlMass, 'RFrlMass', 'Rotor-furl mass (kg)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! BoomMass - Tail boom mass.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%BoomMass, 'BoomMass', 'Tail boom mass (kg)',ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFinMass - Tail fin mass.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFinMass, 'TFinMass', 'Tail fin mass (kg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlIner - Inertia of structure that furls with the rotor about the rotor-furl axis (not including rotor).

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlIner, 'RFrlIner', 'Rotor-furl inertia about rotor-furl axis (kg m^2)', &
                                            ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlIner - Tail boom inertia about tail-furl axis.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlIner, 'TFrlIner', 'Tail boom inertia about tail-furl axis (kg m^2)', &
                  ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !  -------------- ROTOR-FURL ---------------------------------------------------

      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'Section heading: Rotor-Furl', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlMod - Rotor-furl spring/damper model switch.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlMod, 'RFrlMod', 'Rotor-furl spring/damper model switch', &
                  ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlSpr - Rotor-furl spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlSpr, 'RFrlSpr', 'Rotor-furl spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlDmp - Rotor-furl damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDmp, 'RFrlDmp', 'Rotor-furl damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlCDmp - Rotor-furl rate-independent Coulomb-damping moment.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlCDmp, 'RFrlCDmp', 'Rotor-furl rate-independent Coulomb-damping moment (N-m)', &
                                                         ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlUSSP - Rotor-furl up-stop spring position (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlUSSP, 'RFrlUSSP', 'Rotor-furl up-stop spring position (deg)', &
                  ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlUSSP  = InputFileData%RFrlUSSP*D2R
      

      ! RFrlDSSP - Rotor-furl down-stop spring position (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDSSP, 'RFrlDSSP', 'Rotor-furl down-stop spring position (deg)', &
                  ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlDSSP  = InputFileData%RFrlDSSP*D2R
      

      ! RFrlUSSpr - Rotor-furl up-stop spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlUSSpr, 'RFrlUSSpr', 'Rotor-furl up-stop spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlDSSpr - Rotor-furl down-stop spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDSSpr, 'RFrlDSSpr', 'Rotor-furl down-stop spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlUSDP - Rotor-furl up-stop damper position (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlUSDP, 'RFrlUSDP', 'Rotor-furl up-stop damper position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlUSDP  = InputFileData%RFrlUSDP*D2R
      

      ! RFrlDSDP - Rotor-furl down-stop damper position (read in degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDSDP, 'RFrlDSDP', 'Rotor-furl down-stop damper position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%RFrlDSDP  = InputFileData%RFrlDSDP*D2R
      

      ! RFrlUSDmp - Rotor-furl up-stop damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlUSDmp, 'RFrlUSDmp', 'Rotor-furl up-stop damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! RFrlDSDmp - Rotor-furl down-stop damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%RFrlDSDmp, 'RFrlDSDmp', 'Rotor-furl down-stop damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- TAIL-FURL ----------------------------------------------------

      ! Skip the comment line.

   CALL ReadCom ( UnIn, FurlFile, 'tail-furl', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlMod - Tail-furl spring/damper model switch.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlMod, 'TFrlMod', 'Tail-furl spring/damper model switch', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlSpr - Tail-furl spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlSpr, 'TFrlSpr', 'Tail-furl spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlDmp - Tail-furl damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDmp, 'TFrlDmp', 'Tail-furl damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlCDmp - Tail-furl rate-independent Coulomb-damping moment.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlCDmp, 'TFrlCDmp', 'Tail-furl rate-independent Coulomb-damping moment (N-m)', &
                                                                                ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlUSSP - Tail-furl up-stop spring position (read as degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlUSSP, 'TFrlUSSP', 'Tail-furl up-stop spring position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFrlUSSP  = InputFileData%TFrlUSSP*D2R

      
      ! TFrlDSSP - Tail-furl down-stop spring position (read as degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDSSP, 'TFrlDSSP', 'Tail-furl down-stop spring position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFrlDSSP  = InputFileData%TFrlDSSP*D2R

      
      ! TFrlUSSpr - Tail-furl up-stop spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlUSSpr, 'TFrlUSSpr', 'Tail-furl up-stop spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlDSSpr - Tail-furl down-stop spring constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDSSpr, 'TFrlDSSpr', 'Tail-furl down-stop spring constant (N-m/rad)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlUSDP - Tail-furl up-stop damper position.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlUSDP, 'TFrlUSDP', 'Tail-furl up-stop damper position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFrlUSDP  = InputFileData%TFrlUSDP*D2R

      
      ! TFrlDSDP - Tail-furl down-stop damper position (read as degrees and converted to radians here)

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDSDP, 'TFrlDSDP', 'Tail-furl down-stop damper position (deg)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%TFrlDSDP  = InputFileData%TFrlDSDP*D2R   

      
      ! TFrlUSDmp - Tail-furl up-stop damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlUSDmp, 'TFrlUSDmp', 'Tail-furl up-stop damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TFrlDSDmp - Tail-furl down-stop damping constant.

   CALL ReadVar ( UnIn, FurlFile, InputFileData%TFrlDSDmp, 'TFrlDSDmp', 'Tail-furl down-stop damping constant (N-m/(rad/s))', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Close the ElastoDyn furling file:

   CLOSE ( UnIn )

   RETURN
CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................
END SUBROUTINE ReadFurlFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadTowerFile( TwrFile, InputFileData, ReadAdmVals, UnEc, ErrStat, ErrMsg )
! This routine reads the tower file  input.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables:

   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   INTEGER(IntKi),           INTENT(IN)     :: UnEc                                ! I/O unit for echo file. If present and > 0, write to UnEc
   LOGICAL,                  INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message
   CHARACTER(*),             INTENT(IN)     :: TwrFile                             ! Name of the tower input file data
   TYPE(ED_InputFile),       INTENT(INOUT)  :: InputFileData                       ! All the data in the ElastoDyn input file


      ! Local variables:

   REAL(ReKi)                   :: AdjFASt                                         ! Factor to adjust tower fore-aft stiffness
   REAL(ReKi)                   :: AdjSSSt                                         ! Factor to adjust tower side-to-side stiffness
   REAL(ReKi)                   :: AdjTwMa                                         ! Factor to adjust tower mass density

   REAL(ReKi)                   :: TmpRAry(10)                                     ! Temporary variable to read table from file (up to 10 columns)

   INTEGER(IntKi)               :: I                                               ! A generic DO index.
   INTEGER(IntKi)               :: UnIn                                            ! Unit number for reading file
   INTEGER(IntKi)               :: NInputCols                                      ! Number of columns to be read from the file
   INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error status
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                         ! Temporary Err msg



   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the tower input file.

   CALL OpenFInpFile ( UnIn, TwrFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Add a separator to the echo file if appropriate.
   IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Tower input data from file "'//TRIM( TwrFile )//'":'


   !  -------------- FILE HEADER ---------------------------------------------------

   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 1', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 2', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 3', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !  -------------- TOWER PARAMETERS ---------------------------------------------

   CALL ReadCom ( UnIn, TwrFile, 'heading for tower parameters', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! NTwInpSt - Number of tower input stations.

   CALL ReadVar ( UnIn, TwrFile, InputFileData%NTwInpSt, 'NTwInpSt', 'Number of tower input stations', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Allocate the input arrays based on this NTwInpSt input
   CALL Alloc_TowerInputProperties( InputFileData, ReadAdmVals, ErrStat, ErrMsg )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwrFADmp - Tower fore-aft structural damping ratios.

   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwrFADmp, SIZE(InputFileData%TwrFADmp), 'TwrFADmp', &
                                     'Tower fore-aft structural damping ratios (%)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwrSSDmp - Tower side-to-side structural damping ratios.

   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwrSSDmp, SIZE(InputFileData%TwrSSDmp), 'TwrSSDmp', &
                                     'Tower side-to-side structural damping ratios (%)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- TOWER ADJUSTMENT FACTORS -------------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower adjustment factors', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! FAStTunr - Tower fore-aft modal stiffness tuners.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%FAStTunr, SIZE(InputFileData%FAStTunr), 'FAStTunr', &
                                     'Tower fore-aft modal stiffness tuners (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! SSStTunr - Tower side-to-side modal stiffness tuners.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%SSStTunr, SIZE(InputFileData%SSStTunr), 'SSStTunr', &
                                     'Tower side-to-side modal stiffness tuners (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! AdjTwMa - Factor to adjust tower mass density.

   CALL ReadVar ( UnIn, TwrFile, AdjTwMa, 'AdjTwMa', 'Factor to adjust tower mass density (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjFASt - Factor to adjust tower fore-aft stiffness.

   CALL ReadVar ( UnIn, TwrFile, AdjFASt, 'AdjFASt', 'Factor to adjust tower fore-aft stiffness (-)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjSSSt - Factor to adjust tower side-to-side stiffness.

   CALL ReadVar ( UnIn, TwrFile, AdjSSSt, 'AdjSSSt', 'Factor to adjust tower side-to-side stiffness (-)', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


         ! Check the locally-defined adjustment factors: AdjTwMa, AdjFASt, AdjSSSt

   IF ( AdjTwMa <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjTwMa must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF

   IF ( AdjFASt <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjFASt must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF

   IF ( AdjSSSt <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjSSSt must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF


   !  -------------- DISTRIBUTED TOWER PROPERTIES ---------------------------------

      ! Skip the comment lines.
   CALL ReadCom ( UnIn, TwrFile, 'heading for distributed tower parameters', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter names', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter units', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! Read the table.

   IF ( ReadAdmVals ) THEN
      NInputCols = 10
   ELSE
      NInputCols = 4
   END IF


   DO I=1,InputFileData%NTwInpSt

      CALL ReadAry( UnIn, TwrFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Tower input station table', &
                    ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN

      InputFileData%HtFract( I) = TmpRAry(1)
      InputFileData%TMassDen(I) = TmpRAry(2)*AdjTwMa   ! Apply the correction factors to the elemental data.
      InputFileData%TwFAStif(I) = TmpRAry(3)*AdjFASt   ! Apply the correction factors to the elemental data.
      InputFileData%TwSSStif(I) = TmpRAry(4)*AdjSSSt   ! Apply the correction factors to the elemental data.

      IF ( NInputCols > 4 ) THEN
         InputFileData%TwGJStif(I) = TmpRAry( 5)
         InputFileData%TwEAStif(I) = TmpRAry( 6)
         InputFileData%TwFAIner(I) = TmpRAry( 7)
         InputFileData%TwSSIner(I) = TmpRAry( 8)
         InputFileData%TwFAcgOf(I) = TmpRAry( 9)
         InputFileData%TwSScgOf(I) = TmpRAry(10)
      END IF

   END DO ! I


   !  -------------- TOWER FORE-AFT MODE SHAPES -----------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower fore-aft mode shapes', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwFAM1Sh - Tower fore-aft mode-1 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwFAM1Sh, SIZE(InputFileData%TwFAM1Sh), 'TwFAM1Sh', &
                           'Tower fore-aft mode-1 shape coefficients (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwFAM2Sh - Tower fore-aft mode-2 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwFAM2Sh, SIZE(InputFileData%TwFAM2Sh), 'TwFAM2Sh', &
                           'Tower fore-aft mode-2 shape coefficients  (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- TOWER SIDE-TO-SIDE MODE SHAPES -------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower side-to-side mode shapes', ErrStat2, ErrMsg2, UnEc  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! TwSSM1Sh - Tower side-to-side mode-1 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwSSM1Sh, SIZE(InputFileData%TwSSM1Sh), 'TwSSM1Sh', &
                           'Tower side-to-side mode-1 shape coefficients (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! TwSSM2Sh - Tower side-to-side mode-2 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwSSM2Sh, SIZE(InputFileData%TwSSM2Sh), 'TwSSM2Sh', &
                           'Tower side-to-side mode-2 shape coefficients (-)', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Close the tower file.
   CLOSE ( UnIn )


   RETURN
CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................
END SUBROUTINE ReadTowerFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadPrimaryFile( InputFile, InputFileData, BldFile, FurlFile, TwrFile, OutFileRoot, UnEc, ErrStat, ErrMsg )
! This routine reads in the primary ElastoDyn input file and places the values it reads in the InputFileData structure.
!   It opens an echo file if requested and returns the (still-open) echo file to the calling routine.
!   It also returns the names of the BldFile, FurlFile, and TrwFile for further reading of inputs.
!..................................................................................................................................


   IMPLICIT                        NONE

      ! Passed variables
   INTEGER(IntKi),     INTENT(OUT)    :: UnEc                                ! I/O unit for echo file. If > 0, file is open for writing.
   INTEGER(IntKi),     INTENT(OUT)    :: ErrStat                             ! Error status

   CHARACTER(*),       INTENT(IN)     :: InputFile                           ! Name of the file containing the primary input data
   CHARACTER(*),       INTENT(OUT)    :: ErrMsg                              ! Error message
   CHARACTER(*),       INTENT(OUT)    :: TwrFile                             ! name of the file containing tower inputs
   CHARACTER(*),       INTENT(OUT)    :: FurlFile                            ! name of the file containing furling inputs
   CHARACTER(*),       INTENT(OUT)    :: BldFile(MaxBl)                      ! name of the files containing blade inputs
   CHARACTER(*),       INTENT(IN)     :: OutFileRoot                         ! The rootname of the echo file, possibly opened in this routine

   TYPE(ED_InputFile), INTENT(INOUT)  :: InputFileData                       ! All the data in the ElastoDyn input file
   
      ! Local variables:
   INTEGER(IntKi)               :: I                                         ! loop counter
   INTEGER(IntKi)               :: NumOuts                                   ! Number of output channel names read from the file 
   INTEGER(IntKi)               :: UnIn                                      ! Unit number for reading file
     
   INTEGER(IntKi)               :: ErrStat2                                  ! Temporary Error status
   LOGICAL                      :: Echo                                      ! Determines if an echo file should be written
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                   ! Temporary Error message
   CHARACTER(1024)              :: PriPath                                   ! Path name of the primary file
   CHARACTER(1024)              :: FTitle                                    ! "File Title": the 2nd line of the input file, which contains a description of its contents
   
      ! Initialize some variables:
   Echo = .FALSE.
   UnEc = -1                             ! Echo file not opened, yet
   CALL GetPath( InputFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.

   
      ! Get an available unit number for the file.

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the Primary input file.

   CALL OpenFInpFile ( UnIn, InputFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      

      ! Allocate arrays for input, based on maximum allowed number of blades and outputs
   CALL AllocAry( InputFileData%BlPitch, MaxBl, 'BlPitch input array', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL AllocAry( InputFileData%PreCone, MaxBl, 'Precone input array', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   CALL AllocAry( InputFileData%TipMass, MaxBl, 'TipMass input array', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL AllocAry( InputFileData%OutList, MaxOutPts, "ElastoDyn Input File's Outlist", ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      
   ! Read the lines up/including to the "Echo" simulation control variable
   ! If echo is FALSE, don't write these lines to the echo file. 
   ! If Echo is TRUE, rewind and write on the second try.
   
   I    = 1 ! the number of times we've read the file (used for the Echo variable)
   DO 
   !-------------------------- HEADER ---------------------------------------------
      CALL ReadCom( UnIn, InputFile, 'File Header: Module Version (line 1)', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
      CALL ReadStr( UnIn, InputFile, FTitle, 'FTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- SIMULATION CONTROL --------------------------------------   
      CALL ReadCom( UnIn, InputFile, 'Section Header: Simulation Control', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
         ! Echo - Echo input to "<RootName>.ech".
   
      CALL ReadVar( UnIn, InputFile, Echo, 'Echo',   'Echo switch', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
   
      IF (.NOT. Echo .OR. I > 1) EXIT !exit this loop
   
         ! Otherwise, open the echo file, then rewind the input file and echo everything we've read
      
      I = I + 1         ! make sure we do this only once (increment counter that says how many times we've read this file)
   
      CALL OpenEcho ( UnEc, TRIM(OutFileRoot)//'.ech', ErrStat2, ErrMsg2, ED_Ver )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
      CALL WrScr( ' Heading of the '//TRIM(ED_Ver%Name)//' input file: '//TRIM( FTitle ) )      
      IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Data from '//TRIM(ED_Ver%Name)//' primary input file "'//TRIM( InputFile )//'":'
   
      REWIND( UnIn, IOSTAT=ErrStat2 )   
         CALL CheckError( ErrID_Fatal, 'Error rewinding file "'//TRIM(InputFile)//'".' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      
   END DO    
                             

      ! DT - Requested integration time for ElastoDyn (seconds):
   CALL ReadVar( UnIn, InputFile, InputFileData%DT, "DT", "Requested integration time for ElastoDyn (seconds)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN   
      
   !---------------------- ENVIRONMENTAL CONDITION ---------------------------------
      CALL ReadCom( UnIn, InputFile, 'Section Header: Environmental Condition', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! Gravity - Gravitational acceleration (m/s^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%Gravity, "Gravity", "Gravitational acceleration (m/s^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN         
   
   !---------------------- DEGREES OF FREEDOM --------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Feature Flags', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
      ! FlapDOF1 - First flapwise blade mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%FlapDOF1, "FlapDOF1", "First flapwise blade mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! FlapDOF2 - Second flapwise blade mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%FlapDOF2, "FlapDOF2", "Second flapwise blade mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! EdgeDOF - Edgewise blade mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%EdgeDOF, "EdgeDOF", "Edgewise blade mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TeetDOF - Rotor-teeter DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetDOF, "TeetDOF", "Rotor-teeter DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DrTrDOF - Drivetrain rotational-flexibility DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%DrTrDOF, "DrTrDOF", "Drivetrain rotational-flexibility DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenDOF - Generator DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenDOF, "GenDOF", "Generator DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! YawDOF - Nacelle-yaw DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawDOF, "YawDOF", "Nacelle-yaw DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwFADOF1 - First tower fore-aft bending-mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwFADOF1, "TwFADOF1", "First tower fore-aft bending-mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwFADOF2 - Second tower fore-aft bending-mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwFADOF2, "TwFADOF2", "Second tower fore-aft bending-mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwSSDOF1 - First tower side-to-side bending-mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwSSDOF1, "TwSSDOF1", "First tower side-to-side bending-mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwSSDOF2 - Second tower side-to-side bending-mode DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwSSDOF2, "TwSSDOF2", "Second tower side-to-side bending-mode DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmSgDOF - Platform horizontal surge translation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmSgDOF, "PtfmSgDOF", "Platform horizontal surge translation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmSwDOF - Platform horizontal sway translation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmSwDOF, "PtfmSwDOF", "Platform horizontal sway translation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmHvDOF - Platform vertical heave translation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmHvDOF, "PtfmHvDOF", "Platform vertical heave translation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmRDOF - Platform roll tilt rotation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmRDOF, "PtfmRDOF", "Platform roll tilt rotation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmPDOF - Platform pitch tilt rotation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmPDOF, "PtfmPDOF", "Platform pitch tilt rotation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmYDOF - Platform yaw rotation DOF (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmYDOF, "PtfmYDOF", "Platform yaw rotation DOF (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
  
   !---------------------- INITIAL CONDITIONS --------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Initial Conditions', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN  
   
      ! OoPDefl - Initial out-of-plane blade-tip displacement (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%OoPDefl, "OoPDefl", "Initial out-of-plane blade-tip displacement (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! IPDefl - Initial in-plane blade-tip deflection (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%IPDefl, "IPDefl", "Initial in-plane blade-tip deflection (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! BlPitch - Initial blade pitch angles (deg) (read from file in degrees and converted to radians here):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%BlPitch, MaxBl, "BlPitch", "Initial blade pitch angles (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%BlPitch = InputFileData%BlPitch*D2R

      ! TeetDefl - Initial teeter angle (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetDefl, "TeetDefl", "Initial teeter angle (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%TeetDefl = InputFileData%TeetDefl*D2R

      ! Azimuth - Initial azimuth angle for blade 1 (degrees) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%Azimuth, "Azimuth", "Initial azimuth angle for blade 1 (degrees)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%Azimuth = InputFileData%Azimuth*D2R

      ! RotSpeed - Initial rotor speed (RPM) (read in RPM and converted to rad/sec here):
   CALL ReadVar( UnIn, InputFile, InputFileData%RotSpeed, "RotSpeed", "Initial rotor speed (RPM)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%RotSpeed = InputFileData%RotSpeed*RPM2RPS

      ! NacYaw - Initial nacelle-yaw angle (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacYaw, "RotSpeed", "Initial nacelle-yaw angle (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%NacYaw = InputFileData%NacYaw*D2R   
   
      ! TTDspFA - Initial fore-aft tower-top displacement (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TTDspFA, "TTDspFA", "Initial fore-aft tower-top displacement (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TTDspSS - Initial side-to-side tower-top displacement (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TTDspSS, "TTDspSS", "Initial side-to-side tower-top displacement (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmSurge - Initial horizontal surge translational displacement of platform (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmSurge, "PtfmSurge", "Initial horizontal surge translational displacement of platform (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmSway - Initial horizontal sway translational displacement of platform (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmSway, "PtfmSway", "Initial horizontal sway translational displacement of platform (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmHeave - Initial vertical heave translational displacement of platform (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmHeave, "PtfmHeave", "Initial vertical heave translational displacement of platform (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmRoll - Initial roll tilt rotational displacement of platform (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmRoll, "PtfmRoll", "Initial roll tilt rotational displacement of platform (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%PtfmRoll = InputFileData%PtfmRoll*D2R

      ! PtfmPitch - Initial pitch tilt rotational displacement of platform (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmPitch, "PtfmPitch", "Initial pitch tilt rotational displacement of platform (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%PtfmPitch = InputFileData%PtfmPitch*D2R

      ! PtfmYaw - Initial yaw rotational displacement of platform (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmYaw, "PtfmYaw", "Initial yaw rotational displacement of platform (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%PtfmYaw = InputFileData%PtfmYaw*D2R
   
   !---------------------- TURBINE CONFIGURATION -----------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Turbine Configuration', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! NumBl - Number of blades (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%NumBl, "NumBl", "Number of blades (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TipRad - Preconed blade-tip radius (distance from the rotor apex to the blade tip) (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TipRad, "TipRad", "Preconed blade-tip radius (distance from the rotor apex to the blade tip) (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! HubRad - Preconed hub radius (distance from the rotor apex to the blade root) (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%HubRad, "HubRad", "Preconed hub radius (distance from the rotor apex to the blade root) (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PreCone - Rotor precone angles (deg) (read from file in degrees and converted to radians here):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%PreCone, MaxBl, "PreCone", "Rotor precone angles (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%PreCone = InputFileData%PreCone*D2R

      ! HubCM - Distance from rotor apex to hub mass (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%HubCM, "HubCM", "Distance from rotor apex to hub mass (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! UndSling - Undersling length (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%UndSling, "UndSling", "Undersling length (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Delta3 - Delta-3 angle for teetering rotors (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%Delta3, "Delta3", "Delta-3 angle for teetering rotors (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%Delta3 = InputFileData%Delta3*D2R

      ! AzimB1Up - Azimuth value to use for I/O when blade 1 points up (degrees) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%AzimB1Up, "AzimB1Up", "Azimuth value to use for I/O when blade 1 points up (degrees)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%AzimB1Up = InputFileData%AzimB1Up*D2R
      
      ! OverHang - Distance from yaw axis to rotor apex or teeter pin (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%OverHang, "OverHang", "Distance from yaw axis to rotor apex or teeter pin (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! ShftGagL - Distance from hub or teeter pin to shaft strain gages (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%ShftGagL, "ShftGagL", "Distance from hub or teeter pin to shaft strain gages (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! ShftTilt - Rotor shaft tilt angle (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%ShftTilt, "ShftTilt", "Rotor shaft tilt angle (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%ShftTilt = InputFileData%ShftTilt*D2R

      ! NacCMxn - Downwind distance from tower-top to nacelle CM (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacCMxn, "NacCMxn", "Downwind distance from tower-top to nacelle CM (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NacCMyn - Lateral distance from tower-top to nacelle CM (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacCMyn, "NacCMyn", "Lateral distance from tower-top to nacelle CM (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NacCMzn - Vertical distance from tower-top to nacelle CM (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacCMzn, "NacCMzn", "Vertical distance from tower-top to nacelle CM (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NcIMUxn - Downwind distance from the tower-top to the nacelle IMU (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NcIMUxn, "NcIMUxn", "Downwind distance from the tower-top to the nacelle IMU (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NcIMUyn - Lateral distance from the tower-top to the nacelle IMU (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NcIMUyn, "NcIMUyn", "Lateral distance from the tower-top to the nacelle IMU (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NcIMUzn - Vertical distance from the tower-top to the nacelle IMU (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%NcIMUzn, "NcIMUzn", "Vertical distance from the tower-top to the nacelle IMU (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Twr2Shft - Vertical distance from the tower-top to the rotor shaft (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%Twr2Shft, "Twr2Shft", "Vertical distance from the tower-top to the rotor shaft (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TowerHt - Height of tower above ground level (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TowerHt, "TowerHt", "Height of tower above ground level (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwrRBHt - Tower rigid base height (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwrRBHt, "TwrRBHt", "Tower rigid base height (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwrDraft - Downward distance from the ground [onshore] or MSL [offshore] to the tower base platform connection (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwrDraft, "TwrDraft", "Downward distance from the ground [onshore] or MSL [offshore] to the tower base platform connection (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmCM - Downward distance from the ground [onshore] or MSL [offshore] to the platform CM (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmCM, "PtfmCM", "Downward distance from the ground [onshore] or MSL [offshore] to the platform CM (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmRef - Downward distance from the ground [onshore] or MSL [offshore] to the platform reference point (meters):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmRef, "PtfmRef", "Downward distance from the ground [onshore] or MSL [offshore] to the platform reference point (meters)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !---------------------- MASS AND INERTIA ----------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Mass and Inertia', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! TipMass - Tip-brake masses (kg):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%TipMass, MaxBl, "TipMass", "Tip-brake masses (kg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! HubMass - Hub mass (kg):
   CALL ReadVar( UnIn, InputFile, InputFileData%HubMass, "HubMass", "Hub mass (kg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! HubIner - Hub inertia about teeter axis (2-blader) or rotor axis (3-blader) (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%HubIner, "HubIner", "Hub inertia about teeter axis (2-blader) or rotor axis (3-blader) (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenIner - Generator inertia about HSS (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenIner, "GenIner", "Generator inertia about HSS (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NacMass - Nacelle mass (kg):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacMass, "NacMass", "Nacelle mass (kg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NacYIner - Nacelle yaw inertia (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacYIner, "NacYIner", "Nacelle yaw inertia (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! YawBrMass - Yaw bearing mass (kg):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawBrMass, "YawBrMass", "Yaw bearing mass (kg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmMass - Platform mass (kg):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmMass, "PtfmMass", "Platform mass (kg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmRIner - Platform inertia for roll tilt rotation about the platform CM (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmRIner, "PtfmRIner", "Platform inertia for roll tilt rotation about the platform CM (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmPIner - Platform inertia for pitch tilt rotation about the platform CM (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmPIner, "PtfmPIner", "Platform inertia for pitch tilt rotation about the platform CM (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! PtfmYIner - Platform inertia for yaw rotation about the platform CM (kg m^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%PtfmYIner, "PtfmYIner", "Platform inertia for yaw rotation about the platform CM (kg m^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- BLADE ---------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Blade', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      
      ! InpBl - Blade file Input data for individual blades (see BladeInputData type):
   DO I = 1,MaxBl
      CALL ReadVar ( UnIn, InputFile, BldFile(I), 'BldFile('//TRIM(Num2Lstr(I))//')', 'Name of the file containing properties for blade '//TRIM(Num2Lstr(I)), ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
      IF ( PathIsRelative( BldFile(I) ) ) BldFile(I) = TRIM(PriPath)//TRIM(BldFile(I))         
   END DO
   
   !---------------------- ROTOR-TEETER --------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Rotor-Teeter', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! TeetMod - Rotor-teeter spring/damper model switch (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetMod, "TeetMod", "Rotor-teeter spring/damper model switch (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TeetDmpP - Rotor-teeter damper position (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetDmpP, "TeetDmpP", "Rotor-teeter damper position (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%TeetDmpP = InputFileData%TeetDmpP*D2R

      ! TeetDmp - Rotor-teeter damping constant (N-m/(rad/s)):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetDmp, "TeetDmp", "Rotor-teeter damping constant (N-m/(rad/s))", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TeetCDmp - Rotor-teeter rate-independent Coulomb-damping (N-m):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetCDmp, "TeetCDmp", "Rotor-teeter rate-independent Coulomb-damping (N-m)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TeetSStP - Rotor-teeter soft-stop position (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetSStP, "TeetSStP", "Rotor-teeter soft-stop position (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%TeetSStP = InputFileData%TeetSStP*D2R

      ! TeetHStP - Rotor-teeter hard-stop position (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetHStP, "TeetHStP", "Rotor-teeter hard-stop position (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%TeetHStP = InputFileData%TeetHStP*D2R

      ! TeetSSSp - Rotor-teeter soft-stop linear-spring constant (N-m/rad):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetSSSp, "TeetSSSp", "Rotor-teeter soft-stop linear-spring constant (N-m/rad)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TeetHSSp - Rotor-teeter hard-stop linear-spring constant (N-m/rad):
   CALL ReadVar( UnIn, InputFile, InputFileData%TeetHSSp, "TeetHSSp", "Rotor-teeter hard-stop linear-spring constant (N-m/rad)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- DRIVETRAIN ----------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Drivetrain', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
      ! GBoxEff - Gearbox efficiency (%) (read from file in % and converted to fraction here):
   CALL ReadVar( UnIn, InputFile, InputFileData%GBoxEff, "GBoxEff", "Gearbox efficiency (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%GBoxEff = InputFileData%GBoxEff*0.01  

      ! GBRatio - Gearbox ratio (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%GBRatio, "GBRatio", "Gearbox ratio (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DTTorSpr - Drivetrain torsional spring (N-m/rad):
   CALL ReadVar( UnIn, InputFile, InputFileData%DTTorSpr, "DTTorSpr", "Drivetrain torsional spring (N-m/rad)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DTTorDmp - Drivetrain torsional damper (N-m/(rad/s)):
   CALL ReadVar( UnIn, InputFile, InputFileData%DTTorDmp, "DTTorDmp", "Drivetrain torsional damper (N-m/(rad/s))", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- FURLING -------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Furling', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! Furling - Use Additional Furling parameters? (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%Furling, "Furling", "Use Additional Furling parameters? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! FurlFile - Name of the file containing furling parameters:
   CALL ReadVar ( UnIn, InputFile, FurlFile, 'FurlFile', 'Name of the file containing furling parameters', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( FurlFile ) ) FurlFile = TRIM(PriPath)//TRIM(FurlFile)         
                  
   !---------------------- TOWER ---------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Tower', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TwrNodes - Number of tower nodes used in the analysis (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%TwrNodes, "TwrNodes", "Number of tower nodes used in the analysis (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! TwrFile - Name of the file containing tower properties:
   CALL ReadVar ( UnIn, InputFile, TwrFile, 'TwrFile', 'Name of the file containing tower properties', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( TwrFile ) ) TwrFile = TRIM(PriPath)//TRIM(TwrFile)         
      
   !---------------------- OUTPUT --------------------------------------------------         
   CALL ReadCom( UnIn, InputFile, 'Section Header: Output', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SumPrint - Print summary data to <RootName>_ElastoDyn.sum (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%SumPrint, "SumPrint", "Print summary data to <RootName>_ElastoDyn.sum (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutFile - Switch to determine where output will be placed: (1: in module output file only; 2: in glue code output file only; 3: both) (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%OutFile, "OutFile", "Switch to determine where output will be placed: (1: in module output file only; 2: in glue code output file only; 3: both) (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !    OutFileFmt - Format for module tabular (time-marching) output: (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both):
   !CALL ReadVar( UnIn, InputFile, InputFileData%OutFileFmt, "OutFileFmt", "Format for module tabular (time-marching) output: (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both)", ErrStat2, ErrMsg2, UnEc)
   !   CALL CheckError( ErrStat2, ErrMsg2 )
   !   IF ( ErrStat >= AbortErrLev ) RETURN      
      
      ! TabDelim - Flag to cause tab-delimited text output (delimited by space otherwise) (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TabDelim, "TabDelim", "Flag to cause tab-delimited text output (delimited by space otherwise) (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutFmt - Format used for module's text tabular output (except time); resulting field should be 10 characters (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%OutFmt, "OutFmt", "Format used for module's text tabular output (except time); resulting field should be 10 characters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Tstart - Time to start module's tabular output (seconds):
   CALL ReadVar( UnIn, InputFile, InputFileData%Tstart, "Tstart", "Time to start module's tabular output (seconds)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DecFact - Decimation factor for module's tabular output (1=output every step) (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%DecFact, "DecFact", "Decimation factor for module's tabular output (1=output every step) (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NTwGages - Number of tower strain gages (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%NTwGages, "NTwGages", "Number of tower strain gages (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      IF ( InputFileData%NTwGages > SIZE(InputFileData%TwrGagNd) ) THEN
         CALL CheckError( ErrID_Warn, ' Warning: number of tower strain gages exceeds '// &
                                      TRIM(Num2LStr(SIZE(InputFileData%TwrGagNd)))//'.')
         InputFileData%NTwGages = SIZE(InputFileData%TwrGagNd)
      END IF
      
      ! TwrGagNd - Nodes closest to the tower strain gages (-):
   CALL ReadAry( UnIn, InputFile, InputFileData%TwrGagNd, InputFileData%NTwGages, "TwrGagNd", "Nodes closest to the tower strain gages (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NBlGages - Number of blade strain gages (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%NBlGages, "NBlGages", "Number of blade strain gages (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      IF ( InputFileData%NBlGages > SIZE(InputFileData%BldGagNd) ) THEN
         CALL CheckError( ErrID_Warn, ' Warning: number of blade strain gages exceeds '//&
                                        TRIM(Num2LStr(SIZE(InputFileData%BldGagNd))) //'.')
         InputFileData%NBlGages = SIZE(InputFileData%BldGagNd)
      END IF
      
      ! BldGagNd - Nodes closest to the blade strain gages (-):
   CALL ReadAry( UnIn, InputFile, InputFileData%BldGagNd, InputFileData%NBlGages, "BldGagNd", "Nodes closest to the blade strain gages (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !---------------------- OUTLIST  --------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: OutList', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutList - List of user-requested output channels (-):
   CALL ReadOutputList ( UnIn, InputFile, InputFileData%OutList, InputFileData%NumOuts, 'OutList', "List of user-requested output channels", ErrStat2, ErrMsg2, UnEc  )     ! Routine in NWTC Subroutine Library
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      
   !---------------------- END OF FILE -----------------------------------------
      
   CLOSE ( UnIn )
   RETURN


CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' Error in ElastoDyn ReadPrimaryFile: '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................
END SUBROUTINE ReadPrimaryFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidatePrimaryData( InputFileData, ErrStat, ErrMsg )
! This routine validates the inputs from the primary input file.
! note that all angles are assumed to be in radians in this routine:
!..................................................................................................................................

      ! Passed variables:

   TYPE(ED_InputFile),       INTENT(IN)     :: InputFileData                       ! All the data in the ElastoDyn input file

   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

      ! Local variables:
   REAL(ReKi)                               :: SmallAngleLimit_Rad                 ! Largest input angle considered "small" (check in input file), radians
   INTEGER(IntKi)                           :: K                                   ! blade number
   INTEGER(IntKi)                           :: FmtWidth                            ! width of the field returned by the specified OutFmt
   INTEGER(IntKi)                           :: ErrStat2                            ! Temporary error status
   CHARACTER(LEN(ErrMsg))                   :: ErrMsg2                             ! Temporary rror message

   
      ! Initialize error status and angle limit defined locally (in correct units)
      
   ErrStat = ErrID_None
   ErrMsg  = ''
   SmallAngleLimit_Rad = SmallAngleLimit_Deg*D2R                                 

      ! Make sure the number of blades is valid:
   IF ( ( InputFileData%NumBl < 2 ) .OR. ( InputFileData%NumBl > 3 ) ) THEN
      CALL SetErrors( ErrID_Fatal, 'NumBl must be either 2 or 3.')
   END IF

      ! Don't allow these parameters to be negative (i.e., they must be in the range (0,inf)):
   CALL CheckNegative( InputFileData%Gravity,   'Gravity',   ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%RotSpeed,  'RotSpeed',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%TipRad,    'TipRad',    ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%HubRad,    'HubRad',    ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%DTTorSpr,  'DTTorSpr',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%DTTorDmp,  'DTTorDmp',  ErrStat, ErrMsg )
   
   CALL CheckNegative( InputFileData%PtfmMass,  'PtfmMass',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%PtfmRIner, 'PtfmRIner', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%PtfmPIner, 'PtfmPIner', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%PtfmYIner, 'PtfmYIner', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%YawBrMass, 'YawBrMass', ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%NacMass,   'NacMass',   ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%HubMass,   'HubMass',   ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%Twr2Shft,  'Twr2Shft',  ErrStat, ErrMsg )

   DO K=1,InputFileData%NumBl
      CALL CheckNegative( InputFileData%TipMass(K), 'TipMass('//TRIM( Num2LStr( K ) )//')',   ErrStat, ErrMsg )
   ENDDO ! K
   
   CALL CheckNegative( InputFileData%NacYIner,  'NacYIner',  ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%GenIner,   'GenIner',   ErrStat, ErrMsg )
   CALL CheckNegative( InputFileData%HubIner,   'HubIner',   ErrStat, ErrMsg )

      ! Check that TowerHt is in the range [0,inf):   
   IF ( InputFileData%TowerHt <= 0.0_ReKi )     CALL SetErrors( ErrID_Fatal, 'TowerHt must be greater than zero.' )
   
      ! Check that these integers are in appropriate ranges:   
   IF ( InputFileData%TwrNodes < 1_IntKi ) CALL SetErrors( ErrID_Fatal, 'TwrNodes must not be less than 1.' )

      ! Check that the gearbox efficiency is valid:
   IF ( ( InputFileData%GBoxEff <= 0.0_ReKi ) .OR. ( InputFileData%GBoxEff > 100.0 ) ) THEN
      CALL SetErrors( ErrID_Fatal, 'GBoxEff must be in the range (0,1] (i.e., (0,100] percent).' )
   END IF       
   
      ! warn if 2nd modes are enabled without their corresponding 1st modes
      
   IF ( InputFileData%FlapDOF2 .AND. ( .NOT. InputFileData%FlapDOF1 ) )  THEN  ! Print out warning when flap mode 1 is not enabled and flap mode 2 is enabled
      CALL SetErrors( ErrID_Warn, '2nd blade flap mode is enabled without the 1st. '//&
                    ' This designation is recommended only for debugging purposes.')
   ENDIF
   
   IF ( InputFileData%TwFADOF2 .AND. ( .NOT. InputFileData%TwFADOF1 ) )  THEN  ! Print out warning when tower fore-aft mode 1 is not enabled and fore-aft mode 2 is enabled
      CALL SetErrors( ErrID_Warn, '2nd tower fore-aft mode is enabled without the 1st. '//&
                    ' This designation is recommended only for debugging purposes.')
   ENDIF
   
   IF ( InputFileData%TwSSDOF2 .AND. ( .NOT. InputFileData%TwSSDOF1 ) )  THEN  ! Print out warning when tower side-to-side mode 1 is not enabled and side-to-side mode 2 is enabled
      CALL SetErrors( ErrID_Warn, '2nd tower side-to-side mode is enabled without the 1st. '//&
                    ' This designation is recommended only for debugging purposes.')
   ENDIF


      ! Check that turbine configuration makes sense:

   IF ( InputFileData%Furling ) THEN
      IF ( InputFileData%OverHang > 0.0_ReKi )  THEN   ! Print out warning when downwind turbine is modeled with furling.
         CALL SetErrors( ErrID_Warn, 'Furling designation (Furling = True) specified for downwind rotor '// &
                    'configuration (OverHang > 0). Check for possible errors in the input file(s).')
      END IF      
   ENDIF      
      
   IF ( InputFileData%TwrRBHt >= ( InputFileData%TowerHt + InputFileData%TwrDraft ) ) THEN
      CALL SetErrors( ErrID_Fatal, 'TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.')
   ELSE
      CALL CheckNegative( InputFileData%TwrRBHt, 'TwrRBHt', ErrStat, ErrMsg ) 
   END IF
   
   IF ( InputFileData%PtfmCM  < InputFileData%TwrDraft ) &
      CALL SetErrors( ErrID_Fatal, 'PtfmCM must not be less than TwrDraft.')
   IF ( InputFileData%PtfmRef < InputFileData%TwrDraft ) &
      CALL SetErrors( ErrID_Fatal, 'PtfmRef must not be less than TwrDraft.')
   IF ( InputFileData%HubRad >= InputFileData%TipRad ) &
      CALL SetErrors( ErrID_Fatal, 'HubRad must be less than TipRad.' )
   IF ( InputFileData%TwrDraft <= -1.*InputFileData%TowerHt ) &
      CALL SetErrors( ErrID_Fatal, 'TwrDraft must be greater than -TowerHt.' )
      
   IF ( InputFileData%TowerHt + InputFileData%Twr2Shft + InputFileData%OverHang*SIN(InputFileData%ShftTilt) &
                              <= InputFileData%TipRad )  THEN
      CALL SetErrors( ErrID_Fatal, 'TowerHt + Twr2Shft + OverHang*SIN(ShftTilt) must be greater than TipRad.' )
   END IF
   

   IF ( InputFileData%NumBl == 2_IntKi )  THEN
      IF ( ( InputFileData%TeetDefl <= -pi ) .OR. ( InputFileData%TeetDefl > pi ) )  &
         CALL SetErrors( ErrID_Fatal, 'TeetDefl must be in the range (-pi, pi] radians (i.e., [-180,180] degrees).' )

      IF ( ABS( InputFileData%Delta3 ) >= PiBy2 )  &
         CALL SetErrors( ErrID_Fatal, 'Delta3 must be in the range (pi/2, pi/2) radians (i.e., (-90, 90) degrees).' )

      IF ( ( InputFileData%TeetSStP < 0.0_ReKi ) .OR. ( InputFileData%TeetSStP > pi ) )  &
         CALL SetErrors( ErrID_Fatal, 'TeetSStP must be in the range [0, pi] radians (i.e., [0,180] degrees).' )

      IF ( ( InputFileData%TeetDmpP < 0.0_ReKi ) .OR. ( InputFileData%TeetDmpP > pi ) )  &
         CALL SetErrors( ErrID_Fatal, 'TeetDmpP must be in the range [0, pi] radians (i.e., [0,180] degrees).' )

      IF ( ( InputFileData%TeetHStP < InputFileData%TeetSStP ) .OR. ( InputFileData%TeetHStP > pi ) )  &
         CALL SetErrors( ErrID_Fatal, 'TeetHStP must be in the range [TeetSStP, pi] radians (i.e., [TeetSStP, 180] degrees).' )
   
      IF ( ( InputFileData%TeetMod /= 0_IntKi ) .AND. ( InputFileData%TeetMod /= 1_IntKi ) .AND. &
           ( InputFileData%TeetMod /= 2_IntKi ) )  &
         CALL SetErrors( ErrID_Fatal, 'TeetMod must be 0, 1, or 2.' )

      CALL CheckNegative( InputFileData%TeetDmp,   'TeetDmp',   ErrStat, ErrMsg )
      CALL CheckNegative( InputFileData%TeetCDmp,  'TeetCDmp',  ErrStat, ErrMsg )
      CALL CheckNegative( InputFileData%TeetSSSp,  'TeetSSSp',  ErrStat, ErrMsg )
      CALL CheckNegative( InputFileData%TeetHSSp,  'TeetHSSp',  ErrStat, ErrMsg )   
   ENDIF

      ! check these angles for appropriate ranges:
   IF ( ( InputFileData%NacYaw <= -pi ) .OR. ( InputFileData%NacYaw > pi ) ) &
      CALL SetErrors( ErrID_Fatal, 'NacYaw must be in the range (-pi, pi] radians (i.e., (-90, 90] degrees).' )
   IF ( ( InputFileData%Azimuth  < 0.0_ReKi ) .OR. ( InputFileData%Azimuth  >= TwoPi ) ) &
      CALL SetErrors( ErrID_Fatal, 'Azimuth must be in the range [0, 2pi) radians (i.e., [0, 360) degrees).' )
   IF ( ( InputFileData%AzimB1Up < 0.0_ReKi ) .OR. ( InputFileData%AzimB1Up >= TwoPi ) )  &
      CALL SetErrors( ErrID_Fatal, 'AzimB1Up must be in the range [0, 2pi) radians (i.e., [0, 360) degrees).' )

   DO K=1,InputFileData%NumBl
      IF ( ( InputFileData%BlPitch(K) <= -pi ) .OR. ( InputFileData%BlPitch(K) > pi ) )  THEN
         CALL SetErrors( ErrID_Fatal, 'BlPitch('//TRIM(Num2LStr(K))//')'//' must be greater than -pi radians and '// &
                                      'less than or equal to pi radians (i.e., in the range (-180, 180] degrees).' )
      END IF      
      IF ( ABS( InputFileData%PreCone(K) ) >= PiBy2 )  &
         CALL SetErrors( ErrID_Fatal, 'PreCone('//TRIM( Num2LStr( K ) )//') must be in the range (-pi/2, pi/2) '//&
                                      'radians (i.e., (-90, 90) degrees).' )
   END DO
   
      ! Check that these angles are in the range [-pi/2, pi/2] radians (i.e., [-90, 90] degrees ):
   CALL CheckAngle90Range( InputFileData%ShftTilt, 'ShftTilt', ErrStat, ErrMsg ) 


      ! Check for violations of the small-angle assumption (15-degree limit, using radians):
   IF ( ABS( InputFileData%PtfmRoll ) > SmallAngleLimit_Rad ) THEN
      CALL SetErrors( ErrID_Fatal, 'PtfmRoll must be between -'//TRIM(Num2LStr(SmallAngleLimit_Rad))//' and ' &
                                                               //TRIM(Num2LStr(SmallAngleLimit_Rad))//' radians.' )
   END IF

   IF ( ABS( InputFileData%PtfmPitch ) > SmallAngleLimit_Rad ) THEN
      CALL SetErrors( ErrID_Fatal, 'PtfmPitch must be between -'//TRIM(Num2LStr(SmallAngleLimit_Rad))//' and ' &
                                                                //TRIM(Num2LStr(SmallAngleLimit_Rad))//' radians.' )
   END IF   
    
   IF ( ABS( InputFileData%PtfmYaw ) > SmallAngleLimit_Rad ) THEN
      CALL SetErrors( ErrID_Fatal, 'PtfmYaw must be between -'//TRIM(Num2LStr(SmallAngleLimit_Rad))//' and ' &
                                                              //TRIM(Num2LStr(SmallAngleLimit_Rad))//' radians.' )
   END IF      
      
      ! Check the output parameters:
   IF ( InputFileData%DecFact < 1_IntKi )  CALL SetErrors( ErrID_Fatal, 'DecFact must be greater than 0.' )
   
   IF ( ( InputFileData%NTwGages < 0_IntKi ) .OR. ( InputFileData%NTwGages > 9_IntKi ) )  &
         CALL SetErrors( ErrID_Fatal, 'NTwGages must be between 0 and 9 (inclusive).' )
   
   IF ( ( InputFileData%NBlGages < 0_IntKi ) .OR. ( InputFileData%NBlGages > 9_IntKi ) )  &
         CALL SetErrors( ErrID_Fatal, 'NBlGages must be between 0 and 9 (inclusive).' )

      ! Check to see if all TwrGagNd(:) analysis points are existing analysis points:

   IF ( ANY(InputFileData%TwrGagNd < 1_IntKi) .OR. ANY(InputFileData%TwrGagNd > InputFileData%TwrNodes) ) THEN      
         CALL SetErrors( ErrID_Fatal, ' All TwrGagNd values must be between 1 and '//&
                        TRIM( Num2LStr( InputFileData%TwrNodes ) )//' (inclusive).' )   
   END IF      
     
      ! Check to see if all BldGagNd(:) analysis points are existing analysis points:
      
   IF ( ANY(InputFileData%BldGagNd < 1_IntKi) .OR. ANY(InputFileData%BldGagNd > InputFileData%InpBlMesh(1)%BldNodes) ) THEN      
         CALL SetErrors( ErrID_Fatal, ' All BldGagNd values must be between 1 and '//&
                        TRIM( Num2LStr( InputFileData%InpBlMesh(1)%BldNodes ) )//' (inclusive).' )   
   END IF   
   

  
      ! Check that InputFileData%OutFmt is a valid format specifier and will fit over the column headings
   CALL ChkRealFmtStr( InputFileData%OutFmt, 'OutFmt', FmtWidth, ErrStat2, ErrMsg2 )   
   IF ( ErrStat /= ErrID_None ) CALL SetErrors(ErrStat2, ErrMsg2 )
   IF ( FmtWidth /= OutStrLen ) CALL SetErrors(ErrID_Warn, 'OutFmt produces a column width of '//TRIM(Num2LStr(FmtWidth))//&
                                                           ' instead of '//TRIM(Num2LStr(OutStrLen))//' characters.' )

   
   ! bjj: figure out what to do with these checks...
   !IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%OoPDefl /= 0.0 ) )  &
   !   CALL ProgAbort ( ' Initial out-of-plane blade-tip displacements must be zero when ElastoDyn is interfaced with Simulink'// &
   !                ' or Labview. Set OoPDefl to 0.0 or use the standard version of ElastoDyn.'                )
   !
   !IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%IPDefl  /= 0.0 ) )  &
   !   CALL ProgAbort ( ' Initial in-plane blade-tip displacements must be zero when ElastoDyn is interfaced with Simulink'// &
   !                ' or Labview. Set IPDefl to 0.0 or use the standard version of ElastoDyn.'                 )
   !
   !IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspFA /= 0.0 ) )  &
   !   CALL ProgAbort ( ' Initial fore-aft tower-top displacements must be zero when ElastoDyn is interfaced with Simulink'// &
   !                ' or Labview. Set TTDspFA to 0.0 or use the standard version of ElastoDyn.'               )
   !
   !IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspSS /= 0.0 ) )  &
   !   CALL ProgAbort ( ' Initial side-to-side tower-top displacements must be zero when ElastoDyn is interfaced with Simulink'// &
   !                ' or Labview. Set TTDspSS to 0.0 or use the standard version of ElastoDyn.'                      )   
   
   RETURN

CONTAINS
   !-------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE SetErrors( ErrStat3, ErrMsg3 )
   ! This routine sets the error message and flag when an error has occurred
   !...............................................................................................................................
   INTEGER(IntKi), INTENT(IN) :: ErrStat3     ! Error status for this error
   CHARACTER(*),   INTENT(IN) :: ErrMsg3      ! Error message for this error

      ErrStat = MAX( ErrStat, ErrStat3 )
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  '//TRIM(ErrMsg3)

   END SUBROUTINE SetErrors
   !-------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE ValidatePrimaryData
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CheckNegative( Var, VarDesc, ErrStat, ErrMsg )
! This routine checks that an value is in the range [0, inf). If not, ErrStat = ErrID_Fatal
! Note that ErrStat and ErrMsg are INTENT(INOUT).
!..................................................................................................................................
REAL(ReKi),     INTENT(IN)    :: Var         ! Variable to check
CHARACTER(*),   INTENT(IN)    :: VarDesc     ! Description of variable (used in error message)
INTEGER(IntKi), INTENT(INOUT) :: ErrStat     ! Error status to update if Var is not in specified range
CHARACTER(*),   INTENT(INOUT) :: ErrMsg      ! Error message to update if Var is not in specified range

   IF (  Var < 0.0_ReKi )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine// &
                  '  '//TRIM(VarDesc)//' must not be negative.'
   END IF

END SUBROUTINE CheckNegative
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CheckAngle90Range( Var, VarDesc, ErrStat, ErrMsg )
! This routine checks that an angle is in the range [-pi/2, pi/2] radians. If not, ErrStat = ErrID_Fatal
! Note that all values are assumed to be in radians, even if read in degrees ( [-90 deg, 90 deg] )
! Note that ErrStat and ErrMsg are INTENT(INOUT).
!...............................................................................................................................
REAL(ReKi),     INTENT(IN)    :: Var         ! Variable to check
CHARACTER(*),   INTENT(IN)    :: VarDesc     ! Description of variable (used in error message)
INTEGER(IntKi), INTENT(INOUT) :: ErrStat     ! Error status to update if Var is not in specified range
CHARACTER(*),   INTENT(INOUT) :: ErrMsg      ! Error message to update if Var is not in specified range

   IF ( ABS( Var ) > PiBy2 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine// &
                  '  '//TRIM(VarDesc)//' must be between -pi/2 and pi/2 radians (i.e., in the range [-90, 90] degrees).'
   END IF

END SUBROUTINE CheckAngle90Range
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_ContStates( x, p, InputFileData, ErrStat, ErrMsg  )
! This routine initializes the continuous states of the module.
! It assumes the parameters are set and that InputFileData contains initial conditions for the continuous states.
!..................................................................................................................................
   TYPE(ED_ContinuousStateType), INTENT(OUT)    :: x              ! Initial continuous states
   TYPE(ED_ParameterType),       INTENT(IN)     :: p              ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),           INTENT(IN)     :: InputFileData  ! Data stored in the module's input file   
   INTEGER(IntKi),               INTENT(OUT)    :: ErrStat        ! Error status
   CHARACTER(*),                 INTENT(OUT)    :: ErrMsg         ! Error message

   
      ! First allocate the arrays stored here:
      
   CALL AllocAry( x%QT, p%NDOF,   'QT',   ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

   CALL AllocAry( x%QDT, p%NDOF,  'QDT',  ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN   
      

END SUBROUTINE Init_ContStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_OtherStates( OtherState, p, InputFileData, ErrStat, ErrMsg  )
! This routine initializes the other states of the module.
! It assumes the parameters are set and that InputFileData contains initial conditions for the continuous states.
!..................................................................................................................................
   TYPE(ED_OtherStateType),      INTENT(OUT)    :: OtherState        ! Initial other states
   TYPE(ED_ParameterType),       INTENT(IN)     :: p                 ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),           INTENT(IN)     :: InputFileData    ! Data stored in the module's input file   
   INTEGER(IntKi),               INTENT(OUT)    :: ErrStat           ! Error status
   CHARACTER(*),                 INTENT(OUT)    :: ErrMsg            ! Error message

      ! local variables
   REAL(ReKi)                                   :: InitQE1(p%NumBl)  ! Initial value of the 1st blade edge DOF
   REAL(ReKi)                                   :: InitQF1(p%NumBl)  ! Initial value of the 1st blade flap DOF
   REAL(ReKi)                                   :: InitQF2(p%NumBl)  ! Initial value of the 2nd blade flap DOF
   INTEGER(IntKi)                               :: I                 ! loop counter      
   
   
      ! First allocate the arrays stored here:
      
   CALL Alloc_OtherState( OtherState, p, ErrStat, ErrMsg )
   IF (ErrStat /= ErrID_None) RETURN
   
      ! Now initialize the IC array = CSHIFT( (/NMX, NMX-1, ... , 1 /), -1 )
      ! this keeps track of the position in the array of continuous states (stored in other states)

   OtherState%IC(1) = 1
   DO I = 2,NMX
      OtherState%IC(I) = OtherState%IC(1) - I + 1 + NMX
   ENDDO      
   
   
      ! Initialize the accelerations to zero.

   OtherState%QD2 = 0.0_ReKi
   
   
      ! Calculate/apply the initial blade DOF values to the corresponding DOFs.

   CALL InitBlDefl ( p, InputFileData, InitQF1, InitQF2, InitQE1, ErrStat, ErrMsg  )

   OtherState%Q ( DOF_BF(:,1), 1 ) = InitQF1   ! These come from InitBlDefl().
   OtherState%Q ( DOF_BF(:,2), 1 ) = InitQF2   ! These come from InitBlDefl().
   OtherState%Q ( DOF_BE(:,1), 1 ) = InitQE1   ! These come from InitBlDefl().
   OtherState%QD( DOF_BF(:,1), 1 ) = 0.0
   OtherState%QD( DOF_BF(:,2), 1 ) = 0.0
   OtherState%QD( DOF_BE(:,1), 1 ) = 0.0

      ! Teeter Motion

   IF ( p%NumBl == 2 )  THEN !note, DOF_Teet doesn't exist for 3-bladed turbine, so don't include an ELSE here

      ! Set initial teeter angle to TeetDefl and initial teeter angular velocity to 0.

      OtherState%Q (DOF_Teet,1) = InputFileData%TeetDefl
      OtherState%QD(DOF_Teet,1) = 0.0
   ENDIF

      ! Generator azimuth

      ! Set initial generator azimuth angle.  Turn rotor on, whether it is
      !   fixed or variable speed.  If it is fixed speed, set up the
      !   fixed rpm.

   !JASON: CHANGE THESE MOD() FUNCTIONS INTO MODULO() FUNCTIONS SO THAT YOU CAN ELIMINATE ADDING 360:
   OtherState%Q (DOF_GeAz,1) = MOD( (InputFileData%Azimuth - p%AzimB1Up)*R2D + 270.0 + 360.0, 360.0 )*D2R   ! Internal position of blade 1
   OtherState%QD(DOF_GeAz,1) = p%RotSpeed                                               ! Rotor speed in rad/sec.

   
      ! Shaft compliance

   ! The initial shaft compliance displacements and velocities are all zero.
   !   They will remain zero if the drivetrain DOF is disabled:

   OtherState%Q (DOF_DrTr,1) = 0.0
   OtherState%QD(DOF_DrTr,1) = 0.0
   

   
   
      ! Rotor-furl motion

      ! Set initial rotor-furl angle to RotFurl.  If rotor-furl is off, this
      !   becomes a fixed rotor-furl angle.

   OtherState%Q (DOF_RFrl,1) = InputFileData%RotFurl
   OtherState%QD(DOF_RFrl,1) = 0.0



      ! Tail-furl motion

      ! Set initial tail-furl angle to TailFurl.  If tail-furl is off, this becomes a fixed tail-furl angle.

   OtherState%Q (DOF_TFrl,1) = InputFileData%TailFurl
   OtherState%QD(DOF_TFrl,1) = 0.0



      ! Yaw Motion

      ! Set initial yaw angle to NacYaw.  If yaw is off, this becomes a fixed yaw angle.

   OtherState%Q (DOF_Yaw ,1) = InputFileData%NacYaw
   OtherState%QD(DOF_Yaw ,1) = 0.0



      ! Tower motion

      ! Assign all the displacements to mode 1 unless it is disabled.  If mode 1
      !   is disabled and mode 2 is enabled, assign all displacements to mode 2.
      ! If both modes are disabled, set the displacements to zero.

   OtherState%Q   (DOF_TFA1,1) =  0.0
   OtherState%Q   (DOF_TSS1,1) =  0.0
   OtherState%Q   (DOF_TFA2,1) =  0.0
   OtherState%Q   (DOF_TSS2,1) =  0.0

   IF (    InputFileData%TwFADOF1 )  THEN   ! First fore-aft tower mode is enabled.
      OtherState%Q(DOF_TFA1,1) =  InputFileData%TTDspFA
   ELSEIF( InputFileData%TwFADOF2 )  THEN   ! Second fore-aft tower mode is enabled, but first is not.
      OtherState%Q(DOF_TFA2,1) =  InputFileData%TTDspFA
   ENDIF

   IF (    InputFileData%TwSSDOF1 )  THEN   ! First side-to-side tower mode is enabled.
      OtherState%Q(DOF_TSS1,1) = -InputFileData%TTDspSS
   ELSEIF( InputFileData%TwSSDOF2 )  THEN   ! Second side-to-side tower mode is enabled, but first is not.
      OtherState%Q(DOF_TSS2,1) = -InputFileData%TTDspSS
   ENDIF

   OtherState%QD  (DOF_TFA1,1) =  0.0
   OtherState%QD  (DOF_TSS1,1) =  0.0
   OtherState%QD  (DOF_TFA2,1) =  0.0
   OtherState%QD  (DOF_TSS2,1) =  0.0



      ! Platform Motion

      ! Set initial platform displacements.  If platform DOFs are off, these
      !   become fixed platform displacements.

   OtherState%Q (DOF_Sg  ,1) = InputFileData%PtfmSurge
   OtherState%Q (DOF_Sw  ,1) = InputFileData%PtfmSway
   OtherState%Q (DOF_Hv  ,1) = InputFileData%PtfmHeave
   OtherState%Q (DOF_R   ,1) = InputFileData%PtfmRoll
   OtherState%Q (DOF_P   ,1) = InputFileData%PtfmPitch
   OtherState%Q (DOF_Y   ,1) = InputFileData%PtfmYaw
   OtherState%QD(DOF_Sg  ,1) = 0.0
   OtherState%QD(DOF_Sw  ,1) = 0.0
   OtherState%QD(DOF_Hv  ,1) = 0.0
   OtherState%QD(DOF_R   ,1) = 0.0
   OtherState%QD(DOF_P   ,1) = 0.0
   OtherState%QD(DOF_Y   ,1) = 0.0            
      
      

END SUBROUTINE Init_OtherStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetOutParam(OutList, p, ErrStat, ErrMsg )
! This routine checks to see if any inputted output channels (stored in the OutList(:)) are invalid. It returns a 
! warning if any of the channels are not available outputs from the module.
!  It assigns the settings for OutParam(:) (i.e, the index, name, and units of the output channels, WriteOutput(:)).
!  the sign is set to 
! It requires that these variables are set: p%NumBl, p%NBlGages, p%NTwGages, p%NumOuts
! and it sets the values of p%OutParam.
!.................................................................................................................................. 
   
   IMPLICIT                        NONE


      ! Passed variables
      
   CHARACTER(OutStrLen),      INTENT(IN)     :: OutList(:)                        ! The list out user-requested outputs
   TYPE(ED_ParameterType),    INTENT(INOUT)  :: p                                 ! The parameters of the structural dynamics module
   INTEGER(IntKi),            INTENT(OUT)    :: ErrStat                           ! The error status code; If not present code aborts
   CHARACTER(*),              INTENT(OUT)    :: ErrMsg                            ! The error message, if an error occurred 
    
   
      ! Local variables.

   INTEGER                      :: I                                               ! Generic loop-counting index.
   INTEGER                      :: J                                               ! Generic loop-counting index.
   INTEGER                      :: INDX                                            ! Index for valid arrays.
  
   LOGICAL                      :: CheckOutListAgain                               ! Flag used to determine if output parameter starting with "M" is valid (or the negative of another parameter)
   
   CHARACTER(OutStrLen)         :: OutListTmp                                      ! A string to temporarily hold OutList(I)
   

! NOTE: The following lines of code were generated by a Matlab script called "Write_ChckOutLst.m"
!      using the parameters listed in the "OutListParameters.xlsx" Excel file. Any changes to these 
!      lines should be modified in the Matlab script and/or Excel worksheet as necessary. 
! This code was generated by Write_ChckOutLst.m at 14-Feb-2013 13:25:07.
   CHARACTER(OutStrLenM1), PARAMETER  :: ValidParamAry(1019) =  (/ &                         ! This lists the names of the allowed parameters, which must be sorted alphabetically
                               "AZIMUTH  ","BLDPITCH1","BLDPITCH2","BLDPITCH3","BLPITCH1 ","BLPITCH2 ","BLPITCH3 ", &
                               "CTHRSTARM","CTHRSTAZM","CTHRSTRAD","GENACCEL ","GENCP    ","GENCQ    ","GENPWR   ", &
                               "GENSPEED ","GENTQ    ","HORWINDV ","HORWNDDIR","HSSBRTQ  ","HSSHFTA  ","HSSHFTCP ", &
                               "HSSHFTCQ ","HSSHFTPWR","HSSHFTTQ ","HSSHFTV  ","IPDEFL1  ","IPDEFL2  ","IPDEFL3  ", &
                               "LSSGAGA  ","LSSGAGAXA","LSSGAGAXS","LSSGAGFXA","LSSGAGFXS","LSSGAGFYA","LSSGAGFYS", &
                               "LSSGAGFZA","LSSGAGFZS","LSSGAGMXA","LSSGAGMXS","LSSGAGMYA","LSSGAGMYS","LSSGAGMZA", &
                               "LSSGAGMZS","LSSGAGP  ","LSSGAGPXA","LSSGAGPXS","LSSGAGV  ","LSSGAGVXA","LSSGAGVXS", &
                               "LSSHFTCP ","LSSHFTCQ ","LSSHFTCT ","LSSHFTFXA","LSSHFTFXS","LSSHFTFYA","LSSHFTFYS", &
                               "LSSHFTFZA","LSSHFTFZS","LSSHFTMXA","LSSHFTMXS","LSSHFTPWR","LSSHFTTQ ","LSSTIPA  ", &
                               "LSSTIPAXA","LSSTIPAXS","LSSTIPMYA","LSSTIPMYS","LSSTIPMZA","LSSTIPMZS","LSSTIPP  ", &
                               "LSSTIPPXA","LSSTIPPXS","LSSTIPV  ","LSSTIPVXA","LSSTIPVXS","NACYAW   ","NACYAWA  ", &
                               "NACYAWERR","NACYAWP  ","NACYAWV  ","NCIMURAXS","NCIMURAYS","NCIMURAZS","NCIMURVXS", &
                               "NCIMURVYS","NCIMURVZS","NCIMUTAXS","NCIMUTAYS","NCIMUTAZS","NCIMUTVXS","NCIMUTVYS", &
                               "NCIMUTVZS","OOPDEFL1 ","OOPDEFL2 ","OOPDEFL3 ","PTCHDEFL1","PTCHDEFL2","PTCHDEFL3", &
                               "PTCHPMZB1","PTCHPMZB2","PTCHPMZB3","PTCHPMZC1","PTCHPMZC2","PTCHPMZC3","PTFMFXI  ", &
                               "PTFMFXT  ","PTFMFYI  ","PTFMFYT  ","PTFMFZI  ","PTFMFZT  ","PTFMHEAVE","PTFMMXI  ", &
                               "PTFMMXT  ","PTFMMYI  ","PTFMMYT  ","PTFMMZI  ","PTFMMZT  ","PTFMPITCH","PTFMRAXI ", &
                               "PTFMRAXT ","PTFMRAYI ","PTFMRAYT ","PTFMRAZI ","PTFMRAZT ","PTFMRDXI ","PTFMRDYI ", &
                               "PTFMRDZI ","PTFMROLL ","PTFMRVXI ","PTFMRVXT ","PTFMRVYI ","PTFMRVYT ","PTFMRVZI ", &
                               "PTFMRVZT ","PTFMSURGE","PTFMSWAY ","PTFMTAXI ","PTFMTAXT ","PTFMTAYI ","PTFMTAYT ", &
                               "PTFMTAZI ","PTFMTAZT ","PTFMTDXI ","PTFMTDXT ","PTFMTDYI ","PTFMTDYT ","PTFMTDZI ", &
                               "PTFMTDZT ","PTFMTVXI ","PTFMTVXT ","PTFMTVYI ","PTFMTVYT ","PTFMTVZI ","PTFMTVZT ", &
                               "PTFMYAW  ","QD2_B1E1 ","QD2_B1F1 ","QD2_B1F2 ","QD2_B2E1 ","QD2_B2F1 ","QD2_B2F2 ", &
                               "QD2_B3E1 ","QD2_B3F1 ","QD2_B3F2 ","QD2_DRTR ","QD2_GEAZ ","QD2_HV   ","QD2_P    ", &
                               "QD2_R    ","QD2_RFRL ","QD2_SG   ","QD2_SW   ","QD2_TEET ","QD2_TFA1 ","QD2_TFA2 ", &
                               "QD2_TFRL ","QD2_TSS1 ","QD2_TSS2 ","QD2_Y    ","QD2_YAW  ","QD_B1E1  ","QD_B1F1  ", &
                               "QD_B1F2  ","QD_B2E1  ","QD_B2F1  ","QD_B2F2  ","QD_B3E1  ","QD_B3F1  ","QD_B3F2  ", &
                               "QD_DRTR  ","QD_GEAZ  ","QD_HV    ","QD_P     ","QD_R     ","QD_RFRL  ","QD_SG    ", &
                               "QD_SW    ","QD_TEET  ","QD_TFA1  ","QD_TFA2  ","QD_TFRL  ","QD_TSS1  ","QD_TSS2  ", &
                               "QD_Y     ","QD_YAW   ","Q_B1E1   ","Q_B1F1   ","Q_B1F2   ","Q_B2E1   ","Q_B2F1   ", &
                               "Q_B2F2   ","Q_B3E1   ","Q_B3F1   ","Q_B3F2   ","Q_DRTR   ","Q_GEAZ   ","Q_HV     ", &
                               "Q_P      ","Q_R      ","Q_RFRL   ","Q_SG     ","Q_SW     ","Q_TEET   ","Q_TFA1   ", &
                               "Q_TFA2   ","Q_TFRL   ","Q_TSS1   ","Q_TSS2   ","Q_Y      ","Q_YAW    ","RFRLBRM  ", &
                               "ROLLDEFL1","ROLLDEFL2","ROLLDEFL3","ROOTFXB1 ","ROOTFXB2 ","ROOTFXB3 ","ROOTFXC1 ", &
                               "ROOTFXC2 ","ROOTFXC3 ","ROOTFYB1 ","ROOTFYB2 ","ROOTFYB3 ","ROOTFYC1 ","ROOTFYC2 ", &
                               "ROOTFYC3 ","ROOTFZB1 ","ROOTFZB2 ","ROOTFZB3 ","ROOTFZC1 ","ROOTFZC2 ","ROOTFZC3 ", &
                               "ROOTMEDG1","ROOTMEDG2","ROOTMEDG3","ROOTMFLP1","ROOTMFLP2","ROOTMFLP3","ROOTMIP1 ", &
                               "ROOTMIP2 ","ROOTMIP3 ","ROOTMOOP1","ROOTMOOP2","ROOTMOOP3","ROOTMXB1 ","ROOTMXB2 ", &
                               "ROOTMXB3 ","ROOTMXC1 ","ROOTMXC2 ","ROOTMXC3 ","ROOTMYB1 ","ROOTMYB2 ","ROOTMYB3 ", &
                               "ROOTMYC1 ","ROOTMYC2 ","ROOTMYC3 ","ROOTMZB1 ","ROOTMZB2 ","ROOTMZB3 ","ROOTMZC1 ", &
                               "ROOTMZC2 ","ROOTMZC3 ","ROTACCEL ","ROTCP    ","ROTCQ    ","ROTCT    ","ROTFURL  ", &
                               "ROTFURLA ","ROTFURLP ","ROTFURLV ","ROTPWR   ","ROTSPEED ","ROTTEETA ","ROTTEETP ", &
                               "ROTTEETV ","ROTTHRUST","ROTTORQ  ","SPN1ALXB1","SPN1ALXB2","SPN1ALXB3","SPN1ALYB1", &
                               "SPN1ALYB2","SPN1ALYB3","SPN1ALZB1","SPN1ALZB2","SPN1ALZB3","SPN1FLXB1","SPN1FLXB2", &
                               "SPN1FLXB3","SPN1FLYB1","SPN1FLYB2","SPN1FLYB3","SPN1FLZB1","SPN1FLZB2","SPN1FLZB3", &
                               "SPN1MLXB1","SPN1MLXB2","SPN1MLXB3","SPN1MLYB1","SPN1MLYB2","SPN1MLYB3","SPN1MLZB1", &
                               "SPN1MLZB2","SPN1MLZB3","SPN1RDXB1","SPN1RDXB2","SPN1RDXB3","SPN1RDYB1","SPN1RDYB2", &
                               "SPN1RDYB3","SPN1RDZB1","SPN1RDZB2","SPN1RDZB3","SPN1TDXB1","SPN1TDXB2","SPN1TDXB3", &
                               "SPN1TDYB1","SPN1TDYB2","SPN1TDYB3","SPN1TDZB1","SPN1TDZB2","SPN1TDZB3","SPN2ALXB1", &
                               "SPN2ALXB2","SPN2ALXB3","SPN2ALYB1","SPN2ALYB2","SPN2ALYB3","SPN2ALZB1","SPN2ALZB2", &
                               "SPN2ALZB3","SPN2FLXB1","SPN2FLXB2","SPN2FLXB3","SPN2FLYB1","SPN2FLYB2","SPN2FLYB3", &
                               "SPN2FLZB1","SPN2FLZB2","SPN2FLZB3","SPN2MLXB1","SPN2MLXB2","SPN2MLXB3","SPN2MLYB1", &
                               "SPN2MLYB2","SPN2MLYB3","SPN2MLZB1","SPN2MLZB2","SPN2MLZB3","SPN2RDXB1","SPN2RDXB2", &
                               "SPN2RDXB3","SPN2RDYB1","SPN2RDYB2","SPN2RDYB3","SPN2RDZB1","SPN2RDZB2","SPN2RDZB3", &
                               "SPN2TDXB1","SPN2TDXB2","SPN2TDXB3","SPN2TDYB1","SPN2TDYB2","SPN2TDYB3","SPN2TDZB1", &
                               "SPN2TDZB2","SPN2TDZB3","SPN3ALXB1","SPN3ALXB2","SPN3ALXB3","SPN3ALYB1","SPN3ALYB2", &
                               "SPN3ALYB3","SPN3ALZB1","SPN3ALZB2","SPN3ALZB3","SPN3FLXB1","SPN3FLXB2","SPN3FLXB3", &
                               "SPN3FLYB1","SPN3FLYB2","SPN3FLYB3","SPN3FLZB1","SPN3FLZB2","SPN3FLZB3","SPN3MLXB1", &
                               "SPN3MLXB2","SPN3MLXB3","SPN3MLYB1","SPN3MLYB2","SPN3MLYB3","SPN3MLZB1","SPN3MLZB2", &
                               "SPN3MLZB3","SPN3RDXB1","SPN3RDXB2","SPN3RDXB3","SPN3RDYB1","SPN3RDYB2","SPN3RDYB3", &
                               "SPN3RDZB1","SPN3RDZB2","SPN3RDZB3","SPN3TDXB1","SPN3TDXB2","SPN3TDXB3","SPN3TDYB1", &
                               "SPN3TDYB2","SPN3TDYB3","SPN3TDZB1","SPN3TDZB2","SPN3TDZB3","SPN4ALXB1","SPN4ALXB2", &
                               "SPN4ALXB3","SPN4ALYB1","SPN4ALYB2","SPN4ALYB3","SPN4ALZB1","SPN4ALZB2","SPN4ALZB3", &
                               "SPN4FLXB1","SPN4FLXB2","SPN4FLXB3","SPN4FLYB1","SPN4FLYB2","SPN4FLYB3","SPN4FLZB1", &
                               "SPN4FLZB2","SPN4FLZB3","SPN4MLXB1","SPN4MLXB2","SPN4MLXB3","SPN4MLYB1","SPN4MLYB2", &
                               "SPN4MLYB3","SPN4MLZB1","SPN4MLZB2","SPN4MLZB3","SPN4RDXB1","SPN4RDXB2","SPN4RDXB3", &
                               "SPN4RDYB1","SPN4RDYB2","SPN4RDYB3","SPN4RDZB1","SPN4RDZB2","SPN4RDZB3","SPN4TDXB1", &
                               "SPN4TDXB2","SPN4TDXB3","SPN4TDYB1","SPN4TDYB2","SPN4TDYB3","SPN4TDZB1","SPN4TDZB2", &
                               "SPN4TDZB3","SPN5ALXB1","SPN5ALXB2","SPN5ALXB3","SPN5ALYB1","SPN5ALYB2","SPN5ALYB3", &
                               "SPN5ALZB1","SPN5ALZB2","SPN5ALZB3","SPN5FLXB1","SPN5FLXB2","SPN5FLXB3","SPN5FLYB1", &
                               "SPN5FLYB2","SPN5FLYB3","SPN5FLZB1","SPN5FLZB2","SPN5FLZB3","SPN5MLXB1","SPN5MLXB2", &
                               "SPN5MLXB3","SPN5MLYB1","SPN5MLYB2","SPN5MLYB3","SPN5MLZB1","SPN5MLZB2","SPN5MLZB3", &
                               "SPN5RDXB1","SPN5RDXB2","SPN5RDXB3","SPN5RDYB1","SPN5RDYB2","SPN5RDYB3","SPN5RDZB1", &
                               "SPN5RDZB2","SPN5RDZB3","SPN5TDXB1","SPN5TDXB2","SPN5TDXB3","SPN5TDYB1","SPN5TDYB2", &
                               "SPN5TDYB3","SPN5TDZB1","SPN5TDZB2","SPN5TDZB3","SPN6ALXB1","SPN6ALXB2","SPN6ALXB3", &
                               "SPN6ALYB1","SPN6ALYB2","SPN6ALYB3","SPN6ALZB1","SPN6ALZB2","SPN6ALZB3","SPN6FLXB1", &
                               "SPN6FLXB2","SPN6FLXB3","SPN6FLYB1","SPN6FLYB2","SPN6FLYB3","SPN6FLZB1","SPN6FLZB2", &
                               "SPN6FLZB3","SPN6MLXB1","SPN6MLXB2","SPN6MLXB3","SPN6MLYB1","SPN6MLYB2","SPN6MLYB3", &
                               "SPN6MLZB1","SPN6MLZB2","SPN6MLZB3","SPN6RDXB1","SPN6RDXB2","SPN6RDXB3","SPN6RDYB1", &
                               "SPN6RDYB2","SPN6RDYB3","SPN6RDZB1","SPN6RDZB2","SPN6RDZB3","SPN6TDXB1","SPN6TDXB2", &
                               "SPN6TDXB3","SPN6TDYB1","SPN6TDYB2","SPN6TDYB3","SPN6TDZB1","SPN6TDZB2","SPN6TDZB3", &
                               "SPN7ALXB1","SPN7ALXB2","SPN7ALXB3","SPN7ALYB1","SPN7ALYB2","SPN7ALYB3","SPN7ALZB1", &
                               "SPN7ALZB2","SPN7ALZB3","SPN7FLXB1","SPN7FLXB2","SPN7FLXB3","SPN7FLYB1","SPN7FLYB2", &
                               "SPN7FLYB3","SPN7FLZB1","SPN7FLZB2","SPN7FLZB3","SPN7MLXB1","SPN7MLXB2","SPN7MLXB3", &
                               "SPN7MLYB1","SPN7MLYB2","SPN7MLYB3","SPN7MLZB1","SPN7MLZB2","SPN7MLZB3","SPN7RDXB1", &
                               "SPN7RDXB2","SPN7RDXB3","SPN7RDYB1","SPN7RDYB2","SPN7RDYB3","SPN7RDZB1","SPN7RDZB2", &
                               "SPN7RDZB3","SPN7TDXB1","SPN7TDXB2","SPN7TDXB3","SPN7TDYB1","SPN7TDYB2","SPN7TDYB3", &
                               "SPN7TDZB1","SPN7TDZB2","SPN7TDZB3","SPN8ALXB1","SPN8ALXB2","SPN8ALXB3","SPN8ALYB1", &
                               "SPN8ALYB2","SPN8ALYB3","SPN8ALZB1","SPN8ALZB2","SPN8ALZB3","SPN8FLXB1","SPN8FLXB2", &
                               "SPN8FLXB3","SPN8FLYB1","SPN8FLYB2","SPN8FLYB3","SPN8FLZB1","SPN8FLZB2","SPN8FLZB3", &
                               "SPN8MLXB1","SPN8MLXB2","SPN8MLXB3","SPN8MLYB1","SPN8MLYB2","SPN8MLYB3","SPN8MLZB1", &
                               "SPN8MLZB2","SPN8MLZB3","SPN8RDXB1","SPN8RDXB2","SPN8RDXB3","SPN8RDYB1","SPN8RDYB2", &
                               "SPN8RDYB3","SPN8RDZB1","SPN8RDZB2","SPN8RDZB3","SPN8TDXB1","SPN8TDXB2","SPN8TDXB3", &
                               "SPN8TDYB1","SPN8TDYB2","SPN8TDYB3","SPN8TDZB1","SPN8TDZB2","SPN8TDZB3","SPN9ALXB1", &
                               "SPN9ALXB2","SPN9ALXB3","SPN9ALYB1","SPN9ALYB2","SPN9ALYB3","SPN9ALZB1","SPN9ALZB2", &
                               "SPN9ALZB3","SPN9FLXB1","SPN9FLXB2","SPN9FLXB3","SPN9FLYB1","SPN9FLYB2","SPN9FLYB3", &
                               "SPN9FLZB1","SPN9FLZB2","SPN9FLZB3","SPN9MLXB1","SPN9MLXB2","SPN9MLXB3","SPN9MLYB1", &
                               "SPN9MLYB2","SPN9MLYB3","SPN9MLZB1","SPN9MLZB2","SPN9MLZB3","SPN9RDXB1","SPN9RDXB2", &
                               "SPN9RDXB3","SPN9RDYB1","SPN9RDYB2","SPN9RDYB3","SPN9RDZB1","SPN9RDZB2","SPN9RDZB3", &
                               "SPN9TDXB1","SPN9TDXB2","SPN9TDXB3","SPN9TDYB1","SPN9TDYB2","SPN9TDYB3","SPN9TDZB1", &
                               "SPN9TDZB2","SPN9TDZB3","TAILFURL ","TAILFURLA","TAILFURLP","TAILFURLV","TEETAYA  ", &
                               "TEETDEFL ","TEETPYA  ","TEETVYA  ","TFINALPHA","TFINCDRAG","TFINCLIFT","TFINCPFX ", &
                               "TFINCPFY ","TFINDNPRS","TFRLBRM  ","TIP2TWR1 ","TIP2TWR2 ","TIP2TWR3 ","TIPALXB1 ", &
                               "TIPALXB2 ","TIPALXB3 ","TIPALYB1 ","TIPALYB2 ","TIPALYB3 ","TIPALZB1 ","TIPALZB2 ", &
                               "TIPALZB3 ","TIPCLRNC1","TIPCLRNC2","TIPCLRNC3","TIPDXB1  ","TIPDXB2  ","TIPDXB3  ", &
                               "TIPDXC1  ","TIPDXC2  ","TIPDXC3  ","TIPDYB1  ","TIPDYB2  ","TIPDYB3  ","TIPDYC1  ", &
                               "TIPDYC2  ","TIPDYC3  ","TIPDZB1  ","TIPDZB2  ","TIPDZB3  ","TIPDZC1  ","TIPDZC2  ", &
                               "TIPDZC3  ","TIPRDXB1 ","TIPRDXB2 ","TIPRDXB3 ","TIPRDYB1 ","TIPRDYB2 ","TIPRDYB3 ", &
                               "TIPRDZB1 ","TIPRDZB2 ","TIPRDZB3 ","TIPRDZC1 ","TIPRDZC2 ","TIPRDZC3 ","TIPSPDRAT", &
                               "TOTWINDV ","TSR      ","TTDSPAX  ","TTDSPFA  ","TTDSPPTCH","TTDSPROLL","TTDSPSS  ", &
                               "TTDSPTWST","TWHT1ALXT","TWHT1ALYT","TWHT1ALZT","TWHT1FLXT","TWHT1FLYT","TWHT1FLZT", &
                               "TWHT1MLXT","TWHT1MLYT","TWHT1MLZT","TWHT1RDXT","TWHT1RDYT","TWHT1RDZT","TWHT1RPXI", &
                               "TWHT1RPYI","TWHT1RPZI","TWHT1TDXT","TWHT1TDYT","TWHT1TDZT","TWHT1TPXI","TWHT1TPYI", &
                               "TWHT1TPZI","TWHT2ALXT","TWHT2ALYT","TWHT2ALZT","TWHT2FLXT","TWHT2FLYT","TWHT2FLZT", &
                               "TWHT2MLXT","TWHT2MLYT","TWHT2MLZT","TWHT2RDXT","TWHT2RDYT","TWHT2RDZT","TWHT2RPXI", &
                               "TWHT2RPYI","TWHT2RPZI","TWHT2TDXT","TWHT2TDYT","TWHT2TDZT","TWHT2TPXI","TWHT2TPYI", &
                               "TWHT2TPZI","TWHT3ALXT","TWHT3ALYT","TWHT3ALZT","TWHT3FLXT","TWHT3FLYT","TWHT3FLZT", &
                               "TWHT3MLXT","TWHT3MLYT","TWHT3MLZT","TWHT3RDXT","TWHT3RDYT","TWHT3RDZT","TWHT3RPXI", &
                               "TWHT3RPYI","TWHT3RPZI","TWHT3TDXT","TWHT3TDYT","TWHT3TDZT","TWHT3TPXI","TWHT3TPYI", &
                               "TWHT3TPZI","TWHT4ALXT","TWHT4ALYT","TWHT4ALZT","TWHT4FLXT","TWHT4FLYT","TWHT4FLZT", &
                               "TWHT4MLXT","TWHT4MLYT","TWHT4MLZT","TWHT4RDXT","TWHT4RDYT","TWHT4RDZT","TWHT4RPXI", &
                               "TWHT4RPYI","TWHT4RPZI","TWHT4TDXT","TWHT4TDYT","TWHT4TDZT","TWHT4TPXI","TWHT4TPYI", &
                               "TWHT4TPZI","TWHT5ALXT","TWHT5ALYT","TWHT5ALZT","TWHT5FLXT","TWHT5FLYT","TWHT5FLZT", &
                               "TWHT5MLXT","TWHT5MLYT","TWHT5MLZT","TWHT5RDXT","TWHT5RDYT","TWHT5RDZT","TWHT5RPXI", &
                               "TWHT5RPYI","TWHT5RPZI","TWHT5TDXT","TWHT5TDYT","TWHT5TDZT","TWHT5TPXI","TWHT5TPYI", &
                               "TWHT5TPZI","TWHT6ALXT","TWHT6ALYT","TWHT6ALZT","TWHT6FLXT","TWHT6FLYT","TWHT6FLZT", &
                               "TWHT6MLXT","TWHT6MLYT","TWHT6MLZT","TWHT6RDXT","TWHT6RDYT","TWHT6RDZT","TWHT6RPXI", &
                               "TWHT6RPYI","TWHT6RPZI","TWHT6TDXT","TWHT6TDYT","TWHT6TDZT","TWHT6TPXI","TWHT6TPYI", &
                               "TWHT6TPZI","TWHT7ALXT","TWHT7ALYT","TWHT7ALZT","TWHT7FLXT","TWHT7FLYT","TWHT7FLZT", &
                               "TWHT7MLXT","TWHT7MLYT","TWHT7MLZT","TWHT7RDXT","TWHT7RDYT","TWHT7RDZT","TWHT7RPXI", &
                               "TWHT7RPYI","TWHT7RPZI","TWHT7TDXT","TWHT7TDYT","TWHT7TDZT","TWHT7TPXI","TWHT7TPYI", &
                               "TWHT7TPZI","TWHT8ALXT","TWHT8ALYT","TWHT8ALZT","TWHT8FLXT","TWHT8FLYT","TWHT8FLZT", &
                               "TWHT8MLXT","TWHT8MLYT","TWHT8MLZT","TWHT8RDXT","TWHT8RDYT","TWHT8RDZT","TWHT8RPXI", &
                               "TWHT8RPYI","TWHT8RPZI","TWHT8TDXT","TWHT8TDYT","TWHT8TDZT","TWHT8TPXI","TWHT8TPYI", &
                               "TWHT8TPZI","TWHT9ALXT","TWHT9ALYT","TWHT9ALZT","TWHT9FLXT","TWHT9FLYT","TWHT9FLZT", &
                               "TWHT9MLXT","TWHT9MLYT","TWHT9MLZT","TWHT9RDXT","TWHT9RDYT","TWHT9RDZT","TWHT9RPXI", &
                               "TWHT9RPYI","TWHT9RPZI","TWHT9TDXT","TWHT9TDYT","TWHT9TDZT","TWHT9TPXI","TWHT9TPYI", &
                               "TWHT9TPZI","TWRBSFXT ","TWRBSFYT ","TWRBSFZT ","TWRBSMXT ","TWRBSMYT ","TWRBSMZT ", &
                               "TWRCLRNC1","TWRCLRNC2","TWRCLRNC3","TWSTDEFL1","TWSTDEFL2","TWSTDEFL3","UWIND    ", &
                               "VERWNDDIR","VWIND    ","WINDVXI  ","WINDVYI  ","WINDVZI  ","WWIND    ","YAWACCEL ", &
                               "YAWAZN   ","YAWAZP   ","YAWBRFXN ","YAWBRFXP ","YAWBRFYN ","YAWBRFYP ","YAWBRFZN ", &
                               "YAWBRFZP ","YAWBRMXN ","YAWBRMXP ","YAWBRMYN ","YAWBRMYP ","YAWBRMZN ","YAWBRMZP ", &
                               "YAWBRRAXP","YAWBRRAYP","YAWBRRAZP","YAWBRRDXT","YAWBRRDYT","YAWBRRDZT","YAWBRRVXP", &
                               "YAWBRRVYP","YAWBRRVZP","YAWBRTAXP","YAWBRTAYP","YAWBRTAZP","YAWBRTDXP","YAWBRTDXT", &
                               "YAWBRTDYP","YAWBRTDYT","YAWBRTDZP","YAWBRTDZT","YAWMOM   ","YAWPOS   ","YAWPZN   ", &
                               "YAWPZP   ","YAWRATE  ","YAWVZN   ","YAWVZP   "/)
   INTEGER(IntKi), PARAMETER :: ParamIndxAry(1019) =  (/ &                          ! This lists the index into AllOuts(:) of the allowed parameters ValidParamAry(:)
                                LSSTipPxa , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , &
                                CThrstRad , CThrstAzm , CThrstRad ,   HSShftA ,     GenCp ,     GenCq ,    GenPwr , &
                                  HSShftV ,     GenTq ,  HorWindV , HorWndDir ,   HSSBrTq ,   HSShftA ,  HSShftCp , &
                                 HSShftCq , HSShftPwr ,  HSShftTq ,   HSShftV ,   TipDyc1 ,   TipDyc2 ,   TipDyc3 , &
                                LSSGagAxa , LSSGagAxa , LSSGagAxa , LSShftFxa , LSShftFxa , LSShftFya , LSShftFys , &
                                LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa , LSSGagMya , LSSGagMys , LSSGagMza , &
                                LSSGagMzs , LSSGagPxa , LSSGagPxa , LSSGagPxa , LSSGagVxa , LSSGagVxa , LSSGagVxa , &
                                    RotCp ,     RotCq ,     RotCt , LSShftFxa , LSShftFxa , LSShftFya , LSShftFys , &
                                LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa ,    RotPwr , LSShftMxa , LSSTipAxa , &
                                LSSTipAxa , LSSTipAxa , LSSTipMya , LSSTipMys , LSSTipMza , LSSTipMzs , LSSTipPxa , &
                                LSSTipPxa , LSSTipPxa , LSSTipVxa , LSSTipVxa , LSSTipVxa ,    YawPzn ,    YawAzn , &
                                NacYawErr ,    YawPzn ,    YawVzn , NcIMURAxs , NcIMURAys , NcIMURAzs , NcIMURVxs , &
                                NcIMURVys , NcIMURVzs , NcIMUTAxs , NcIMUTAys , NcIMUTAzs , NcIMUTVxs , NcIMUTVys , &
                                NcIMUTVzs ,   TipDxc1 ,   TipDxc2 ,   TipDxc3 ,  TipRDyb1 ,  TipRDyb2 ,  TipRDyb3 , &
                                PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 ,   PtfmFxi , &
                                  PtfmFxt ,   PtfmFyi ,   PtfmFyt ,   PtfmFzi ,   PtfmFzt ,  PtfmTDzi ,   PtfmMxi , &
                                  PtfmMxt ,   PtfmMyi ,   PtfmMyt ,   PtfmMzi ,   PtfmMzt ,  PtfmRDyi ,  PtfmRAxi , &
                                 PtfmRAxt ,  PtfmRAyi ,  PtfmRAyt ,  PtfmRAzi ,  PtfmRAzt ,  PtfmRDxi ,  PtfmRDyi , &
                                 PtfmRDzi ,  PtfmRDxi ,  PtfmRVxi ,  PtfmRVxt ,  PtfmRVyi ,  PtfmRVyt ,  PtfmRVzi , &
                                 PtfmRVzt ,  PtfmTDxi ,  PtfmTDyi ,  PtfmTAxi ,  PtfmTAxt ,  PtfmTAyi ,  PtfmTAyt , &
                                 PtfmTAzi ,  PtfmTAzt ,  PtfmTDxi ,  PtfmTDxt ,  PtfmTDyi ,  PtfmTDyt ,  PtfmTDzi , &
                                 PtfmTDzt ,  PtfmTVxi ,  PtfmTVxt ,  PtfmTVyi ,  PtfmTVyt ,  PtfmTVzi ,  PtfmTVzt , &
                                 PtfmRDzi ,  QD2_B1E1 ,  QD2_B1F1 ,  QD2_B1F2 ,  QD2_B2E1 ,  QD2_B2F1 ,  QD2_B2F2 , &
                                 QD2_B3E1 ,  QD2_B3F1 ,  QD2_B3F2 ,  QD2_DrTr ,  QD2_GeAz ,    QD2_Hv ,     QD2_P , &
                                    QD2_R ,  QD2_RFrl ,    QD2_Sg ,    QD2_Sw ,  QD2_Teet ,  QD2_TFA1 ,  QD2_TFA2 , &
                                 QD2_TFrl ,  QD2_TSS1 ,  QD2_TSS2 ,     QD2_Y ,   QD2_Yaw ,   QD_B1E1 ,   QD_B1F1 , &
                                  QD_B1F2 ,   QD_B2E1 ,   QD_B2F1 ,   QD_B2F2 ,   QD_B3E1 ,   QD_B3F1 ,   QD_B3F2 , &
                                  QD_DrTr ,   QD_GeAz ,     QD_Hv ,      QD_P ,      QD_R ,   QD_RFrl ,     QD_Sg , &
                                    QD_Sw ,   QD_Teet ,   QD_TFA1 ,   QD_TFA2 ,   QD_TFrl ,   QD_TSS1 ,   QD_TSS2 , &
                                     QD_Y ,    QD_Yaw ,    Q_B1E1 ,    Q_B1F1 ,    Q_B1F2 ,    Q_B2E1 ,    Q_B2F1 , &
                                   Q_B2F2 ,    Q_B3E1 ,    Q_B3F1 ,    Q_B3F2 ,    Q_DrTr ,    Q_GeAz ,      Q_Hv , &
                                      Q_P ,       Q_R ,    Q_RFrl ,      Q_Sg ,      Q_Sw ,    Q_Teet ,    Q_TFA1 , &
                                   Q_TFA2 ,    Q_TFrl ,    Q_TSS1 ,    Q_TSS2 ,       Q_Y ,     Q_Yaw ,   RFrlBrM , &
                                 TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  RootFxb1 ,  RootFxb2 ,  RootFxb3 ,  RootFxc1 , &
                                 RootFxc2 ,  RootFxc3 ,  RootFyb1 ,  RootFyb2 ,  RootFyb3 ,  RootFyc1 ,  RootFyc2 , &
                                 RootFyc3 ,  RootFzc1 ,  RootFzc2 ,  RootFzc3 ,  RootFzc1 ,  RootFzc2 ,  RootFzc3 , &
                                 RootMxb1 ,  RootMxb2 ,  RootMxb3 ,  RootMyb1 ,  RootMyb2 ,  RootMyb3 ,  RootMxc1 , &
                                 RootMxc2 ,  RootMxc3 ,  RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMxb1 ,  RootMxb2 , &
                                 RootMxb3 ,  RootMxc1 ,  RootMxc2 ,  RootMxc3 ,  RootMyb1 ,  RootMyb2 ,  RootMyb3 , &
                                 RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMzc1 ,  RootMzc2 ,  RootMzc3 ,  RootMzc1 , &
                                 RootMzc2 ,  RootMzc3 , LSSTipAxa ,     RotCp ,     RotCq ,     RotCt ,  RotFurlP , &
                                 RotFurlA ,  RotFurlP ,  RotFurlV ,    RotPwr , LSSTipVxa ,   TeetAya ,   TeetPya , &
                                  TeetVya , LSShftFxa , LSShftMxa , Spn1ALxb1 , Spn1ALxb2 , Spn1ALxb3 , Spn1ALyb1 , &
                                Spn1ALyb2 , Spn1ALyb3 , Spn1ALzb1 , Spn1ALzb2 , Spn1ALzb3 , Spn1FLxb1 , Spn1FLxb2 , &
                                Spn1FLxb3 , Spn1FLyb1 , Spn1FLyb2 , Spn1FLyb3 , Spn1FLzb1 , Spn1FLzb2 , Spn1FLzb3 , &
                                Spn1MLxb1 , Spn1MLxb2 , Spn1MLxb3 , Spn1MLyb1 , Spn1MLyb2 , Spn1MLyb3 , Spn1MLzb1 , &
                                Spn1MLzb2 , Spn1MLzb3 , Spn1RDxb1 , Spn1RDxb2 , Spn1RDxb3 , Spn1RDyb1 , Spn1RDyb2 , &
                                Spn1RDyb3 , Spn1RDzb1 , Spn1RDzb2 , Spn1RDzb3 , Spn1TDxb1 , Spn1TDxb2 , Spn1TDxb3 , &
                                Spn1TDyb1 , Spn1TDyb2 , Spn1TDyb3 , Spn1TDzb1 , Spn1TDzb2 , Spn1TDzb3 , Spn2ALxb1 , &
                                Spn2ALxb2 , Spn2ALxb3 , Spn2ALyb1 , Spn2ALyb2 , Spn2ALyb3 , Spn2ALzb1 , Spn2ALzb2 , &
                                Spn2ALzb3 , Spn2FLxb1 , Spn2FLxb2 , Spn2FLxb3 , Spn2FLyb1 , Spn2FLyb2 , Spn2FLyb3 , &
                                Spn2FLzb1 , Spn2FLzb2 , Spn2FLzb3 , Spn2MLxb1 , Spn2MLxb2 , Spn2MLxb3 , Spn2MLyb1 , &
                                Spn2MLyb2 , Spn2MLyb3 , Spn2MLzb1 , Spn2MLzb2 , Spn2MLzb3 , Spn2RDxb1 , Spn2RDxb2 , &
                                Spn2RDxb3 , Spn2RDyb1 , Spn2RDyb2 , Spn2RDyb3 , Spn2RDzb1 , Spn2RDzb2 , Spn2RDzb3 , &
                                Spn2TDxb1 , Spn2TDxb2 , Spn2TDxb3 , Spn2TDyb1 , Spn2TDyb2 , Spn2TDyb3 , Spn2TDzb1 , &
                                Spn2TDzb2 , Spn2TDzb3 , Spn3ALxb1 , Spn3ALxb2 , Spn3ALxb3 , Spn3ALyb1 , Spn3ALyb2 , &
                                Spn3ALyb3 , Spn3ALzb1 , Spn3ALzb2 , Spn3ALzb3 , Spn3FLxb1 , Spn3FLxb2 , Spn3FLxb3 , &
                                Spn3FLyb1 , Spn3FLyb2 , Spn3FLyb3 , Spn3FLzb1 , Spn3FLzb2 , Spn3FLzb3 , Spn3MLxb1 , &
                                Spn3MLxb2 , Spn3MLxb3 , Spn3MLyb1 , Spn3MLyb2 , Spn3MLyb3 , Spn3MLzb1 , Spn3MLzb2 , &
                                Spn3MLzb3 , Spn3RDxb1 , Spn3RDxb2 , Spn3RDxb3 , Spn3RDyb1 , Spn3RDyb2 , Spn3RDyb3 , &
                                Spn3RDzb1 , Spn3RDzb2 , Spn3RDzb3 , Spn3TDxb1 , Spn3TDxb2 , Spn3TDxb3 , Spn3TDyb1 , &
                                Spn3TDyb2 , Spn3TDyb3 , Spn3TDzb1 , Spn3TDzb2 , Spn3TDzb3 , Spn4ALxb1 , Spn4ALxb2 , &
                                Spn4ALxb3 , Spn4ALyb1 , Spn4ALyb2 , Spn4ALyb3 , Spn4ALzb1 , Spn4ALzb2 , Spn4ALzb3 , &
                                Spn4FLxb1 , Spn4FLxb2 , Spn4FLxb3 , Spn4FLyb1 , Spn4FLyb2 , Spn4FLyb3 , Spn4FLzb1 , &
                                Spn4FLzb2 , Spn4FLzb3 , Spn4MLxb1 , Spn4MLxb2 , Spn4MLxb3 , Spn4MLyb1 , Spn4MLyb2 , &
                                Spn4MLyb3 , Spn4MLzb1 , Spn4MLzb2 , Spn4MLzb3 , Spn4RDxb1 , Spn4RDxb2 , Spn4RDxb3 , &
                                Spn4RDyb1 , Spn4RDyb2 , Spn4RDyb3 , Spn4RDzb1 , Spn4RDzb2 , Spn4RDzb3 , Spn4TDxb1 , &
                                Spn4TDxb2 , Spn4TDxb3 , Spn4TDyb1 , Spn4TDyb2 , Spn4TDyb3 , Spn4TDzb1 , Spn4TDzb2 , &
                                Spn4TDzb3 , Spn5ALxb1 , Spn5ALxb2 , Spn5ALxb3 , Spn5ALyb1 , Spn5ALyb2 , Spn5ALyb3 , &
                                Spn5ALzb1 , Spn5ALzb2 , Spn5ALzb3 , Spn5FLxb1 , Spn5FLxb2 , Spn5FLxb3 , Spn5FLyb1 , &
                                Spn5FLyb2 , Spn5FLyb3 , Spn5FLzb1 , Spn5FLzb2 , Spn5FLzb3 , Spn5MLxb1 , Spn5MLxb2 , &
                                Spn5MLxb3 , Spn5MLyb1 , Spn5MLyb2 , Spn5MLyb3 , Spn5MLzb1 , Spn5MLzb2 , Spn5MLzb3 , &
                                Spn5RDxb1 , Spn5RDxb2 , Spn5RDxb3 , Spn5RDyb1 , Spn5RDyb2 , Spn5RDyb3 , Spn5RDzb1 , &
                                Spn5RDzb2 , Spn5RDzb3 , Spn5TDxb1 , Spn5TDxb2 , Spn5TDxb3 , Spn5TDyb1 , Spn5TDyb2 , &
                                Spn5TDyb3 , Spn5TDzb1 , Spn5TDzb2 , Spn5TDzb3 , Spn6ALxb1 , Spn6ALxb2 , Spn6ALxb3 , &
                                Spn6ALyb1 , Spn6ALyb2 , Spn6ALyb3 , Spn6ALzb1 , Spn6ALzb2 , Spn6ALzb3 , Spn6FLxb1 , &
                                Spn6FLxb2 , Spn6FLxb3 , Spn6FLyb1 , Spn6FLyb2 , Spn6FLyb3 , Spn6FLzb1 , Spn6FLzb2 , &
                                Spn6FLzb3 , Spn6MLxb1 , Spn6MLxb2 , Spn6MLxb3 , Spn6MLyb1 , Spn6MLyb2 , Spn6MLyb3 , &
                                Spn6MLzb1 , Spn6MLzb2 , Spn6MLzb3 , Spn6RDxb1 , Spn6RDxb2 , Spn6RDxb3 , Spn6RDyb1 , &
                                Spn6RDyb2 , Spn6RDyb3 , Spn6RDzb1 , Spn6RDzb2 , Spn6RDzb3 , Spn6TDxb1 , Spn6TDxb2 , &
                                Spn6TDxb3 , Spn6TDyb1 , Spn6TDyb2 , Spn6TDyb3 , Spn6TDzb1 , Spn6TDzb2 , Spn6TDzb3 , &
                                Spn7ALxb1 , Spn7ALxb2 , Spn7ALxb3 , Spn7ALyb1 , Spn7ALyb2 , Spn7ALyb3 , Spn7ALzb1 , &
                                Spn7ALzb2 , Spn7ALzb3 , Spn7FLxb1 , Spn7FLxb2 , Spn7FLxb3 , Spn7FLyb1 , Spn7FLyb2 , &
                                Spn7FLyb3 , Spn7FLzb1 , Spn7FLzb2 , Spn7FLzb3 , Spn7MLxb1 , Spn7MLxb2 , Spn7MLxb3 , &
                                Spn7MLyb1 , Spn7MLyb2 , Spn7MLyb3 , Spn7MLzb1 , Spn7MLzb2 , Spn7MLzb3 , Spn7RDxb1 , &
                                Spn7RDxb2 , Spn7RDxb3 , Spn7RDyb1 , Spn7RDyb2 , Spn7RDyb3 , Spn7RDzb1 , Spn7RDzb2 , &
                                Spn7RDzb3 , Spn7TDxb1 , Spn7TDxb2 , Spn7TDxb3 , Spn7TDyb1 , Spn7TDyb2 , Spn7TDyb3 , &
                                Spn7TDzb1 , Spn7TDzb2 , Spn7TDzb3 , Spn8ALxb1 , Spn8ALxb2 , Spn8ALxb3 , Spn8ALyb1 , &
                                Spn8ALyb2 , Spn8ALyb3 , Spn8ALzb1 , Spn8ALzb2 , Spn8ALzb3 , Spn8FLxb1 , Spn8FLxb2 , &
                                Spn8FLxb3 , Spn8FLyb1 , Spn8FLyb2 , Spn8FLyb3 , Spn8FLzb1 , Spn8FLzb2 , Spn8FLzb3 , &
                                Spn8MLxb1 , Spn8MLxb2 , Spn8MLxb3 , Spn8MLyb1 , Spn8MLyb2 , Spn8MLyb3 , Spn8MLzb1 , &
                                Spn8MLzb2 , Spn8MLzb3 , Spn8RDxb1 , Spn8RDxb2 , Spn8RDxb3 , Spn8RDyb1 , Spn8RDyb2 , &
                                Spn8RDyb3 , Spn8RDzb1 , Spn8RDzb2 , Spn8RDzb3 , Spn8TDxb1 , Spn8TDxb2 , Spn8TDxb3 , &
                                Spn8TDyb1 , Spn8TDyb2 , Spn8TDyb3 , Spn8TDzb1 , Spn8TDzb2 , Spn8TDzb3 , Spn9ALxb1 , &
                                Spn9ALxb2 , Spn9ALxb3 , Spn9ALyb1 , Spn9ALyb2 , Spn9ALyb3 , Spn9ALzb1 , Spn9ALzb2 , &
                                Spn9ALzb3 , Spn9FLxb1 , Spn9FLxb2 , Spn9FLxb3 , Spn9FLyb1 , Spn9FLyb2 , Spn9FLyb3 , &
                                Spn9FLzb1 , Spn9FLzb2 , Spn9FLzb3 , Spn9MLxb1 , Spn9MLxb2 , Spn9MLxb3 , Spn9MLyb1 , &
                                Spn9MLyb2 , Spn9MLyb3 , Spn9MLzb1 , Spn9MLzb2 , Spn9MLzb3 , Spn9RDxb1 , Spn9RDxb2 , &
                                Spn9RDxb3 , Spn9RDyb1 , Spn9RDyb2 , Spn9RDyb3 , Spn9RDzb1 , Spn9RDzb2 , Spn9RDzb3 , &
                                Spn9TDxb1 , Spn9TDxb2 , Spn9TDxb3 , Spn9TDyb1 , Spn9TDyb2 , Spn9TDyb3 , Spn9TDzb1 , &
                                Spn9TDzb2 , Spn9TDzb3 , TailFurlP , TailFurlA , TailFurlP , TailFurlV ,   TeetAya , &
                                  TeetPya ,   TeetPya ,   TeetVya , TFinAlpha , TFinCDrag , TFinCLift ,  TFinCPFx , &
                                 TFinCPFy , TFinDnPrs ,   TFrlBrM , TipClrnc1 , TipClrnc2 , TipClrnc3 ,  TipALxb1 , &
                                 TipALxb2 ,  TipALxb3 ,  TipALyb1 ,  TipALyb2 ,  TipALyb3 ,  TipALzb1 ,  TipALzb2 , &
                                 TipALzb3 , TipClrnc1 , TipClrnc2 , TipClrnc3 ,   TipDxb1 ,   TipDxb2 ,   TipDxb3 , &
                                  TipDxc1 ,   TipDxc2 ,   TipDxc3 ,   TipDyb1 ,   TipDyb2 ,   TipDyb3 ,   TipDyc1 , &
                                  TipDyc2 ,   TipDyc3 ,   TipDzc1 ,   TipDzc2 ,   TipDzc3 ,   TipDzc1 ,   TipDzc2 , &
                                  TipDzc3 ,  TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  TipRDyb1 ,  TipRDyb2 ,  TipRDyb3 , &
                                 TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 , TipSpdRat , &
                                 TotWindV , TipSpdRat , YawBrTDzt , YawBrTDxt , YawBrRDyt , YawBrRDxt , YawBrTDyt , &
                                YawBrRDzt , TwHt1ALxt , TwHt1ALyt , TwHt1ALzt , TwHt1FLxt , TwHt1FLyt , TwHt1FLzt , &
                                TwHt1MLxt , TwHt1MLyt , TwHt1MLzt , TwHt1RDxt , TwHt1RDyt , TwHt1RDzt , TwHt1RPxi , &
                                TwHt1RPyi , TwHt1RPzi , TwHt1TDxt , TwHt1TDyt , TwHt1TDzt , TwHt1TPxi , TwHt1TPyi , &
                                TwHt1TPzi , TwHt2ALxt , TwHt2ALyt , TwHt2ALzt , TwHt2FLxt , TwHt2FLyt , TwHt2FLzt , &
                                TwHt2MLxt , TwHt2MLyt , TwHt2MLzt , TwHt2RDxt , TwHt2RDyt , TwHt2RDzt , TwHt2RPxi , &
                                TwHt2RPyi , TwHt2RPzi , TwHt2TDxt , TwHt2TDyt , TwHt2TDzt , TwHt2TPxi , TwHt2TPyi , &
                                TwHt2TPzi , TwHt3ALxt , TwHt3ALyt , TwHt3ALzt , TwHt3FLxt , TwHt3FLyt , TwHt3FLzt , &
                                TwHt3MLxt , TwHt3MLyt , TwHt3MLzt , TwHt3RDxt , TwHt3RDyt , TwHt3RDzt , TwHt3RPxi , &
                                TwHt3RPyi , TwHt3RPzi , TwHt3TDxt , TwHt3TDyt , TwHt3TDzt , TwHt3TPxi , TwHt3TPyi , &
                                TwHt3TPzi , TwHt4ALxt , TwHt4ALyt , TwHt4ALzt , TwHt4FLxt , TwHt4FLyt , TwHt4FLzt , &
                                TwHt4MLxt , TwHt4MLyt , TwHt4MLzt , TwHt4RDxt , TwHt4RDyt , TwHt4RDzt , TwHt4RPxi , &
                                TwHt4RPyi , TwHt4RPzi , TwHt4TDxt , TwHt4TDyt , TwHt4TDzt , TwHt4TPxi , TwHt4TPyi , &
                                TwHt4TPzi , TwHt5ALxt , TwHt5ALyt , TwHt5ALzt , TwHt5FLxt , TwHt5FLyt , TwHt5FLzt , &
                                TwHt5MLxt , TwHt5MLyt , TwHt5MLzt , TwHt5RDxt , TwHt5RDyt , TwHt5RDzt , TwHt5RPxi , &
                                TwHt5RPyi , TwHt5RPzi , TwHt5TDxt , TwHt5TDyt , TwHt5TDzt , TwHt5TPxi , TwHt5TPyi , &
                                TwHt5TPzi , TwHt6ALxt , TwHt6ALyt , TwHt6ALzt , TwHt6FLxt , TwHt6FLyt , TwHt6FLzt , &
                                TwHt6MLxt , TwHt6MLyt , TwHt6MLzt , TwHt6RDxt , TwHt6RDyt , TwHt6RDzt , TwHt6RPxi , &
                                TwHt6RPyi , TwHt6RPzi , TwHt6TDxt , TwHt6TDyt , TwHt6TDzt , TwHt6TPxi , TwHt6TPyi , &
                                TwHt6TPzi , TwHt7ALxt , TwHt7ALyt , TwHt7ALzt , TwHt7FLxt , TwHt7FLyt , TwHt7FLzt , &
                                TwHt7MLxt , TwHt7MLyt , TwHt7MLzt , TwHt7RDxt , TwHt7RDyt , TwHt7RDzt , TwHt7RPxi , &
                                TwHt7RPyi , TwHt7RPzi , TwHt7TDxt , TwHt7TDyt , TwHt7TDzt , TwHt7TPxi , TwHt7TPyi , &
                                TwHt7TPzi , TwHt8ALxt , TwHt8ALyt , TwHt8ALzt , TwHt8FLxt , TwHt8FLyt , TwHt8FLzt , &
                                TwHt8MLxt , TwHt8MLyt , TwHt8MLzt , TwHt8RDxt , TwHt8RDyt , TwHt8RDzt , TwHt8RPxi , &
                                TwHt8RPyi , TwHt8RPzi , TwHt8TDxt , TwHt8TDyt , TwHt8TDzt , TwHt8TPxi , TwHt8TPyi , &
                                TwHt8TPzi , TwHt9ALxt , TwHt9ALyt , TwHt9ALzt , TwHt9FLxt , TwHt9FLyt , TwHt9FLzt , &
                                TwHt9MLxt , TwHt9MLyt , TwHt9MLzt , TwHt9RDxt , TwHt9RDyt , TwHt9RDzt , TwHt9RPxi , &
                                TwHt9RPyi , TwHt9RPzi , TwHt9TDxt , TwHt9TDyt , TwHt9TDzt , TwHt9TPxi , TwHt9TPyi , &
                                TwHt9TPzi ,  TwrBsFxt ,  TwrBsFyt ,  TwrBsFzt ,  TwrBsMxt ,  TwrBsMyt ,  TwrBsMzt , &
                                TipClrnc1 , TipClrnc2 , TipClrnc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 ,   WindVxi , &
                                VerWndDir ,   WindVyi ,   WindVxi ,   WindVyi ,   WindVzi ,   WindVzi ,    YawAzn , &
                                   YawAzn ,    YawAzn ,  YawBrFxn ,  YawBrFxp ,  YawBrFyn ,  YawBrFyp ,  YawBrFzn , &
                                 YawBrFzn ,  YawBrMxn ,  YawBrMxp ,  YawBrMyn ,  YawBrMyp ,  YawBrMzn ,  YawBrMzn , &
                                YawBrRAxp , YawBrRAyp , YawBrRAzp , YawBrRDxt , YawBrRDyt , YawBrRDzt , YawBrRVxp , &
                                YawBrRVyp , YawBrRVzp , YawBrTAxp , YawBrTAyp , YawBrTAzp , YawBrTDxp , YawBrTDxt , &
                                YawBrTDyp , YawBrTDyt , YawBrTDzp , YawBrTDzt ,  YawBrMzn ,    YawPzn ,    YawPzn , &
                                   YawPzn ,    YawVzn ,    YawVzn ,    YawVzn /)
   CHARACTER(OutStrLen), PARAMETER :: ParamUnitsAry(1019) =  (/ &                         ! This lists the units corresponding to the allowed parameters
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(-)       ","(deg)     ","(-)       ","(deg/s^2) ","(-)       ","(-)       ","(kW)      ", &
                               "(rpm)     ","(kN·m)    ","(m/s)     ","(deg)     ","(kN·m)    ","(deg/s^2) ","(-)       ", &
                               "(-)       ","(kW)      ","(kN·m)    ","(rpm)     ","(m)       ","(m)       ","(m)       ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(rpm)     ","(rpm)     ","(rpm)     ", &
                               "(-)       ","(-)       ","(-)       ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kW)      ","(kN·m)    ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(rpm)     ","(rpm)     ","(rpm)     ","(deg)     ","(deg/s^2) ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(m)       ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   ","(deg/s)   ","(deg/s)   ", &
                               "(deg/s)   ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s^2)   ","(rad/s^2) ", &
                               "(rad/s^2) ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ", &
                               "(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(rad/s)   ","(rad/s)   ","(m/s)     ","(rad/s)   ","(rad/s)   ","(rad/s)   ","(m/s)     ", &
                               "(m/s)     ","(rad/s)   ","(m/s)     ","(m/s)     ","(rad/s)   ","(m/s)     ","(m/s)     ", &
                               "(rad/s)   ","(rad/s)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(rad)     ","(rad)     ","(m)       ", &
                               "(rad)     ","(rad)     ","(rad)     ","(m)       ","(m)       ","(rad)     ","(m)       ", &
                               "(m)       ","(rad)     ","(m)       ","(m)       ","(rad)     ","(rad)     ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg/s^2) ","(-)       ","(-)       ","(-)       ","(deg)     ", &
                               "(deg/s^2) ","(deg)     ","(deg/s)   ","(kW)      ","(rpm)     ","(deg/s^2) ","(deg)     ", &
                               "(deg/s)   ","(kN)      ","(kN·m)    ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(deg)     ","(deg/s^2) ","(deg)     ","(deg/s)   ","(deg/s^2) ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg)     ","(-)       ","(-)       ","(kN)      ", &
                               "(kN)      ","(Pa)      ","(kN·m)    ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(-)       ", &
                               "(m/s)     ","(-)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(m)       ", &
                               "(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ","(m/s)     ", &
                               "(deg)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ","(deg)     ","(deg)     ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   "/)
   LOGICAL                  :: InvalidOutput(0:MaxOutPts)                        ! This array determines if the output channel is valid for this configuration

      ! initialize values
   ErrStat = ErrID_none
   ErrMsg  = ""

   InvalidOutput            = .FALSE.
!End of code generated by Matlab script

!BJJ: THE FOLLOWING USED TO BE PART OF THE SCRIPT, BUT I REWROTE IT TO BE MORE EFFICIENT
   
   !IF ( .NOT. CompAero ) THEN
   !   InvalidOutput(  WindVxi) = .TRUE.
   !   InvalidOutput(  WindVyi) = .TRUE.
   !   InvalidOutput(  WindVzi) = .TRUE.
   !   InvalidOutput( TotWindV) = .TRUE.
   !   InvalidOutput( HorWindV) = .TRUE.
   !   InvalidOutput(HorWndDir) = .TRUE.
   !   InvalidOutput(VerWndDir) = .TRUE.
   !   
   !   InvalidOutput(TipSpdRat) = .TRUE.
   !   InvalidOutput(NacYawErr) = .TRUE.
   !   
   !   InvalidOutput(    RotCq) = .TRUE.
   !   InvalidOutput(    RotCp) = .TRUE.
   !   InvalidOutput(    RotCt) = .TRUE.
   !   InvalidOutput( HSShftCq) = .TRUE.
   !   InvalidOutput( HSShftCp) = .TRUE.
   !   InvalidOutput(    GenCq) = .TRUE.
   !   InvalidOutput(    GenCp) = .TRUE.
   !   InvalidOutput(TFinAlpha) = .TRUE.
   !   InvalidOutput(TFinCLift) = .TRUE.
   !   InvalidOutput(TFinCDrag) = .TRUE.
   !   InvalidOutput(TFinDnPrs) = .TRUE.
   !   InvalidOutput( TFinCPFx) = .TRUE.
   !   InvalidOutput( TFinCPFy) = .TRUE.
   !END IF
                            
   DO I = p%NumBl+1,3  ! Invalid blades
      
         ! motions
      
      InvalidOutput(   TipDxc(  I) ) = .TRUE.
      InvalidOutput(   TipDyc(  I) ) = .TRUE.
      InvalidOutput(   TipDzc(  I) ) = .TRUE.
      InvalidOutput(   TipDxb(  I) ) = .TRUE.
      InvalidOutput(   TipDyb(  I) ) = .TRUE.
      InvalidOutput(  TipALxb(  I) ) = .TRUE.
      InvalidOutput(  TipALyb(  I) ) = .TRUE.
      InvalidOutput(  TipALzb(  I) ) = .TRUE.
      InvalidOutput(  TipRDxb(  I) ) = .TRUE.
      InvalidOutput(  TipRDyb(  I) ) = .TRUE.
      InvalidOutput(  TipRDzc(  I) ) = .TRUE.
      InvalidOutput( TipClrnc(  I) ) = .TRUE.

         ! loads
         
      InvalidOutput(  RootFxc(  I) ) = .TRUE.
      InvalidOutput(  RootFyc(  I) ) = .TRUE.
      InvalidOutput(  RootFzc(  I) ) = .TRUE.
      InvalidOutput(  RootFxb(  I) ) = .TRUE.
      InvalidOutput(  RootFyb(  I) ) = .TRUE.
      InvalidOutput(  RootMxc(  I) ) = .TRUE.
      InvalidOutput(  RootMyc(  I) ) = .TRUE.
      InvalidOutput(  RootMzc(  I) ) = .TRUE.
      InvalidOutput(  RootMxb(  I) ) = .TRUE.
      InvalidOutput(  RootMyb(  I) ) = .TRUE.

         ! Blade node motions
 
      InvalidOutput(  SpnALxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnTDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnRDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDzb(:,I) ) = .TRUE.

         ! Blade node loads
                  
      InvalidOutput(  SpnMLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnFLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLzb(:,I) ) = .TRUE.

   END DO

                                     
   DO I = 1,p%NumBl    
      
      DO J = p%NBlGages+1,9 ! Invalid blade gages

         InvalidOutput(  SpnALxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnTDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnRDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDzb(J,I) ) = .TRUE.

            ! Loads
            
         InvalidOutput(  SpnMLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLzb(J,I) ) = .TRUE.
            
         InvalidOutput(  SpnFLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLzb(J,I) ) = .TRUE.


      END DO !J
      
   END DO !I
   
   DO J = p%NTwGages+1,9 !Invalid tower gages

         ! Motions
         
      InvalidOutput( TwHtALxt(J) ) = .TRUE.
      InvalidOutput( TwHtALyt(J) ) = .TRUE.
      InvalidOutput( TwHtALzt(J) ) = .TRUE.

      InvalidOutput( TwHtTDxt(J) ) = .TRUE.
      InvalidOutput( TwHtTDyt(J) ) = .TRUE.
      InvalidOutput( TwHtTDzt(J) ) = .TRUE.

      InvalidOutput( TwHtRDxt(J) ) = .TRUE.
      InvalidOutput( TwHtRDyt(J) ) = .TRUE.
      InvalidOutput( TwHtRDzt(J) ) = .TRUE.

      InvalidOutput( TwHtTPxi(J) ) = .TRUE.
      InvalidOutput( TwHtTPyi(J) ) = .TRUE.
      InvalidOutput( TwHtTPzi(J) ) = .TRUE.

      InvalidOutput( TwHtRPxi(J) ) = .TRUE.
      InvalidOutput( TwHtRPyi(J) ) = .TRUE.
      InvalidOutput( TwHtRPzi(J) ) = .TRUE.

         ! Loads

      InvalidOutput( TwHtMLxt(J) ) = .TRUE.
      InvalidOutput( TwHtMLyt(J) ) = .TRUE.
      InvalidOutput( TwHtMLzt(J) ) = .TRUE.

      InvalidOutput( TwHtFLxt(J) ) = .TRUE.
      InvalidOutput( TwHtFLyt(J) ) = .TRUE.
      InvalidOutput( TwHtFLzt(J) ) = .TRUE.

   END DO      

   IF ( p%NumBl < 3_IntKi ) THEN
      InvalidOutput(PtchPMzc3) = .TRUE.
      
      InvalidOutput(   Q_B3E1) = .TRUE.
      InvalidOutput(   Q_B3F1) = .TRUE.
      InvalidOutput(   Q_B3F2) = .TRUE.
      
      InvalidOutput(  QD_B3E1) = .TRUE.
      InvalidOutput(  QD_B3F1) = .TRUE.
      InvalidOutput(  QD_B3F2) = .TRUE.
   
      InvalidOutput( QD2_B3E1) = .TRUE.
      InvalidOutput( QD2_B3F1) = .TRUE.
      InvalidOutput( QD2_B3F2) = .TRUE.
   ELSE IF ( p%NumBl > 2_IntKi ) THEN
      InvalidOutput(  TeetPya) = .TRUE.
      InvalidOutput(  TeetVya) = .TRUE.
      InvalidOutput(  TeetAya) = .TRUE.

      InvalidOutput(   Q_Teet) = .TRUE.
      InvalidOutput(  QD_Teet) = .TRUE.
      InvalidOutput( QD2_Teet) = .TRUE.
   END IF
         
            
   ALLOCATE ( p%OutParam(0:p%NumOuts) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating memory for the ElastoDyn OutParam array.'
      RETURN
   ENDIF
     
   !-------------------------------------------------------------------------------------------------
   ! Set index, name, and units for the output channels
   ! If a selected output channel is not available in this module, set error flag.
   !-------------------------------------------------------------------------------------------------     
   
      ! Set index, name, and units for the time output channel:
   
   p%OutParam(0)%Indx  = Time      !
   p%OutParam(0)%Name  = 'Time'    ! OutParam(0) is the time channel by default.
   p%OutParam(0)%Units = '(s)'     !
   p%OutParam(0)%SignM = 1
      
   
      ! Set index, name, and units for all of the output channels.
      ! If a selected output channel is not available by this module set ErrStat = ErrID_Warn.
   
   DO I = 1,p%NumOuts
   
      p%OutParam(I)%Name  = OutList(I)   
      OutListTmp          = OutList(I)
   
      ! Reverse the sign (+/-) of the output channel if the user prefixed the
      !   channel name with a '-', '_', 'm', or 'M' character indicating "minus".
   
      
      CheckOutListAgain = .FALSE.
      
      IF      ( INDEX( '-_', OutListTmp(1:1) ) > 0 ) THEN
         p%OutParam(I)%SignM = -1                         ! ex, '-TipDxc1' causes the sign of TipDxc1 to be switched.
         OutListTmp          = OutListTmp(2:)
      ELSE IF ( INDEX( 'mM', OutListTmp(1:1) ) > 0 ) THEN ! We'll assume this is a variable name for now, (if not, we will check later if OutListTmp(2:) is also a variable name)
         CheckOutListAgain   = .TRUE.
         p%OutParam(I)%SignM = 1
      ELSE
         p%OutParam(I)%SignM = 1
      END IF          
      
      CALL Conv2UC( OutListTmp )    ! Convert OutListTmp to upper case
   
   
      Indx = IndexCharAry( OutListTmp(1:OutStrLenM1), ValidParamAry )
      
      
         ! If it started with an "M" (CheckOutListAgain) we didn't find the value in our list (Indx < 1)
         
      IF ( CheckOutListAgain .AND. Indx < 1 ) THEN    ! Let's assume that "M" really meant "minus" and then test again         
         p%OutParam(I)%SignM = -1                     ! ex, 'MTipDxc1' causes the sign of TipDxc1 to be switched.
         OutListTmp          = OutListTmp(2:)
         
         Indx = IndexCharAry( OutListTmp(1:OutStrLenM1), ValidParamAry )         
      END IF
            
      
      IF ( Indx > 0 ) THEN ! we found the channel name
         p%OutParam(I)%Indx     = ParamIndxAry(Indx)
         IF ( InvalidOutput( ParamIndxAry(Indx) ) ) THEN  ! but, it isn't valid for this turbine configuration
            p%OutParam(I)%Units = 'INVALID'   
            p%OutParam(I)%SignM = 0
         ELSE
            p%OutParam(I)%Units = ParamUnitsAry(Indx) ! it's a valid output
         END IF
      ELSE ! this channel isn't valid
         p%OutParam(I)%Indx  = Time                 ! pick any valid channel (I just picked "Time" here because it's universal)
         p%OutParam(I)%Units = 'INVALID'            
         p%OutParam(I)%SignM = 0                    ! multiply all results by zero 
         
         ErrStat = ErrID_Warn
         ErrMsg  = p%OutParam(I)%Name//' is not an available output channel. '//TRIM(ErrMsg)
      END IF
      
   END DO                
   
   RETURN
END SUBROUTINE SetOutParam
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE Coeff(p,InputFileData, ErrStat, ErrMsg)
! This routine is used to compute rotor (blade and hub) properties:
!   KBF(), KBE(), CBF(), CBE(), FreqBF(), FreqBE(), AxRedBld(),
!   TwistedSF(), BldMass(), FirstMom(), SecondMom(), BldCG(),
!   RotMass, RotIner, Hubg1Iner, Hubg2Iner, rSAerCenn1(), and
!   rSAerCenn2()
! tower properties:
!   KTFA(), KTSS(), CTFA(), CTSS(), FreqTFA(), FreqTSS(),
!   AxRedTFA(), AxRedTSS(), TwrFASF(), TwrSSSF(), TwrMass, and
!   TwrTpMass
! structure that furls with the rotor (not including rotor) properties:
!   RrfaIner
! tail boom properties:
!   AtfaIner
! and nacelle properties:
!   Nacd2Iner
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables

   TYPE(ED_ParameterType),        INTENT(INOUT)    :: p                             ! Parameters of the structural dynamics module
   TYPE(ED_InputFile),            INTENT(IN)       :: InputFileData                 ! all the data in the ElastoDyn input file
   INTEGER(IntKi),                INTENT(OUT)      :: ErrStat                       ! Error status
   CHARACTER(1024),               INTENT(OUT)      :: ErrMsg                        ! Error message when ErrStat =/ ErrID_None


      ! Local variables.

   REAL(ReKi)                   :: AxRdBld   (3,3)                                 ! Temporary result holding the current addition to the p%AxRedBld() array.
   REAL(ReKi)                   :: AxRdBldOld(3,3)                                 ! Previous AxRdBld (i.e., AxRdBld from the previous node)
   REAL(ReKi)                   :: AxRdTFA   (2,2)                                 ! Temporary result holding the current addition to the AxRedTFA() array.
   REAL(ReKi)                   :: AxRdTFAOld(2,2)                                 ! Previous AxRdTFA (i.e., AxRdTFA from the previous node)
   REAL(ReKi)                   :: AxRdTSS   (2,2)                                 ! Temporary result holding the current addition to the AxRedTSS() array.
   REAL(ReKi)                   :: AxRdTSSOld(2,2)                                 ! Previous AxRdTSS (i.e., AxRdTSS from the previous node)
   REAL(ReKi)                   :: TmpDist                                         ! Temporary distance used in the calculation of the aero center locations.
   REAL(ReKi)                   :: TmpDistj1                                       ! Temporary distance used in the calculation of the aero center locations.
   REAL(ReKi)                   :: TmpDistj2                                       ! Temporary distance used in the calculation of the aero center locations.
   REAL(ReKi)                   :: ElMassOld                                       ! Previous ElmntMass (i.e., ElmntMass from the previous node)
   REAL(ReKi)                   :: ElmntMass                                       ! (Temporary) mass of an element.
   REAL(ReKi)                   :: ElmntStff                                       ! (Temporary) stiffness of an element.
   REAL(ReKi)                   :: ElStffFA                                        ! (Temporary) tower fore-aft stiffness of an element
   REAL(ReKi)                   :: ElStffSS                                        ! (Temporary) tower side-to-side  stiffness of an element
   REAL(ReKi)                   :: FMomAbvNd (p%NumBl,p%BldNodes)                  ! FMomAbvNd(K,J) = portion of the first moment of blade K about the rotor centerline (not root, like FirstMom(K)) associated with everything above node J (including tip brake masses).
   REAL(ReKi)                   :: KBECent   (p%NumBl,1,1)                         ! Centrifugal-term of generalized edgewise stiffness of the blades.
   REAL(ReKi)                   :: KBFCent   (p%NumBl,2,2)                         ! Centrifugal-term of generalized flapwise stiffness of the blades.
   REAL(ReKi)                   :: KTFAGrav  (2,2)                                 ! Gravitational-term of generalized fore-aft stiffness of the tower.
   REAL(ReKi)                   :: KTSSGrav  (2,2)                                 ! Gravitational-term of generalized side-to-side stiffness of the tower.
   REAL(ReKi)                   :: MBE       (p%NumBl,1,1)                         ! Generalized edgewise mass of the blades.
   REAL(ReKi)                   :: MBF       (p%NumBl,2,2)                         ! Generalized flapwise mass of the blades.
   REAL(ReKi)                   :: MTFA      (2,2)                                 ! Generalized fore-aft mass of the tower.
   REAL(ReKi)                   :: MTSS      (2,2)                                 ! Generalized side-to-side mass of the tower.
   REAL(ReKi)                   :: Shape                                           ! Temporary result holding a value from the SHP function
   REAL(ReKi)                   :: Shape1                                          ! Temporary result holding a value from the SHP function
   REAL(ReKi)                   :: Shape2                                          ! Temporary result holding a value from the SHP function
   REAL(ReKi)                   :: TMssAbvNd (p%TwrNodes)                          ! Portion of the tower mass associated with everything above node J (including tower-top effects)
   REAL(ReKi)                   :: TwstdSF   (2,3,0:1)                             ! Temperory result holding the current addition to the TwistedSF() array.
   REAL(ReKi)                   :: TwstdSFOld(2,3,0:1)                             ! Previous TwstdSF (i.e., TwstdSF from the previous node)

   INTEGER(IntKi)               :: I                                               ! Generic index.
   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.
   INTEGER(IntKi)               :: L                                               ! Generic index


   ErrStat = ErrID_None
   ErrMsg  = ''
   
   !...............................................................................................................................
   ! Calculate the distances from point S on a blade to the aerodynamic center in the j1 and j2 directions:
   !...............................................................................................................................

      !bjj: we're getting rid of rSAerCenn*, but I'll just set these to 0 now:
   !DO K = 1,p%NumBl          ! Loop through the blades
   !
   !   DO J = 1,p%BldNodes    ! Loop through the blade nodes / elements
   !
   !      TmpDist           = ( p%AeroCent(K,J) - 0.25 )*p%Chord(J)   ! Distance along the chordline from point S (25% chord) to the aerodynamic center of the blade element J--positive towards the trailing edge.
   !      TmpDistj1         = TmpDist*p%SAeroTwst(J)                ! Distance along the j1-axis   from point S (25% chord) to the aerodynamic center of the blade element J
   !      TmpDistj2         = TmpDist*p%CAeroTwst(J)                ! Distance along the j2-axis   from point S (25% chord) to the aerodynamic center of the blade element J
   !      p%rSAerCenn1(K,J) = TmpDistj1*p%CThetaS(K,J) - TmpDistj2*p%SThetaS(K,J)
   !      p%rSAerCenn2(K,J) = TmpDistj1*p%SThetaS(K,J) + TmpDistj2*p%CThetaS(K,J)
   !
   !   ENDDO ! J - Blade nodes / elements
   !
   !ENDDO    ! K - Blades
   p%rSAerCenn1 = 0.0
   p%rSAerCenn2 = 0.0
   
   !...............................................................................................................................
   ! Calculate the structure that furls with the rotor inertia term:
   !...............................................................................................................................

   p%RrfaIner  = InputFileData%RFrlIner - p%RFrlMass*(      (p%rVDxn**2    )*( 1.0 - p%CRFrlSkw2*p%CRFrlTlt2 ) &
                                     +    (p%rVDzn**2    )*                    p%CRFrlTlt2   &
                                     +    (p%rVDyn**2    )*( 1.0 - p%SRFrlSkw2*p%CRFrlTlt2 ) &
                                     - 2.0*p%rVDxn*p%rVDzn*        p%CRFrlSkew*p%CSRFrlTlt   &
                                     - 2.0*p%rVDxn*p%rVDyn*        p%CSRFrlSkw*p%CRFrlTlt2   &
                                     - 2.0*p%rVDzn*p%rVDyn*        p%SRFrlSkew*p%CSRFrlTlt     )
   IF ( p%RrfaIner < 0.0 )   THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' RFrlIner must not be less than RFrlMass*( perpendicular distance between rotor-furl'// &
               ' axis and CM of the structure that furls with the rotor [not including rotor] )^2.'
      RETURN
   END IF   

   !...............................................................................................................................
   ! Calculate the tail boom inertia term:
   !...............................................................................................................................

   p%AtfaIner  = p%TFrlIner - p%BoomMass*(   p%rWIxn*p%rWIxn*( 1.0 - p%CTFrlSkw2*p%CTFrlTlt2 ) &
                                       +     p%rWIzn*p%rWIzn*                    p%CTFrlTlt2   &
                                       +     p%rWIyn*p%rWIyn*( 1.0 - p%STFrlSkw2*p%CTFrlTlt2 ) &
                                       - 2.0*p%rWIxn*p%rWIzn*        p%CTFrlSkew*p%CSTFrlTlt   &
                                       - 2.0*p%rWIxn*p%rWIyn*        p%CSTFrlSkw*p%CTFrlTlt2   &
                                       - 2.0*p%rWIzn*p%rWIyn*        p%STFrlSkew*p%CSTFrlTlt     )
   IF ( p%AtfaIner < 0.0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' TFrlIner must not be less than BoomMass*( perpendicular distance between tail-furl'// &
                                        ' axis and tail boom CM )^2.'                           
      RETURN
   ENDIF
   
   !...............................................................................................................................
   ! Calculate the nacelle inertia terms:
   !...............................................................................................................................

   p%Nacd2Iner = InputFileData%NacYIner - p%NacMass*( p%NacCMxn**2 + p%NacCMyn**2 ) ! Nacelle inertia about the d2-axis
   IF ( p%Nacd2Iner < 0.0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' NacYIner must not be less than NacMass*( NacCMxn^2 + NacCMyn^2 ).'
      RETURN
   END IF   

      ! Calculate hub inertia about its centerline passing through its c.g..
      !   This calculation assumes that the hub for a 2-blader is essentially
      !   a uniform cylinder whose centerline is transverse through the cylinder
      !   passing through its c.g..  That is, for a 2-blader, Hubg1Iner =
      !   Hubg2Iner is the inertia of the hub about both the g1- and g2- axes.  For
      !   3-bladers, Hubg1Iner is simply equal to HubIner and Hubg2Iner is zero.
      ! Also, Initialize RotMass and RotIner to associated hub properties:

   IF ( p%NumBl == 2 )  THEN ! 2-blader
      p%Hubg1Iner = ( InputFileData%HubIner - p%HubMass*( ( p%UndSling - p%HubCM )**2 ) )/( p%CosDel3**2 )
      p%Hubg2Iner = p%Hubg1Iner
      IF ( p%Hubg1Iner < 0.0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = ' HubIner must not be less than HubMass*( UndSling - HubCM )^2 for 2-blader.'
         RETURN
      END IF      
   ELSE                    ! 3-blader
      p%Hubg1Iner = InputFileData%HubIner
      p%Hubg2Iner = 0.0
   ENDIF

   p%RotMass   = p%HubMass
   p%RotIner   = p%Hubg1Iner


   !...............................................................................................................................

      ! Initialize several variables to 0.0:

   p%KBF     = 0.0
   p%KBE     = 0.0
   KBFCent   = 0.0
   KBECent   = 0.0

   p%TwrMass = 0.0
   p%KTFA    = 0.0
   p%KTSS    = 0.0
   KTFAGrav  = 0.0
   KTSSGrav  = 0.0



   DO K = 1,p%NumBl          ! Loop through the blades


      ! Initialize BldMass(), FirstMom(), and SecondMom() using TipMass() effects:

      p%BldMass  (K) = p%TipMass(K)
      p%FirstMom (K) = p%TipMass(K)*p%BldFlexL
      p%SecondMom(K) = p%TipMass(K)*p%BldFlexL*p%BldFlexL


      DO J = p%BldNodes,1,-1 ! Loop through the blade nodes / elements in reverse


      ! Calculate the mass of the current element

         ElmntMass    = p%MassB(K,J)*p%DRNodes(J)                        ! Mass of blade element J


      ! Integrate to find some blade properties which will be output in .fsm

         p%BldMass  (K) = p%BldMass  (K) + ElmntMass
         p%FirstMom (K) = p%FirstMom (K) + ElmntMass*p%RNodes(J)
         p%SecondMom(K) = p%SecondMom(K) + ElmntMass*p%RNodes(J)*p%RNodes(J)


      ! Integrate to find FMomAbvNd:

         FMomAbvNd   (K,J) = ( 0.5*ElmntMass )*( p%HubRad + p%RNodes(J  ) + 0.5*p%DRNodes(J  ) )

         IF ( J == p%BldNodes )  THEN ! Outermost blade element
      ! Add the TipMass() effects:

            FMomAbvNd(K,J) = FmomAbvNd(K,J) + p%TipMass(K)*p%TipRad
         ELSE                       ! All other blade elements
      ! Add to FMomAbvNd(K,J) the effects from the (not yet used) portion of element J+1

            FMomAbvNd(K,J) = FMomAbvNd(K,J) + FMomAbvNd(K,J+1) &
                           + ( 0.5*ElMassOld )*( p%HubRad + p%RNodes(J+1) - 0.5*p%DRNodes(J+1) )
         ENDIF


      ! Store the mass of the current element (this will be used for the next element)

         ElMassOld    = ElmntMass


      ENDDO ! J - Blade nodes / elements in reverse


      ! Calculate BldCG() using FirstMom() and BldMass(); and calculate
      !   RotMass and RotIner:

      p%BldCG    (K) = p%FirstMom (K) / p%BldMass    (K)
      p%RotMass      = p%RotMass      + p%BldMass    (K)
      p%RotIner      = p%RotIner      + ( p%SecondMom(K) + p%BldMass  (K)*p%HubRad*( 2.0*p%BldCG(K) + p%HubRad ) )*( p%CosPreC(K)**2 )
   ENDDO ! K - Blades



   DO K = 1,p%NumBl          ! Loop through the blades


      ! Initialize the generalized blade masses using tip mass effects:

      MBF(K,1,1) = p%TipMass(K)
      MBF(K,2,2) = p%TipMass(K)
      MBE(K,1,1) = p%TipMass(K)


      DO J = 1,p%BldNodes    ! Loop through the blade nodes / elements


      ! Integrate to find the generalized mass of the blade (including tip mass effects).
      !   Ignore the cross-correlation terms of MBF (i.e. MBF(i,j) where i /= j) since
      !   these terms will never be used.

         ElmntMass     = p%MassB(K,J)*p%DRNodes(J)                          ! Mass of blade element J

         Shape1 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl1Sh(:,K), 0, ErrStat, ErrMsg )
         Shape2 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl2Sh(:,K), 0, ErrStat, ErrMsg )
         MBF    (K,1,1) = MBF    (K,1,1) + ElmntMass*Shape1*Shape1
         MBF    (K,2,2) = MBF    (K,2,2) + ElmntMass*Shape2*Shape2

         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldEdgSh(:,K), 0, ErrStat, ErrMsg )
         MBE    (K,1,1) = MBE    (K,1,1) + ElmntMass*Shape *Shape


      ! Integrate to find the generalized stiffness of the blade (not including centrifugal
      !    effects).

         ElmntStff      = p%StiffBF(K,J)*p%DRNodes(J)                       ! Flapwise stiffness of blade element J
         Shape1 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl1Sh(:,K), 2, ErrStat, ErrMsg )
         Shape2 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl2Sh(:,K), 2, ErrStat, ErrMsg )
         p%KBF    (K,1,1) = p%KBF    (K,1,1) + ElmntStff*Shape1*Shape1
         p%KBF    (K,1,2) = p%KBF    (K,1,2) + ElmntStff*Shape1*Shape2
         p%KBF    (K,2,1) = p%KBF    (K,2,1) + ElmntStff*Shape2*Shape1
         p%KBF    (K,2,2) = p%KBF    (K,2,2) + ElmntStff*Shape2*Shape2

         ElmntStff      = p%StiffBE(K,J)*p%DRNodes(J)                       ! Edgewise stiffness of blade element J
         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldEdgSh(:,K), 2, ErrStat, ErrMsg )
         p%KBE    (K,1,1) = p%KBE    (K,1,1) + ElmntStff*Shape *Shape


      ! Integrate to find the centrifugal-term of the generalized flapwise and edgewise
      !   stiffness of the blades.  Ignore the cross-correlation terms of KBFCent (i.e.
      !   KBFCent(i,j) where i /= j) since these terms will never be used.

         ElmntStff      = FMomAbvNd(K,J)*p%DRNodes(J)*p%RotSpeed**2   ! Centrifugal stiffness of blade element J

         Shape1 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl1Sh(:,K), 1, ErrStat, ErrMsg )
         Shape2 = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl2Sh(:,K), 1, ErrStat, ErrMsg )
         KBFCent(K,1,1) = KBFCent(K,1,1) + ElmntStff*Shape1*Shape1
         KBFCent(K,2,2) = KBFCent(K,2,2) + ElmntStff*Shape2*Shape2

         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldEdgSh(:,K), 1, ErrStat, ErrMsg )
         KBECent(K,1,1) = KBECent(K,1,1) + ElmntStff*Shape *Shape


      ! Calculate the 2nd derivatives of the twisted shape functions:

         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl1Sh(:,K), 2, ErrStat, ErrMsg )
         p%TwistedSF(K,1,1,J,2) =  Shape*p%CThetaS(K,J)                  ! 2nd deriv. of Phi1(J) for blade K
         p%TwistedSF(K,2,1,J,2) = -Shape*p%SThetaS(K,J)                  ! 2nd deriv. of Psi1(J) for blade K

         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldFl2Sh(:,K), 2, ErrStat, ErrMsg )
         p%TwistedSF(K,1,2,J,2) =  Shape*p%CThetaS(K,J)                  ! 2nd deriv. of Phi2(J) for blade K
         p%TwistedSF(K,2,2,J,2) = -Shape*p%SThetaS(K,J)                  ! 2nd deriv. of Psi2(J) for blade K

         Shape  = SHP( p%RNodesNorm(J), p%BldFlexL, p%BldEdgSh(:,K), 2, ErrStat, ErrMsg )
         p%TwistedSF(K,1,3,J,2) =  Shape*p%SThetaS(K,J)                  ! 2nd deriv. of Phi3(J) for blade K
         p%TwistedSF(K,2,3,J,2) =  Shape*p%CThetaS(K,J)                  ! 2nd deriv. of Psi3(J) for blade K


      ! Integrate to find the 1st derivatives of the twisted shape functions:

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwstdSF     (  I,L,  1) = p%TwistedSF(K,I,L,J,2)*0.5*p%DRNodes(J)
               p%TwistedSF   (K,I,L,J,1) = TwstdSF   ( I,L,  1)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi

         IF ( J /= 1 )  THEN  ! All but the innermost blade element
      ! Add the effects from the (not yet used) portion of element J-1

            DO I = 1,2     ! Loop through Phi and Psi
               DO L = 1,3  ! Loop through all blade DOFs
                  p%TwistedSF(K,I,L,J,1) = p%TwistedSF(K,I,L,J,1) + p%TwistedSF(K,I,L,J-1,1) &
                                       + TwstdSFOld( I,L,  1)
               ENDDO       ! L - All blade DOFs
            ENDDO          ! I - Phi and Psi
         ENDIF


      ! Integrate to find the twisted shape functions themselves (i.e., their zeroeth derivative):

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwstdSF     (  I,L,  0) = p%TwistedSF(K,I,L,J,1)*0.5*p%DRNodes(J)
               p%TwistedSF   (K,I,L,J,0) = TwstdSF   ( I,L,  0)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi

         IF ( J /= 1 )  THEN  ! All but the innermost blade element
      ! Add the effects from the (not yet used) portion of element J-1

            DO I = 1,2     ! Loop through Phi and Psi
               DO L = 1,3  ! Loop through all blade DOFs
                  p%TwistedSF(K,I,L,J,0) = p%TwistedSF(K,I,L,J,0) + p%TwistedSF(K,I,L,J-1,0) &
                                       + TwstdSFOld( I,L,  0)
               ENDDO       ! L - All blade DOFs
            ENDDO          ! I - Phi and Psi
         ENDIF


      ! Integrate to find the blade axial reduction shape functions:

         DO I = 1,3     ! Loop through all blade DOFs
            DO L = 1,3  ! Loop through all blade DOFs
               AxRdBld    (  I,L  ) = 0.5*p%DRNodes(J)*(                          &
                                      p%TwistedSF(K,1,I,J,1)*p%TwistedSF(K,1,L,J,1) &
                                    + p%TwistedSF(K,2,I,J,1)*p%TwistedSF(K,2,L,J,1) )
               p%AxRedBld   (K,I,L,J) = AxRdBld(I,L)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - All blade DOFs

         IF ( J /= 1 )  THEN  ! All but the innermost blade element
      ! Add the effects from the (not yet used) portion of element J-1

            DO I = 1,3     ! Loop through all blade DOFs
               DO L = 1,3  ! Loop through all blade DOFs
                  p%AxRedBld(K,I,L,J) = p%AxRedBld(K,I,L,J) + p%AxRedBld(K,I,L,J-1)   &
                                    + AxRdBldOld(I,L)
               ENDDO       ! L - All blade DOFs
            ENDDO          ! I - All blade DOFs
         ENDIF


      ! Store the TwstdSF and AxRdBld terms of the current element (these will be used for the next element)

         TwstdSFOld = TwstdSF
         AxRdBldOld = AxRdBld


      ENDDO ! J - Blade nodes / elements


      ! Apply the flapwise modal stiffness tuners of the blades to KBF():

      DO I = 1,2     ! Loop through flap DOFs
         DO L = 1,2  ! Loop through flap DOFs
            p%KBF(K,I,L) = SQRT( p%FStTunr(K,I)*p%FStTunr(K,L) )*p%KBF(K,I,L)
         ENDDO       ! L - Flap DOFs
      ENDDO          ! I - Flap DOFs


      ! Calculate the blade natural frequencies:


      DO I = 1,NumBF     ! Loop through flap DOFs
         p%FreqBF(K,I,1) = Inv2Pi*SQRT(   p%KBF(K,I,I)                   /( MBF(K,I,I) - p%TipMass(K) ) )   ! Natural blade I-flap frequency w/o centrifugal stiffening nor     tip mass effects
         p%FreqBF(K,I,2) = Inv2Pi*SQRT(   p%KBF(K,I,I)                   /  MBF(K,I,I)                )     ! Natural blade I-flap frequency w/o centrifugal stiffening, but w/ tip mass effects
         p%FreqBF(K,I,3) = Inv2Pi*SQRT( ( p%KBF(K,I,I) + KBFCent(K,I,I) )/  MBF(K,I,I)                )     ! Natural blade I-flap frequency w/  centrifugal stiffening and     tip mass effects
      ENDDO          ! I - Flap DOFs

      p%FreqBE   (K,1,1) = Inv2Pi*SQRT(   p%KBE(K,1,1)                   /( MBE(K,1,1) - p%TipMass(K) ) )   ! Natural blade 1-edge frequency w/o centrifugal stiffening nor      tip mass effects
      p%FreqBE   (K,1,2) = Inv2Pi*SQRT(   p%KBE(K,1,1)                   /  MBE(K,1,1)                )     ! Natural Blade 1-edge frequency w/o  centrifugal stiffening, but w/ tip mass effects
      p%FreqBE   (K,1,3) = Inv2Pi*SQRT( ( p%KBE(K,1,1) + KBECent(K,1,1) )/  MBE(K,1,1)                )     ! Natural Blade 1-edge frequency w/  centrifugal stiffening and      tip mass effects


      ! Calculate the generalized damping of the blades:

      DO I = 1,NumBF     ! Loop through flap DOFs
         DO L = 1,NumBF  ! Loop through flap DOFs
            p%CBF(K,I,L) = ( 0.01*p%BldFDamp(K,L) )*p%KBF(K,I,L)/( Pi*p%FreqBF(K,L,1) )
         ENDDO       ! L - Flap DOFs
      ENDDO          ! I - Flap DOFs

      p%CBE      (K,1,1) = ( 0.01*p%BldEDamp(K,1) )*p%KBE(K,1,1)/( Pi*p%FreqBE(K,1,1) )


      ! Calculate the 2nd derivatives of the twisted shape functions at the tip:

      Shape  = SHP( 1.0, p%BldFlexL, p%BldFl1Sh(:,K), 2, ErrStat, ErrMsg )
      p%TwistedSF(K,1,1,p%TipNode,2) =  Shape*p%CThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi1(p%TipNode) for blade K
      p%TwistedSF(K,2,1,p%TipNode,2) = -Shape*p%SThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi1(p%TipNode) for blade K

      Shape  = SHP( 1.0, p%BldFlexL, p%BldFl2Sh(:,K), 2, ErrStat, ErrMsg )
      p%TwistedSF(K,1,2,p%TipNode,2) =  Shape*p%CThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi2(p%TipNode) for blade K
      p%TwistedSF(K,2,2,p%TipNode,2) = -Shape*p%SThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi2(p%TipNode) for blade K

      Shape  = SHP( 1.0, p%BldFlexL, p%BldEdgSh(:,K), 2, ErrStat, ErrMsg )
      p%TwistedSF(K,1,3,p%TipNode,2) =  Shape*p%SThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi3(p%TipNode) for blade K
      p%TwistedSF(K,2,3,p%TipNode,2) =  Shape*p%CThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi3(p%TipNode) for blade K


      ! Integrate to find the 1st and zeroeth derivatives of the twisted shape functions
      !   at the tip:

      DO I = 1,2     ! Loop through Phi and Psi
         DO L = 1,3  ! Loop through all blade DOFs
            p%TwistedSF(K,I,L,p%TipNode,1) = p%TwistedSF(K,I,L,p%BldNodes,1) + TwstdSFOld(I,L,1)
            p%TwistedSF(K,I,L,p%TipNode,0) = p%TwistedSF(K,I,L,p%BldNodes,0) + TwstdSFOld(I,L,0)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - Phi and Psi


      ! Integrate to find the blade axial reduction shape functions at the tip:

      DO I = 1,3     ! Loop through all blade DOFs
         DO L = 1,3  ! Loop through all blade DOFs
            p%AxRedBld(K,I,L,p%TipNode) = p%AxRedBld(K,I,L,p%BldNodes) + AxRdBldOld(I,L)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - All blade DOFs


   ENDDO ! K - Blades



      ! Calculate the tower-top mass:

   p%TwrTpMass = p%RotMass + p%RFrlMass + p%BoomMass + p%TFinMass + p%NacMass + p%YawBrMass


   DO J = p%TwrNodes,1,-1 ! Loop through the tower nodes / elements in reverse


      ! Calculate the mass of the current element

      ElmntMass    = p%MassT(J)*p%DHNodes(J)     ! Mass of tower element J


      ! Integrate to find the tower mass which will be output in .fsm

      p%TwrMass      = p%TwrMass + ElmntMass


      ! Integrate to find TMssAbvNd:

      TMssAbvNd   (J) = 0.5*ElmntMass

      IF ( J == p%TwrNodes )  THEN ! Uppermost tower element
      ! Add the TwrTpMass effects:

         TMssAbvNd(J) = TMssAbvNd(J) + p%TwrTpMass
      ELSE                       ! All other tower elements
      ! Add to TMssAbvNd(J) the effects from the (not yet used) portion of element J+1

         TMssAbvNd(J) = 0.5*ElMassOld + TMssAbvNd(J) + TMssAbvNd(J+1)
      ENDIF


      ! Store the mass of the current element (this will be used for the next element)

      ElMassOld    = ElmntMass


   ENDDO ! J - Tower nodes / elements in reverse



      ! Initialize the generalized tower masses using tower-top mass effects:

   DO I = 1,2  ! Loop through all tower modes in a single direction
      MTFA(I,I) = p%TwrTpMass
      MTSS(I,I) = p%TwrTpMass
   ENDDO       ! I - All tower modes in a single direction


   DO J = 1,p%TwrNodes    ! Loop through the tower nodes / elements


      ! Calculate the tower shape functions (all derivatives):

      p%TwrFASF(1,J,2) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM1Sh(:), 2, ErrStat, ErrMsg )
      p%TwrFASF(2,J,2) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM2Sh(:), 2, ErrStat, ErrMsg )
      p%TwrFASF(1,J,1) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM1Sh(:), 1, ErrStat, ErrMsg )
      p%TwrFASF(2,J,1) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM2Sh(:), 1, ErrStat, ErrMsg )
      p%TwrFASF(1,J,0) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM1Sh(:), 0, ErrStat, ErrMsg )
      p%TwrFASF(2,J,0) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwFAM2Sh(:), 0, ErrStat, ErrMsg )

      p%TwrSSSF(1,J,2) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM1Sh(:), 2, ErrStat, ErrMsg )
      p%TwrSSSF(2,J,2) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM2Sh(:), 2, ErrStat, ErrMsg )
      p%TwrSSSF(1,J,1) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM1Sh(:), 1, ErrStat, ErrMsg )
      p%TwrSSSF(2,J,1) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM2Sh(:), 1, ErrStat, ErrMsg )
      p%TwrSSSF(1,J,0) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM1Sh(:), 0, ErrStat, ErrMsg )
      p%TwrSSSF(2,J,0) = SHP( p%HNodesNorm(J), p%TwrFlexL, InputFileData%TwSSM2Sh(:), 0, ErrStat, ErrMsg )


      ! Integrate to find the generalized mass of the tower (including tower-top mass effects).
      !   Ignore the cross-correlation terms of MTFA (i.e. MTFA(i,j) where i /= j) and MTSS
      !   since these terms will never be used.

      ElmntMass      = p%MassT(J)*p%DHNodes(J)                           ! Mass of tower element J

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         MTFA  (I,I) = MTFA  (I,I) + ElmntMass*p%TwrFASF(I,J,0)**2
         MTSS  (I,I) = MTSS  (I,I) + ElmntMass*p%TwrSSSF(I,J,0)**2
      ENDDO          ! I - through all tower DOFs in one direction


      ! Integrate to find the generalized stiffness of the tower (not including gravitational
      !    effects).

      ElStffFA       = p%StiffTFA(J)*p%DHNodes(J)                        ! Fore-aft stiffness of tower element J
      ElStffSS       = p%StiffTSS(J)*p%DHNodes(J)                        ! Side-to-side stiffness of tower element J

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         DO L = 1,2  ! Loop through all tower DOFs in one direction
            p%KTFA (I,L) = p%KTFA    (I,L) + ElStffFA *p%TwrFASF(I,J,2)*p%TwrFASF(L,J,2)
            p%KTSS (I,L) = p%KTSS    (I,L) + ElStffSS *p%TwrSSSF(I,J,2)*p%TwrSSSF(L,J,2)
         ENDDO       ! L - All tower DOFs in one direction
      ENDDO          ! I - through all tower DOFs in one direction


      ! Integrate to find the gravitational-term of the generalized stiffness of the tower.
      !   Ignore the cross-correlation terms of KTFAGrav (i.e. KTFAGrav(i,j) where i /= j)
      !   and KTSSGrav since these terms will never be used.

      ElmntStff      = -TMssAbvNd(J)*p%DHNodes(J)*p%Gravity              ! Gravitational stiffness of tower element J

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         KTFAGrav(I,I) = KTFAGrav(I,I) + ElmntStff*p%TwrFASF(I,J,1)**2
         KTSSGrav(I,I) = KTSSGrav(I,I) + ElmntStff*p%TwrSSSF(I,J,1)**2
      ENDDO


      ! Integrate to find the tower axial reduction shape functions:

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         DO L = 1,2  ! Loop through all tower DOFs in one direction
            AxRdTFA (I,L) = 0.5*p%DHNodes(J)*p%TwrFASF(I,J,1)*p%TwrFASF(L,J,1)
            AxRdTSS (I,L) = 0.5*p%DHNodes(J)*p%TwrSSSF(I,J,1)*p%TwrSSSF(L,J,1)

            p%AxRedTFA(I,L,J) = AxRdTFA(I,L)
            p%AxRedTSS(I,L,J) = AxRdTSS(I,L)
         ENDDO       ! L - All tower DOFs in one direction
      ENDDO

      IF ( J /= 1 )  THEN  ! All but the lowermost tower element
      ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,2     ! Loop through all tower DOFs in one direction
            DO L = 1,2  ! Loop through all tower DOFs in one direction
               p%AxRedTFA(I,L,J) = p%AxRedTFA(I,L,J) + p%AxRedTFA(I,L,J-1)+ AxRdTFAOld(I,L)
               p%AxRedTSS(I,L,J) = p%AxRedTSS(I,L,J) + p%AxRedTSS(I,L,J-1)+ AxRdTSSOld(I,L)
            ENDDO       ! L - All tower DOFs in one direction
         ENDDO
      ENDIF


      ! Store the AxRdTFA and AxRdTSS terms of the current element (these will be used for the next element)

      AxRdTFAOld = AxRdTFA
      AxRdTSSOld = AxRdTSS


   ENDDO ! J - Tower nodes / elements


   ! Apply the modal stiffness tuners of the tower to KTFA() and KTSS():

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         p%KTFA(I,L) = SQRT( InputFileData%FAStTunr(I)*InputFileData%FAStTunr(L) )*p%KTFA(I,L)

         p%KTSS(I,L) = SQRT( InputFileData%SSStTunr(I)*InputFileData%SSStTunr(L) )*p%KTSS(I,L)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO          ! I - through all tower DOFs in one direction


      ! Calculate the tower natural frequencies:

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      p%FreqTFA(I,1) = Inv2Pi*SQRT(   p%KTFA(I,I)                  /( MTFA(I,I) - p%TwrTpMass ) )  ! Natural tower I-fore-aft frequency w/o gravitational destiffening nor tower-top mass effects
      p%FreqTFA(I,2) = Inv2Pi*SQRT( ( p%KTFA(I,I) + KTFAGrav(I,I) )/  MTFA(I,I)               )  ! Natural tower I-fore-aft frequency w/  gravitational destiffening and tower-top mass effects
      p%FreqTSS(I,1) = Inv2Pi*SQRT(   p%KTSS(I,I)                  /( MTSS(I,I) - p%TwrTpMass ) )  ! Natural tower I-side-to-side frequency w/o gravitational destiffening nor tower-top mass effects
      p%FreqTSS(I,2) = Inv2Pi*SQRT( ( p%KTSS(I,I) + KTSSGrav(I,I) )/  MTSS(I,I)               )  ! Natural tower I-side-to-side frequency w/  gravitational destiffening and tower-top mass effects
   ENDDO          ! I - All tower DOFs in one direction


      ! Calculate the generalized damping of the tower:

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         p%CTFA(I,L) = ( 0.01*InputFileData%TwrFADmp(L) )*p%KTFA(I,L)/( Pi*p%FreqTFA(L,1) )

         p%CTSS(I,L) = ( 0.01*InputFileData%TwrSSDmp(L) )*p%KTSS(I,L)/( Pi*p%FreqTSS(L,1) )
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO          ! I - All tower DOFs in one direction


      ! Calculate the tower shape functions (all derivatives) at the tower-top:

   p%TwrFASF(1,p%TTopNode,2) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM1Sh(:), 2, ErrStat, ErrMsg )
   p%TwrFASF(2,p%TTopNode,2) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM2Sh(:), 2, ErrStat, ErrMsg )
   p%TwrFASF(1,p%TTopNode,1) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM1Sh(:), 1, ErrStat, ErrMsg )
   p%TwrFASF(2,p%TTopNode,1) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM2Sh(:), 1, ErrStat, ErrMsg )
   p%TwrFASF(1,p%TTopNode,0) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM1Sh(:), 0, ErrStat, ErrMsg )
   p%TwrFASF(2,p%TTopNode,0) = SHP( 1.0, p%TwrFlexL, InputFileData%TwFAM2Sh(:), 0, ErrStat, ErrMsg )

   p%TwrSSSF(1,p%TTopNode,2) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM1Sh(:), 2, ErrStat, ErrMsg )
   p%TwrSSSF(2,p%TTopNode,2) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM2Sh(:), 2, ErrStat, ErrMsg )
   p%TwrSSSF(1,p%TTopNode,1) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM1Sh(:), 1, ErrStat, ErrMsg )
   p%TwrSSSF(2,p%TTopNode,1) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM2Sh(:), 1, ErrStat, ErrMsg )
   p%TwrSSSF(1,p%TTopNode,0) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM1Sh(:), 0, ErrStat, ErrMsg )
   p%TwrSSSF(2,p%TTopNode,0) = SHP( 1.0, p%TwrFlexL, InputFileData%TwSSM2Sh(:), 0, ErrStat, ErrMsg )


      ! Integrate to find the tower axial reduction shape functions at the tower-top:

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         p%AxRedTFA(I,L,p%TTopNode) = p%AxRedTFA(I,L,p%TwrNodes)+ AxRdTFAOld(I,L)
         p%AxRedTSS(I,L,p%TTopNode) = p%AxRedTSS(I,L,p%TwrNodes)+ AxRdTSSOld(I,L)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO


      ! Calculate the turbine mass:

   p%TurbMass  = p%TwrTpMass + p%TwrMass


   RETURN
END SUBROUTINE Coeff
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE InitBlDefl ( p, InputFileData, InitQF1, InitQF2, InitQE1, ErrStat, ErrMsg )
! This routine calculates the initial blade deflections.
! Base the intial values of the blade DOFs, INITQF1, INITQF2, and
!   INITQE1, on OoPDefl and IPDefl.
! Write messages to the screen if the specified initial tip displacements
!  are incompatible with the enabled DOFs.
!..................................................................................................................................


      IMPLICIT                        NONE

      ! Passed variables:
   TYPE(ED_ParameterType),  INTENT(IN)  :: p                                       ! parameters of the structural dynamics module
   TYPE(ED_InputFile),      INTENT(IN)  :: InputFileData                           ! all the data in the ElastoDyn input file

   REAL(ReKi),              INTENT(OUT) :: InitQE1(p%NumBl)                        ! Initial edge deflection (output).
   REAL(ReKi),              INTENT(OUT) :: InitQF1(p%NumBl)                        ! Initial flap deflection for mode 1 (output).
   REAL(ReKi),              INTENT(OUT) :: InitQF2(p%NumBl)                        ! Initial flap deflection for mode 2 (output).

   INTEGER(IntKi),          INTENT(OUT) :: ErrStat                                 ! Error status
   CHARACTER(1024),         INTENT(OUT) :: ErrMsg                                  ! Error message when ErrStat =/ ErrID_None
   

      ! Local variables:
   REAL(ReKi)                   :: A(2,3)                                          ! Augmented matrix for solution of initial deflections.
   REAL(ReKi)                   :: CosPitch                                        ! Cosine of the pitch for this blade.
   REAL(ReKi)                   :: Det                                             ! Determinate of right-hand side of A.
   REAL(ReKi)                   :: SinPitch                                        ! Sine of the pitch for this blade.
   REAL(ReKi)                   :: TotResid                                        ! Generator torque.

   INTEGER(IntKi)               :: K                                               ! Blade number

      ! some warning messages
   CHARACTER(*), PARAMETER      :: Approx   = ' An approximate characterization of the specified blade deflection will be made.'
   CHARACTER(*), PARAMETER      :: BadIP    = ' Initial blade in-plane tip displacement will be ignored.'
   CHARACTER(*), PARAMETER      :: BadOoP   = ' Initial blade out-of-plane tip displacement will be ignored.'
   CHARACTER(*), PARAMETER      :: Ignore   = ' All initial blade tip displacements will be ignored.'


      ! Initialize variables
   ErrStat = ErrID_None
   ErrMsg  = ''

   InitQE1 = 0.0
   InitQF1 = 0.0
   InitQF2 = 0.0
   !bjj: replace InitQF1 and InitQF2 with an array to avoid so much duplication of logic here...
   
   DO K=1,p%NumBl

         ! Calculate the array of deflections(???).

      CosPitch = COS( InputFileData%BlPitch(K) )
      SinPitch = SIN( InputFileData%BlPitch(K) )

      A(1,2) =  p%TwistedSF(K,1,3,p%TipNode,0)*CosPitch + p%TwistedSF(K,2,3,p%TipNode,0)*SinPitch
      A(2,2) = -p%TwistedSF(K,1,3,p%TipNode,0)*SinPitch + p%TwistedSF(K,2,3,p%TipNode,0)*CosPitch
      A(1,3) =  InputFileData%OoPDefl
      A(2,3) =  InputFileData%IPDefl

      IF ( InputFileData%FlapDOF1 )  THEN                                ! Blade flap mode 1 is enabled

         A(1,1) =  p%TwistedSF(K,1,1,p%TipNode,0)*CosPitch + p%TwistedSF(K,2,1,p%TipNode,0)*SinPitch
         A(2,1) = -p%TwistedSF(K,1,1,p%TipNode,0)*SinPitch + p%TwistedSF(K,2,1,p%TipNode,0)*CosPitch

         DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

         IF ( .NOT. EqualRealNos( DET, 0.0_ReKi ) ) THEN                  ! Apply all flap deflection to mode 1

            InitQF1(K) = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
            InitQE1(K) = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

         ELSEIF ( .NOT. InputFileData%EdgeDOF )  THEN                     ! Blade edge mode 1 is not enabled which caused DET = 0.

            InitQE1(K) = 0.0

            IF ( .NOT. EqualRealNos( A(1,1), 0.0_ReKi ) )  THEN
               IF ( .NOT. EqualRealNos( A(2,1), 0.0_ReKi ) )  THEN        ! Find a solution of the 2 equations in 1 variable that
                                                                          !  minimizes the sum of the squares of the equation's residuals.

                  InitQF1(K) = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

                  TotResid = SQRT( ( A(1,1)*InitQF1(K) - A(1,3) )**2 + ( A(2,1)*InitQF1(K) - A(2,3) )**2 )

                  IF ( .NOT. EqualRealNos( TotResid, 0.0_ReKi ) ) THEN
                     CALL CheckError( ErrID_Warn, Approx )
                  ENDIF

               ELSE !A(1,1) /= 0; A(2,1) == 0

                  InitQF1(K) = A(1,3)/A(1,1)

                  IF ( .NOT. EqualRealNos( InputFileData%IPDefl,  0.0_ReKi ) )  THEN
                     CALL CheckError( ErrID_Warn, BadIP )
                  ENDIF          
            
               ENDIF

            ELSE ! A(1,1) == 0

               IF ( .NOT. EqualRealNos( InputFileData%OoPDefl, 0.0_ReKi ) ) THEN
                  CALL CheckError( ErrID_Warn, BadOoP )
               END IF  
            
               IF ( .NOT. EqualRealNos( A(2,1), 0.0_ReKi ) )   THEN
                  InitQF1(K) = A(2,3)/A(2,1)
               ELSE
                  InitQF1(K) = 0.0
            
                  IF ( .NOT. EqualRealNos( InputFileData%IPDefl,  0.0_ReKi ) )  THEN
                     CALL CheckError( ErrID_Warn, BadIP )
                  ENDIF          

               ENDIF
            ENDIF

         ELSE                                     ! It is impossible to find any "good" solution, so ignore the initial tip displacements

            InitQF1(K) = 0.0
            InitQE1(K) = 0.0

            IF ( ( InputFileData%OoPDefl /= 0.0 ) .OR. ( InputFileData%IPDefl /= 0.0 ) )  THEN
               CALL CheckError( ErrID_Warn, Ignore )
            ENDIF

         ENDIF

      ELSE                                        ! Blade flap mode 1 is not enabled.

         InitQF1(K) = 0.0

         IF ( InputFileData%FlapDOF2 )  THEN                    ! Blade flap mode 2 is enabled.

            A(1,1) =  p%TwistedSF(K,1,2,p%TipNode,0)*CosPitch + p%TwistedSF(K,2,2,p%TipNode,0)*SinPitch
            A(2,1) = -p%TwistedSF(K,1,2,p%TipNode,0)*SinPitch + p%TwistedSF(K,2,2,p%TipNode,0)*CosPitch

            DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

            IF ( .NOT. EqualRealNos( DET, 0.0_ReKi ) ) THEN      ! Apply all flap deflection to mode 2
               InitQF2 = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
               InitQE1 = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

            ELSEIF ( .NOT. InputFileData%EdgeDOF )  THEN          ! Blade edge mode 1 is not enabled which caused DET = 0.

               InitQE1(K) = 0.0

               IF ( .NOT. EqualRealNos( A(1,1), 0.0_ReKi ) )  THEN
                  IF ( .NOT. EqualRealNos( A(2,1), 0.0_ReKi ) )   THEN      ! Find a solution of the 2 equations in 1 variable that
                                                                            !  minimizes the sum of the squares of the equation's residuals
                     InitQF2(K) = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

                     TotResid = SQRT( ( A(1,1)*InitQF2(K) - A(1,3))**2 + ( A(2,1)*InitQF2(K) - A(2,3) )**2 )

                     IF ( .NOT. EqualRealNos( TotResid, 0.0_ReKi ) )  THEN
                        CALL CheckError( ErrID_Warn, Approx )
                     ENDIF
                  ELSE
                     InitQF2(K) = A(1,3)/A(1,1)

                     IF ( .NOT. EqualRealNos( InputFileData%IPDefl, 0.0_ReKi ) ) THEN
                        CALL CheckError( ErrID_Warn, BadIP )
                     ENDIF
                  ENDIF
               ELSE
                  IF ( .NOT. EqualRealNos( InputFileData%OoPDefl, 0.0_ReKi ) ) THEN
                     CALL CheckError( ErrID_Warn, BadOoP )
                  END IF       
            
                  IF ( .NOT. EqualRealNos( A(2,1), 0.0_ReKi ) )  THEN
                     InitQF2(K) = A(2,3)/A(2,1)               
                  ELSE
                     InitQF2(K) = 0.0

                     IF ( .NOT. EqualRealNos( InputFileData%IPDefl, 0.0_ReKi ) )  THEN
                        CALL CheckError( ErrID_Warn, BadIP )
                     ENDIF
               
                  ENDIF
               ENDIF

            ELSE                                  ! It is impossible to find any "good" solution, so ignore
                                                  ! the initial tip displacements.
               InitQF2(K) = 0.0
               InitQE1(K) = 0.0

               IF ( .NOT. EqualRealNos( InputFileData%OoPDefl,  0.0_ReKi ) .OR. &
                    .NOT. EqualRealNos( InputFileData%IPDefl,   0.0_ReKi ) )  THEN
                  CALL CheckError( ErrID_Warn, Ignore )               
                ENDIF
            ENDIF

         ELSE                                     ! Blade flap mode 2 is not enabled.

            InitQF2(K) = 0.0

            IF ( .NOT. EqualRealNos( A(1,2), 0.0_ReKi ) )  THEN

               IF ( .NOT. EqualRealNos( A(2,2), 0.0_ReKi ) )  THEN         ! Find a solution of the 2 equations in 1 variable that minimizes
                                                                           !  the sum of the squares of the equation's residuals.
                  InitQE1(K) = ( A(1,2)*A(1,3) + A(2,2)*A(2,3) )/( A(1,2)**2 + A(2,2)**2 )

                  TotResid = SQRT( ( A(1,2)*InitQE1(K) - A(1,3) )**2 + ( A(2,2)*InitQE1(K) - A(2,3) )**2)

                  IF ( .NOT. EqualRealNos( TotResid, 0.0_ReKi ) )  THEN
                     CALL CheckError( ErrID_Warn, Approx )               
                  ENDIF

               ELSE

                  InitQE1(K) = A(1,3)/A(1,2)

                  IF ( .NOT. EqualRealNos( InputFileData%IPDefl, 0.0_ReKi ) )  THEN
                     CALL CheckError( ErrID_Warn, BadIP )               
                  ENDIF

               ENDIF

            ELSE

               IF ( .NOT. EqualRealNos( InputFileData%OoPDefl, 0.0_ReKi ) ) THEN
                  CALL CheckError( ErrID_Warn, BadOoP )
               END IF 
                  
               IF ( .NOT. EqualRealNos( A(2,2), 0.0_ReKi ) )  THEN
                  InitQE1(K) = A(2,3)/A(2,2)
               ELSE
                  InitQE1(K) = 0.0             
            
                  IF ( .NOT. EqualRealNos( InputFileData%IPDefl,  0.0_ReKi ) )  THEN
                     CALL CheckError( ErrID_Warn, BadIP )
                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   END DO !K

   RETURN
CONTAINS   
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//' Blade '//TRIM(Num2LStr(K))// &
                     ' initial blade tip displacements are Incompat with enabled DOFs: '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
         END IF

      END IF


   END SUBROUTINE CheckError     
   
END SUBROUTINE InitBlDefl
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE SetEnabledDOFIndexArrays( p )
! This routine is used create arrays of DOF indices (pointers / (vector susbscript arrays) that contribute to the QD2T-related
!   linear accelerations of various points within the system in the inertia frame, based on which DOFs are presently enabled.
! NOTE: The order in which the DOFs are tested within this routine and hence the order in which the DOF indices appear in the
!       vector subscript arrays, determines the order in which the states will appear in the linearized model created by FAST
!       when AnalMode == 2.  This order is not necessarily sorted from smallest to largest DOF index.
! bjj: note that this routine is now called only in the initialization routine. It is not available during time simulation.
!----------------------------------------------------------------------------------------------------------------------------------  

   IMPLICIT                        NONE


      ! passed variables
   TYPE(ED_ParameterType), INTENT(INOUT)   :: p                                  ! Parameters of the structural dynamics module

      ! Local Variables:
   INTEGER(IntKi)                   :: I                                          ! Loops through all DOFs.
   INTEGER(IntKi)                   :: K                                          ! Loops through blades.



      ! Initialize total counts to zero.

   p%DOFs%NActvDOF = 0
   p%DOFs%NPCE     = 0
   p%DOFs%NPDE     = 0
   p%DOFs%NPIE     = 0
   p%DOFs%NPTTE    = 0
   p%DOFs%NPTE     = 0
   p%DOFs%NPSBE(:) = 0
   p%DOFs%NPSE (:) = 0
   p%DOFs%NPUE     = 0
   p%DOFs%NPYE     = 0


      ! Test each DOF and include the appropriate indices in the subscript arrays
      !  and total counts:

   IF ( p%DOF_Flag(DOF_Sg  ) )  THEN  ! Platform surge.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_Sg
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_Sg
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_Sg
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_Sg
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_Sg
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_Sg
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_Sg
      p%DOFs%PYE     (  p%DOFs%NPYE    ) = DOF_Sg

   ENDIF


   IF ( p%DOF_Flag(DOF_Sw  ) )  THEN  ! Platform sway.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

       p%DOFs%PS     (  p%DOFs%NActvDOF) = DOF_Sw
       p%DOFs%PCE    (  p%DOFs%NPCE    ) = DOF_Sw
       p%DOFs%PDE    (  p%DOFs%NPDE    ) = DOF_Sw
       p%DOFs%PIE    (  p%DOFs%NPIE    ) = DOF_Sw
       p%DOFs%PTE    (  p%DOFs%NPTE    ) = DOF_Sw
       p%DOFs%PSE    (:,p%DOFs%NPSE (:)) = DOF_Sw
       p%DOFs%PUE    (  p%DOFs%NPUE    ) = DOF_Sw
       p%DOFs%PYE    (  p%DOFs%NPYE    ) = DOF_Sw

   ENDIF


   IF ( p%DOF_Flag(DOF_Hv  ) )  THEN  ! Platform heave.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_Hv
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_Hv
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_Hv
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_Hv
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_Hv
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_Hv
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_Hv
      p%DOFs%PYE     (  p%DOFs%NPYE    ) = DOF_Hv

   ENDIF


   IF ( p%DOF_Flag(DOF_R   ) )  THEN  ! Platform roll.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_R
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_R
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_R
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_R
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_R
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_R
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_R
      p%DOFs%PYE     (  p%DOFs%NPYE    ) = DOF_R

   ENDIF


   IF ( p%DOF_Flag(DOF_P   ) )  THEN  ! Platform pitch.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_P
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_P
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_P
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_P
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_P
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_P
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_P
      p%DOFs%PYE     (  p%DOFs%NPYE    ) = DOF_P

   ENDIF


   IF ( p%DOF_Flag(DOF_Y   ) )  THEN  ! Platform yaw.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1
      p%DOFs%NPYE     = p%DOFs%NPYE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_Y
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_Y
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_Y
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_Y
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_Y
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_Y
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_Y
      p%DOFs%PYE     (  p%DOFs%NPYE    ) = DOF_Y

   ENDIF


   IF ( p%DOF_Flag(DOF_TFA1) )  THEN  ! 1st tower fore-aft.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTTE    = p%DOFs%NPTTE    + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_TFA1
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_TFA1
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_TFA1
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_TFA1
      p%DOFs%PTTE    (  p%DOFs%NPTTE   ) = DOF_TFA1
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_TFA1
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_TFA1
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_TFA1

   ENDIF


   IF ( p%DOF_Flag(DOF_TSS1) )  THEN  ! 1st tower side-to-side.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTTE    = p%DOFs%NPTTE    + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_TSS1
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_TSS1
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_TSS1
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_TSS1
      p%DOFs%PTTE    (  p%DOFs%NPTTE   ) = DOF_TSS1
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_TSS1
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_TSS1
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_TSS1

   ENDIF


   IF ( p%DOF_Flag(DOF_TFA2) )  THEN  ! 2nd tower fore-aft.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTTE    = p%DOFs%NPTTE    + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_TFA2
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_TFA2
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_TFA2
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_TFA2
      p%DOFs%PTTE    (  p%DOFs%NPTTE   ) = DOF_TFA2
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_TFA2
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_TFA2
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_TFA2

   ENDIF


   IF ( p%DOF_Flag(DOF_TSS2) )  THEN  ! 2nd tower side-to-side.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPTTE    = p%DOFs%NPTTE    + 1
      p%DOFs%NPTE     = p%DOFs%NPTE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_TSS2
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_TSS2
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_TSS2
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_TSS2
      p%DOFs%PTTE    (  p%DOFs%NPTTE   ) = DOF_TSS2
      p%DOFs%PTE     (  p%DOFs%NPTE    ) = DOF_TSS2
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_TSS2
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_TSS2

   ENDIF


   IF ( p%DOF_Flag(DOF_Yaw ) )  THEN  ! Nacelle yaw.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1
      p%DOFs%NPUE     = p%DOFs%NPUE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_Yaw
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_Yaw
      p%DOFs%PDE     (  p%DOFs%NPDE    ) = DOF_Yaw
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_Yaw
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_Yaw
      p%DOFs%PUE     (  p%DOFs%NPUE    ) = DOF_Yaw

   ENDIF


   IF ( p%DOF_Flag(DOF_TFrl) )  THEN  ! Tail-furl.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPIE     = p%DOFs%NPIE     + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_TFrl
      p%DOFs%PIE     (  p%DOFs%NPIE    ) = DOF_TFrl

   ENDIF


   IF ( p%DOF_Flag(DOF_RFrl) )  THEN  ! Rotor-furl.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPDE     = p%DOFs%NPDE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1

      p%DOFs%PS     (  p%DOFs%NActvDOF) = DOF_RFrl
      p%DOFs%PCE    (  p%DOFs%NPCE    ) = DOF_RFrl
      p%DOFs%PDE    (  p%DOFs%NPDE    ) = DOF_RFrl
      p%DOFs%PSE    (:,p%DOFs%NPSE (:)) = DOF_RFrl

   ENDIF


   IF ( p%DOF_Flag(DOF_GeAz) )  THEN  ! Generator azimuth.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_GeAz
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_GeAz
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_GeAz

   ENDIF


   IF ( p%DOF_Flag(DOF_DrTr) )  THEN  ! Drivetrain torsion.

      p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
      p%DOFs%NPCE     = p%DOFs%NPCE     + 1
      p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1

      p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_DrTr
      p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_DrTr
      p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_DrTr

   ENDIF


   IF ( p%NumBl == 2 )  THEN
      IF ( p%DOF_Flag(DOF_Teet   ) )  THEN  ! Rotor-teeter.

         p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
         p%DOFs%NPCE     = p%DOFs%NPCE     + 1
         p%DOFs%NPSE (:) = p%DOFs%NPSE (:) + 1

         p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_Teet
         p%DOFs%PCE     (  p%DOFs%NPCE    ) = DOF_Teet
         p%DOFs%PSE     (:,p%DOFs%NPSE (:)) = DOF_Teet

      ENDIF
   ENDIF


   DO K = 1,p%NumBl ! Loop through all blades
      IF ( p%DOF_Flag(DOF_BF(K,1)) )  THEN  ! 1st blade flap.

         p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
         p%DOFs%NPSBE(K) = p%DOFs%NPSBE(K) + 1
         p%DOFs%NPSE (K) = p%DOFs%NPSE (K) + 1

         p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_BF(K,1)
         p%DOFs%PSBE    (K,p%DOFs%NPSBE(K)) = DOF_BF(K,1)
         p%DOFs%PSE     (K,p%DOFs%NPSE (K)) = DOF_BF(K,1)

      ENDIF
   ENDDO          ! K - Blades


   DO K = 1,p%NumBl ! Loop through all blades
      IF ( p%DOF_Flag(DOF_BE(K,1)) )  THEN  ! 1st blade edge.

         p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
         p%DOFs%NPSBE(K) = p%DOFs%NPSBE(K) + 1
         p%DOFs%NPSE (K) = p%DOFs%NPSE (K) + 1

         p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_BE(K,1)
         p%DOFs%PSBE    (K,p%DOFs%NPSBE(K)) = DOF_BE(K,1)
         p%DOFs%PSE     (K,p%DOFs%NPSE (K)) = DOF_BE(K,1)

      ENDIF
   ENDDO          ! K - Blades


   DO K = 1,p%NumBl ! Loop through all blades
      IF ( p%DOF_Flag(DOF_BF(K,2)) )  THEN  ! 2nd blade flap.

         p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1
         p%DOFs%NPSBE(K) = p%DOFs%NPSBE(K) + 1
         p%DOFs%NPSE (K) = p%DOFs%NPSE (K) + 1

         p%DOFs%PS      (  p%DOFs%NActvDOF) = DOF_BF(K,2)
         p%DOFs%PSBE    (K,p%DOFs%NPSBE(K)) = DOF_BF(K,2)
         p%DOFs%PSE     (K,p%DOFs%NPSE (K)) = DOF_BF(K,2)

      ENDIF
   ENDDO          ! K - Blades



      ! Compute the sorted (from smallest to largest p%DOFs index) version of PS(),
      !   SrtPS(), and SrtPSNAUG().  At the same time compute Diag(), which is an
      !   array containing the indices of SrtPS() associated with each enabled
      !   DOF; that is, SrtPS(Diag(I)) = I:
      ! NOTE: This calculation is recomputing NActvDOF as computed above.  This is
      !       of no concern however, since the resulting value will be the same.

   p%DOFs%NActvDOF = 0
   DO I = 1,p%NDOF  ! Loop through all DOFs
      IF ( p%DOF_Flag(I) )  THEN   ! .TRUE. if the corresponding DOF is enabled

         p%DOFs%NActvDOF = p%DOFs%NActvDOF + 1

         p%DOFs%SrtPS     (p%DOFs%NActvDOF) = I
         p%DOFs%SrtPSNAUG (p%DOFs%NActvDOF) = I
         p%DOFs%Diag      (I           ) = p%DOFs%NActvDOF

      ENDIF
   ENDDO          ! I - All DOFs

   p%DOFs%SrtPSNAUG ( p%DOFs%NActvDOF + 1 ) = p%NAug


   RETURN
END SUBROUTINE SetEnabledDOFIndexArrays
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE SetCoordSy( t, CoordSys, RtHSdat, BlPitch, p, x, ErrStat, ErrMsg )


   ! This routine is used to define the internal coordinate systems for this particular time step.
   ! It also sets the TeeterAng and TeetAngVel for this time step.


   IMPLICIT                        NONE

      ! Subroutine arguments (passed variables)

   REAL(DbKi),                   INTENT(IN)    :: t                             ! Current simulation time, in seconds (used only for SmllRotTrans error messages)
   REAL(ReKi),                   INTENT(IN)    :: BlPitch (:)                   ! The current blade pitch                   
   TYPE(ED_CoordSys),            INTENT(INOUT) :: CoordSys                      ! The coordinate systems to be set
   TYPE(ED_RtHndSide),           INTENT(INOUT) :: RtHSdat                       ! data from the RtHndSid module
   TYPE(ED_ParameterType),       INTENT(IN)    :: p                             ! The module's parameters
   TYPE(ED_ContinuousStateType), INTENT(IN)    :: x                             ! The module's continuous states

   INTEGER(IntKi),               INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),                 INTENT(OUT)    :: ErrMsg                       ! Error message

      ! Local variables

   REAL(ReKi)                   :: CAzimuth                                        ! COS( rotor azimuth angle ).
   REAL(ReKi)                   :: CgRotAng                                        ! COS( gRotAng ).
   REAL(ReKi)                   :: CNacYaw                                         ! COS( nacelle yaw angle ).
   REAL(ReKi)                   :: CosPitch                                        ! COS( the current pitch angle ).
   REAL(ReKi)                   :: CPitPTwstA                                      ! COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
   REAL(ReKi)                   :: CPitPTwstS                                      ! COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
   REAL(ReKi)                   :: CRotFurl                                        ! COS( rotor-furl angle ).
   REAL(ReKi)                   :: CTailFurl                                       ! COS( tail-furl angle ).
   REAL(ReKi)                   :: CTeetAng                                        ! COS( TeetAng ).
   REAL(ReKi)                   :: g1Prime   (3)                                   ! = g1.
   REAL(ReKi)                   :: g2Prime   (3)                                   ! completes the right-handed gPrime-vector triad
   REAL(ReKi)                   :: g3Prime   (3)                                   ! = g3 rotated about g1 so that parallel to the pitching axis of blade K (i.e., the current blade in the blade loop).
   REAL(ReKi)                   :: gRotAng                                         ! Angle of rotation about g1 to get from the g to the gPrime system.
   REAL(ReKi)                   :: Lj1       (3)                                   ! vector / direction Lj1 at node J for blade K.
   REAL(ReKi)                   :: Lj2       (3)                                   ! vector / direction Lj2 at node J for blade K.
   REAL(ReKi)                   :: Lj3       (3)                                   ! vector / direction Lj3 at node J for blade K.
   REAL(ReKi)                   :: SAzimuth                                        ! SIN( rotor azimuth angle ).
   REAL(ReKi)                   :: SgRotAng                                        ! SIN( gRotAng ).
   REAL(ReKi)                   :: SinPitch                                        ! SIN( the current pitch angle ).
   REAL(ReKi)                   :: SNacYaw                                         ! SIN( nacelle yaw angle ).
   REAL(ReKi)                   :: SPitPTwstA                                      ! SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of sine.
   REAL(ReKi)                   :: SPitPTwstS                                      ! SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of sine.
   REAL(ReKi)                   :: SRotFurl                                        ! SIN( rotor-furl angle ).
   REAL(ReKi)                   :: STailFurl                                       ! SIN( tail-furl angle ).
   REAL(ReKi)                   :: STeetAng                                        ! SIN( TeetAng ).
   REAL(ReKi)                   :: ThetaFA                                         ! Tower fore-aft tilt deflection angle.
   REAL(ReKi)                   :: ThetaIP                                         ! Blade in-plane deflection angle at node J for blade K.
   REAL(ReKi)                   :: ThetaLxb                                        ! Blade deflection angle about the Lxb (n1) -axis at node J for blade K.
   REAL(ReKi)                   :: ThetaLyb                                        ! Blade deflection angle about the Lyb (n2) -axis at node J for blade K.
   REAL(ReKi)                   :: ThetaOoP                                        ! Blade out-of-plane deflection angle at node J for blade K.
   REAL(ReKi)                   :: ThetaSS                                         ! Tower side-to-side tilt deflection angle.
   REAL(ReKi)                   :: TransMat  (3,3)                                 ! The resulting transformation matrix due to three orthogonal rotations, (-).

   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.


   INTEGER(IntKi)               :: ErrStat2                      ! Temporary error status
   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                       ! Temporary error message

   
   ErrStat = ErrID_None
   ErrMsg  = ''
   
      ! Inertial frame coordinate system:

   CoordSys%z1 = (/ 1.0, 0.0, 0.0 /)   ! Vector / direction z1 (=  xi from the IEC coord. system).
   CoordSys%z2 = (/ 0.0, 1.0, 0.0 /)   ! Vector / direction z2 (=  zi from the IEC coord. system).
   CoordSys%z3 = (/ 0.0, 0.0, 1.0 /)   ! Vector / direction z3 (= -yi from the IEC coord. system).


      ! Tower base / platform coordinate system:

   CALL SmllRotTrans( 'platform displacement', x%QT(DOF_R), x%QT(DOF_Y), -x%QT(DOF_P), TransMat, TRIM(Num2LStr(t))//' s', ErrStat2, ErrMsg2 )  ! Get the transformation matrix, TransMat, from inertial frame to tower base / platform coordinate systems.
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF (ErrStat >= AbortErrLev) RETURN
   
   CoordSys%a1 = TransMat(1,1)*CoordSys%z1 + TransMat(1,2)*CoordSys%z2 + TransMat(1,3)*CoordSys%z3 ! Vector / direction a1 (=  xt from the IEC coord. system).
   CoordSys%a2 = TransMat(2,1)*CoordSys%z1 + TransMat(2,2)*CoordSys%z2 + TransMat(2,3)*CoordSys%z3 ! Vector / direction a2 (=  zt from the IEC coord. system).
   CoordSys%a3 = TransMat(3,1)*CoordSys%z1 + TransMat(3,2)*CoordSys%z2 + TransMat(3,3)*CoordSys%z3 ! Vector / direction a3 (= -yt from the IEC coord. system).


   DO J = 1,p%TwrNodes ! Loop through the tower nodes / elements


      ! Tower element-fixed coordinate system:

      ThetaFA = -p%TwrFASF(1,J       ,1)*x%QT(DOF_TFA1) - p%TwrFASF(2,J       ,1)*x%QT(DOF_TFA2)
      ThetaSS =  p%TwrSSSF(1,J       ,1)*x%QT(DOF_TSS1) + p%TwrSSSF(2,J       ,1)*x%QT(DOF_TSS2)

      CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat, TRIM(Num2LStr(t))//' s', ErrStat2, ErrMsg2 )   ! Get the transformation matrix, TransMat, from tower-base to tower element-fixed coordinate systems.
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF (ErrStat >= AbortErrLev) RETURN

      CoordSys%t1(J,:) = TransMat(1,1)*CoordSys%a1 + TransMat(1,2)*CoordSys%a2 + TransMat(1,3)*CoordSys%a3  ! Vector / direction t1 for tower node J (=  Lxt from the IEC coord. system).
      CoordSys%t2(J,:) = TransMat(2,1)*CoordSys%a1 + TransMat(2,2)*CoordSys%a2 + TransMat(2,3)*CoordSys%a3  ! Vector / direction t2 for tower node J (=  Lzt from the IEC coord. system).
      CoordSys%t3(J,:) = TransMat(3,1)*CoordSys%a1 + TransMat(3,2)*CoordSys%a2 + TransMat(3,3)*CoordSys%a3  ! Vector / direction t3 for tower node J (= -Lyt from the IEC coord. system).


   ENDDO ! J - Tower nodes / elements


      ! Tower-top / base plate coordinate system:

   ThetaFA    = -p%TwrFASF(1,p%TTopNode,1)*x%QT(DOF_TFA1) - p%TwrFASF(2,p%TTopNode,1)*x%QT(DOF_TFA2)
   ThetaSS    =  p%TwrSSSF(1,p%TTopNode,1)*x%QT(DOF_TSS1) + p%TwrSSSF(2,p%TTopNode,1)*x%QT(DOF_TSS2)

   CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat, TRIM(Num2LStr(t))//' s', ErrStat2, ErrMsg2 )   ! Get the transformation matrix, TransMat, from tower-base to tower-top/base-plate coordinate systems.
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF (ErrStat >= AbortErrLev) RETURN

   CoordSys%b1 = TransMat(1,1)*CoordSys%a1 + TransMat(1,2)*CoordSys%a2 + TransMat(1,3)*CoordSys%a3 ! Vector / direction b1 (=  xp from the IEC coord. system).
   CoordSys%b2 = TransMat(2,1)*CoordSys%a1 + TransMat(2,2)*CoordSys%a2 + TransMat(2,3)*CoordSys%a3 ! Vector / direction b2 (=  zp from the IEC coord. system).
   CoordSys%b3 = TransMat(3,1)*CoordSys%a1 + TransMat(3,2)*CoordSys%a2 + TransMat(3,3)*CoordSys%a3 ! Vector / direction b3 (= -yp from the IEC coord. system).


      ! Nacelle / yaw coordinate system:

   CNacYaw  = COS( x%QT(DOF_Yaw ) )
   SNacYaw  = SIN( x%QT(DOF_Yaw ) )

   CoordSys%d1 = CNacYaw*CoordSys%b1 - SNacYaw*CoordSys%b3     ! Vector / direction d1 (=  xn from the IEC coord. system).
   CoordSys%d2 = CoordSys%b2                                   ! Vector / direction d2 (=  zn from the IEC coord. system).
   CoordSys%d3 = SNacYaw*CoordSys%b1 + CNacYaw*CoordSys%b3     ! Vector / direction d3 (= -yn from the IEC coord. system).


      ! Rotor-furl coordinate system:

   CRotFurl = COS( x%QT(DOF_RFrl) )
   SRotFurl = SIN( x%QT(DOF_RFrl) )

   CoordSys%rf1 = ( (   1.0 - p%CRFrlSkw2*p%CRFrlTlt2 )*CRotFurl   + p%CRFrlSkw2*p%CRFrlTlt2          )*CoordSys%d1 &
                + ( p%CRFrlSkew*p%CSRFrlTlt*( 1.0 -     CRotFurl ) - p%SRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d2 &
                + ( p%CSRFrlSkw*p%CRFrlTlt2*( CRotFurl - 1.0     ) -             p%SRFrlTilt*SRotFurl )*CoordSys%d3
   CoordSys%rf2 = ( p%CRFrlSkew*p%CSRFrlTlt*( 1.0 -     CRotFurl ) + p%SRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d1 &
                + (             p%CRFrlTlt2*            CRotFurl   +             p%SRFrlTlt2          )*CoordSys%d2 &
                + ( p%SRFrlSkew*p%CSRFrlTlt*( CRotFurl - 1.0     ) + p%CRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d3
   CoordSys%rf3 = ( p%CSRFrlSkw*p%CRFrlTlt2*( CRotFurl - 1.0     ) +             p%SRFrlTilt*SRotFurl )*CoordSys%d1 &
                + ( p%SRFrlSkew*p%CSRFrlTlt*( CRotFurl - 1.0     ) - p%CRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d2 &
                + ( (   1.0 - p%SRFrlSkw2*p%CRFrlTlt2 )*CRotFurl   + p%SRFrlSkw2*p%CRFrlTlt2          )*CoordSys%d3
   CoordSys%rfa = p%CRFrlSkew*p%CRFrlTilt*CoordSys%d1 + p%SRFrlTilt*CoordSys%d2 - p%SRFrlSkew*p%CRFrlTilt*CoordSys%d3


      ! Shaft coordinate system:

   CoordSys%c1 =  p%CShftSkew*p%CShftTilt*CoordSys%rf1 + p%SShftTilt*CoordSys%rf2 - p%SShftSkew*p%CShftTilt*CoordSys%rf3  ! Vector / direction c1 (=  xs from the IEC coord. system).
   CoordSys%c2 = -p%CShftSkew*p%SShftTilt*CoordSys%rf1 + p%CShftTilt*CoordSys%rf2 + p%SShftSkew*p%SShftTilt*CoordSys%rf3  ! Vector / direction c2 (=  zs from the IEC coord. system).
   CoordSys%c3 =  p%SShftSkew*            CoordSys%rf1                            + p%CShftSkew*            CoordSys%rf3  ! Vector / direction c3 (= -ys from the IEC coord. system).


      ! Azimuth coordinate system:

   CAzimuth = COS( x%QT(DOF_DrTr) + x%QT(DOF_GeAz) )
   SAzimuth = SIN( x%QT(DOF_DrTr) + x%QT(DOF_GeAz) )

   CoordSys%e1 =  CoordSys%c1                                  ! Vector / direction e1 (=  xa from the IEC coord. system).
   CoordSys%e2 =  CAzimuth*CoordSys%c2 + SAzimuth*CoordSys%c3  ! Vector / direction e2 (=  ya from the IEC coord. system).
   CoordSys%e3 = -SAzimuth*CoordSys%c2 + CAzimuth*CoordSys%c3  ! Vector / direction e3 (=  za from the IEC coord. system).


      ! Teeter coordinate system:

      ! Lets define TeetAng, which is the current teeter angle (= QT(DOF_Teet) for
      !   2-blader or 0 for 3-blader) and is used in place of QT(DOF_Teet)
      !   throughout SUBROUTINE RtHS().  Doing it this way, we can run the same
      !   equations of motion for both the 2 and 3-blader configurations even
      !   though a 3-blader does not have a teetering DOF.

   IF ( p%NumBl == 2 )  THEN ! 2-blader
      RtHSdat%TeetAng    = x%QT (DOF_Teet)
      RtHSdat%TeetAngVel = x%QDT(DOF_Teet)
   ELSE                    ! 3-blader
      RtHSdat%TeetAng    = 0.0  ! Teeter is not an available DOF for a 3-blader
      RtHSdat%TeetAngVel = 0.0  ! Teeter is not an available DOF for a 3-blader
   ENDIF
   CTeetAng = COS( RtHSdat%TeetAng )
   STeetAng = SIN( RtHSdat%TeetAng )

   CoordSys%f1 = CTeetAng*CoordSys%e1 - STeetAng*CoordSys%e3       ! Vector / direction f1.
   CoordSys%f2 = CoordSys%e2                                       ! Vector / direction f2.
   CoordSys%f3 = STeetAng*CoordSys%e1 + CTeetAng*CoordSys%e3       ! Vector / direction f3.


      ! Hub / delta-3 coordinate system:

   CoordSys%g1 =  CoordSys%f1                                      ! Vector / direction g1 (=  xh from the IEC coord. system).
   CoordSys%g2 =  p%CosDel3*CoordSys%f2 + p%SinDel3*CoordSys%f3    ! Vector / direction g2 (=  yh from the IEC coord. system).
   CoordSys%g3 = -p%SinDel3*CoordSys%f2 + p%CosDel3*CoordSys%f3    ! Vector / direction g3 (=  zh from the IEC coord. system).


   DO K = 1,p%NumBl ! Loop through all blades


      ! Hub (Prime) coordinate system rotated to match blade K.

       gRotAng = p%TwoPiNB*(K-1)
      CgRotAng = COS( gRotAng )
      SgRotAng = SIN( gRotAng )

      g1Prime =  CoordSys%g1
      g2Prime =  CgRotAng*CoordSys%g2 + SgRotAng*CoordSys%g3
      g3Prime = -SgRotAng*CoordSys%g2 + CgRotAng*CoordSys%g3


      ! Coned coordinate system:

      CoordSys%i1(K,:) = p%CosPreC(K)*g1Prime - p%SinPreC(K)*g3Prime  ! i1(K,:) = vector / direction i1 for blade K (=  xcK from the IEC coord. system).
      CoordSys%i2(K,:) = g2Prime                                      ! i2(K,:) = vector / direction i2 for blade K (=  ycK from the IEC coord. system).
      CoordSys%i3(K,:) = p%SinPreC(K)*g1Prime + p%CosPreC(K)*g3Prime  ! i3(K,:) = vector / direction i3 for blade K (=  zcK from the IEC coord. system).


      ! Blade / pitched coordinate system:

      CosPitch = COS( BlPitch(K) )
      SinPitch = SIN( BlPitch(K) )

      CoordSys%j1(K,:) = CosPitch*CoordSys%i1(K,:) - SinPitch*CoordSys%i2(K,:)      ! j1(K,:) = vector / direction j1 for blade K (=  xbK from the IEC coord. system).
      CoordSys%j2(K,:) = SinPitch*CoordSys%i1(K,:) + CosPitch*CoordSys%i2(K,:)      ! j2(K,:) = vector / direction j2 for blade K (=  ybK from the IEC coord. system).
      CoordSys%j3(K,:) = CoordSys%i3(K,:)                                           ! j3(K,:) = vector / direction j3 for blade K (=  zbK from the IEC coord. system).


   !JASON: USE TipNode HERE INSTEAD OF p%BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements


      ! Blade coordinate system aligned with local structural axes (not element fixed):

         Lj1 = p%CThetaS(K,J)*CoordSys%j1(K,:) - p%SThetaS(K,J)*CoordSys%j2(K,:)  ! vector / direction Lj1 at node J for blade K
         Lj2 = p%SThetaS(K,J)*CoordSys%j1(K,:) + p%CThetaS(K,J)*CoordSys%j2(K,:)  ! vector / direction Lj2 at node J for blade K
         Lj3 = CoordSys%j3(K,:)                                               ! vector / direction Lj3 at node J for blade K


      ! Blade element-fixed coordinate system aligned with local structural axes:

         ThetaOoP =   p%TwistedSF(K,1,1,J,1)*x%QT( DOF_BF(K,1) ) &
                    + p%TwistedSF(K,1,2,J,1)*x%QT( DOF_BF(K,2) ) &
                    + p%TwistedSF(K,1,3,J,1)*x%QT( DOF_BE(K,1) )
         ThetaIP  = - p%TwistedSF(K,2,1,J,1)*x%QT( DOF_BF(K,1) ) &
                    - p%TwistedSF(K,2,2,J,1)*x%QT( DOF_BF(K,2) ) &
                    - p%TwistedSF(K,2,3,J,1)*x%QT( DOF_BE(K,1) )

         ThetaLxb = p%CThetaS(K,J)*ThetaIP - p%SThetaS(K,J)*ThetaOoP
         ThetaLyb = p%SThetaS(K,J)*ThetaIP + p%CThetaS(K,J)*ThetaOoP

         CALL SmllRotTrans( 'blade deflection', ThetaLxb, ThetaLyb, 0.0, TransMat, TRIM(Num2LStr(t))//' s', ErrStat2, ErrMsg2 ) ! Get the transformation matrix, TransMat, from blade coordinate system aligned with local structural axes (not element fixed) to blade element-fixed coordinate system aligned with local structural axes.
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN

         CoordSys%n1(K,J,:) = TransMat(1,1)*Lj1 + TransMat(1,2)*Lj2 + TransMat(1,3)*Lj3   ! Vector / direction n1 for node J of blade K (= LxbK from the IEC coord. system).
         CoordSys%n2(K,J,:) = TransMat(2,1)*Lj1 + TransMat(2,2)*Lj2 + TransMat(2,3)*Lj3   ! Vector / direction n2 for node J of blade K (= LybK from the IEC coord. system).
         CoordSys%n3(K,J,:) = TransMat(3,1)*Lj1 + TransMat(3,2)*Lj2 + TransMat(3,3)*Lj3   ! Vector / direction n3 for node J of blade K (= LzbK from the IEC coord. system).


      ! Blade element-fixed coordinate system used for calculating and returning
      !    aerodynamics loads:
      ! This coordinate system is rotated about positive n3 by the angle
      !    BlPitch(K) + ThetaS(K,J) and is coincident with the i-vector triad
      !    when the blade is undeflected.

         CPitPTwstS = CosPitch*p%CThetaS(K,J) - SinPitch*p%SThetaS(K,J)  ! = COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
         SPitPTwstS = CosPitch*p%SThetaS(K,J) + SinPitch*p%CThetaS(K,J)  ! = SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of   sine.

         CoordSys%m1(K,J,:)  =  CPitPTwstS*CoordSys%n1(K,J,:) + SPitPTwstS*CoordSys%n2(K,J,:)   ! m1(K,J,:) = vector / direction m1 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
         CoordSys%m2(K,J,:)  = -SPitPTwstS*CoordSys%n1(K,J,:) + CPitPTwstS*CoordSys%n2(K,J,:)   ! m2(K,J,:) = vector / direction m2 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
         CoordSys%m3(K,J,:)  =  CoordSys%n3(K,J,:)                                              ! m3(K,J,:) = vector / direction m3 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).


      ! Calculate the trailing edge coordinate system used in noise calculations.
      ! This coordinate system is blade element-fixed and oriented with the local
      !   aerodynamic axes (te2 points toward trailing edge, te1 points toward
      !   suction surface):

         CPitPTwstA = CosPitch*p%CAeroTwst(J) - SinPitch*p%SAeroTwst(J)  ! = COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
         SPitPTwstA = CosPitch*p%SAeroTwst(J) + SinPitch*p%CAeroTwst(J)  ! = SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of   sine.

         CoordSys%te1(K,J,:) =  CPitPTwstA*CoordSys%m1(K,J,:) - SPitPTwstA*CoordSys%m2(K,J,:)   ! te1(K,J,:) = vector / direction te1 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
         CoordSys%te2(K,J,:) =  SPitPTwstA*CoordSys%m1(K,J,:) + CPitPTwstA*CoordSys%m2(K,J,:)   ! te2(K,J,:) = vector / direction te2 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
         CoordSys%te3(K,J,:) =  CoordSys%m3(K,J,:)                                              ! te3(K,J,:) = vector / direction te3 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).


      ENDDO ! J - Blade nodes / elements


   ENDDO ! K - Blades


      ! Tail-furl coordinate system:

   CTailFurl = COS( x%QT(DOF_TFrl) )
   STailFurl = SIN( x%QT(DOF_TFrl) )

   CoordSys%tf1 = ( ( 1.0 - p%CTFrlSkw2*p%CTFrlTlt2 )*CTailFurl  + p%CTFrlSkw2*p%CTFrlTlt2           )*CoordSys%d1 &
                + ( p%CTFrlSkew*p%CSTFrlTlt*(  1.0 - CTailFurl ) - p%STFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d2 &
                + ( p%CSTFrlSkw*p%CTFrlTlt2*( CTailFurl - 1.0  ) -             p%STFrlTilt*STailFurl )*CoordSys%d3
   CoordSys%tf2 = ( p%CTFrlSkew*p%CSTFrlTlt*(  1.0 - CTailFurl ) + p%STFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d1 &
                + (             p%CTFrlTlt2*         CTailFurl +               p%STFrlTlt2           )*CoordSys%d2 &
                + ( p%STFrlSkew*p%CSTFrlTlt*( CTailFurl - 1.0  ) + p%CTFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d3
   CoordSys%tf3 = ( p%CSTFrlSkw*p%CTFrlTlt2*( CTailFurl - 1.0  ) +             p%STFrlTilt*STailFurl )*CoordSys%d1 &
                + ( p%STFrlSkew*p%CSTFrlTlt*( CTailFurl - 1.0  ) - p%CTFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d2 &
                + ( ( 1.0 - p%STFrlSkw2*p%CTFrlTlt2 )*CTailFurl  + p%STFrlSkw2*p%CTFrlTlt2           )*CoordSys%d3
   CoordSys%tfa = p%CTFrlSkew*p%CTFrlTilt*CoordSys%d1 + p%STFrlTilt*CoordSys%d2 - p%STFrlSkew*p%CTFrlTilt*CoordSys%d3


      ! Tail fin coordinate system:

   CoordSys%p1 = (                           p%CTFinSkew*p%CTFinTilt             )*CoordSys%tf1 &   ! Vector / direction p1 (= tail fin  x).
               + (                                       p%STFinTilt             )*CoordSys%tf2 &
               + (                         - p%STFinSkew*p%CTFinTilt             )*CoordSys%tf3
   CoordSys%p2 = ( p%STFinSkew*p%STFinBank - p%CTFinSkew*p%STFinTilt*p%CTFinBank )*CoordSys%tf1 &   ! Vector / direction p2 (= tail fin  z).
               + (                                       p%CTFinTilt*p%CTFinBank )*CoordSys%tf2 &
               + ( p%CTFinSkew*p%STFinBank + p%STFinSkew*p%STFinTilt*p%CTFinBank )*CoordSys%tf3
   CoordSys%p3 = ( p%STFinSkew*p%CTFinBank + p%CTFinSkew*p%STFinTilt*p%STFinBank )*CoordSys%tf1 &   ! Vector / direction p3 (= tail fin -y).
               + (                         -             p%CTFinTilt*p%STFinBank )*CoordSys%tf2 &
               + ( p%CTFinSkew*p%CTFinBank - p%STFinSkew*p%STFinTilt*p%STFinBank )*CoordSys%tf3

   RETURN
CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)


      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         ErrMsg = TRIM(ErrMsg)//NewLine//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
         END IF

      END IF


   END SUBROUTINE CheckError
END SUBROUTINE SetCoordSy
!----------------------------------------------------------------------------------------------------------------------------------  
END MODULE ElastoDyn
!**********************************************************************************************************************************

