
MODULE StructDyn_Parameters

      ! This module contains definitions of compile-time PARAMETERS for the StrucyDyn module.
      ! Every variable defined here MUST have the PARAMETER attribute.


   USE NWTC_Library


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


      ! Parameters related to output length -- Possibly a local variable elsewhere????
   INTEGER(IntKi), PARAMETER        :: OutStrLen  = 10                                  ! number of characters allowed in the output data headers
   INTEGER(IntKi), PARAMETER        :: OutStrLenM = OutStrLen-1                         ! number of characters allowed in the output data headers, excluding a minus sign or "M"

   
     ! Parameters:


     ! Indices for computing output channels:
     ! NOTES: 
     !    (1) These parameters are in the order stored in "OutListParameters.xlsx"
     !    (2) Array AllOuts() must be dimensioned to the value of the largest output parameter
     !    (3) If an index (MaxOutPts) ever becomes greater or equal to 1000, the logic to create ARRAY/1 in the FAST-to-ADAMS preprocessor will have to be changed.

     !  Time: 

   INTEGER, PARAMETER             :: Time      =    0


  ! Wind Motions:

   INTEGER, PARAMETER             :: WindVxi   =    1
   INTEGER, PARAMETER             :: WindVyi   =    2
   INTEGER, PARAMETER             :: WindVzi   =    3
   INTEGER, PARAMETER             :: TotWindV  =    4
   INTEGER, PARAMETER             :: HorWindV  =    5
   INTEGER, PARAMETER             :: HorWndDir =    6
   INTEGER, PARAMETER             :: VerWndDir =    7


  ! Blade 1 Tip Motions:

   INTEGER, PARAMETER             :: TipDxc1   =    8
   INTEGER, PARAMETER             :: TipDyc1   =    9
   INTEGER, PARAMETER             :: TipDzc1   =   10
   INTEGER, PARAMETER             :: TipDxb1   =   11
   INTEGER, PARAMETER             :: TipDyb1   =   12
   INTEGER, PARAMETER             :: TipALxb1  =   13
   INTEGER, PARAMETER             :: TipALyb1  =   14
   INTEGER, PARAMETER             :: TipALzb1  =   15
   INTEGER, PARAMETER             :: TipRDxb1  =   16
   INTEGER, PARAMETER             :: TipRDyb1  =   17
   INTEGER, PARAMETER             :: TipRDzc1  =   18
   INTEGER, PARAMETER             :: TipClrnc1 =   19


  ! Blade 2 Tip Motions:

   INTEGER, PARAMETER             :: TipDxc2   =   20
   INTEGER, PARAMETER             :: TipDyc2   =   21
   INTEGER, PARAMETER             :: TipDzc2   =   22
   INTEGER, PARAMETER             :: TipDxb2   =   23
   INTEGER, PARAMETER             :: TipDyb2   =   24
   INTEGER, PARAMETER             :: TipALxb2  =   25
   INTEGER, PARAMETER             :: TipALyb2  =   26
   INTEGER, PARAMETER             :: TipALzb2  =   27
   INTEGER, PARAMETER             :: TipRDxb2  =   28
   INTEGER, PARAMETER             :: TipRDyb2  =   29
   INTEGER, PARAMETER             :: TipRDzc2  =   30
   INTEGER, PARAMETER             :: TipClrnc2 =   31


  ! Blade 3 Tip Motions:

   INTEGER, PARAMETER             :: TipDxc3   =   32
   INTEGER, PARAMETER             :: TipDyc3   =   33
   INTEGER, PARAMETER             :: TipDzc3   =   34
   INTEGER, PARAMETER             :: TipDxb3   =   35
   INTEGER, PARAMETER             :: TipDyb3   =   36
   INTEGER, PARAMETER             :: TipALxb3  =   37
   INTEGER, PARAMETER             :: TipALyb3  =   38
   INTEGER, PARAMETER             :: TipALzb3  =   39
   INTEGER, PARAMETER             :: TipRDxb3  =   40
   INTEGER, PARAMETER             :: TipRDyb3  =   41
   INTEGER, PARAMETER             :: TipRDzc3  =   42
   INTEGER, PARAMETER             :: TipClrnc3 =   43


  ! Blade 1 Local Span Motions:

   INTEGER, PARAMETER             :: Spn1ALxb1 =   44
   INTEGER, PARAMETER             :: Spn1ALyb1 =   45
   INTEGER, PARAMETER             :: Spn1ALzb1 =   46
   INTEGER, PARAMETER             :: Spn2ALxb1 =   47
   INTEGER, PARAMETER             :: Spn2ALyb1 =   48
   INTEGER, PARAMETER             :: Spn2ALzb1 =   49
   INTEGER, PARAMETER             :: Spn3ALxb1 =   50
   INTEGER, PARAMETER             :: Spn3ALyb1 =   51
   INTEGER, PARAMETER             :: Spn3ALzb1 =   52
   INTEGER, PARAMETER             :: Spn4ALxb1 =   53
   INTEGER, PARAMETER             :: Spn4ALyb1 =   54
   INTEGER, PARAMETER             :: Spn4ALzb1 =   55
   INTEGER, PARAMETER             :: Spn5ALxb1 =   56
   INTEGER, PARAMETER             :: Spn5ALyb1 =   57
   INTEGER, PARAMETER             :: Spn5ALzb1 =   58
   INTEGER, PARAMETER             :: Spn6ALxb1 =   59
   INTEGER, PARAMETER             :: Spn6ALyb1 =   60
   INTEGER, PARAMETER             :: Spn6ALzb1 =   61
   INTEGER, PARAMETER             :: Spn7ALxb1 =   62
   INTEGER, PARAMETER             :: Spn7ALyb1 =   63
   INTEGER, PARAMETER             :: Spn7ALzb1 =   64
   INTEGER, PARAMETER             :: Spn8ALxb1 =   65
   INTEGER, PARAMETER             :: Spn8ALyb1 =   66
   INTEGER, PARAMETER             :: Spn8ALzb1 =   67
   INTEGER, PARAMETER             :: Spn9ALxb1 =   68
   INTEGER, PARAMETER             :: Spn9ALyb1 =   69
   INTEGER, PARAMETER             :: Spn9ALzb1 =   70
   INTEGER, PARAMETER             :: Spn1TDxb1 =   71
   INTEGER, PARAMETER             :: Spn1TDyb1 =   72
   INTEGER, PARAMETER             :: Spn1TDzb1 =   73
   INTEGER, PARAMETER             :: Spn2TDxb1 =   74
   INTEGER, PARAMETER             :: Spn2TDyb1 =   75
   INTEGER, PARAMETER             :: Spn2TDzb1 =   76
   INTEGER, PARAMETER             :: Spn3TDxb1 =   77
   INTEGER, PARAMETER             :: Spn3TDyb1 =   78
   INTEGER, PARAMETER             :: Spn3TDzb1 =   79
   INTEGER, PARAMETER             :: Spn4TDxb1 =   80
   INTEGER, PARAMETER             :: Spn4TDyb1 =   81
   INTEGER, PARAMETER             :: Spn4TDzb1 =   82
   INTEGER, PARAMETER             :: Spn5TDxb1 =   83
   INTEGER, PARAMETER             :: Spn5TDyb1 =   84
   INTEGER, PARAMETER             :: Spn5TDzb1 =   85
   INTEGER, PARAMETER             :: Spn6TDxb1 =   86
   INTEGER, PARAMETER             :: Spn6TDyb1 =   87
   INTEGER, PARAMETER             :: Spn6TDzb1 =   88
   INTEGER, PARAMETER             :: Spn7TDxb1 =   89
   INTEGER, PARAMETER             :: Spn7TDyb1 =   90
   INTEGER, PARAMETER             :: Spn7TDzb1 =   91
   INTEGER, PARAMETER             :: Spn8TDxb1 =   92
   INTEGER, PARAMETER             :: Spn8TDyb1 =   93
   INTEGER, PARAMETER             :: Spn8TDzb1 =   94
   INTEGER, PARAMETER             :: Spn9TDxb1 =   95
   INTEGER, PARAMETER             :: Spn9TDyb1 =   96
   INTEGER, PARAMETER             :: Spn9TDzb1 =   97
   INTEGER, PARAMETER             :: Spn1RDxb1 =   98
   INTEGER, PARAMETER             :: Spn1RDyb1 =   99
   INTEGER, PARAMETER             :: Spn1RDzb1 =  100
   INTEGER, PARAMETER             :: Spn2RDxb1 =  101
   INTEGER, PARAMETER             :: Spn2RDyb1 =  102
   INTEGER, PARAMETER             :: Spn2RDzb1 =  103
   INTEGER, PARAMETER             :: Spn3RDxb1 =  104
   INTEGER, PARAMETER             :: Spn3RDyb1 =  105
   INTEGER, PARAMETER             :: Spn3RDzb1 =  106
   INTEGER, PARAMETER             :: Spn4RDxb1 =  107
   INTEGER, PARAMETER             :: Spn4RDyb1 =  108
   INTEGER, PARAMETER             :: Spn4RDzb1 =  109
   INTEGER, PARAMETER             :: Spn5RDxb1 =  110
   INTEGER, PARAMETER             :: Spn5RDyb1 =  111
   INTEGER, PARAMETER             :: Spn5RDzb1 =  112
   INTEGER, PARAMETER             :: Spn6RDxb1 =  113
   INTEGER, PARAMETER             :: Spn6RDyb1 =  114
   INTEGER, PARAMETER             :: Spn6RDzb1 =  115
   INTEGER, PARAMETER             :: Spn7RDxb1 =  116
   INTEGER, PARAMETER             :: Spn7RDyb1 =  117
   INTEGER, PARAMETER             :: Spn7RDzb1 =  118
   INTEGER, PARAMETER             :: Spn8RDxb1 =  119
   INTEGER, PARAMETER             :: Spn8RDyb1 =  120
   INTEGER, PARAMETER             :: Spn8RDzb1 =  121
   INTEGER, PARAMETER             :: Spn9RDxb1 =  122
   INTEGER, PARAMETER             :: Spn9RDyb1 =  123
   INTEGER, PARAMETER             :: Spn9RDzb1 =  124


  ! Blade 2 Local Span Motions:

   INTEGER, PARAMETER             :: Spn1ALxb2 =  125
   INTEGER, PARAMETER             :: Spn1ALyb2 =  126
   INTEGER, PARAMETER             :: Spn1ALzb2 =  127
   INTEGER, PARAMETER             :: Spn2ALxb2 =  128
   INTEGER, PARAMETER             :: Spn2ALyb2 =  129
   INTEGER, PARAMETER             :: Spn2ALzb2 =  130
   INTEGER, PARAMETER             :: Spn3ALxb2 =  131
   INTEGER, PARAMETER             :: Spn3ALyb2 =  132
   INTEGER, PARAMETER             :: Spn3ALzb2 =  133
   INTEGER, PARAMETER             :: Spn4ALxb2 =  134
   INTEGER, PARAMETER             :: Spn4ALyb2 =  135
   INTEGER, PARAMETER             :: Spn4ALzb2 =  136
   INTEGER, PARAMETER             :: Spn5ALxb2 =  137
   INTEGER, PARAMETER             :: Spn5ALyb2 =  138
   INTEGER, PARAMETER             :: Spn5ALzb2 =  139
   INTEGER, PARAMETER             :: Spn6ALxb2 =  140
   INTEGER, PARAMETER             :: Spn6ALyb2 =  141
   INTEGER, PARAMETER             :: Spn6ALzb2 =  142
   INTEGER, PARAMETER             :: Spn7ALxb2 =  143
   INTEGER, PARAMETER             :: Spn7ALyb2 =  144
   INTEGER, PARAMETER             :: Spn7ALzb2 =  145
   INTEGER, PARAMETER             :: Spn8ALxb2 =  146
   INTEGER, PARAMETER             :: Spn8ALyb2 =  147
   INTEGER, PARAMETER             :: Spn8ALzb2 =  148
   INTEGER, PARAMETER             :: Spn9ALxb2 =  149
   INTEGER, PARAMETER             :: Spn9ALyb2 =  150
   INTEGER, PARAMETER             :: Spn9ALzb2 =  151
   INTEGER, PARAMETER             :: Spn1TDxb2 =  152
   INTEGER, PARAMETER             :: Spn1TDyb2 =  153
   INTEGER, PARAMETER             :: Spn1TDzb2 =  154
   INTEGER, PARAMETER             :: Spn2TDxb2 =  155
   INTEGER, PARAMETER             :: Spn2TDyb2 =  156
   INTEGER, PARAMETER             :: Spn2TDzb2 =  157
   INTEGER, PARAMETER             :: Spn3TDxb2 =  158
   INTEGER, PARAMETER             :: Spn3TDyb2 =  159
   INTEGER, PARAMETER             :: Spn3TDzb2 =  160
   INTEGER, PARAMETER             :: Spn4TDxb2 =  161
   INTEGER, PARAMETER             :: Spn4TDyb2 =  162
   INTEGER, PARAMETER             :: Spn4TDzb2 =  163
   INTEGER, PARAMETER             :: Spn5TDxb2 =  164
   INTEGER, PARAMETER             :: Spn5TDyb2 =  165
   INTEGER, PARAMETER             :: Spn5TDzb2 =  166
   INTEGER, PARAMETER             :: Spn6TDxb2 =  167
   INTEGER, PARAMETER             :: Spn6TDyb2 =  168
   INTEGER, PARAMETER             :: Spn6TDzb2 =  169
   INTEGER, PARAMETER             :: Spn7TDxb2 =  170
   INTEGER, PARAMETER             :: Spn7TDyb2 =  171
   INTEGER, PARAMETER             :: Spn7TDzb2 =  172
   INTEGER, PARAMETER             :: Spn8TDxb2 =  173
   INTEGER, PARAMETER             :: Spn8TDyb2 =  174
   INTEGER, PARAMETER             :: Spn8TDzb2 =  175
   INTEGER, PARAMETER             :: Spn9TDxb2 =  176
   INTEGER, PARAMETER             :: Spn9TDyb2 =  177
   INTEGER, PARAMETER             :: Spn9TDzb2 =  178
   INTEGER, PARAMETER             :: Spn1RDxb2 =  179
   INTEGER, PARAMETER             :: Spn1RDyb2 =  180
   INTEGER, PARAMETER             :: Spn1RDzb2 =  181
   INTEGER, PARAMETER             :: Spn2RDxb2 =  182
   INTEGER, PARAMETER             :: Spn2RDyb2 =  183
   INTEGER, PARAMETER             :: Spn2RDzb2 =  184
   INTEGER, PARAMETER             :: Spn3RDxb2 =  185
   INTEGER, PARAMETER             :: Spn3RDyb2 =  186
   INTEGER, PARAMETER             :: Spn3RDzb2 =  187
   INTEGER, PARAMETER             :: Spn4RDxb2 =  188
   INTEGER, PARAMETER             :: Spn4RDyb2 =  189
   INTEGER, PARAMETER             :: Spn4RDzb2 =  190
   INTEGER, PARAMETER             :: Spn5RDxb2 =  191
   INTEGER, PARAMETER             :: Spn5RDyb2 =  192
   INTEGER, PARAMETER             :: Spn5RDzb2 =  193
   INTEGER, PARAMETER             :: Spn6RDxb2 =  194
   INTEGER, PARAMETER             :: Spn6RDyb2 =  195
   INTEGER, PARAMETER             :: Spn6RDzb2 =  196
   INTEGER, PARAMETER             :: Spn7RDxb2 =  197
   INTEGER, PARAMETER             :: Spn7RDyb2 =  198
   INTEGER, PARAMETER             :: Spn7RDzb2 =  199
   INTEGER, PARAMETER             :: Spn8RDxb2 =  200
   INTEGER, PARAMETER             :: Spn8RDyb2 =  201
   INTEGER, PARAMETER             :: Spn8RDzb2 =  202
   INTEGER, PARAMETER             :: Spn9RDxb2 =  203
   INTEGER, PARAMETER             :: Spn9RDyb2 =  204
   INTEGER, PARAMETER             :: Spn9RDzb2 =  205


  ! Blade 3 Local Span Motions:

   INTEGER, PARAMETER             :: Spn1ALxb3 =  206
   INTEGER, PARAMETER             :: Spn1ALyb3 =  207
   INTEGER, PARAMETER             :: Spn1ALzb3 =  208
   INTEGER, PARAMETER             :: Spn2ALxb3 =  209
   INTEGER, PARAMETER             :: Spn2ALyb3 =  210
   INTEGER, PARAMETER             :: Spn2ALzb3 =  211
   INTEGER, PARAMETER             :: Spn3ALxb3 =  212
   INTEGER, PARAMETER             :: Spn3ALyb3 =  213
   INTEGER, PARAMETER             :: Spn3ALzb3 =  214
   INTEGER, PARAMETER             :: Spn4ALxb3 =  215
   INTEGER, PARAMETER             :: Spn4ALyb3 =  216
   INTEGER, PARAMETER             :: Spn4ALzb3 =  217
   INTEGER, PARAMETER             :: Spn5ALxb3 =  218
   INTEGER, PARAMETER             :: Spn5ALyb3 =  219
   INTEGER, PARAMETER             :: Spn5ALzb3 =  220
   INTEGER, PARAMETER             :: Spn6ALxb3 =  221
   INTEGER, PARAMETER             :: Spn6ALyb3 =  222
   INTEGER, PARAMETER             :: Spn6ALzb3 =  223
   INTEGER, PARAMETER             :: Spn7ALxb3 =  224
   INTEGER, PARAMETER             :: Spn7ALyb3 =  225
   INTEGER, PARAMETER             :: Spn7ALzb3 =  226
   INTEGER, PARAMETER             :: Spn8ALxb3 =  227
   INTEGER, PARAMETER             :: Spn8ALyb3 =  228
   INTEGER, PARAMETER             :: Spn8ALzb3 =  229
   INTEGER, PARAMETER             :: Spn9ALxb3 =  230
   INTEGER, PARAMETER             :: Spn9ALyb3 =  231
   INTEGER, PARAMETER             :: Spn9ALzb3 =  232
   INTEGER, PARAMETER             :: Spn1TDxb3 =  233
   INTEGER, PARAMETER             :: Spn1TDyb3 =  234
   INTEGER, PARAMETER             :: Spn1TDzb3 =  235
   INTEGER, PARAMETER             :: Spn2TDxb3 =  236
   INTEGER, PARAMETER             :: Spn2TDyb3 =  237
   INTEGER, PARAMETER             :: Spn2TDzb3 =  238
   INTEGER, PARAMETER             :: Spn3TDxb3 =  239
   INTEGER, PARAMETER             :: Spn3TDyb3 =  240
   INTEGER, PARAMETER             :: Spn3TDzb3 =  241
   INTEGER, PARAMETER             :: Spn4TDxb3 =  242
   INTEGER, PARAMETER             :: Spn4TDyb3 =  243
   INTEGER, PARAMETER             :: Spn4TDzb3 =  244
   INTEGER, PARAMETER             :: Spn5TDxb3 =  245
   INTEGER, PARAMETER             :: Spn5TDyb3 =  246
   INTEGER, PARAMETER             :: Spn5TDzb3 =  247
   INTEGER, PARAMETER             :: Spn6TDxb3 =  248
   INTEGER, PARAMETER             :: Spn6TDyb3 =  249
   INTEGER, PARAMETER             :: Spn6TDzb3 =  250
   INTEGER, PARAMETER             :: Spn7TDxb3 =  251
   INTEGER, PARAMETER             :: Spn7TDyb3 =  252
   INTEGER, PARAMETER             :: Spn7TDzb3 =  253
   INTEGER, PARAMETER             :: Spn8TDxb3 =  254
   INTEGER, PARAMETER             :: Spn8TDyb3 =  255
   INTEGER, PARAMETER             :: Spn8TDzb3 =  256
   INTEGER, PARAMETER             :: Spn9TDxb3 =  257
   INTEGER, PARAMETER             :: Spn9TDyb3 =  258
   INTEGER, PARAMETER             :: Spn9TDzb3 =  259
   INTEGER, PARAMETER             :: Spn1RDxb3 =  260
   INTEGER, PARAMETER             :: Spn1RDyb3 =  261
   INTEGER, PARAMETER             :: Spn1RDzb3 =  262
   INTEGER, PARAMETER             :: Spn2RDxb3 =  263
   INTEGER, PARAMETER             :: Spn2RDyb3 =  264
   INTEGER, PARAMETER             :: Spn2RDzb3 =  265
   INTEGER, PARAMETER             :: Spn3RDxb3 =  266
   INTEGER, PARAMETER             :: Spn3RDyb3 =  267
   INTEGER, PARAMETER             :: Spn3RDzb3 =  268
   INTEGER, PARAMETER             :: Spn4RDxb3 =  269
   INTEGER, PARAMETER             :: Spn4RDyb3 =  270
   INTEGER, PARAMETER             :: Spn4RDzb3 =  271
   INTEGER, PARAMETER             :: Spn5RDxb3 =  272
   INTEGER, PARAMETER             :: Spn5RDyb3 =  273
   INTEGER, PARAMETER             :: Spn5RDzb3 =  274
   INTEGER, PARAMETER             :: Spn6RDxb3 =  275
   INTEGER, PARAMETER             :: Spn6RDyb3 =  276
   INTEGER, PARAMETER             :: Spn6RDzb3 =  277
   INTEGER, PARAMETER             :: Spn7RDxb3 =  278
   INTEGER, PARAMETER             :: Spn7RDyb3 =  279
   INTEGER, PARAMETER             :: Spn7RDzb3 =  280
   INTEGER, PARAMETER             :: Spn8RDxb3 =  281
   INTEGER, PARAMETER             :: Spn8RDyb3 =  282
   INTEGER, PARAMETER             :: Spn8RDzb3 =  283
   INTEGER, PARAMETER             :: Spn9RDxb3 =  284
   INTEGER, PARAMETER             :: Spn9RDyb3 =  285
   INTEGER, PARAMETER             :: Spn9RDzb3 =  286


  ! Blade Pitch Motions:

   INTEGER, PARAMETER             :: PtchPMzc1 =  287
   INTEGER, PARAMETER             :: PtchPMzc2 =  288
   INTEGER, PARAMETER             :: PtchPMzc3 =  289


  ! Teeter Motions:

   INTEGER, PARAMETER             :: TeetPya   =  290
   INTEGER, PARAMETER             :: TeetVya   =  291
   INTEGER, PARAMETER             :: TeetAya   =  292


  ! Shaft Motions:

   INTEGER, PARAMETER             :: LSSTipPxa =  293
   INTEGER, PARAMETER             :: LSSTipVxa =  294
   INTEGER, PARAMETER             :: LSSTipAxa =  295
   INTEGER, PARAMETER             :: LSSGagPxa =  296
   INTEGER, PARAMETER             :: LSSGagVxa =  297
   INTEGER, PARAMETER             :: LSSGagAxa =  298
   INTEGER, PARAMETER             :: HSShftV   =  299
   INTEGER, PARAMETER             :: HSShftA   =  300
   INTEGER, PARAMETER             :: TipSpdRat =  301


  ! Nacelle IMU Motions:

   INTEGER, PARAMETER             :: NcIMUTVxs =  302
   INTEGER, PARAMETER             :: NcIMUTVys =  303
   INTEGER, PARAMETER             :: NcIMUTVzs =  304
   INTEGER, PARAMETER             :: NcIMUTAxs =  305
   INTEGER, PARAMETER             :: NcIMUTAys =  306
   INTEGER, PARAMETER             :: NcIMUTAzs =  307
   INTEGER, PARAMETER             :: NcIMURVxs =  308
   INTEGER, PARAMETER             :: NcIMURVys =  309
   INTEGER, PARAMETER             :: NcIMURVzs =  310
   INTEGER, PARAMETER             :: NcIMURAxs =  311
   INTEGER, PARAMETER             :: NcIMURAys =  312
   INTEGER, PARAMETER             :: NcIMURAzs =  313


  ! Rotor-Furl Motions:

   INTEGER, PARAMETER             :: RotFurlP  =  314
   INTEGER, PARAMETER             :: RotFurlV  =  315
   INTEGER, PARAMETER             :: RotFurlA  =  316


  ! Tail-Furl Motions:

   INTEGER, PARAMETER             :: TailFurlP =  317
   INTEGER, PARAMETER             :: TailFurlV =  318
   INTEGER, PARAMETER             :: TailFurlA =  319


  ! Nacelle Yaw Motions:

   INTEGER, PARAMETER             :: YawPzn    =  320
   INTEGER, PARAMETER             :: YawVzn    =  321
   INTEGER, PARAMETER             :: YawAzn    =  322
   INTEGER, PARAMETER             :: NacYawErr =  323


  ! Tower-Top / Yaw Bearing Motions:

   INTEGER, PARAMETER             :: YawBrTDxp =  324
   INTEGER, PARAMETER             :: YawBrTDyp =  325
   INTEGER, PARAMETER             :: YawBrTDzp =  326
   INTEGER, PARAMETER             :: YawBrTDxt =  327
   INTEGER, PARAMETER             :: YawBrTDyt =  328
   INTEGER, PARAMETER             :: YawBrTDzt =  329
   INTEGER, PARAMETER             :: YawBrTAxp =  330
   INTEGER, PARAMETER             :: YawBrTAyp =  331
   INTEGER, PARAMETER             :: YawBrTAzp =  332
   INTEGER, PARAMETER             :: YawBrRDxt =  333
   INTEGER, PARAMETER             :: YawBrRDyt =  334
   INTEGER, PARAMETER             :: YawBrRDzt =  335
   INTEGER, PARAMETER             :: YawBrRVxp =  336
   INTEGER, PARAMETER             :: YawBrRVyp =  337
   INTEGER, PARAMETER             :: YawBrRVzp =  338
   INTEGER, PARAMETER             :: YawBrRAxp =  339
   INTEGER, PARAMETER             :: YawBrRAyp =  340
   INTEGER, PARAMETER             :: YawBrRAzp =  341


  ! Local Tower Motions:

   INTEGER, PARAMETER             :: TwHt1ALxt =  342
   INTEGER, PARAMETER             :: TwHt1ALyt =  343
   INTEGER, PARAMETER             :: TwHt1ALzt =  344
   INTEGER, PARAMETER             :: TwHt2ALxt =  345
   INTEGER, PARAMETER             :: TwHt2ALyt =  346
   INTEGER, PARAMETER             :: TwHt2ALzt =  347
   INTEGER, PARAMETER             :: TwHt3ALxt =  348
   INTEGER, PARAMETER             :: TwHt3ALyt =  349
   INTEGER, PARAMETER             :: TwHt3ALzt =  350
   INTEGER, PARAMETER             :: TwHt4ALxt =  351
   INTEGER, PARAMETER             :: TwHt4ALyt =  352
   INTEGER, PARAMETER             :: TwHt4ALzt =  353
   INTEGER, PARAMETER             :: TwHt5ALxt =  354
   INTEGER, PARAMETER             :: TwHt5ALyt =  355
   INTEGER, PARAMETER             :: TwHt5ALzt =  356
   INTEGER, PARAMETER             :: TwHt6ALxt =  357
   INTEGER, PARAMETER             :: TwHt6ALyt =  358
   INTEGER, PARAMETER             :: TwHt6ALzt =  359
   INTEGER, PARAMETER             :: TwHt7ALxt =  360
   INTEGER, PARAMETER             :: TwHt7ALyt =  361
   INTEGER, PARAMETER             :: TwHt7ALzt =  362
   INTEGER, PARAMETER             :: TwHt8ALxt =  363
   INTEGER, PARAMETER             :: TwHt8ALyt =  364
   INTEGER, PARAMETER             :: TwHt8ALzt =  365
   INTEGER, PARAMETER             :: TwHt9ALxt =  366
   INTEGER, PARAMETER             :: TwHt9ALyt =  367
   INTEGER, PARAMETER             :: TwHt9ALzt =  368
   INTEGER, PARAMETER             :: TwHt1TDxt =  369
   INTEGER, PARAMETER             :: TwHt1TDyt =  370
   INTEGER, PARAMETER             :: TwHt1TDzt =  371
   INTEGER, PARAMETER             :: TwHt2TDxt =  372
   INTEGER, PARAMETER             :: TwHt2TDyt =  373
   INTEGER, PARAMETER             :: TwHt2TDzt =  374
   INTEGER, PARAMETER             :: TwHt3TDxt =  375
   INTEGER, PARAMETER             :: TwHt3TDyt =  376
   INTEGER, PARAMETER             :: TwHt3TDzt =  377
   INTEGER, PARAMETER             :: TwHt4TDxt =  378
   INTEGER, PARAMETER             :: TwHt4TDyt =  379
   INTEGER, PARAMETER             :: TwHt4TDzt =  380
   INTEGER, PARAMETER             :: TwHt5TDxt =  381
   INTEGER, PARAMETER             :: TwHt5TDyt =  382
   INTEGER, PARAMETER             :: TwHt5TDzt =  383
   INTEGER, PARAMETER             :: TwHt6TDxt =  384
   INTEGER, PARAMETER             :: TwHt6TDyt =  385
   INTEGER, PARAMETER             :: TwHt6TDzt =  386
   INTEGER, PARAMETER             :: TwHt7TDxt =  387
   INTEGER, PARAMETER             :: TwHt7TDyt =  388
   INTEGER, PARAMETER             :: TwHt7TDzt =  389
   INTEGER, PARAMETER             :: TwHt8TDxt =  390
   INTEGER, PARAMETER             :: TwHt8TDyt =  391
   INTEGER, PARAMETER             :: TwHt8TDzt =  392
   INTEGER, PARAMETER             :: TwHt9TDxt =  393
   INTEGER, PARAMETER             :: TwHt9TDyt =  394
   INTEGER, PARAMETER             :: TwHt9TDzt =  395
   INTEGER, PARAMETER             :: TwHt1RDxt =  396
   INTEGER, PARAMETER             :: TwHt1RDyt =  397
   INTEGER, PARAMETER             :: TwHt1RDzt =  398
   INTEGER, PARAMETER             :: TwHt2RDxt =  399
   INTEGER, PARAMETER             :: TwHt2RDyt =  400
   INTEGER, PARAMETER             :: TwHt2RDzt =  401
   INTEGER, PARAMETER             :: TwHt3RDxt =  402
   INTEGER, PARAMETER             :: TwHt3RDyt =  403
   INTEGER, PARAMETER             :: TwHt3RDzt =  404
   INTEGER, PARAMETER             :: TwHt4RDxt =  405
   INTEGER, PARAMETER             :: TwHt4RDyt =  406
   INTEGER, PARAMETER             :: TwHt4RDzt =  407
   INTEGER, PARAMETER             :: TwHt5RDxt =  408
   INTEGER, PARAMETER             :: TwHt5RDyt =  409
   INTEGER, PARAMETER             :: TwHt5RDzt =  410
   INTEGER, PARAMETER             :: TwHt6RDxt =  411
   INTEGER, PARAMETER             :: TwHt6RDyt =  412
   INTEGER, PARAMETER             :: TwHt6RDzt =  413
   INTEGER, PARAMETER             :: TwHt7RDxt =  414
   INTEGER, PARAMETER             :: TwHt7RDyt =  415
   INTEGER, PARAMETER             :: TwHt7RDzt =  416
   INTEGER, PARAMETER             :: TwHt8RDxt =  417
   INTEGER, PARAMETER             :: TwHt8RDyt =  418
   INTEGER, PARAMETER             :: TwHt8RDzt =  419
   INTEGER, PARAMETER             :: TwHt9RDxt =  420
   INTEGER, PARAMETER             :: TwHt9RDyt =  421
   INTEGER, PARAMETER             :: TwHt9RDzt =  422
   INTEGER, PARAMETER             :: TwHt1TPxi =  423
   INTEGER, PARAMETER             :: TwHt1TPyi =  424
   INTEGER, PARAMETER             :: TwHt1TPzi =  425
   INTEGER, PARAMETER             :: TwHt2TPxi =  426
   INTEGER, PARAMETER             :: TwHt2TPyi =  427
   INTEGER, PARAMETER             :: TwHt2TPzi =  428
   INTEGER, PARAMETER             :: TwHt3TPxi =  429
   INTEGER, PARAMETER             :: TwHt3TPyi =  430
   INTEGER, PARAMETER             :: TwHt3TPzi =  431
   INTEGER, PARAMETER             :: TwHt4TPxi =  432
   INTEGER, PARAMETER             :: TwHt4TPyi =  433
   INTEGER, PARAMETER             :: TwHt4TPzi =  434
   INTEGER, PARAMETER             :: TwHt5TPxi =  435
   INTEGER, PARAMETER             :: TwHt5TPyi =  436
   INTEGER, PARAMETER             :: TwHt5TPzi =  437
   INTEGER, PARAMETER             :: TwHt6TPxi =  438
   INTEGER, PARAMETER             :: TwHt6TPyi =  439
   INTEGER, PARAMETER             :: TwHt6TPzi =  440
   INTEGER, PARAMETER             :: TwHt7TPxi =  441
   INTEGER, PARAMETER             :: TwHt7TPyi =  442
   INTEGER, PARAMETER             :: TwHt7TPzi =  443
   INTEGER, PARAMETER             :: TwHt8TPxi =  444
   INTEGER, PARAMETER             :: TwHt8TPyi =  445
   INTEGER, PARAMETER             :: TwHt8TPzi =  446
   INTEGER, PARAMETER             :: TwHt9TPxi =  447
   INTEGER, PARAMETER             :: TwHt9TPyi =  448
   INTEGER, PARAMETER             :: TwHt9TPzi =  449
   INTEGER, PARAMETER             :: TwHt1RPxi =  450
   INTEGER, PARAMETER             :: TwHt1RPyi =  451
   INTEGER, PARAMETER             :: TwHt1RPzi =  452
   INTEGER, PARAMETER             :: TwHt2RPxi =  453
   INTEGER, PARAMETER             :: TwHt2RPyi =  454
   INTEGER, PARAMETER             :: TwHt2RPzi =  455
   INTEGER, PARAMETER             :: TwHt3RPxi =  456
   INTEGER, PARAMETER             :: TwHt3RPyi =  457
   INTEGER, PARAMETER             :: TwHt3RPzi =  458
   INTEGER, PARAMETER             :: TwHt4RPxi =  459
   INTEGER, PARAMETER             :: TwHt4RPyi =  460
   INTEGER, PARAMETER             :: TwHt4RPzi =  461
   INTEGER, PARAMETER             :: TwHt5RPxi =  462
   INTEGER, PARAMETER             :: TwHt5RPyi =  463
   INTEGER, PARAMETER             :: TwHt5RPzi =  464
   INTEGER, PARAMETER             :: TwHt6RPxi =  465
   INTEGER, PARAMETER             :: TwHt6RPyi =  466
   INTEGER, PARAMETER             :: TwHt6RPzi =  467
   INTEGER, PARAMETER             :: TwHt7RPxi =  468
   INTEGER, PARAMETER             :: TwHt7RPyi =  469
   INTEGER, PARAMETER             :: TwHt7RPzi =  470
   INTEGER, PARAMETER             :: TwHt8RPxi =  471
   INTEGER, PARAMETER             :: TwHt8RPyi =  472
   INTEGER, PARAMETER             :: TwHt8RPzi =  473
   INTEGER, PARAMETER             :: TwHt9RPxi =  474
   INTEGER, PARAMETER             :: TwHt9RPyi =  475
   INTEGER, PARAMETER             :: TwHt9RPzi =  476


  ! Platform Motions:

   INTEGER, PARAMETER             :: PtfmTDxt  =  477
   INTEGER, PARAMETER             :: PtfmTDyt  =  478
   INTEGER, PARAMETER             :: PtfmTDzt  =  479
   INTEGER, PARAMETER             :: PtfmTDxi  =  480
   INTEGER, PARAMETER             :: PtfmTDyi  =  481
   INTEGER, PARAMETER             :: PtfmTDzi  =  482
   INTEGER, PARAMETER             :: PtfmTVxt  =  483
   INTEGER, PARAMETER             :: PtfmTVyt  =  484
   INTEGER, PARAMETER             :: PtfmTVzt  =  485
   INTEGER, PARAMETER             :: PtfmTVxi  =  486
   INTEGER, PARAMETER             :: PtfmTVyi  =  487
   INTEGER, PARAMETER             :: PtfmTVzi  =  488
   INTEGER, PARAMETER             :: PtfmTAxt  =  489
   INTEGER, PARAMETER             :: PtfmTAyt  =  490
   INTEGER, PARAMETER             :: PtfmTAzt  =  491
   INTEGER, PARAMETER             :: PtfmTAxi  =  492
   INTEGER, PARAMETER             :: PtfmTAyi  =  493
   INTEGER, PARAMETER             :: PtfmTAzi  =  494
   INTEGER, PARAMETER             :: PtfmRDxi  =  495
   INTEGER, PARAMETER             :: PtfmRDyi  =  496
   INTEGER, PARAMETER             :: PtfmRDzi  =  497
   INTEGER, PARAMETER             :: PtfmRVxt  =  498
   INTEGER, PARAMETER             :: PtfmRVyt  =  499
   INTEGER, PARAMETER             :: PtfmRVzt  =  500
   INTEGER, PARAMETER             :: PtfmRVxi  =  501
   INTEGER, PARAMETER             :: PtfmRVyi  =  502
   INTEGER, PARAMETER             :: PtfmRVzi  =  503
   INTEGER, PARAMETER             :: PtfmRAxt  =  504
   INTEGER, PARAMETER             :: PtfmRAyt  =  505
   INTEGER, PARAMETER             :: PtfmRAzt  =  506
   INTEGER, PARAMETER             :: PtfmRAxi  =  507
   INTEGER, PARAMETER             :: PtfmRAyi  =  508
   INTEGER, PARAMETER             :: PtfmRAzi  =  509


  ! Blade 1 Root Loads:

   INTEGER, PARAMETER             :: RootFxc1  =  510
   INTEGER, PARAMETER             :: RootFyc1  =  511
   INTEGER, PARAMETER             :: RootFzc1  =  512
   INTEGER, PARAMETER             :: RootFxb1  =  513
   INTEGER, PARAMETER             :: RootFyb1  =  514
   INTEGER, PARAMETER             :: RootMxc1  =  515
   INTEGER, PARAMETER             :: RootMyc1  =  516
   INTEGER, PARAMETER             :: RootMzc1  =  517
   INTEGER, PARAMETER             :: RootMxb1  =  518
   INTEGER, PARAMETER             :: RootMyb1  =  519


  ! Blade 2 Root Loads:

   INTEGER, PARAMETER             :: RootFxc2  =  520
   INTEGER, PARAMETER             :: RootFyc2  =  521
   INTEGER, PARAMETER             :: RootFzc2  =  522
   INTEGER, PARAMETER             :: RootFxb2  =  523
   INTEGER, PARAMETER             :: RootFyb2  =  524
   INTEGER, PARAMETER             :: RootMxc2  =  525
   INTEGER, PARAMETER             :: RootMyc2  =  526
   INTEGER, PARAMETER             :: RootMzc2  =  527
   INTEGER, PARAMETER             :: RootMxb2  =  528
   INTEGER, PARAMETER             :: RootMyb2  =  529


  ! Blade 3 Root Loads:

   INTEGER, PARAMETER             :: RootFxc3  =  530
   INTEGER, PARAMETER             :: RootFyc3  =  531
   INTEGER, PARAMETER             :: RootFzc3  =  532
   INTEGER, PARAMETER             :: RootFxb3  =  533
   INTEGER, PARAMETER             :: RootFyb3  =  534
   INTEGER, PARAMETER             :: RootMxc3  =  535
   INTEGER, PARAMETER             :: RootMyc3  =  536
   INTEGER, PARAMETER             :: RootMzc3  =  537
   INTEGER, PARAMETER             :: RootMxb3  =  538
   INTEGER, PARAMETER             :: RootMyb3  =  539


  ! Blade 1 Local Span Loads:

   INTEGER, PARAMETER             :: Spn1MLxb1 =  540
   INTEGER, PARAMETER             :: Spn1MLyb1 =  541
   INTEGER, PARAMETER             :: Spn1MLzb1 =  542
   INTEGER, PARAMETER             :: Spn2MLxb1 =  543
   INTEGER, PARAMETER             :: Spn2MLyb1 =  544
   INTEGER, PARAMETER             :: Spn2MLzb1 =  545
   INTEGER, PARAMETER             :: Spn3MLxb1 =  546
   INTEGER, PARAMETER             :: Spn3MLyb1 =  547
   INTEGER, PARAMETER             :: Spn3MLzb1 =  548
   INTEGER, PARAMETER             :: Spn4MLxb1 =  549
   INTEGER, PARAMETER             :: Spn4MLyb1 =  550
   INTEGER, PARAMETER             :: Spn4MLzb1 =  551
   INTEGER, PARAMETER             :: Spn5MLxb1 =  552
   INTEGER, PARAMETER             :: Spn5MLyb1 =  553
   INTEGER, PARAMETER             :: Spn5MLzb1 =  554
   INTEGER, PARAMETER             :: Spn6MLxb1 =  555
   INTEGER, PARAMETER             :: Spn6MLyb1 =  556
   INTEGER, PARAMETER             :: Spn6MLzb1 =  557
   INTEGER, PARAMETER             :: Spn7MLxb1 =  558
   INTEGER, PARAMETER             :: Spn7MLyb1 =  559
   INTEGER, PARAMETER             :: Spn7MLzb1 =  560
   INTEGER, PARAMETER             :: Spn8MLxb1 =  561
   INTEGER, PARAMETER             :: Spn8MLyb1 =  562
   INTEGER, PARAMETER             :: Spn8MLzb1 =  563
   INTEGER, PARAMETER             :: Spn9MLxb1 =  564
   INTEGER, PARAMETER             :: Spn9MLyb1 =  565
   INTEGER, PARAMETER             :: Spn9MLzb1 =  566
   INTEGER, PARAMETER             :: Spn1FLxb1 =  567
   INTEGER, PARAMETER             :: Spn1FLyb1 =  568
   INTEGER, PARAMETER             :: Spn1FLzb1 =  569
   INTEGER, PARAMETER             :: Spn2FLxb1 =  570
   INTEGER, PARAMETER             :: Spn2FLyb1 =  571
   INTEGER, PARAMETER             :: Spn2FLzb1 =  572
   INTEGER, PARAMETER             :: Spn3FLxb1 =  573
   INTEGER, PARAMETER             :: Spn3FLyb1 =  574
   INTEGER, PARAMETER             :: Spn3FLzb1 =  575
   INTEGER, PARAMETER             :: Spn4FLxb1 =  576
   INTEGER, PARAMETER             :: Spn4FLyb1 =  577
   INTEGER, PARAMETER             :: Spn4FLzb1 =  578
   INTEGER, PARAMETER             :: Spn5FLxb1 =  579
   INTEGER, PARAMETER             :: Spn5FLyb1 =  580
   INTEGER, PARAMETER             :: Spn5FLzb1 =  581
   INTEGER, PARAMETER             :: Spn6FLxb1 =  582
   INTEGER, PARAMETER             :: Spn6FLyb1 =  583
   INTEGER, PARAMETER             :: Spn6FLzb1 =  584
   INTEGER, PARAMETER             :: Spn7FLxb1 =  585
   INTEGER, PARAMETER             :: Spn7FLyb1 =  586
   INTEGER, PARAMETER             :: Spn7FLzb1 =  587
   INTEGER, PARAMETER             :: Spn8FLxb1 =  588
   INTEGER, PARAMETER             :: Spn8FLyb1 =  589
   INTEGER, PARAMETER             :: Spn8FLzb1 =  590
   INTEGER, PARAMETER             :: Spn9FLxb1 =  591
   INTEGER, PARAMETER             :: Spn9FLyb1 =  592
   INTEGER, PARAMETER             :: Spn9FLzb1 =  593


  ! Blade 2 Local Span Loads:

   INTEGER, PARAMETER             :: Spn1MLxb2 =  594
   INTEGER, PARAMETER             :: Spn1MLyb2 =  595
   INTEGER, PARAMETER             :: Spn1MLzb2 =  596
   INTEGER, PARAMETER             :: Spn2MLxb2 =  597
   INTEGER, PARAMETER             :: Spn2MLyb2 =  598
   INTEGER, PARAMETER             :: Spn2MLzb2 =  599
   INTEGER, PARAMETER             :: Spn3MLxb2 =  600
   INTEGER, PARAMETER             :: Spn3MLyb2 =  601
   INTEGER, PARAMETER             :: Spn3MLzb2 =  602
   INTEGER, PARAMETER             :: Spn4MLxb2 =  603
   INTEGER, PARAMETER             :: Spn4MLyb2 =  604
   INTEGER, PARAMETER             :: Spn4MLzb2 =  605
   INTEGER, PARAMETER             :: Spn5MLxb2 =  606
   INTEGER, PARAMETER             :: Spn5MLyb2 =  607
   INTEGER, PARAMETER             :: Spn5MLzb2 =  608
   INTEGER, PARAMETER             :: Spn6MLxb2 =  609
   INTEGER, PARAMETER             :: Spn6MLyb2 =  610
   INTEGER, PARAMETER             :: Spn6MLzb2 =  611
   INTEGER, PARAMETER             :: Spn7MLxb2 =  612
   INTEGER, PARAMETER             :: Spn7MLyb2 =  613
   INTEGER, PARAMETER             :: Spn7MLzb2 =  614
   INTEGER, PARAMETER             :: Spn8MLxb2 =  615
   INTEGER, PARAMETER             :: Spn8MLyb2 =  616
   INTEGER, PARAMETER             :: Spn8MLzb2 =  617
   INTEGER, PARAMETER             :: Spn9MLxb2 =  618
   INTEGER, PARAMETER             :: Spn9MLyb2 =  619
   INTEGER, PARAMETER             :: Spn9MLzb2 =  620
   INTEGER, PARAMETER             :: Spn1FLxb2 =  621
   INTEGER, PARAMETER             :: Spn1FLyb2 =  622
   INTEGER, PARAMETER             :: Spn1FLzb2 =  623
   INTEGER, PARAMETER             :: Spn2FLxb2 =  624
   INTEGER, PARAMETER             :: Spn2FLyb2 =  625
   INTEGER, PARAMETER             :: Spn2FLzb2 =  626
   INTEGER, PARAMETER             :: Spn3FLxb2 =  627
   INTEGER, PARAMETER             :: Spn3FLyb2 =  628
   INTEGER, PARAMETER             :: Spn3FLzb2 =  629
   INTEGER, PARAMETER             :: Spn4FLxb2 =  630
   INTEGER, PARAMETER             :: Spn4FLyb2 =  631
   INTEGER, PARAMETER             :: Spn4FLzb2 =  632
   INTEGER, PARAMETER             :: Spn5FLxb2 =  633
   INTEGER, PARAMETER             :: Spn5FLyb2 =  634
   INTEGER, PARAMETER             :: Spn5FLzb2 =  635
   INTEGER, PARAMETER             :: Spn6FLxb2 =  636
   INTEGER, PARAMETER             :: Spn6FLyb2 =  637
   INTEGER, PARAMETER             :: Spn6FLzb2 =  638
   INTEGER, PARAMETER             :: Spn7FLxb2 =  639
   INTEGER, PARAMETER             :: Spn7FLyb2 =  640
   INTEGER, PARAMETER             :: Spn7FLzb2 =  641
   INTEGER, PARAMETER             :: Spn8FLxb2 =  642
   INTEGER, PARAMETER             :: Spn8FLyb2 =  643
   INTEGER, PARAMETER             :: Spn8FLzb2 =  644
   INTEGER, PARAMETER             :: Spn9FLxb2 =  645
   INTEGER, PARAMETER             :: Spn9FLyb2 =  646
   INTEGER, PARAMETER             :: Spn9FLzb2 =  647


  ! Blade 3 Local Span Loads:

   INTEGER, PARAMETER             :: Spn1MLxb3 =  648
   INTEGER, PARAMETER             :: Spn1MLyb3 =  649
   INTEGER, PARAMETER             :: Spn1MLzb3 =  650
   INTEGER, PARAMETER             :: Spn2MLxb3 =  651
   INTEGER, PARAMETER             :: Spn2MLyb3 =  652
   INTEGER, PARAMETER             :: Spn2MLzb3 =  653
   INTEGER, PARAMETER             :: Spn3MLxb3 =  654
   INTEGER, PARAMETER             :: Spn3MLyb3 =  655
   INTEGER, PARAMETER             :: Spn3MLzb3 =  656
   INTEGER, PARAMETER             :: Spn4MLxb3 =  657
   INTEGER, PARAMETER             :: Spn4MLyb3 =  658
   INTEGER, PARAMETER             :: Spn4MLzb3 =  659
   INTEGER, PARAMETER             :: Spn5MLxb3 =  660
   INTEGER, PARAMETER             :: Spn5MLyb3 =  661
   INTEGER, PARAMETER             :: Spn5MLzb3 =  662
   INTEGER, PARAMETER             :: Spn6MLxb3 =  663
   INTEGER, PARAMETER             :: Spn6MLyb3 =  664
   INTEGER, PARAMETER             :: Spn6MLzb3 =  665
   INTEGER, PARAMETER             :: Spn7MLxb3 =  666
   INTEGER, PARAMETER             :: Spn7MLyb3 =  667
   INTEGER, PARAMETER             :: Spn7MLzb3 =  668
   INTEGER, PARAMETER             :: Spn8MLxb3 =  669
   INTEGER, PARAMETER             :: Spn8MLyb3 =  670
   INTEGER, PARAMETER             :: Spn8MLzb3 =  671
   INTEGER, PARAMETER             :: Spn9MLxb3 =  672
   INTEGER, PARAMETER             :: Spn9MLyb3 =  673
   INTEGER, PARAMETER             :: Spn9MLzb3 =  674
   INTEGER, PARAMETER             :: Spn1FLxb3 =  675
   INTEGER, PARAMETER             :: Spn1FLyb3 =  676
   INTEGER, PARAMETER             :: Spn1FLzb3 =  677
   INTEGER, PARAMETER             :: Spn2FLxb3 =  678
   INTEGER, PARAMETER             :: Spn2FLyb3 =  679
   INTEGER, PARAMETER             :: Spn2FLzb3 =  680
   INTEGER, PARAMETER             :: Spn3FLxb3 =  681
   INTEGER, PARAMETER             :: Spn3FLyb3 =  682
   INTEGER, PARAMETER             :: Spn3FLzb3 =  683
   INTEGER, PARAMETER             :: Spn4FLxb3 =  684
   INTEGER, PARAMETER             :: Spn4FLyb3 =  685
   INTEGER, PARAMETER             :: Spn4FLzb3 =  686
   INTEGER, PARAMETER             :: Spn5FLxb3 =  687
   INTEGER, PARAMETER             :: Spn5FLyb3 =  688
   INTEGER, PARAMETER             :: Spn5FLzb3 =  689
   INTEGER, PARAMETER             :: Spn6FLxb3 =  690
   INTEGER, PARAMETER             :: Spn6FLyb3 =  691
   INTEGER, PARAMETER             :: Spn6FLzb3 =  692
   INTEGER, PARAMETER             :: Spn7FLxb3 =  693
   INTEGER, PARAMETER             :: Spn7FLyb3 =  694
   INTEGER, PARAMETER             :: Spn7FLzb3 =  695
   INTEGER, PARAMETER             :: Spn8FLxb3 =  696
   INTEGER, PARAMETER             :: Spn8FLyb3 =  697
   INTEGER, PARAMETER             :: Spn8FLzb3 =  698
   INTEGER, PARAMETER             :: Spn9FLxb3 =  699
   INTEGER, PARAMETER             :: Spn9FLyb3 =  700
   INTEGER, PARAMETER             :: Spn9FLzb3 =  701


  ! Hub and Rotor Loads:

   INTEGER, PARAMETER             :: LSShftFxa =  702
   INTEGER, PARAMETER             :: LSShftFya =  703
   INTEGER, PARAMETER             :: LSShftFza =  704
   INTEGER, PARAMETER             :: LSShftFys =  705
   INTEGER, PARAMETER             :: LSShftFzs =  706
   INTEGER, PARAMETER             :: LSShftMxa =  707
   INTEGER, PARAMETER             :: LSSTipMya =  708
   INTEGER, PARAMETER             :: LSSTipMza =  709
   INTEGER, PARAMETER             :: LSSTipMys =  710
   INTEGER, PARAMETER             :: LSSTipMzs =  711
   INTEGER, PARAMETER             :: CThrstAzm =  712
   INTEGER, PARAMETER             :: CThrstRad =  713
   INTEGER, PARAMETER             :: RotPwr    =  714
   INTEGER, PARAMETER             :: RotCq     =  715
   INTEGER, PARAMETER             :: RotCp     =  716
   INTEGER, PARAMETER             :: RotCt     =  717


  ! Shaft Strain Gage Loads:

   INTEGER, PARAMETER             :: LSSGagMya =  718
   INTEGER, PARAMETER             :: LSSGagMza =  719
   INTEGER, PARAMETER             :: LSSGagMys =  720
   INTEGER, PARAMETER             :: LSSGagMzs =  721


  ! Generator and High-Speed Shaft Loads:

   INTEGER, PARAMETER             :: HSShftTq  =  722
   INTEGER, PARAMETER             :: HSShftPwr =  723
   INTEGER, PARAMETER             :: HSShftCq  =  724
   INTEGER, PARAMETER             :: HSShftCp  =  725
   INTEGER, PARAMETER             :: GenTq     =  726
   INTEGER, PARAMETER             :: GenPwr    =  727
   INTEGER, PARAMETER             :: GenCq     =  728
   INTEGER, PARAMETER             :: GenCp     =  729
   INTEGER, PARAMETER             :: HSSBrTq   =  730


  ! Rotor-Furl Bearing Loads:

   INTEGER, PARAMETER             :: RFrlBrM   =  731


  ! Tail-Furl Bearing Loads:

   INTEGER, PARAMETER             :: TFrlBrM   =  732


  ! Tail Fin Aerodynamic Loads:

   INTEGER, PARAMETER             :: TFinAlpha =  733
   INTEGER, PARAMETER             :: TFinCLift =  734
   INTEGER, PARAMETER             :: TFinCDrag =  735
   INTEGER, PARAMETER             :: TFinDnPrs =  736
   INTEGER, PARAMETER             :: TFinCPFx  =  737
   INTEGER, PARAMETER             :: TFinCPFy  =  738


  ! Tower-Top / Yaw Bearing Loads:

   INTEGER, PARAMETER             :: YawBrFxn  =  739
   INTEGER, PARAMETER             :: YawBrFyn  =  740
   INTEGER, PARAMETER             :: YawBrFzn  =  741
   INTEGER, PARAMETER             :: YawBrFxp  =  742
   INTEGER, PARAMETER             :: YawBrFyp  =  743
   INTEGER, PARAMETER             :: YawBrMxn  =  744
   INTEGER, PARAMETER             :: YawBrMyn  =  745
   INTEGER, PARAMETER             :: YawBrMzn  =  746
   INTEGER, PARAMETER             :: YawBrMxp  =  747
   INTEGER, PARAMETER             :: YawBrMyp  =  748


  ! Tower Base Loads:

   INTEGER, PARAMETER             :: TwrBsFxt  =  749
   INTEGER, PARAMETER             :: TwrBsFyt  =  750
   INTEGER, PARAMETER             :: TwrBsFzt  =  751
   INTEGER, PARAMETER             :: TwrBsMxt  =  752
   INTEGER, PARAMETER             :: TwrBsMyt  =  753
   INTEGER, PARAMETER             :: TwrBsMzt  =  754


  ! Local Tower Loads:

   INTEGER, PARAMETER             :: TwHt1MLxt =  755
   INTEGER, PARAMETER             :: TwHt1MLyt =  756
   INTEGER, PARAMETER             :: TwHt1MLzt =  757
   INTEGER, PARAMETER             :: TwHt2MLxt =  758
   INTEGER, PARAMETER             :: TwHt2MLyt =  759
   INTEGER, PARAMETER             :: TwHt2MLzt =  760
   INTEGER, PARAMETER             :: TwHt3MLxt =  761
   INTEGER, PARAMETER             :: TwHt3MLyt =  762
   INTEGER, PARAMETER             :: TwHt3MLzt =  763
   INTEGER, PARAMETER             :: TwHt4MLxt =  764
   INTEGER, PARAMETER             :: TwHt4MLyt =  765
   INTEGER, PARAMETER             :: TwHt4MLzt =  766
   INTEGER, PARAMETER             :: TwHt5MLxt =  767
   INTEGER, PARAMETER             :: TwHt5MLyt =  768
   INTEGER, PARAMETER             :: TwHt5MLzt =  769
   INTEGER, PARAMETER             :: TwHt6MLxt =  770
   INTEGER, PARAMETER             :: TwHt6MLyt =  771
   INTEGER, PARAMETER             :: TwHt6MLzt =  772
   INTEGER, PARAMETER             :: TwHt7MLxt =  773
   INTEGER, PARAMETER             :: TwHt7MLyt =  774
   INTEGER, PARAMETER             :: TwHt7MLzt =  775
   INTEGER, PARAMETER             :: TwHt8MLxt =  776
   INTEGER, PARAMETER             :: TwHt8MLyt =  777
   INTEGER, PARAMETER             :: TwHt8MLzt =  778
   INTEGER, PARAMETER             :: TwHt9MLxt =  779
   INTEGER, PARAMETER             :: TwHt9MLyt =  780
   INTEGER, PARAMETER             :: TwHt9MLzt =  781
   INTEGER, PARAMETER             :: TwHt1FLxt =  782
   INTEGER, PARAMETER             :: TwHt1FLyt =  783
   INTEGER, PARAMETER             :: TwHt1FLzt =  784
   INTEGER, PARAMETER             :: TwHt2FLxt =  785
   INTEGER, PARAMETER             :: TwHt2FLyt =  786
   INTEGER, PARAMETER             :: TwHt2FLzt =  787
   INTEGER, PARAMETER             :: TwHt3FLxt =  788
   INTEGER, PARAMETER             :: TwHt3FLyt =  789
   INTEGER, PARAMETER             :: TwHt3FLzt =  790
   INTEGER, PARAMETER             :: TwHt4FLxt =  791
   INTEGER, PARAMETER             :: TwHt4FLyt =  792
   INTEGER, PARAMETER             :: TwHt4FLzt =  793
   INTEGER, PARAMETER             :: TwHt5FLxt =  794
   INTEGER, PARAMETER             :: TwHt5FLyt =  795
   INTEGER, PARAMETER             :: TwHt5FLzt =  796
   INTEGER, PARAMETER             :: TwHt6FLxt =  797
   INTEGER, PARAMETER             :: TwHt6FLyt =  798
   INTEGER, PARAMETER             :: TwHt6FLzt =  799
   INTEGER, PARAMETER             :: TwHt7FLxt =  800
   INTEGER, PARAMETER             :: TwHt7FLyt =  801
   INTEGER, PARAMETER             :: TwHt7FLzt =  802
   INTEGER, PARAMETER             :: TwHt8FLxt =  803
   INTEGER, PARAMETER             :: TwHt8FLyt =  804
   INTEGER, PARAMETER             :: TwHt8FLzt =  805
   INTEGER, PARAMETER             :: TwHt9FLxt =  806
   INTEGER, PARAMETER             :: TwHt9FLyt =  807
   INTEGER, PARAMETER             :: TwHt9FLzt =  808


  ! Platform Loads:

   INTEGER, PARAMETER             :: PtfmFxt   =  809
   INTEGER, PARAMETER             :: PtfmFyt   =  810
   INTEGER, PARAMETER             :: PtfmFzt   =  811
   INTEGER, PARAMETER             :: PtfmFxi   =  812
   INTEGER, PARAMETER             :: PtfmFyi   =  813
   INTEGER, PARAMETER             :: PtfmFzi   =  814
   INTEGER, PARAMETER             :: PtfmMxt   =  815
   INTEGER, PARAMETER             :: PtfmMyt   =  816
   INTEGER, PARAMETER             :: PtfmMzt   =  817
   INTEGER, PARAMETER             :: PtfmMxi   =  818
   INTEGER, PARAMETER             :: PtfmMyi   =  819
   INTEGER, PARAMETER             :: PtfmMzi   =  820


  ! Mooring Line Loads: (these will be in HydroDyn)

   INTEGER, PARAMETER             :: Fair1Ten  =  821
   INTEGER, PARAMETER             :: Fair1Ang  =  822
   INTEGER, PARAMETER             :: Anch1Ten  =  823
   INTEGER, PARAMETER             :: Anch1Ang  =  824
   INTEGER, PARAMETER             :: Fair2Ten  =  825
   INTEGER, PARAMETER             :: Fair2Ang  =  826
   INTEGER, PARAMETER             :: Anch2Ten  =  827
   INTEGER, PARAMETER             :: Anch2Ang  =  828
   INTEGER, PARAMETER             :: Fair3Ten  =  829
   INTEGER, PARAMETER             :: Fair3Ang  =  830
   INTEGER, PARAMETER             :: Anch3Ten  =  831
   INTEGER, PARAMETER             :: Anch3Ang  =  832
   INTEGER, PARAMETER             :: Fair4Ten  =  833
   INTEGER, PARAMETER             :: Fair4Ang  =  834
   INTEGER, PARAMETER             :: Anch4Ten  =  835
   INTEGER, PARAMETER             :: Anch4Ang  =  836
   INTEGER, PARAMETER             :: Fair5Ten  =  837
   INTEGER, PARAMETER             :: Fair5Ang  =  838
   INTEGER, PARAMETER             :: Anch5Ten  =  839
   INTEGER, PARAMETER             :: Anch5Ang  =  840
   INTEGER, PARAMETER             :: Fair6Ten  =  841
   INTEGER, PARAMETER             :: Fair6Ang  =  842
   INTEGER, PARAMETER             :: Anch6Ten  =  843
   INTEGER, PARAMETER             :: Anch6Ang  =  844
   INTEGER, PARAMETER             :: Fair7Ten  =  845
   INTEGER, PARAMETER             :: Fair7Ang  =  846
   INTEGER, PARAMETER             :: Anch7Ten  =  847
   INTEGER, PARAMETER             :: Anch7Ang  =  848
   INTEGER, PARAMETER             :: Fair8Ten  =  849
   INTEGER, PARAMETER             :: Fair8Ang  =  850
   INTEGER, PARAMETER             :: Anch8Ten  =  851
   INTEGER, PARAMETER             :: Anch8Ang  =  852
   INTEGER, PARAMETER             :: Fair9Ten  =  853
   INTEGER, PARAMETER             :: Fair9Ang  =  854
   INTEGER, PARAMETER             :: Anch9Ten  =  855
   INTEGER, PARAMETER             :: Anch9Ang  =  856


  ! Wave Motions: (these will be in HydroDyn)

   INTEGER, PARAMETER             :: WaveElev  =  857
   INTEGER, PARAMETER             :: Wave1Vxi  =  858
   INTEGER, PARAMETER             :: Wave1Vyi  =  859
   INTEGER, PARAMETER             :: Wave1Vzi  =  860
   INTEGER, PARAMETER             :: Wave1Axi  =  861
   INTEGER, PARAMETER             :: Wave1Ayi  =  862
   INTEGER, PARAMETER             :: Wave1Azi  =  863
   INTEGER, PARAMETER             :: Wave2Vxi  =  864
   INTEGER, PARAMETER             :: Wave2Vyi  =  865
   INTEGER, PARAMETER             :: Wave2Vzi  =  866
   INTEGER, PARAMETER             :: Wave2Axi  =  867
   INTEGER, PARAMETER             :: Wave2Ayi  =  868
   INTEGER, PARAMETER             :: Wave2Azi  =  869
   INTEGER, PARAMETER             :: Wave3Vxi  =  870
   INTEGER, PARAMETER             :: Wave3Vyi  =  871
   INTEGER, PARAMETER             :: Wave3Vzi  =  872
   INTEGER, PARAMETER             :: Wave3Axi  =  873
   INTEGER, PARAMETER             :: Wave3Ayi  =  874
   INTEGER, PARAMETER             :: Wave3Azi  =  875
   INTEGER, PARAMETER             :: Wave4Vxi  =  876
   INTEGER, PARAMETER             :: Wave4Vyi  =  877
   INTEGER, PARAMETER             :: Wave4Vzi  =  878
   INTEGER, PARAMETER             :: Wave4Axi  =  879
   INTEGER, PARAMETER             :: Wave4Ayi  =  880
   INTEGER, PARAMETER             :: Wave4Azi  =  881
   INTEGER, PARAMETER             :: Wave5Vxi  =  882
   INTEGER, PARAMETER             :: Wave5Vyi  =  883
   INTEGER, PARAMETER             :: Wave5Vzi  =  884
   INTEGER, PARAMETER             :: Wave5Axi  =  885
   INTEGER, PARAMETER             :: Wave5Ayi  =  886
   INTEGER, PARAMETER             :: Wave5Azi  =  887
   INTEGER, PARAMETER             :: Wave6Vxi  =  888
   INTEGER, PARAMETER             :: Wave6Vyi  =  889
   INTEGER, PARAMETER             :: Wave6Vzi  =  890
   INTEGER, PARAMETER             :: Wave6Axi  =  891
   INTEGER, PARAMETER             :: Wave6Ayi  =  892
   INTEGER, PARAMETER             :: Wave6Azi  =  893
   INTEGER, PARAMETER             :: Wave7Vxi  =  894
   INTEGER, PARAMETER             :: Wave7Vyi  =  895
   INTEGER, PARAMETER             :: Wave7Vzi  =  896
   INTEGER, PARAMETER             :: Wave7Axi  =  897
   INTEGER, PARAMETER             :: Wave7Ayi  =  898
   INTEGER, PARAMETER             :: Wave7Azi  =  899
   INTEGER, PARAMETER             :: Wave8Vxi  =  900
   INTEGER, PARAMETER             :: Wave8Vyi  =  901
   INTEGER, PARAMETER             :: Wave8Vzi  =  902
   INTEGER, PARAMETER             :: Wave8Axi  =  903
   INTEGER, PARAMETER             :: Wave8Ayi  =  904
   INTEGER, PARAMETER             :: Wave8Azi  =  905
   INTEGER, PARAMETER             :: Wave9Vxi  =  906
   INTEGER, PARAMETER             :: Wave9Vyi  =  907
   INTEGER, PARAMETER             :: Wave9Vzi  =  908
   INTEGER, PARAMETER             :: Wave9Axi  =  909
   INTEGER, PARAMETER             :: Wave9Ayi  =  910
   INTEGER, PARAMETER             :: Wave9Azi  =  911


  ! Internal Degrees of Freedom:

   INTEGER, PARAMETER             :: Q_B1E1    =  912
   INTEGER, PARAMETER             :: Q_B2E1    =  913
   INTEGER, PARAMETER             :: Q_B3E1    =  914
   INTEGER, PARAMETER             :: Q_B1F1    =  915
   INTEGER, PARAMETER             :: Q_B2F1    =  916
   INTEGER, PARAMETER             :: Q_B3F1    =  917
   INTEGER, PARAMETER             :: Q_B1F2    =  918
   INTEGER, PARAMETER             :: Q_B2F2    =  919
   INTEGER, PARAMETER             :: Q_B3F2    =  920
   INTEGER, PARAMETER             :: Q_Teet    =  921
   INTEGER, PARAMETER             :: Q_DrTr    =  922
   INTEGER, PARAMETER             :: Q_GeAz    =  923
   INTEGER, PARAMETER             :: Q_RFrl    =  924
   INTEGER, PARAMETER             :: Q_TFrl    =  925
   INTEGER, PARAMETER             :: Q_Yaw     =  926
   INTEGER, PARAMETER             :: Q_TFA1    =  927
   INTEGER, PARAMETER             :: Q_TSS1    =  928
   INTEGER, PARAMETER             :: Q_TFA2    =  929
   INTEGER, PARAMETER             :: Q_TSS2    =  930
   INTEGER, PARAMETER             :: Q_Sg      =  931
   INTEGER, PARAMETER             :: Q_Sw      =  932
   INTEGER, PARAMETER             :: Q_Hv      =  933
   INTEGER, PARAMETER             :: Q_R       =  934
   INTEGER, PARAMETER             :: Q_P       =  935
   INTEGER, PARAMETER             :: Q_Y       =  936
   INTEGER, PARAMETER             :: QD_B1E1   =  937
   INTEGER, PARAMETER             :: QD_B2E1   =  938
   INTEGER, PARAMETER             :: QD_B3E1   =  939
   INTEGER, PARAMETER             :: QD_B1F1   =  940
   INTEGER, PARAMETER             :: QD_B2F1   =  941
   INTEGER, PARAMETER             :: QD_B3F1   =  942
   INTEGER, PARAMETER             :: QD_B1F2   =  943
   INTEGER, PARAMETER             :: QD_B2F2   =  944
   INTEGER, PARAMETER             :: QD_B3F2   =  945
   INTEGER, PARAMETER             :: QD_Teet   =  946
   INTEGER, PARAMETER             :: QD_DrTr   =  947
   INTEGER, PARAMETER             :: QD_GeAz   =  948
   INTEGER, PARAMETER             :: QD_RFrl   =  949
   INTEGER, PARAMETER             :: QD_TFrl   =  950
   INTEGER, PARAMETER             :: QD_Yaw    =  951
   INTEGER, PARAMETER             :: QD_TFA1   =  952
   INTEGER, PARAMETER             :: QD_TSS1   =  953
   INTEGER, PARAMETER             :: QD_TFA2   =  954
   INTEGER, PARAMETER             :: QD_TSS2   =  955
   INTEGER, PARAMETER             :: QD_Sg     =  956
   INTEGER, PARAMETER             :: QD_Sw     =  957
   INTEGER, PARAMETER             :: QD_Hv     =  958
   INTEGER, PARAMETER             :: QD_R      =  959
   INTEGER, PARAMETER             :: QD_P      =  960
   INTEGER, PARAMETER             :: QD_Y      =  961
   INTEGER, PARAMETER             :: QD2_B1E1  =  962
   INTEGER, PARAMETER             :: QD2_B2E1  =  963
   INTEGER, PARAMETER             :: QD2_B3E1  =  964
   INTEGER, PARAMETER             :: QD2_B1F1  =  965
   INTEGER, PARAMETER             :: QD2_B2F1  =  966
   INTEGER, PARAMETER             :: QD2_B3F1  =  967
   INTEGER, PARAMETER             :: QD2_B1F2  =  968
   INTEGER, PARAMETER             :: QD2_B2F2  =  969
   INTEGER, PARAMETER             :: QD2_B3F2  =  970
   INTEGER, PARAMETER             :: QD2_Teet  =  971
   INTEGER, PARAMETER             :: QD2_DrTr  =  972
   INTEGER, PARAMETER             :: QD2_GeAz  =  973
   INTEGER, PARAMETER             :: QD2_RFrl  =  974
   INTEGER, PARAMETER             :: QD2_TFrl  =  975
   INTEGER, PARAMETER             :: QD2_Yaw   =  976
   INTEGER, PARAMETER             :: QD2_TFA1  =  977
   INTEGER, PARAMETER             :: QD2_TSS1  =  978
   INTEGER, PARAMETER             :: QD2_TFA2  =  979
   INTEGER, PARAMETER             :: QD2_TSS2  =  980
   INTEGER, PARAMETER             :: QD2_Sg    =  981
   INTEGER, PARAMETER             :: QD2_Sw    =  982
   INTEGER, PARAMETER             :: QD2_Hv    =  983
   INTEGER, PARAMETER             :: QD2_R     =  984
   INTEGER, PARAMETER             :: QD2_P     =  985
   INTEGER, PARAMETER             :: QD2_Y     =  986


     ! The maximum number of output channels which can be output by the code.
   INTEGER, PARAMETER             :: MaxOutPts =  986
   

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

!will be part of HydroDyn:
INTEGER,  PARAMETER          :: WaveVxi(9) = (/Wave1Vxi,Wave2Vxi,Wave3Vxi,Wave4Vxi,Wave5Vxi,Wave6Vxi,Wave7Vxi,Wave8Vxi,Wave9Vxi/)
INTEGER,  PARAMETER          :: WaveVyi(9) = (/Wave1Vyi,Wave2Vyi,Wave3Vyi,Wave4Vyi,Wave5Vyi,Wave6Vyi,Wave7Vyi,Wave8Vyi,Wave9Vyi/)
INTEGER,  PARAMETER          :: WaveVzi(9) = (/Wave1Vzi,Wave2Vzi,Wave3Vzi,Wave4Vzi,Wave5Vzi,Wave6Vzi,Wave7Vzi,Wave8Vzi,Wave9Vzi/)
INTEGER,  PARAMETER          :: WaveAxi(9) = (/Wave1Axi,Wave2Axi,Wave3Axi,Wave4Axi,Wave5Axi,Wave6Axi,Wave7Axi,Wave8Axi,Wave9Axi/)
INTEGER,  PARAMETER          :: WaveAyi(9) = (/Wave1Ayi,Wave2Ayi,Wave3Ayi,Wave4Ayi,Wave5Ayi,Wave6Ayi,Wave7Ayi,Wave8Ayi,Wave9Ayi/)
INTEGER,  PARAMETER          :: WaveAzi(9) = (/Wave1Azi,Wave2Azi,Wave3Azi,Wave4Azi,Wave5Azi,Wave6Azi,Wave7Azi,Wave8Azi,Wave9Azi/)
INTEGER,  PARAMETER          :: FairTen(9) = (/Fair1Ten,Fair2Ten,Fair3Ten,Fair4Ten,Fair5Ten,Fair6Ten,Fair7Ten,Fair8Ten,Fair9Ten/)
INTEGER,  PARAMETER          :: FairAng(9) = (/Fair1Ang,Fair2Ang,Fair3Ang,Fair4Ang,Fair5Ang,Fair6Ang,Fair7Ang,Fair8Ang,Fair9Ang/)
INTEGER,  PARAMETER          :: AnchTen(9) = (/Anch1Ten,Anch2Ten,Anch3Ten,Anch4Ten,Anch5Ten,Anch6Ten,Anch7Ten,Anch8Ten,Anch9Ten/)
INTEGER,  PARAMETER          :: AnchAng(9) = (/Anch1Ang,Anch2Ang,Anch3Ang,Anch4Ang,Anch5Ang,Anch6Ang,Anch7Ang,Anch8Ang,Anch9Ang/)


INTEGER(B2Ki), PARAMETER     :: FileFmtID_WithTime    = 1         ! ID for OutputFileFmtID to specify that the time channel should be included in the output file (use if the output can occur at variable times)
INTEGER(B2Ki), PARAMETER     :: FileFmtID_WithoutTime = 2         ! ID for OutputFileFmtID to specify that the time channel does not need to be included in the output file (used only with constant time-step output)


END MODULE StructDyn_Parameters
!**********************************************************************************************************************************
!**********************************************************************************************************************************
! The StructDyn.f90, StructDyn_Types.f90, and StructDyn_Parameters.f90 files make up the StructDyn module of the
! FAST Modularization Framework. StructDyn_Types is auto-generated based on FAST_Registry.txt.
!
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of StructDyn.
!
!    StructDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with StructDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE StructDyn

   USE NWTC_Library

   USE StructDyn_Parameters
   USE StructDyn_Types


   IMPLICIT NONE

!BJJ REMOVE FOR NOW:   PRIVATE

   TYPE(ProgDesc), PARAMETER  :: StrD_Ver = ProgDesc( 'StructDyn', 'v1.00.00a-bjj', '01-January-2013' )



      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: StrD_Init                           ! Initialization routine
   PUBLIC :: StrD_End                            ! Ending routine (includes clean up)

   PUBLIC :: StrD_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                 !   continuous states, and updating discrete states
   PUBLIC :: StrD_CalcOutput                     ! Routine for computing outputs

   PUBLIC :: StrD_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   PUBLIC :: StrD_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   PUBLIC :: StrD_UpdateDiscState                ! Tight coupling routine for updating discrete states

   !PUBLIC :: StrD_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
   !PUBLIC :: StrD_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the continuous
   !                                              !   states (x)
   !PUBLIC :: StrD_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the discrete
   !                                              !   states (xd)
   !PUBLIC :: StrD_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the constraint
   !                                              !   states (z)


CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

      TYPE(StrD_InitInputType),       INTENT(IN   )  :: InitInp     ! Input data for initialization routine
      TYPE(StrD_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input; input mesh must be defined
      TYPE(StrD_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
      TYPE(StrD_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
      TYPE(StrD_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
      TYPE(StrD_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states
      TYPE(StrD_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                    !   only the output mesh is initialized)
      REAL(DbKi),                     INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                                    !   (1) StrD_UpdateStates() is called in loose coupling &
                                                                    !   (2) StrD_UpdateDiscState() is called in tight coupling.
                                                                    !   Input is the suggested time from the glue code;
                                                                    !   Output is the actual coupling interval that will be used
                                                                    !   by the glue code.
      TYPE(StrD_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Local variables

      TYPE(StrD_InputFile)                           :: InputFileData  ! Data stored in the module's input file



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Initialize the NWTC Subroutine Library

      CALL NWTC_Init( )

         ! Display the module information

      CALL DispNVD( StrD_Ver )


         ! Read the input file and validate the data

!      CALL StrD_ReadInput( InitInp%InputFile, InputFileData, ErrStat, ErrMsg )
!      CALL StrD_ValidateInput( InputFileData, p, ErrStat, ErrMsg )


      CALL StrD_InitDOFs( OtherState%DOFs, p, ErrStat, ErrMsg )


         ! Define parameters here:


      p%DT  = Interval


         ! Define initial system states here:

      xd%DummyDiscState          = 0
      z%DummyConstrState         = 0

      ! x =
      ! OtherState =

         ! Define initial guess for the system inputs here:

!      u%DummyInput = 0


         ! Define system output initializations (set up mesh) here:

      y%WriteOutput = 0


         ! Define initialization-routine output here:

      InitOut%WriteOutputHdr = (/ 'Time      ', 'Column2   ' /)
      InitOut%WriteOutputUnt = (/ '(s)',  '(-)'     /)


         ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
         !   this module must be called here:

       !Interval = p%DT


END SUBROUTINE StrD_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(StrD_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(StrD_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(StrD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(StrD_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL StrD_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL StrD_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL StrD_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL StrD_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL StrD_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL StrD_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL StrD_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE StrD_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                      INTENT(IN   ) :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),            INTENT(IN   ) :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(StrD_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at Time;
                                                                    !   Output: Continuous states at Time + Interval
      TYPE(StrD_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time  + Interval
      TYPE(StrD_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at Time;
                                                                    !   Output: Constraint states at Time
      TYPE(StrD_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                  INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(StrD_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at Time
      TYPE(StrD_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                 :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                         :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL StrD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  END IF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL StrD_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL StrD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in StrD_UpdateDiscState(), so StrD_CalcOutput(),
         !   StrD_CalcContStateDeriv(), and StrD_CalcConstrStates() must be called first (see above).

      CALL StrD_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL StrD_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE StrD_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcOutput( Time, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Local variables:

      REAL(ReKi)                   :: AnchTe                        ! Instantaneous effective tension in a mooring line at the anchor   (N  )
      REAL(ReKi)                   :: AnchTeAng                     ! Instantaneous vertical angle    of a mooring line at the anchor   (rad)
      REAL(ReKi)                   :: AngAccEB  (3)                 ! Angular acceleration of the base plate                                                (body B) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: AngAccER  (3)                 ! Angular acceleration of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: AngAccEX  (3)                 ! Angular acceleration of the platform                                                  (body X) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: ComDenom                      ! Common denominator used in several expressions.
      REAL(ReKi)                   :: CThrstys                      ! Estimate of the ys-location of the center of thrust.
      REAL(ReKi)                   :: CThrstzs                      ! Estimate of the zs-location of the center of thrust.
      REAL(ReKi)                   :: FairTe                        ! Instantaneous effective tension in a mooring line at the fairlead (N  )
      REAL(ReKi)                   :: FairTeAng                     ! Instantaneous vertical angle    of a mooring line at the fairlead (rad)
      REAL(ReKi)                   :: FrcMGagB  (3)                 ! Total force at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
      REAL(ReKi)                   :: FrcFGagT  (3)                 ! Total force at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
      REAL(ReKi)                   :: FrcONcRt  (3)                 ! Total force at the yaw bearing (point O  ) due to the nacelle, generator, and rotor.
      REAL(ReKi)                   :: FrcPRot   (3)                 ! Total force at the teeter pin  (point P  ) due to the rotor.
      REAL(ReKi)                   :: FrcT0Trb  (3)                 ! Total force at the base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
      REAL(ReKi)                   :: FZHydro   (3)                 ! Total platform hydrodynamic force at the platform reference (point Z).
      REAL(ReKi)                   :: HHWndVec  (3)                 ! Hub-height wind vector in the AeroDyn coordinate system.
      REAL(ReKi)                   :: LinAccEIMU(3)                 ! Total linear acceleration of the nacelle IMU (point IMU) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: LinAccEO  (3)                 ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: LinAccEZ  (3)                 ! Total linear acceleration of the platform refernce (point Z) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: MomBNcRt  (3)                 ! Total moment at the base plate      (body B) / yaw bearing                           (point O) due to the nacelle, generator, and rotor.
      REAL(ReKi)                   :: MomFGagT  (3)                 ! Total moment at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
      REAL(ReKi)                   :: MomLPRot  (3)                 ! Total moment at the low-speed shaft (body L) / teeter pin                            (point P) due to the rotor.
      REAL(ReKi)                   :: MomMGagB  (3)                 ! Total moment at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
      REAL(ReKi)                   :: MomNGnRt  (3)                 ! Total moment at the nacelle         (body N) / specified point on rotor-furl axis    (point V) due to the structure that furls with the rotor, generator, and rotor.
      REAL(ReKi)                   :: MomNTail  (3)                 ! Total moment at the nacelle         (body N) / specified point on  tail-furl axis    (point W) due to the tail.
      REAL(ReKi)                   :: MomX0Trb  (3)                 ! Total moment at the tower base      (body X) / base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
      REAL(ReKi)                   :: MXHydro   (3)                 ! Total platform hydrodynamic moment acting at the platform (body X) / platform reference (point Z).
      REAL(ReKi)                   :: rOPO      (3)                 ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).
      REAL(ReKi)                   :: rOSTip    (3)                 ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip).
      REAL(ReKi)                   :: rOSTipxn                      ! Component of rOSTip directed along the xn-axis.
      REAL(ReKi)                   :: rOSTipyn                      ! Component of rOSTip directed along the yn-axis.
      REAL(ReKi)                   :: rOSTipzn                      ! Component of rOSTip directed along the zn-axis.
      REAL(ReKi)                   :: rTPT      (3)                 ! Position vector from the undeflected tower node (point T prime) to the deflected node (point T)
      REAL(ReKi)                   :: rSPS      (3)                 ! Position vector from the undeflected blade node (point S prime) to the deflected node (point S)
      REAL(ReKi)                   :: rSTipPSTip(3)                 ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip).
      REAL(ReKi)                   :: TmpVec    (3)                 ! A temporary vector used in various computations.
      REAL(ReKi)                   :: TmpVec2   (3)                 ! A temporary vector.

      INTEGER(IntKi)               :: I                             ! Generic index
      INTEGER(IntKi)               :: J                             ! Loops through nodes / elements.
      INTEGER(IntKi)               :: K                             ! Loops through blades.


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:

!      y%WriteOutput(1) = REAL(Time,ReKi)
!      y%WriteOutput(2) = 1.0_ReKi


   RETURN

END SUBROUTINE StrD_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

!      dxdt%DummyContState = 0


END SUBROUTINE StrD_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                       !   Output: Discrete states at Time + Interval
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE StrD_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_ConstraintStateType), INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                    !     the input values described above
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE StrD_CalcConstrStateResidual
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! WE ARE NOT YET IMPLEMENTING THE JACOBIANS...
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPInput( Time, u, p, x, xd, z, OtherState, dYdu, dXdu, dXddu, dZdu, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the inputs (u). The partial derivatives dY/du, dX/du, dXd/du, and DZ/du are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                   INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),               INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),         INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),           INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),         INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),              INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPInputType),     INTENT(  OUT), OPTIONAL :: dYdu       ! Partial derivatives of output equations
!                                                                                       !   (Y) with respect to the inputs (u)
!      TYPE(StrD_PartialContStatePInputType),  INTENT(  OUT), OPTIONAL :: dXdu       ! Partial derivatives of continuous state
!                                                                                       !   equations (X) with respect to inputs (u)
!      TYPE(StrD_PartialDiscStatePInputType),  INTENT(  OUT), OPTIONAL :: dXddu      ! Partial derivatives of discrete state
!                                                                                       !   equations (Xd) with respect to inputs (u)
!      TYPE(StrD_PartialConstrStatePInputType),INTENT(  OUT), OPTIONAL :: dZdu       ! Partial derivatives of constraint state
!                                                                                       !   equations (Z) with respect to inputs (u)
!      INTEGER(IntKi),                            INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                              INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!      IF ( PRESENT( dYdu ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the inputs (u) here:
!
!         dYdu%DummyOutput%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dXdu ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the inputs (u) here:
!
!         dXdu%DummyContState%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dXddu ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the inputs (u) here:
!
!         dXddu%DummyDiscState%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dZdu ) ) THEN
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the inputs (u) here:
!
!         dZdu%DummyConstrState%DummyInput = 0
!
!      END IF
!
!
!END SUBROUTINE StrD_JacobianPInput
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPContState( Time, u, p, x, xd, z, OtherState, dYdx, dXdx, dXddx, dZdx, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the continuous states (x). The partial derivatives dY/dx, dX/dx, dXd/dx, and DZ/dx are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                    INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPContStateType),     INTENT(  OUT), OPTIONAL :: dYdx       ! Partial derivatives of output equations
!                                                                                           !   (Y) with respect to the continuous
!                                                                                           !   states (x)
!      TYPE(StrD_PartialContStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXdx       ! Partial derivatives of continuous state
!                                                                                           !   equations (X) with respect to
!                                                                                           !   the continuous states (x)
!      TYPE(StrD_PartialDiscStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXddx      ! Partial derivatives of discrete state
!                                                                                           !   equations (Xd) with respect to
!                                                                                           !   the continuous states (x)
!      TYPE(StrD_PartialConstrStatePContStateType),INTENT(  OUT), OPTIONAL :: dZdx       ! Partial derivatives of constraint state
!                                                                                           !   equations (Z) with respect to
!                                                                                           !   the continuous states (x)
!      INTEGER(IntKi),                                INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                  INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!
!      IF ( PRESENT( dYdx ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the continuous states (x) here:
!
!         dYdx%DummyOutput%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdx ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the continuous states (x) here:
!
!         dXdx%DummyContState%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddx ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the continuous states (x) here:
!
!         dXddx%DummyDiscState%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdx ) ) THEN
!
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the continuous states (x) here:
!
!         dZdx%DummyConstrState%DummyContState = 0
!
!      END IF
!
!
!   END SUBROUTINE StrD_JacobianPContState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPDiscState( Time, u, p, x, xd, z, OtherState, dYdxd, dXdxd, dXddxd, dZdxd, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the discrete states (xd). The partial derivatives dY/dxd, dX/dxd, dXd/dxd, and DZ/dxd are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                    INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPDiscStateType),     INTENT(  OUT), OPTIONAL :: dYdxd      ! Partial derivatives of output equations
!                                                                                           !  (Y) with respect to the discrete
!                                                                                           !  states (xd)
!      TYPE(StrD_PartialContStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXdxd      ! Partial derivatives of continuous state
!                                                                                           !   equations (X) with respect to the
!                                                                                           !   discrete states (xd)
!      TYPE(StrD_PartialDiscStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXddxd     ! Partial derivatives of discrete state
!                                                                                           !   equations (Xd) with respect to the
!                                                                                           !   discrete states (xd)
!      TYPE(StrD_PartialConstrStatePDiscStateType),INTENT(  OUT), OPTIONAL :: dZdxd      ! Partial derivatives of constraint state
!                                                                                           !   equations (Z) with respect to the
!                                                                                           !   discrete states (xd)
!      INTEGER(IntKi),                                INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                  INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!      IF ( PRESENT( dYdxd ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the discrete states (xd) here:
!
!         dYdxd%DummyOutput%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdxd ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the discrete states (xd) here:
!
!         dXdxd%DummyContState%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddxd ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the discrete states (xd) here:
!
!         dXddxd%DummyDiscState%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdxd ) ) THEN
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the discrete states (xd) here:
!
!         dZdxd%DummyConstrState%DummyDiscState = 0
!
!      END IF
!
!
!
!END SUBROUTINE StrD_JacobianPDiscState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPConstrState( Time, u, p, x, xd, z, OtherState, dYdz, dXdz, dXddz, dZdz, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the constraint states (z). The partial derivatives dY/dz, dX/dz, dXd/dz, and DZ/dz are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                      INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                         INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                     INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),               INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),                 INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),               INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                    INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPConstrStateType),     INTENT(  OUT), OPTIONAL :: dYdz       ! Partial derivatives of output
!                                                                                             !  equations (Y) with respect to the
!                                                                                             !  constraint states (z)
!      TYPE(StrD_PartialContStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXdz       ! Partial derivatives of continuous
!                                                                                             !  state equations (X) with respect to
!                                                                                             !  the constraint states (z)
!      TYPE(StrD_PartialDiscStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXddz      ! Partial derivatives of discrete state
!                                                                                             !  equations (Xd) with respect to the
!                                                                                             !  constraint states (z)
!      TYPE(StrD_PartialConstrStatePConstrStateType),INTENT(  OUT), OPTIONAL :: dZdz       ! Partial derivatives of constraint
!                                                                                             ! state equations (Z) with respect to
!                                                                                             !  the constraint states (z)
!      INTEGER(IntKi),                                  INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                    INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!      IF ( PRESENT( dYdz ) ) THEN
!
!            ! Calculate the partial derivative of the output equations (Y) with respect to the constraint states (z) here:
!
!         dYdz%DummyOutput%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdz ) ) THEN
!
!            ! Calculate the partial derivative of the continuous state equations (X) with respect to the constraint states (z) here:
!
!         dXdz%DummyContState%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddz ) ) THEN
!
!            ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the constraint states (z) here:
!
!         dXddz%DummyDiscState%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdz ) ) THEN
!
!            ! Calculate the partial derivative of the constraint state equations (Z) with respect to the constraint states (z) here:
!
!         dZdz%DummyConstrState%DummyConstrState = 0
!
!      END IF
!
!
!END SUBROUTINE StrD_JacobianPConstrState
!!----------------------------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_ReadInput( InputFileName, InputFileData, ErrStat, ErrMsg )
! This subroutine reads the input file and stores all the data in the StrD_InputFile structure.
! It does not perform data validation.
!..................................................................................................................................

      ! Passed variables

   CHARACTER(*), INTENT(IN)               :: InputFileName  ! Name of the input file

   TYPE(StrD_InputFile), INTENT(OUT)      :: InputFileData  ! Data stored in the module's input file
   INTEGER(IntKi),       INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),         INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! local variables

   INTEGER(IntKi)                         :: UnIn           ! Unit number for the input file
   INTEGER(IntKi)                         :: UnEcho         ! Unit number for the echo file

   ErrStat = ErrID_None
   ErrMsg  = ''


!===================== FAST_Input

   CALL ExitThisRoutine(ErrID_None, '')


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
         ErrMsg = 'Error in StrD_ReadInput: '//TRIM(ErrMsg)
      END IF


   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_ReadInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_ValidateInput( InputFileData, p, ErrStat, ErrMsg )
! This subroutine validates the input file data
!..................................................................................................................................

   TYPE(StrD_InputFile),     INTENT(IN)       :: InputFileData  ! Data stored in the module's input file
   TYPE(StrD_ParameterType), INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! local variables

   INTEGER(IntKi)                             :: UnIn           ! Unit number for input file


      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) THEN
      RETURN
   ELSE
      ErrStat = ErrID_Info
   END IF

!!!!!!!!!!!!!!!!!!!
      ! Check to see if any inputted output channels are ill-conditioned (and if so, Abort)
   !    and set values for OutParam(:):

  ! CALL ChckOutLst( InputFileData%OutList, p, ErrStat, ErrMsg )

!!!!!!!!!!!!!!!!!

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
         ErrMsg = 'Error in StrD_ValidateInput: '//TRIM(ErrMsg)
      END IF


      !.........................................................................................................................
      ! Close file
      !.........................................................................................................................
      CLOSE(UnIn)

   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_ValidateInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_InitDOFs( DOFs, p, ErrStat, ErrMsg )
! This subroutine initialized the ActiveDOF data type
!..................................................................................................................................

   TYPE(ActiveDOFs),         INTENT(INOUT)    :: DOFs           ! ActiveDOF data 
   TYPE(StrD_ParameterType), INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! Local variables
   INTEGER(IntKi)                             :: K              ! Loop counter (for blades)
      
      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''

   
   
      ! BJJ: note that this method will cause an error if allocating data that has already been allocated...

   ALLOCATE ( DOFs%NPSBE(p%NumBl), DOFs%NPSE(p%NumBl),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs NPSBE and NPSE arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%PCE(p%NDOF), DOFs%PDE(p%NDOF), DOFs%PIE(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PCE, PDE, and PIE arrays.' )
      RETURN
   ENDIF
   
   
   ALLOCATE (  DOFs%PTTE(p%NDOF), DOFs%PTE(p%NDOF), DOFs%PS(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PTTE, PTE, and PS arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%PUE(p%NDOF), DOFs%PYE(p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PUE and PYE arrays.' )
      RETURN
   ENDIF

   
!bjj was   ALLOCATE ( DOFs%PSBE(p%NumBl,3), DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   ALLOCATE ( DOFs%PSBE(p%NumBl,(NumBE+NumBF)), DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PSBE and PSE arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%SrtPS(p%NDOF), DOFs%SrtPSNAUG(p%NAug),  DOFs%Diag(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs SrtPS, SrtPSNAUG, and Diag arrays.' )
      RETURN
   ENDIF
   

   !...............................................................................................................................
! BJJ: these are now parameters....

   !...............................................................................................................................
   
      ! Allocate and Initialize arrays for DOFS that contribute to the angular velocity of the hub and blade elements
   
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
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, DOF_Teet, & 
                           DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                                   /)
      ENDDO          ! K - All blades

   ELSE                    ! 3-blader

         ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements (body M) in the inertia frame:
      DO K = 1,p%NumBl ! Loop through all blades
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, &           
                           DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                         /)
      ENDDO          ! K - All blades

   ENDIF

   
   
   
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
         ErrMsg = 'Error in StrD_AllocDOFs: '//TRIM(ErrMsg)
      END IF


   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_InitDOFs
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION SHP(Fract, FlexL, ModShpAry, Deriv, ErrStat, ErrMsg)
! SHP calculates the Derive-derivative of the shape function ModShpAry at Fract.
! NOTE: This function only works for Deriv = 0, 1, or 2.
!----------------------------------------------------------------------------------------------------------------------------------

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
SUBROUTINE CoordSys_Alloc( CoordSys, p, ErrStat, ErrMsg )

   ! This subroutine allocates the coordinate systems in the StrD_CoordSys type.

IMPLICIT NONE

   ! passed arguments

TYPE(StrD_CoordSys),      INTENT(OUT) :: CoordSys       ! The coordinate systems, with arrays to be allocated
TYPE(StrD_ParameterType), INTENT(IN)  :: p              ! Parameters of the structural dynamics module

INTEGER(IntKi),           INTENT(OUT) :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT) :: ErrMsg         ! Err msg


   ! local variables

CHARACTER(200), PARAMETER        :: ErrTxt = 'coordinate system arrays in SUBROUTINE CoordSys_Alloc.'


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
END SUBROUTINE CoordSys_Alloc
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadBladeFile ( BldFile, p, BladeKInputFileData, ReadAdmVals, ErrStat, ErrMsg )


   ! This routine reads a blade file and validates the input.

IMPLICIT                        NONE


   ! Passed variables:

TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
CHARACTER(*),             INTENT(IN)     :: BldFile                             ! Name of the blade input file data
LOGICAL,                  INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file 

INTEGER(IntKi),           INTENT(OUT)    :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT)    :: ErrMsg         ! Err msg


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

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 1', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 2', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 3', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile, 'blade parameters', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   ! NBlInpSt - Number of blade input stations.

CALL ReadVar ( UnIn, BldFile, BladeKInputFileData%NBlInpSt, 'NBlInpSt', 'Number of blade input stations', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN
   

   ! Allocate the arrays based on this NBlInpSt input 
CALL Alloc_BladeInputProperties( BladeKInputFileData, ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   
   ! CalcBMode - Calculate blade mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadVar ( UnIn, BldFile, BladeKInputFileData%CalcBMode, 'CalcBMode', 'Calculate blade mode shapes' )
CALL ReadCom ( UnIn, BldFile, 'currently ignored CalcBMode', ErrStat2, ErrMsg2  )
   BladeKInputFileData%CalcBMode = .FALSE.
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFlDmp - Blade structural damping ratios in flapwise direction.

CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldFlDmp, SIZE(BladeKInputFileData%BldFlDmp), 'BldFlDmp', &
                                    'Blade structural damping ratios in flapwise direction', ErrStat2  )
   ErrMsg2 = ' Error reading BldFlDmp array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! BldEdDmp - Blade structural damping ratios in edgewise direction.

CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldEdDmp, SIZE(BladeKInputFileData%BldEdDmp), 'BldEdDmp', &
                                    'Blade structural damping ratios in edgewise direction', ErrStat2 )
   ErrMsg2 = ' Error reading BldEdDmp array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

!  -------------- BLADE ADJUSTMENT FACTORS -------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile, 'blade adjustment factors', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   
   ! FlStTunr(1) - Blade flapwise modal stiffness tuners.

CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%FlStTunr, SIZE(BladeKInputFileData%FlStTunr), 'FlStTunr', &
                                               'Blade flapwise modal stiffness tuners', ErrStat2 )
   ErrMsg2 = ' Error reading FlStTunr array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjBlMs - Factor to adjust blade mass density.

CALL ReadVar ( UnIn, BldFile, AdjBlMs, 'AdjBlMs', 'Factor to adjust blade mass density', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjFlSt - Factor to adjust blade flap stiffness.

CALL ReadVar ( UnIn, BldFile, AdjFlSt, 'AdjFlSt', 'Factor to adjust blade flap stiffness', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjEdSt - Factor to adjust blade edge stiffness.

CALL ReadVar ( UnIn, BldFile, AdjEdSt, 'AdjEdSt', 'Factor to adjust blade edge stiffness', ErrStat2, ErrMsg2 )
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

CALL ReadCom ( UnIn, BldFile, 'distributed blade parameters'     , ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter names', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter units', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! Read the table.

IF ( ReadAdmVals ) THEN
   NInputCols = 17
ELSE
   NInputCols = 6
END IF   


DO I=1,BladeKInputFileData%NBlInpSt

   CALL ReadAry( UnIn, BldFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Blade input station table', ErrStat2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   BladeKInputFileData%BlFract( I) = TmpRAry(1)
   BladeKInputFileData%AerCen(  I) = TmpRAry(2)
   BladeKInputFileData%StrcTwst(I) = TmpRAry(3)
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

CALL ReadCom ( UnIn, BldFile, 'blade mode shapes', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFl1Sh - Blade-flap mode-1 shape coefficients.
CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl1Sh, SIZE(BladeKInputFileData%BldFl1Sh), 'BldFl1Sh', &
                        'Blade-flap mode-1 shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldFl1Sh array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFl2Sh - Blade-flap mode-2 shape coefficients.

CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl2Sh, SIZE(BladeKInputFileData%BldFl2Sh), 'BldFl2Sh', &
                 'Blade-flap mode-2 shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldFl2Sh array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldEdgSh - Blade-edge mode shape coefficients.
   
CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldEdgSh, SIZE(BladeKInputFileData%BldEdgSh), 'BldEdgSh', &
                  'Blade-edge mode shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldEdgSh array from '//TRIM(BldFile)//'.'
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
SUBROUTINE Alloc_BladeInputProperties( BladeKInputFileData, ErrStat, ErrMsg )
! This routine allocates arrays for the blade properties from the input file
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat        ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg         ! Err msg

   
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

   
      ! BJJ: note that these used to be allocated 2:PolyOrd  :
   
   CALL AllocAry  ( BladeKInputFileData%BldFl1Sh,  PolyOrd-1, 'BldFl1Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldFl2Sh,  PolyOrd-1, 'BldFl2Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldEdgSh,  PolyOrd-1, 'BldEdgSh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
   
END SUBROUTINE Alloc_BladeInputProperties
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateBladeData ( p, BladeKInputFileData, ChkAdmVals, ErrStat, ErrMsg )
! This routine checks the blade file input data for errors
!----------------------------------------------------------------------------------------------------------------------------------
   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   LOGICAL,                  INTENT(IN)     :: ChkAdmVals                          ! Logical to determine if Adams inputs should be validated 
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
      
         ! Check that StrcTwst is contained in (-180.0, 180.0]:
      IF ( ( BladeKInputFileData%StrcTwst(I) <= -180.0_ReKi ) .OR. ( BladeKInputFileData%StrcTwst(I) > 180.0_ReKi ) )  THEN
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
   
   
   IF ( ChkAdmVals ) THEN  ! Check values for Adams input

      
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
!----------------------------------------------------------------------------------------------------------------------------------
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
SUBROUTINE SetBladeParams( p, BladeInData, SetAdmVals, ErrStat, ErrMsg )
! This takes the blade input file data and sets the corresponding blade parameters, performing linear interpolation of the
! input data to the specified blade mesh.
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeInData(:)                      ! Program input data for all blades
   LOGICAL,                  INTENT(IN)     :: SetAdmVals                          ! Logical to determine if Adams inputs should be set 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message
   
      ! Local variables:
   REAL(ReKi)                               :: x                                   ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                          :: K                                   ! Blade number
   INTEGER(IntKi )                          :: J                                   ! Index for the node arrays
   INTEGER(IntKi)                           :: InterpInd                           ! Index for the interpolation routine

   ErrStat = ErrID_None
   ErrMsg  = ''
   
   
   CALL Alloc_BladeParameters( p, SetAdmVals, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

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
       !p%CalcBModes(K) = BladeInData(K)%CalcBMode
     
      p%BldEdgSh(:,K) = BladeInData(K)%BldEdgSh
      p%BldFl1Sh(:,K) = BladeInData(K)%BldFl1Sh
      p%BldFl2Sh(:,K) = BladeInData(K)%BldFl2Sh
      

   END DO ! ( Blades )

   
   p%ThetaS  = D2R*p%ThetaS
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
END SUBROUTINE SetBladeParams
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_BladeParameters( p, SetAdmVals, ErrStat, ErrMsg )
! This routine allocates arrays for the blade parameters.
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   LOGICAL,                  INTENT(IN)     :: SetAdmVals                          ! Logical to determine if Adams inputs should be set 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Err msg

         
   
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

   
!IF ( SetAdmVals ) THEN      
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
!END IF
   
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
SUBROUTINE ReadTowerFile( p, InputFileData, TwrFile, ReadAdmVals, ErrStat, ErrMsg )
! This routine reads the tower file  input.
!----------------------------------------------------------------------------------------------------------------------------------

   IMPLICIT                        NONE

      ! Passed variables:

   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
   TYPE(StrD_InputFile),     INTENT(INOUT)  :: InputFileData                       ! All the data in the StructDyn input file
   CHARACTER(*),             INTENT(IN)     :: TwrFile                             ! Name of the tower input file data
   LOGICAL,                  INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file 

   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat        ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg         ! Err msg


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
   !IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Tower input data from file "'//TRIM( TwrFile )//'":'


   !  -------------- FILE HEADER ---------------------------------------------------

   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 1', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 2', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'unused tower file header line 3', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !  -------------- TOWER PARAMETERS ---------------------------------------------

   CALL ReadCom ( UnIn, TwrFile, 'heading for tower parameters', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! NTwInpSt - Number of tower input stations.

   CALL ReadVar ( UnIn, TwrFile, InputFileData%NTwInpSt, 'NTwInpSt', 'Number of tower input stations', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   
      ! Allocate the input arrays based on this NTwInpSt input 
   CALL Alloc_TowerInputProperties( InputFileData, ErrStat, ErrMsg )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   

      ! CalcTMode - Calculate tower mode shapes (switch).

   !JASON: ADD LOGIC FOR THIS NEW VARIABLE:
   !JASON:CALL ReadVar ( UnIn, TwrFile, InputFileData%CalcTMode, 'CalcTMode', 'Calculate tower mode shapes', ErrStat2, ErrMsg2 )
   CALL ReadCom ( UnIn, TwrFile, 'currently ignored CalcTMode', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwrFADmp - Tower fore-aft structural damping ratios.

   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwrFADmp, SIZE(InputFileData%TwrFADmp), 'TwrFADmp', & 
                                     'Tower fore-aft structural damping ratios', ErrStat2 )
      ErrMsg2 = ' Error reading TwrFADmp array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   
      ! TwrSSDmp - Tower side-to-side structural damping ratios.

   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwrSSDmp, SIZE(InputFileData%TwrSSDmp), 'TwrSSDmp', & 
                                     'Tower side-to-side structural damping ratios', ErrStat2 )
      ErrMsg2 = ' Error reading TwrSSDmp array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
  

   !  -------------- TOWER ADJUSTMENT FACTORS -------------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower adjustment factors', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! FAStTunr - Tower fore-aft modal stiffness tuners.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%FAStTunr, SIZE(InputFileData%FAStTunr), 'FAStTunr', & 
                                     'Tower fore-aft modal stiffness tuners', ErrStat2 )
      ErrMsg2 = ' Error reading FAStTunr array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! SSStTunr - Tower side-to-side modal stiffness tuners.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%SSStTunr, SIZE(InputFileData%SSStTunr), 'SSStTunr', & 
                                     'Tower side-to-side modal stiffness tuners', ErrStat2 )
      ErrMsg2 = ' Error reading SSStTunr array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! AdjTwMa - Factor to adjust tower mass density.

   CALL ReadVar ( UnIn, TwrFile, AdjTwMa, 'AdjTwMa', 'Factor to adjust tower mass density', ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjFASt - Factor to adjust tower fore-aft stiffness.

   CALL ReadVar ( UnIn, TwrFile, AdjFASt, 'AdjFASt', 'Factor to adjust tower fore-aft stiffness', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! AdjSSSt - Factor to adjust tower side-to-side stiffness.

   CALL ReadVar ( UnIn, TwrFile, AdjSSSt, 'AdjSSSt', 'Factor to adjust tower side-to-side stiffness', ErrStat2, ErrMsg2  )
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
   CALL ReadCom ( UnIn, TwrFile, 'heading for distributed tower parameters', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter names', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter units', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! Read the table.

   IF ( ReadAdmVals ) THEN
      NInputCols = 10
   ELSE
      NInputCols = 4
   END IF   


   DO I=1,InputFileData%NTwInpSt

      CALL ReadAry( UnIn, TwrFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Tower input station table', ErrStat2 )
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
   ENDDO ! I   
   

   !  -------------- TOWER FORE-AFT MODE SHAPES -----------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower fore-aft mode shapes', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! TwFAM1Sh - Tower fore-aft mode-1 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwFAM1Sh, SIZE(InputFileData%TwFAM1Sh), 'TwFAM1Sh', &
                           'Tower fore-aft mode-1 shape coefficients', ErrStat2 )
      ErrMsg2 = ' Error reading TwFAM1Sh array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   
      ! TwFAM2Sh - Tower fore-aft mode-2 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwFAM2Sh, SIZE(InputFileData%TwFAM2Sh), 'TwFAM1Sh', &
                           'Tower fore-aft mode-2 shape coefficients', ErrStat2 )
      ErrMsg2 = ' Error reading TwFAM2Sh array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- TOWER SIDE-TO-SIDE MODE SHAPES -------------------------------


      ! Skip the comment line.
   CALL ReadCom ( UnIn, TwrFile, 'heading for tower side-to-side mode shapes', ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! TwSSM1Sh - Tower side-to-side mode-1 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwSSM1Sh, SIZE(InputFileData%TwSSM1Sh), 'TwSSM1Sh', &
                           'Tower side-to-side mode-1 shape coefficients', ErrStat2 )
      ErrMsg2 = ' Error reading TwSSM1Sh array from '//TRIM(TwrFile)//'.'
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN



      ! TwSSM2Sh - Tower side-to-side mode-2 shape coefficients.
   CALL ReadAryLines ( UnIn, TwrFile, InputFileData%TwSSM2Sh, SIZE(InputFileData%TwSSM2Sh), 'TwSSM2Sh', &
                           'Tower side-to-side mode-2 shape coefficients', ErrStat2 )
      ErrMsg2 = ' Error reading TwSSM2Sh array from '//TRIM(TwrFile)//'.'
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
SUBROUTINE ValidateTowerData ( p, InputFileData, ChkAdmVals, ErrStat, ErrMsg )
! This routine checks the tower file input data for errors
!----------------------------------------------------------------------------------------------------------------------------------
   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
   TYPE(StrD_InputFile),     INTENT(INOUT)  :: InputFileData                       ! Data stored in the module's input file
   LOGICAL,                  INTENT(IN)     :: ChkAdmVals                          ! Logical to determine if Adams inputs should be validated 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

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
   
   
   IF (ChkAdmVals) THEN
      
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
SUBROUTINE Alloc_TowerInputProperties( InputFileData, ErrStat, ErrMsg )
! This routine allocates arrays for the tower properties from the input file
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(StrD_InputFile),     INTENT(INOUT)  :: InputFileData      ! All the data in the StructDyn input file
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
SUBROUTINE Alloc_TowerParameters( p, SetAdmVals, ErrStat, ErrMsg )
! This routine allocates arrays for the tower parameters.
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   LOGICAL,                  INTENT(IN)     :: SetAdmVals                          ! Logical to determine if Adams inputs should be set 
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
   
!IF ( SetAdmVals ) THEN      
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
!END IF

      ! these are for HydroDyn? 
   CALL AllocAry  ( p%DiamT,         p%TwrNodes, 'DiamT'     , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%CAT,           p%TwrNodes, 'CAT'       , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( p%CDT,           p%TwrNodes, 'CDT'       , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN     
      
  
   
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
SUBROUTINE SetTowerParams( p, InputFileData, SetAdmVals, ErrStat, ErrMsg  )
! This takes the tower input file data and sets the corresponding tower parameters, performing linear interpolation of the
! input data to the specified tower mesh.
!----------------------------------------------------------------------------------------------------------------------------------

   IMPLICIT                        NONE


      ! Passed variables
   
   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                            ! Parameters of the structural dynamics module
   TYPE(StrD_InputFile),     INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   LOGICAL,                  INTENT(IN)     :: SetAdmVals                   ! Logical to determine if Adams inputs should be set 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message

      ! Local variables:

   REAL(ReKi)                               :: x                            ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                          :: J                            ! Index for the node arrays
   INTEGER(IntKi)                           :: InterpInd                    ! Index for the interpolation routine


      ! Initialize data      
   ErrStat = ErrID_None
   ErrMsg  = ''
      
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
      
      IF ( SetAdmVals )  THEN          ! An ADAMS model will be created; thus, read in all the cols.
         p%StiffTGJ  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwGJStif, InterpInd, InputFileData%NTwInpSt )           
         p%StiffTEA  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwEAStif, InterpInd, InputFileData%NTwInpSt )           
         p%InerTFA   (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwFAIner, InterpInd, InputFileData%NTwInpSt )           
         p%InerTSS   (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwSSIner, InterpInd, InputFileData%NTwInpSt )           
         p%cgOffTFA  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwFAcgOf, InterpInd, InputFileData%NTwInpSt )           
         p%cgOffTSS  (J) = InterpStp( p%HNodesNorm(J), InputFileData%HtFract, InputFileData%TwSScgOf, InterpInd, InputFileData%NTwInpSt )           
      END IF      
      
   END DO ! J

   
   
   !...............................................................................................................................   
   ! Set other tower parameters:
   !...............................................................................................................................   

   p%TTopNode = p%TwrNodes + 1
   

      ! these are for HydroDyn ?
   p%DiamT(:) = InputFileData%TwrDiam
   p%CAT(:)   = InputFileData%TwrCA
   p%CDT(:)   = InputFileData%TwrCD
        
   
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
   
END SUBROUTINE SetTowerParams
!----------------------------------------------------------------------------------------------------------------------------------

END MODULE StructDyn
!**********************************************************************************************************************************

