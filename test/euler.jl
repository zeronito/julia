# This file is a part of Julia. License is MIT: https://julialang.org/license

## Project Euler
#
#  problems: http://projecteuler.net/problems
#  solutions: https://code.google.com/p/projecteuler-solutions/wiki/ProjectEulerSolutions

#1: 233168
@test sum(filter(n->(n%3==0)|(n%5==0),1:999)) == 233168

#2: 4613732
function euler2(n)
    t, i, j = 0, 1, 2
    while j <= n
          t += j
          i, j = j, i+j
          i, j = j, i+j
          i, j = j, i+j
    end
    return t
end
@test euler2(4000000) == 4613732

#4: 906609
function euler4(n)
    m = 1
    for a = 10^n-1:-1:10^(n-1)
        for b = 10^n-1:-1:max(a,-fld(-m,a))
            p = a*b
            d = digits(p)
            if d == reverse(d) && p > m
                m = p
                b < -fld(-m,a) && break
            end
        end
    end
    return m
end
@test euler4(3) == 906609

#5: 232792560
@test lcm(1:20) == 232792560

#6: 25164150
@test sum(1:100)^2 - sum((1:100).^2) == 25164150

#8: 40824
function euler8(n,m)
    d = digits(n)
    maximum([prod(d[k:k+m-1]) for k=1:length(d)-m+1])
end
let n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
    @test euler8(n,5) == 40824
end

#9: 31875000
function euler9(n)
    for a = 1:n, b = 1:n-a, c = n-a-b
        a^2 + b^2 == c^2 && return a*b*c
    end
end
@test euler9(1000) == 31875000


#11: 70600674
function euler11(grid,n)
    m = typemin(eltype(grid))
    for i = n:size(grid,1)-n+1,
        j = n:size(grid,2)-n+1,
        di = -1:1, dj = -1:1
        di == dj == 0 && continue
        idx = sub2ind(size(grid),
                      di==0 ? fill(i,n) : range(i,di,n),
                      dj==0 ? fill(j,n) : range(j,dj,n))
        m = max(m,prod(grid[idx]))
    end
    return m
end
let grid = [
    08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
]
    @test euler11(grid,4) == 70600674
end

#12: 76576500

#13: 5537376230
let nums = [
    37107287533902102798797998220837590246510135740250
    46376937677490009712648124896970078050417018260538
    74324986199524741059474233309513058123726617309629
    91942213363574161572522430563301811072406154908250
    23067588207539346171171980310421047513778063246676
    89261670696623633820136378418383684178734361726757
    28112879812849979408065481931592621691275889832738
    44274228917432520321923589422876796487670272189318
    47451445736001306439091167216856844588711603153276
    70386486105843025439939619828917593665686757934951
    62176457141856560629502157223196586755079324193331
    64906352462741904929101432445813822663347944758178
    92575867718337217661963751590579239728245598838407
    58203565325359399008402633568948830189458628227828
    80181199384826282014278194139940567587151170094390
    35398664372827112653829987240784473053190104293586
    86515506006295864861532075273371959191420517255829
    71693888707715466499115593487603532921714970056938
    54370070576826684624621495650076471787294438377604
    53282654108756828443191190634694037855217779295145
    36123272525000296071075082563815656710885258350721
    45876576172410976447339110607218265236877223636045
    17423706905851860660448207621209813287860733969412
    81142660418086830619328460811191061556940512689692
    51934325451728388641918047049293215058642563049483
    62467221648435076201727918039944693004732956340691
    15732444386908125794514089057706229429197107928209
    55037687525678773091862540744969844508330393682126
    18336384825330154686196124348767681297534375946515
    80386287592878490201521685554828717201219257766954
    78182833757993103614740356856449095527097864797581
    16726320100436897842553539920931837441497806860984
    48403098129077791799088218795327364475675590848030
    87086987551392711854517078544161852424320693150332
    59959406895756536782107074926966537676326235447210
    69793950679652694742597709739166693763042633987085
    41052684708299085211399427365734116182760315001271
    65378607361501080857009149939512557028198746004375
    35829035317434717326932123578154982629742552737307
    94953759765105305946966067683156574377167401875275
    88902802571733229619176668713819931811048770190271
    25267680276078003013678680992525463401061632866526
    36270218540497705585629946580636237993140746255962
    24074486908231174977792365466257246923322810917141
    91430288197103288597806669760892938638285025333403
    34413065578016127815921815005561868836468420090470
    23053081172816430487623791969842487255036638784583
    11487696932154902810424020138335124462181441773470
    63783299490636259666498587618221225225512486764533
    67720186971698544312419572409913959008952310058822
    95548255300263520781532296796249481641953868218774
    76085327132285723110424803456124867697064507995236
    37774242535411291684276865538926205024910326572967
    23701913275725675285653248258265463092207058596522
    29798860272258331913126375147341994889534765745501
    18495701454879288984856827726077713721403798879715
    38298203783031473527721580348144513491373226651381
    34829543829199918180278916522431027392251122869539
    40957953066405232632538044100059654939159879593635
    29746152185502371307642255121183693803580388584903
    41698116222072977186158236678424689157993532961922
    62467957194401269043877107275048102390895523597457
    23189706772547915061505504953922979530901129967519
    86188088225875314529584099251203829009407770775672
    11306739708304724483816533873502340845647058077308
    82959174767140363198008187129011875491310547126581
    97623331044818386269515456334926366572897563400500
    42846280183517070527831839425882145521227251250327
    55121603546981200581762165212827652751691296897789
    32238195734329339946437501907836945765883352399886
    75506164965184775180738168837861091527357929701337
    62177842752192623401942399639168044983993173312731
    32924185707147349566916674687634660915035914677504
    99518671430235219628894890102423325116913619626622
    73267460800591547471830798392868535206946944540724
    76841822524674417161514036427982273348055556214818
    97142617910342598647204516893989422179826088076852
    87783646182799346313767754307809363333018982642090
    10848802521674670883215120185883543223812876952786
    71329612474782464538636993009049310363619763878039
    62184073572399794223406235393808339651327408011116
    66627891981488087797941876876144230030984490851411
    60661826293682836764744779239180335110989069790714
    85786944089552990653640447425576083659976645795096
    66024396409905389607120198219976047599490197230297
    64913982680032973156037120041377903785566085089252
    16730939319872750275468906903707539413042652315011
    94809377245048795150954100921645863754710598436791
    78639167021187492431995700641917969777599028300699
    15368713711936614952811305876380278410754449733078
    40789923115535562561142322423255033685442488917353
    44889911501440648020369068063960672322193204149535
    41503128880339536053299340368006977710650566631954
    81234880673210146739058568557934581403627822703280
    82616570773948327592232845941706525094512325230608
    22918802058777319719839450180888072429661980811197
    77158542502016545090413245809786882778948721859617
    72107838435069186155435662884062257473692284509516
    20849603980134001723930671666823555245252804609722
    53503534226472524250874054075591789781264330331690
]
    @test sum(digits(sum(nums))[end-9:end].*Int64(10).^(0:9)) == 5537376230
end

#14: 837799
function euler14(m)
    c = zeros(Int,m)
    c[1] = 1
    for n::Int64 = 2:m
        nʹ, d = n, 0
        while nʹ > length(c) || c[nʹ] == 0
            nʹ = iseven(nʹ) ? nʹ>>1 : 3nʹ+1
            d += 1
        end
        d += c[nʹ]
        while n > length(c) || c[n] == 0
            n <= length(c) && (c[n] = d)
            n = iseven(n) ? n>>1 : 3n+1
            d -= 1
        end
    end
    indmax(c)
end
@test euler14(999999) == 837799

#15: 137846528820

#16: 1366
@test sum(digits(big(2)^1000)) == 1366

#17: 21124
#18: 1074
#19: 171

#20: 648
@test sum(digits(factorial(big(100)))) == 648

#21: 31626
#22: 871198282
#23: 4179871
#24: 2783915460
#25: 4782
#26: 983
#27: -59231
#28: 669171001
#29: 9183
#30: 443839
#31: 73682
#32: 45228
#33: 100
#34: 40730
#35: 55
#36: 872187
#37: 748317
#38: 932718654
#39: 840
#40: 210
#41: 7652413
#42: 162
#43: 16695334890
#44: 5482660
#45: 1533776805
#46: 5777
#47: 134043
#48: 9110846700
#49: 296962999629
#50: 997651
#51: 121313
#52: 142857
#53: 4075
#54: 376
#55: 249
#56: 972
#57: 153
#58: 26241
#59: 107359
#60: 26033
#61: 28684
#62: 127035954683
#63: 49
#64: 1322
#65: 272
#66: 661
#67: 7273
#68: 6531031914842725
#69: 510510
#70: 8319823
#71: 428570
#72: 303963552391
#73: 7295372
#74: 402
#75: 161667
#76: 190569291
#77: 71
#78: 55374
#79: 73162890
#80: 40886
#81: 427337
#82: 260324
#83: 425185
#84: 101524
#85: 2772
#86: 1818
#87: 1097343
#88: 7587457
#89: 743
#90: 1217
#91: 14234
#92: 8581146
#93: 1258
#94: 518408346
#95: 14316
#96: 24702
#97: 8739992577
#98: 18769
#99: 709
#100: 756872327473
#101: 37076114526
#102: 228
#103: 20313839404245
#104: 329468
#105: 73702
#106: 21384
#107: 259679
#108: 180180
#109: 38182
#110: 9350130049860600
#111: 612407567715
#112: 1587000
#113: 51161058134250
#114: 16475640049
#115: 168
#116: 20492570929
#117: 100808458960497
#118: 44680
#119: 248155780267521
#120: 333082500
#121: 2269
#122: 1582
#123: 21035
#124: 21417
#125: 2906969179
#126: 18522
#127: 18407904
#128: 14516824220
#129: 1000023
#130: 149253
#131: 173
#132: 843296
#133: 453647705
#134: 18613426663617118
#135: 4989
#136: 2544559
#137: 1120149658760
#138: 1118049290473932
#139: 10057761
#140: 5673835352990
#141: 878454337159
#142: 1006193
#143: 30758397
#144: 354
#145: 608720
#146: 676333270
#147: 846910284
#148: 2129970655314432
#149: 52852124
#150: -271248680
#151: 0.464399
#152: 301
#153: 17971254122360635
#154: 479742450
#155: 3857447
#156: 21295121502550
#157: 53490
#158: 409511334375
#159: 14489159
#160: 16576
#161: 20574308184277971
#162: 3D58725572C62302
#163: 343047
#164: 378158756814587
#165: 2868868
#166: 7130034
#167: 3916160068885
#168: 59206
#169: 178653872807
#170: 9857164023
#171: 142989277
#172: 227485267000992000
#173: 1572729
#174: 209566
#175: 1,13717420,8
#176: 96818198400000
#177: 129325
#178: 126461847755
#179: 986262
#180: 285196020571078987
#181: 83735848679360680
#182: 399788195976
#183: 48861552
#184: 1725323624056
#185: 4640261571849533
#186: 2325629
#187: 17427258
#188: 95962097
#189: 10834893628237824
#190: 371048281
#191: 1918080160
#192: 57060635927998347
#193: 684465067343069
#194: 61190912
#195: 75085391
#196: 322303240771079935
#197: 1.710637717
#198: 52374425
#199: 0.00396087
#200: 229161792008
#201: 115039000
#202: 1209002624
#203: 34029210557338
#204: 2944730
#205: 0.5731441
#206: 1389019170
#207: 44043947822
#208: 331951449665644800
#209: 15964587728784
#210: 1598174770174689458
#211: 1922364685
#212: 328968937309
#213: 330.721154
#214: 1677366278943
#215: 806844323190414
#216: 5437849
#217: 6273134
#218: 0
#219: 64564225042
#220: 139776,963904
#221: 1884161251122450
#222: 1590933
#223: 61614848
#224: 4137330
#225: 2009
#226: 0.11316017
#227: 3780.618622
#228: 86226
#229: 11325263
#230: 850481152593119296
#231: 7526965179680
#232: 0.83648556
#233: 271204031455541309
#234: 1259187438574927161
#235: 1.002322108633
#236: 123/59
#237: 15836928
#238: 9922545104535661
#239: 0.001887854841
#240: 7448717393364181966
#241: 482316491800641154
#242: 997104142249036713
#243: 892371480
#244: 96356848
#245: 288084712410001
#246: 810834388
#247: 782252
#248: 23507044290
#249: 9275262564250418
#250: 1425480602091519
#251: 18946051
#252: 104924.0
#253: 11.492847
#254: 8184523820510
#255: 4.4474011180
#256: 85765680
#257: 139012411
#258: 12747994
#259: 20101196798
#260: 167542057
#261: 238890850232021
#262: 2531.205
#263: 2039506520
#264: 2816417.1055
#265: 209110240768
#266: 1096883702440585
#267: 0.999992836187
#268: 785478606870985
#269: 1311109198529286
#270: 82282080
#271: 4617456485273129588
#272: 8495585919506151122
#273: 2032447591196869022
#274: 1601912348822
#275: 15030564
#276: 5777137137739632912
#277: 1125977393124310
#278: 1228215747273908452
#279: 416577688
#280: 430.088247
#281: 1485776387445623
#282: 1098988351
#283: 28038042525570324
#284: 5a411d7b
#285: 157055.80999
#286: 52.6494571953
#287: 313135496
#288: 605857431263981935
#289: 6567944538
#290: 20444710234716473
#291: 4037526
#292: 3600060866
#293: 2209
#294: 789184709
#295: 4884650818
#296: 1137208419
#297: 2252639041804718029
#298: 1.76882294
#299: 549936643
#300: 8.0540771484375
#301: 2178309
#302: 1170060
#303: 1111981904675169
#304: 283988410192
#305: 18174995535140
#306: 852938
#307: 0.7311720251
#308: 1539669807660924
#309: 210139
#310: 2586528661783
#311: 2466018557
#312: 324681947
#313: 2057774861813004
#314: 132.52756426
#315: 13625242
#316: 542934735751917735
#317: 1856532.8455
#318: 709313889
#319: 268457129
#320: 278157919195482643
#321: 2470433131948040
#322: 999998760323313995
#323: 6.3551758451
#324: 96972774
#325: 54672965
#326: 1966666166408794329
#327: 34315549139516
#328: 260511850222
#329: 199740353/29386561536000
#330: 15955822
#331: 467178235146843549
#332: 2717.751525
#333: 3053105
#334: 150320021261690835
#335: 5032316
#336: CAGBIHEFJDK
#337: 85068035
#338: 15614292
#339: 19823.542204
#340: 291504964
#341: 56098610614277014
#342: 5943040885644
#343: 269533451410884183
#344: 65579304332
#345: 13938
#346: 336108797689259276
#347: 11109800204052
#348: 1004195061
#349: 115384615384614952
#350: 84664213
#351: 11762187201804552
#352: 378563.260589
#353: 1.2759860331
#354: 58065134
#355: 1726545007
#356: 28010159
#357: 1739023853137
#358: 3284144505
#359: 40632119
#360: 878825614395267072
#361: 178476944
#362: 457895958010
#363: 0.0000372091
#364: 44855254
#365: 162619462356610313
#366: 88351299
#367: 48271207
#368: 253.6135092068
#369: 862400558448
#370: 41791929448408
#371: 40.66368097
#372: 301450082318807027
#373: 727227472448913
#374: 334420941
#375: 7435327983715286168
#376: 973059630185670
#377: 732385277
#378: 147534623725724718
#379: 132314136838185
#380: 6.3202e25093
#381: 139602943319822
#382: 697003956
#383: 22173624649806
#384: 3354706415856332783
#385: 3776957309612153700
#386: 528755790
#387: 696067597313468
#388: 831907372805129931
#389: 2406376.3623
#390: 2919133642971
#391: 61029882288
#392: 3.1486734435
#393: 112398351350823112
#394: 3.2370342194
#395: 28.2453753155
#396: 173214653
#397: 141630459461893728
#398: 2010.59096
#399: 1508395636674243,6.5e27330467
#400: 438505383468410633
#401: 281632621
#402: 356019862
#403: 18224771
#404: 1199215615081353
#405: 237696125
#406: 36813.12757207
#407: 39782849136421
#408: 299742733
#409: 253223948
#410:
#411: 9936352
#412: 38788800
#413: 3079418648040719
#414:
#415:
#416:
#417: 446572970925740
#418: 1177163565297340320
#419: 998567458,1046245404,43363922
#420: 145159332
#421:
#422:
#423:
#424:
#425: 46479497324
#426:
#427:
#428:
#429: 98792821
#430: 5000624921.38
