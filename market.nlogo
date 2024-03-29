__includes []
extensions []

breed [traders trader]
traders-own [my-threshold my-action done? myRedditUser]
turtles-own []
patches-own []
globals [
  current-info   ;; \epsilon(t)
  current-reddit ;; NA
  current-price  ;;
  current-return
  previous-price
  g
  z
  vol]

to setup
;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
random-seed 42
set current-price  100
create-traders num-traders [trader-setup]
ask patches [set pcolor white]
if write-output? [
    carefully [file-delete (word "data/netlogo-results-trader-RR" redditRatio "-RI" redditImportance ".csv")] []
    carefully [file-delete (word "data/netlogo-results-tick-RR" redditRatio "-RI" redditImportance ".csv")] []
    carefully [file-delete (word "data/netlogo-results-assumptions-RR" redditRatio "-RI" redditImportance ".csv")] []

    ;; setup trader info file
    let states (map [x -> word x ", "] ([self] of traders))
    let return-traders ""
    foreach states [x -> set return-traders (word return-traders x)]
    file-open (word "data/netlogo-results-trader-RR" redditRatio "-RI" redditImportance ".csv")
    file-print return-traders
    file-close-all

    ;; setup tick file
    set states (map [x -> word x ".state, "] ([self] of traders))
    set return-traders ""
    foreach states [x -> set return-traders (word return-traders x)]
    let threshold (map [x -> word x ".threshold, "] ([self] of traders))
    let return-threshold ""
    foreach threshold [x -> set return-threshold (word return-threshold x)]
    file-open (word "data/netlogo-results-tick-RR" redditRatio "-RI" redditImportance ".csv")
    file-print (word "ticks" ", " "current-price"  ", " "g" ", " "vol" ", " "current-reddit" ", " "current-info" ", " return-traders return-threshold)
    file-close-all


    ;; row of whether or not they use reddit info-stream
    set states (map [x -> word x ", "] ([myRedditUser] of traders))
    let return-reddit ""
    foreach states [x -> set return-reddit (word return-reddit x)]

;    ;; row of whether or not they use reddit info-stream
;    set states (map [x -> word x ", "] ([my-threshold] of traders))
;    let return-threshold ""
;    foreach states [x -> set return-threshold (word return-threshold x)]

    file-open (word "data/netlogo-results-trader-RR" redditRatio "-RI" redditImportance ".csv")
    file-print return-reddit
;    file-print return-threshold
    file-close-all

    ;; csv of assumptions
    file-open (word "data/netlogo-results-assumptions-RR" redditRatio "-RI" redditImportance ".csv")
    file-print("redditRatio, redditImportance, num-traders, lambda, d, %-change, maxTicks")
    file-print(word redditRatio ", " redditImportance ", " num-traders ", " lamda ", " d ", " %-change ", " maxTicks)
    file-close-all

    file-open (word "data/netlogo-results-tick-RR" redditRatio "-RI" redditImportance ".csv")


  ]

end

to trader-setup
  set size 1
  set my-threshold random-float 1
  set done? false
  ifelse random-float 1.0 < redditRatio [set myRedditUser true][set myRedditUser false]
end

to go
  set z 0
  set vol 0
  create-information
  ask traders [set done? false]
  while [count traders with [not done?] > 0]
  [ask traders [trade-2]]
  calculate-price
  calculate-return
  set vol sum [abs my-action] of traders
  if not write-output? [do-plots]
  if write-output? [do-output]
  tick
  if ticks > maxTicks [set write-output? false file-close-all stop]
end

to create-information
  ifelse not newInfo?
  [
    ifelse paper?
    ;; paper case
    [set current-info random-normal 0 (d ^ 2)
    ]

    [set current-info random-normal 0 d] ;; original line
  ]
  [
    ifelse ticks < 2
    [
      set current-reddit random-normal 0 (d ^ 2)
      set current-info random-normal 0 (d ^ 2)
    ]
    [
      set current-reddit max list (min list (current-reddit - 0.3 * current-info + random-normal 0 (d ^ 2)) (d * 2)) (d * -0.3)
      ifelse abs(current-info) > (d * 1.2)
      [
        set current-info -0.01
      ]
      [
        set current-info current-info + random-normal 0 (d ^ 2)
      ]
    ]
  ]
end

to trade-2
  ;; for now only looking at paper case
  ifelse useBothFeeds? and myRedditUser
  [
    ifelse ((redditImportance * current-reddit) + ((1 - redditImportance) * current-info)) < (-1 * my-threshold) or ((redditImportance * current-reddit) + ((1 - redditImportance) * current-info)) > my-threshold
    [
      ifelse ((redditImportance * current-reddit) + ((1 - redditImportance) * current-info)) > 0
      [set my-action 1 set z z + 1][set my-action -1 set z z - 1]
    ]
    [set my-action 0]
  ]
  [
    ;; paper case
    ifelse current-info < (-1 * my-threshold) or current-info > my-threshold
    [
      ifelse current-info > 0
      [set my-action 1 set z z + 1][set my-action -1 set z z - 1]
    ]
    [set my-action 0]
  ]
  set done? true
end

to calculate-price
set previous-price current-price
set g ((z / (count traders)) / lamda)
set current-price max list (exp g * previous-price) 1
end

to calculate-return
set current-return g ;; correct
end

to change-threshold
set my-threshold abs current-return
;set my-threshold current-return
end

to do-plots
  set-current-plot "Current Price"
  plot current-price

  set-current-plot "Current Return"
  plot current-return

  set-current-plot "Excess Demand"
  plot sum [my-action] of traders

  set-current-plot "g"
  plot g

  set-current-plot "vol"
  plot vol

  set-current-plot "infoPlot"
  set-current-plot-pen "info1"
  plot current-info
  set-current-plot-pen "info2"
  plot current-reddit
end

to move-around
set xcor my-action
set ycor my-threshold * 2
set color scale-color color my-threshold -1 1
end

to do-output
  let states (map [x -> word x ", "][my-action] of traders)
  let return-states ""
  foreach states [x -> set return-states (word return-states x)]

  let thresh (map [x -> word x ", "][my-threshold] of traders)
  let return-thresh ""
  foreach thresh [x -> set return-thresh (word return-thresh x)]


  file-print (word ticks "," current-price ", " g ", " vol ", " current-reddit ", " current-info ", " return-states return-thresh)
end

;;;;;;;;;;;;;;;;;;;;;
;;;;; old stuff ;;;;;
;;;;;;;;;;;;;;;;;;;;;

to trade ;; not used
ifelse abs current-info > abs my-threshold
  [
    ifelse current-info > 0 ; and current-info - my-threshold > 0
      [set my-action 1]
      [
        if current-info < 0 ; and current-info + my-threshold < 0
          [set my-action -1]
      ]
  ]
  [
    set my-action 0
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
759
10
986
150
-1
-1
43.8
1
10
1
1
1
0
1
1
1
-2
2
-1
1
0
0
1
ticks
30.0

BUTTON
8
7
74
40
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
80
8
143
41
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
150
9
213
42
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
485
10
615
43
num-traders
num-traders
0
2000
200.0
1
1
NIL
HORIZONTAL

SLIDER
357
10
482
43
d
d
0
1
0.036
.001
1
NIL
HORIZONTAL

SLIDER
220
10
353
43
%-change
%-change
0
1
0.0
.01
1
NIL
HORIZONTAL

PLOT
15
485
536
635
Current Price
Time
Price
0.0
10.0
100.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
15
331
1028
481
Current Return
NIL
NIL
0.0
10.0
0.0
0.1
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
16
189
759
327
Excess Demand
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
619
11
743
44
lamda
lamda
0
100
75.0
1
1
NIL
HORIZONTAL

PLOT
16
64
757
184
g
NIL
NIL
0.0
10.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SWITCH
760
65
901
98
write-output?
write-output?
1
1
-1000

PLOT
542
485
1028
635
vol
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
15
636
1028
786
infoPlot
NIL
NIL
0.0
10.0
-0.1
0.1
true
false
"" ""
PENS
"info1" 1.0 0 -16777216 true "" ""
"info2" 1.0 0 -2674135 true "" ""

SWITCH
760
106
871
139
newInfo?
newInfo?
0
1
-1000

SWITCH
874
106
977
139
paper?
paper?
1
1
-1000

SWITCH
761
143
909
176
useBothFeeds?
useBothFeeds?
0
1
-1000

SLIDER
761
179
933
212
redditRatio
redditRatio
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
762
214
934
247
redditImportance
redditImportance
0
1
0.0
.01
1
NIL
HORIZONTAL

INPUTBOX
914
19
1064
79
maxTicks
2000.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="project_data" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="maxTicks">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="redditRatio">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="d">
      <value value="0.036"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-output?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paper?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="redditImportance">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="useBothFeeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newInfo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-traders">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lamda">
      <value value="75"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
