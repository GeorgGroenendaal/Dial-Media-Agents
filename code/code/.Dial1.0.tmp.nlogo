extensions [array vid]
breed [medias media]
turtles-own [
   props            ; a list of pairs: < evidence importance >
   init-props       ; a list of pairs: < evidence importance > the initial values
   announcements    ; a list of 4-tuples: < key <evidence importance> ticks trust>
   attacks          ; pairs: < attacking-agent prop >
   questions        ; pairs: < requesting-agent prop >
   profit-strategy  ; list of learned profits of all strategy
   prior-size       ; prior size for profit
 ]
patches-own [ pprops]

globals [delta action-prob-pairs current-prop number-of-props total-odds totalsim totalsize filenaam agentsorderedatstart triangles strategy-shapes plottitle _recording-save-file-name]

to creat-media-agents
  create-medias 50
  ask medias [ setxy random-xcor random-ycor ]
  ask medias [ set color red ]
  ;  crt number-of-agents [setxy random-xcor random-ycor

              ;set props generateopinions
              ;set init-props props
              ;set announcements []
              ;set attacks []
              ;set questions []
              ;set color  scale-color red  first (item current-prop props)  1 0
              ;set color red
              ;set label who
              ;set label-color 66
              ;set size (random-float 2) + 1
              ;set profit-strategy [0 0 0]
   ;         ]

end
; Utility functions
to-report second [l]
report item 1 l
end

to-report zip [l m]
  ifelse empty? l [report l]
    [report fput (list (first l) (first m)) (zip (butfirst l) (butfirst m))]
end

to-report sign [x]
  ifelse x >= 0 [report 1] [report -1]
end

; The Acceptance of Announcements


; Environment Oriented.
; The patches are used for ako anonimous communication. The information of announcements
; are accumulated in the patches according to the accept function. Forgetting is a
; a gradual move towards neutral values ((e,i) = (0.5, 0.5))



to-report forget-pevidence [pevidence]
ifelse abs (pevidence - 0.5) > forgetspeed
   [report  pevidence - sign (pevidence - 0.5) * forgetspeed ]
   [report  0.5]
end

to-report forget-pimportance [pimportance]
ifelse abs (pimportance - neutral-importance) > forgetspeed
   [report  pimportance - sign (pimportance - neutral-importance) * forgetspeed ]
   [report  neutral-importance]
end

to forget-announcements
   let yesterday ticks - 10
   set announcements filter [ ?1 -> yesterday < item 2  ?1 ]  announcements
end


; Dialogue Oriented

; Initialization and the Main Loop
;;to-report random-prop ; to create a proposition with random evidence and importance values
                      ; used in setup
;;report list  (random-float 1) (random-float 1)
;;end

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set plottitle "High force-of-Arguments and High force-of-Norms"
  reset-ticks
  set delta 1e-5
  set number-of-props number-of-propositions
  set current-prop min (list (position current-proposition
                   ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]) (number-of-props - 1))
  ;; create turtles with random  locations, evidence and importance values
  set strategy-shapes ["circle" "default" "face happy"]
  set-default-shape turtles "circle"
  ask patches [set pcolor blue
               set pprops[]
               repeat number-of-props [ set pprops fput (list 0.5 neutral-importance) pprops]]
  crt number-of-agents [setxy random-xcor random-ycor
              set props generateopinions
              set init-props props
              set announcements []
              set attacks []
              set questions []
              set color  scale-color yellow  first (item current-prop props)  1 0
              set label who
              set label-color 66
              set size (random-float 2) + 1
              set profit-strategy [0 0 0]
            ]
  set totalsize  sum [size] of turtles
  setup-plot
  ;update-plotfile ;; !!!!!!!
end

to-report incr-total-odds [ee]
set total-odds total-odds + ee
report total-odds
end

to-report find-action [c l]
  while [c >= first (first l) ] [set l but-first l]
  report first l
end

to go
   set total-odds 0
   set action-prob-pairs (map [ [?1 ?2] -> list (incr-total-odds ?1) ?2 ]
         (list chance-announce chance-question chance-attack chance-walk chance-learn-by-neighbour
               chance-learn-by-environment  chance-mutation chance-change-strategy)
         (list "announce" "question" "attack" "walk" "learn-by-neighbour"
              "learn-by-environment" "mutate" "change-strategy" ))
   set totalsim 0
   ask turtles [act]
   ask patches [  ; to forget
       set pprops map [ ?1 -> list (forget-pevidence first ?1) (forget-pimportance second ?1) ] pprops ]

   ask turtles [
       forget-announcements
       answer-questions
       reply-attacks
       ]
   let f totalsize / (totalsize + totalsim)
   ask turtles [set size max (list 0 (size * f))]
;   update-plot ;; !!!!!!
   show-world
   tick
end

to-report similar-attitude [a b]
  report sum (map [ [?1 ?2] -> agreementfactor first ?1 first ?2 ] a b )
end

to act
  if breed = medias [ show "media agent" ]
  set prior-size size
  run second (find-action (random-float total-odds) action-prob-pairs)
  let sim   force-of-norms * similar-attitude props pprops / number-of-propositions
  set sim   sim - lack-of-princ-penalty * similar-attitude props init-props / number-of-propositions
  if size + sim > delta [
    set size size +  sim
    set totalsim totalsim + sim
  ]
  if size < 0 [show "ALERT"]
  foreach [0 1 2] [ ?1 ->
    ifelse item ?1 strategy-shapes = shape [set profit-strategy replace-item ?1 profit-strategy (size - prior-size) ]
      [set profit-strategy replace-item ?1 profit-strategy (item ?1 profit-strategy + 0.001) ]
  ]
end

;to-report inrange [a b c]
;  ifelse b <  a [report a][ifelse b < c [report b][report c]]
;end

; Agent's Actions



to announce ;turtle procedure
 if size > announce-threshold [
   let announce-odds sum map [ ?1 -> second ?1 ] props
   let choice random-float announce-odds
   let  p 0
   let choice-inc second first props
   while [choice >= choice-inc] [
     set p p + 1
     set choice-inc choice-inc + second item p props
   ]

  let w  who
  let evidence (first item p props  + firmness-of-principle * first item p init-props) /
               (firmness-of-principle + 1)
  let importance (second item p props  + firmness-of-principle * second item p init-props) /
               (firmness-of-principle + 1)
  let loud random-float loudness * size
  ask other turtles with [distance myself < loud]
         [ update-announcement w p evidence importance]
  ask patches with [distance myself < loud]
         [ announce-patch myself p evidence importance]
         ;; this is a tricky way to pass a turtle via an ask patches command to a number of patches
   ]
end

to-report find-location [a b]
  output b
  if empty? b [report false]
  let fl find-location a but-first b
  ifelse fl [
      ifelse first first b = a  and not fl [report 0][report (1 + fl)]
    ][report false]
end

to update-announcement [w p ev i ] ; w = sender, p = proposition
  ; update memory
  let key number-of-agents * p + w
  let loc find-location key announcements
  ifelse loc [set announcements
            replace-item loc announcements (list key (list ev i) ticks)
            ; doe iets met trust
         ]
        [   set announcements fput (list key (list ev i) ticks) announcements]
  ; now the SJT stuff accept (agreement > 0.2) or reject (agreement < -0,2)
  let evidence first item p props
  let importance second item p props
  let agree (agreementfactor evidence  ev)
  if agree > 1 - attraction [
    setopinion p  (list (accepte evidence ev) (accepti agree importance i))
    ] ; accept p
  if agree < rejection - 1 [
     setopinion p  (list (accepte evidence (1 - ev)) (accepti ( agreementfactor evidence (1 - ev)) importance i))
    ]  ; attack agent w on p
end

;; update the patches with the information of the announcement
;; which is proportional to the distance from the agent that made the announcement.
to announce-patch [agnt loc evid imp]  ;; patch procedure; input agent is a turtle
  let rsquared (distance agnt + 1) ^ 2
  let pevid first item loc pprops
  let pimp second item loc pprops
  let agree (agreementfactor evid pevid)
  set pevid 0.5 + ((accepte evid pevid) - 0.5 + rsquared * (pevid - 0.5)) /(rsquared + 1)
  set pimp ((accepti agree imp pimp)  + rsquared * pimp) / (rsquared + 1)
  set pprops replace-item loc pprops (list pevid pimp)
end

to-report agreementfactor [e1 e2]
  report (2 * e1 - 1) * (2 * e2 - 1)
end

to-report accepte [e1 e2]
  ifelse e1 < 0.5 [
       ifelse e2 < 0.5 [
             report   2 * e1 * e2
         ] [
             report   e1 + e2 - 0.5
    ]] [ifelse e2 < 0.5 [
             report   e1 + e2 - 0.5
         ] [
             report   2 * e1 + 2 * e2 - 2 * e1 * e2 - 1
       ]
  ]
end

to-report accepti [agree i1 i2]
  report (i1 + i2 + agree * (2 * i1 * i2 - i1 - i2)) / 2
end

to question
   let imp  map [ ?1 -> second ?1 ] props
   let max-imp-question  position max imp imp    ; my most important proposition
;   let me self
   let candidate one-of other turtles with [distance myself < visual-horizon]
   if candidate != nobody     ; ask a passer-by
       [ask candidate [set questions fput (list myself max-imp-question) questions]]
end

to answer-questions
  if not empty? questions [
     let q one-of questions
     let ag first q
     let ag-dist distance ag
     let w  who
;     let pps props
     let evidence first (item (second q) props)
     let importance second (item (second q) props)
     ask other turtles with [distance myself <= ag-dist]
       [ update-announcement w (second q)  evidence importance]
;    ask patches with [distance ag < loud ]
;       [ announce-patch ag (second q) evidence importance]
     set questions []
    ]
end

to-report agrees [v] ; rank the announcements for attack
   let i floor (first v / number-of-agents)
   let t  (first v) mod number-of-agents
   ifelse [size] of turtle t < announce-threshold or distance turtle t < visual-horizon [report 1]
        [report agreementfactor (first item i props) first second v]
end

to attack
  if size > announce-threshold and not empty? announcements  [
     let agree (map [ ?1 -> agrees ?1 ] announcements)
     let loc position (min agree) agree
     let key 0
     if item loc agree < 0 [
       set key first (item loc announcements)
 ;      create-link-to turtle (key mod number-of-agents) [set color 15]
       ask turtle (key mod number-of-agents) [
           set attacks fput (list myself floor (key / number-of-agents)) attacks]

       show (word self " attacks " (key mod number-of-agents))
     ]
  ]
end

to reply-attacks
if size > 1 [
   let pr  filter [ ?1 -> [size] of first ?1 > 1 ] attacks ; only attacks one ofthe agents who have sufficient reputation
   if not empty? pr [
      let a one-of pr ; win == s (Epro) = s (Eopenv + Eprenv)
      let p second a
      let epro first item p props
      let ipro second item p props
      let eop first item p [props] of first a
      let iop second item p [props] of first a
      let eprenv (first item p pprops + [first item p pprops] of first a) / 2
      let win 0
      ifelse agreementfactor epro eprenv > agreementfactor eop eprenv
          [ set win ipro * epro * force-of-argumentation][  ;; 2* weggelaten
            set win (-( ipro * (1 - epro) * force-of-argumentation))
          ]  ;; 2* weggelaten
      ifelse win > 0 [set win min (list win (delta + [size] of first a))]
                     [set win max (list win (-(size + delta)))]
      set size size +  win
      ask first a [set size size - win]
      ifelse win > 0 [
                       ask patches with [distance first a <  loudness] [announce-patch first a p epro ipro]
                     ]
                     [
                       ask patches with [distance myself <  loudness] [announce-patch myself p eop iop]
                     ]
      ; update the beliefs of the proponent and the opponent
      let agree (agreementfactor epro  eop)
      ifelse win > 1 - winthreshold [
;                       setopinion p  (list (accepte epro epro) (accepti agree ipro ipro))
                       ask first a [setopinion p  (list (accepte epro eop) (accepti agree ipro iop))]
                     ]
                     [if win < winthreshold - 1[
                       setopinion p  (list (accepte eop epro) (accepti agree iop ipro))
;                       ask first a [setopinion p  (list (accepte epro eop) (accepti agree ipro iop))]
                       ]
                     ]

    show (word self "replies attack on " p " of " first a " and wins " win)
  ]]
set attacks []
 ;  ask my-in-links [die]
end


to walk
  find-direction
  rt random undirectedness - random undirectedness
  fd random-float stepsize
end

to find-direction  ;; face to the most similar agent
  let p props
  let best-match  0
  ifelse (shape = "face happy") ; commando: ask n-of 30 turtles [set shape "face happy"]
     [set best-match min-one-of patches in-radius (visual-horizon) [similar-attitude p pprops] ]
     [set best-match max-one-of patches in-radius (visual-horizon) [similar-attitude p pprops]] ; of:: min-one-of...
;set best-match max-one-of patches in-radius (visual-horizon) [similar-attitude p pprops]
  if best-match != nobody [face best-match]
  if shape = "default" [ifelse random 2 =  0 [right 90][left 90]]
end

to change-strategy
  let i position max profit-strategy  profit-strategy
  set shape item i strategy-shapes
  set profit-strategy replace-item i profit-strategy 0
end

to learn-by-neighbour
  let nb one-of turtles-on neighbors
  if nb != nobody [
      let  i random number-of-props
;      setopinion i  (item i [props] of nb) ; zonder acceptance
      let evidence first item i props
      let importance second item i props
      let ev first (item i [props] of nb)
      let imp second (item i [props] of nb)
      let agree (agreementfactor evidence ev)
      setopinion i  (list (accepte evidence ev) (accepti agree importance imp))
    ]
end

to learn-by-environment
   let prop-odds sum map [ ?1 -> second ?1 ] props
   let choice random-float prop-odds
   let  p 0
   let choice-inc second first props
   while [choice >= choice-inc] [
     set p p + 1
     set choice-inc choice-inc + second item p props
   ]
;   let  i random number-of-props
   setopinion p (item p pprops)
end

to mutate
  setopinion (random number-of-props)  (list (random-float 1) (random-float 1))
end

; Convex opinion set routines

to-report nondec [n l h] ;number of elements lowest value highest value
  let v []
  repeat n [set v fput (random-float (h - l) + l) v]
  report sort v
end
to-report noninc [n l h] ;number of elements lowest value highest value
  let v []
  repeat n [set v fput (random-float (h - l) + l) v]
  report sort-by [ [?1 ?2] -> ?1 > ?2 ] v
end

to-report valley [n] ;number of elements
  let v random n
  report sentence (noninc v 0 1)(nondec (n - v) 0 1)
end

to-report hill [n] ;number of elements
  let h random n
  report sentence (nondec h 0 1)(noninc (n - h) 0 1)
end

to-report convexlist1 [n] ; purely random list
  ifelse n = 0 [report []]
     [report sentence random-float 1 convexlist (n - 1)]
end
to-report convexlist [n] ; create a list with convex values of length n
;  report runresult item ((random 4) + 0) (list "nondec n 0 1" "noninc n 0 1" "hill n" "valley n")
  ; valleys are not allowed
  report runresult item ((random 3) + 0) (list "nondec n 0 1" "noninc n 0 1" "hill n")
end

to-report testnoninc [l]
  ifelse length l <= 1
    [report l]
    [ifelse first l >= second l [report testnoninc butfirst l]
      [report l]]
end

to-report testnondec [l]
  ifelse length l <= 1
    [report l]
    [ifelse first l <= second l [report testnondec butfirst l]
      [report l]]
end

to-report testconvex [l]
  if length l <= 1 [report [] ]
  while [first l = second l] [report testconvex butfirst l]
  ifelse first l < second l [
    report testnoninc (testnondec l) ][
    report testnondec l ; (testnoninc l) ; valleys are not allowed
  ]
end

to setopinion [p evi] ; prop evidence importance
  ; this is the only place where convexity is tested
  ifelse length testconvex (replace-item p (map [ ?1 -> first ?1 ] props)  first evi) <= 1 ; new opion is convex
    [set props replace-item p props evi]
    [if size > inconspenalty + delta [
      set totalsim totalsim -  inconspenalty
      set size size -  inconspenalty ]
     ]  ; else punishment for inconsistency
;set props replace-item p props evi
end

to-report generateopinions
  let evids convexlist number-of-props
  let imps []
  repeat number-of-props [ set imps fput (random-float 1) imps]
  report zip evids imps
end

; create groups
to createtriangles
  clear-links
  set triangles []
  ask turtles [create-links-with other turtles in-radius visual-horizon
                 [set color blue]]
  ask turtles with [count link-neighbors >= 2]
    [let w1 who
     ask link-neighbors
       [let w2 who ask link-neighbors [checktriangles w1 w2]]]
end

to checktriangles [w1 w2] ; check if this one is connectected to the first turtle
  if link-neighbor? turtle w1 [addtriangle sort (list w1 w2 who)]
end

to addtriangle [t] ; add a triangle to the list
  if not member? t triangles [set triangles fput t triangles]
end

to jointriangles ; join triangles/groups with 2 common agents
  let m length triangles
  let changed true
  let i 0 let j 0
  while [changed] [
    set changed false
    set i 0
    while [i < m - 1] [
      set j i + 1
      while [j < m] [
        ifelse n-or-more-common 2 item i triangles item j triangles [
          set triangles replace-item j triangles union item i triangles item j triangles
          set triangles remove-item i triangles
          set m m - 1
          set changed true
        ] [
          set j j + 1
        ]
      ]
      set i i + 1
    ]
  ]
  showlinks 0 triangles
end

to-report n-or-more-common [n l1 l2]
  if n = 0 [report true]
  if empty? l1 or empty? l2[report  false]
  ifelse first l1 = first l2 [report n-or-more-common (n - 1) but-first l1 but-first l2] [
    ifelse first l1 < first l2 [report n-or-more-common n  but-first l1  l2] [
      report n-or-more-common n  l1 but-first l2
    ]
  ]
end

to-report union [l1 l2]
  ifelse empty? l1 [report  l2] [
    ifelse empty? l2 [report l1] [
      ifelse first l1 = first l2 [report union but-first l1 l2] [
        ifelse first l1 < first l2 [report sentence first l1 union but-first l1 l2] [
          report sentence first l2 union l1 but-first l2
        ]
      ]
    ]
  ]
end

to showlinks [n l]
; indexpos, triangles, color links
  let ag 0
  let lnk nobody
  if l != [] [
    let agset first l
    foreach agset [ ?1 ->
      set ag  ?1
      ask turtle ag [
        foreach agset [ ??1 ->
          set lnk link ag ??1
          if lnk != nobody [ask lnk  [set color  scale-color red  n 0 length triangles] ]
        ]
      ]
    ]
    showlinks n + 1 but-first l
  ]
end

;Computation of Output Parameters.

; report the agents LOA LON and LOR preferred opinion
to-report LOA
; number of props with evidence > 0.8 (significance level)
  report length filter [ ?1 -> first ?1 > 0.8 ] props
end

to-report LON
; number of props with evidence > 0.8 (significance level)
  report length filter [ ?1 -> first ?1 <= 0.8 and first ?1 >= 0.2 ] props
end

to-report LOR
; number of props with evidence > 0.8 (significance level)
  report length filter [ ?1 -> first ?1 < 0.8 ] props
end


; Prefered Opinion is the opinion with the highest importance.
to-report preferredopinion
  let ev map [ ?1 -> first ?1 ] props
  report position (max ev) ev
end



; show the world and other output

to show-world
  set current-prop min (list
                          (position current-proposition ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"])
                          (number-of-props - 1))
  ifelse viewmode [show-evid][show-imp]
  update-plot
end

to show-imp        ;; show a map of the importance values black red white for turtles
                   ;; black green white for patches
  ask patches [set pcolor  scale-color green  (second item current-prop pprops) 0 1]
  ask turtles [set color  scale-color red   (second item current-prop props) 1 -0]
end

to show-evid       ;; show the evidence mode again
                   ;; show a map of the evidence values black yellow white for turtles
                   ;; black blue white for patches
  ask patches [set pcolor  scale-color blue   (first item current-prop pprops) 0 1]
  ask turtles [set color  scale-color yellow   (first item current-prop props) 1 -0]
end

to show-importance set viewmode false show-world end
to show-evidence set viewmode true show-world end

; show the world (and other output) of all  propositions
to showall
  let tmp current-prop
  set current-prop 0
  repeat (number-of-props ) [show-evid update-plot wait 1 set current-prop current-prop + 1]
  set current-prop tmp
end
to showalli
  let tmp current-prop
  set current-prop 0
  repeat (number-of-props ) [show-imp update-plot wait 1 set current-prop current-prop + 1]
  set current-prop tmp
end


to setup-plot
  set-current-plot "Distribution of Evidence"
  set-plot-y-range 0 number-of-agents
  set-plot-x-range 0 1.01
  set-histogram-num-bars 20
  set-current-plot "Importance Distribution"
  set-plot-x-range 0 1
  set-plot-y-range 0 number-of-agents
  set-histogram-num-bars 10
  set-current-plot plottitle
  set-plot-y-range 0 1
end
to setup-plotfile
  set filenaam user-new-file
  file-open filenaam
  set agentsorderedatstart sort-by [ [?1 ?2] -> [first item current-prop props] of ?1 <
                                    [first item current-prop props] of ?2 ] turtles

  set-current-plot "Distribution of Evidence"
  set-plot-y-range 0 number-of-agents
  set-plot-x-range 0 1
  set-histogram-num-bars 20
  set-current-plot "Importance Distribution"
  set-plot-x-range 0 1
  set-plot-y-range 0 number-of-agents
  set-histogram-num-bars 10
  set-current-plot plottitle
  set-plot-y-range 0 1
end

to update-plot

  let tmp 0
  set-current-plot "Distribution of Evidence"
    histogram [first item current-prop props] of turtles
    set-current-plot "Importance Distribution"
    histogram [second item current-prop props] of turtles
 set-current-plot plottitle
    set-current-plot-pen "Reputation Distribution";; black
    set tmp report-authority plot tmp
    set-current-plot-pen "Spatial Distribution" ;;"friend ratio" ;; green
    set tmp  clustering plot tmp
    set-current-plot-pen "Average Belief" ;; blue
    set tmp  report-eopop  plot tmp
    set-current-plot-pen "Belief Distribution" ;; yellow
    set tmp  report-ginievid plot tmp
    set-current-plot-pen "Average Importance" ;; green
    set tmp  report-iopop plot tmp
    set-current-plot-pen "Importance Distribution" ;; yellow
    set tmp  report-giniimp plot tmp
end

to update-plotfile
if (ticks > 199)[
  let tmp 0
  set-current-plot "Distribution of Evidence"
    histogram [first item current-prop props] of turtles ; evidence of currently displayed prop
    set-current-plot "Importance Distribution"
    histogram [second item current-prop props] of turtles ;; importance of currently displayed prop
 set-current-plot plottitle

    file-type force-of-argumentation
    set-current-plot-pen "SpatialDistribution" ;;"friend ratio" ;; green
    set tmp  clustering plot tmp  file-type " " file-type  tmp
    set-current-plot-pen "ReputationDistribution" ;; black
    set tmp  report-authority plot tmp  file-type " " file-type  tmp
    set-current-plot-pen "LOA";; black
    set tmp report-LOA plot tmp file-type " " file-type  tmp
    set-current-plot-pen "LON" ;; pink
    set tmp  report-LON  plot tmp file-type " " file-type  tmp
    set-current-plot-pen "giniprefop" ;; orange
    set tmp  report-giniprefop plot tmp file-type " " file-type  tmp
    set-current-plot-pen "EOPOP" ;; cyan
    set tmp  report-eopop plot tmp file-type " " file-type  tmp
    set-current-plot-pen "IOPOP" ;; yellow
    set tmp  report-iopop plot tmp  file-type " " file-type  tmp
  file-print""]
end

; Lattitude of Acceptance. Average number of props with evidence > 0.8
to-report report-LOA
  report mean [LOA] of turtles / number-of-props
end

; Lattitude of Non-commitment. Average number of props with 0.2 <= evidence <= 0.8
to-report report-LON
  report mean [LON] of turtles / number-of-props
end


to-report report-giniprefop ;gini of preferred opinion
  report gini ranks number-of-propositions
                    [abs ( 2 * (preferredopinion - number-of-propositions / 2 ) /
                           number-of-propositions) ] of turtles
end

; Average evidence of Preferred Opinion.
to-report report-eopop
  report mean [abs  (2 * first item preferredopinion props - 1)] of turtles
end

to-report report-iopop
  report mean [ second item preferredopinion props] of turtles
end


to-report report-ginievid
  report gini [abs  (2 * first item preferredopinion props - 1)] of turtles
end

to-report report-giniimp
  report gini [ second item preferredopinion props] of turtles
end


to-report report-authority
;   report max [size] of turtles / number-of-agents
   report gini  [size] of turtles
end





to-report report-pressure
;;  let tmp (turtles with [ (sign first item current-prop props) = (sign first item current-prop pprops)] )
;;  report (mean [min lput visual-horizon ([distance myself] of same-kind-neighbours in-radius visual-horizon)] of turtles ) / visual-horizon
report gini ranks 10 [abs first item current-prop props] of turtles
end

to-report report-unanimpatch
 let  L [abs first item current-prop pprops] of patches ;
 report gini  L
end

to-report gini [Lin] ;; expects a list of values.
;  Orders by the lowest rank first (or highest value)
  let L sort-by [ [?1 ?2] -> ?1 > ?2 ] Lin
  let N  length L
  if N <= 1 [report 0]
  let i 0
  let numerator 0
  while [i < N ] [
    set numerator numerator + (i + 1) * (item i L)
    set i i + 1
  ]
  let u  mean L
  ifelse  u = 0 [report 0] [
    report (N + 1) / (N - 1) - 2 * numerator / (N * (N - 1) * u)
  ]
 end

to-report cohesion
  let evid 0 let cnt 0 ;; count number of patches with neighbour with opposite opinion
  ask patches [
    set evid first item current-prop pprops
    if min [ (evid - 0.5) * (first item current-prop pprops - 0.5)] of neighbors < 0 [
      set cnt cnt + 1
    ]
  ]
  report cnt / 1600 ;; count patches
end

to-report clustering1
  createtriangles
  jointriangles
  report mean map [ ?1 -> length ?1 ] triangles / 20
end

to-report clustering
  report 1 - mean [avg-dist] of turtles / visual-horizon
end

to-report avg-dist
  let m mean [distance myself] of turtles in-radius visual-horizon
  ifelse m = 0 [report 1][report m]
end

to-report talking
  report count turtles with [size > 1.3]  / number-of-agents
end

to-report ranks [n L] ;; n- aantal klassen L- data
;  if L = [] [set L [1]]
  let al n-values n [0]
  let c 1
  if max l != 0 [set c 0.999 * n / max L]
  let ar array:from-list al
  foreach L [ ?1 -> let v floor (c * ?1) array:set ar v (array:item ar v) + 1 ]
  report array:to-list ar
end

to params
  set filenaam user-new-file
  file-open filenaam
  file-print "\\begin{tabular}{|lll|}\\hline"
  file-print "Parameter & Range & Value \\\\ \\hline"
  file-type "force-of-argumentation  & 0-1 &" file-print force-of-argumentation
  file-type "\\\\  chance-announce & 0-100 &" file-print chance-announce
  file-type "\\\\  loudness  & 0-20 &" file-print loudness
  file-type "\\\\  chance-walk & 0-100 &"  file-print chance-walk
  file-type "\\\\  stepsize  & 0-2 &" file-print stepsize
  file-type "\\\\  visual-horizon  & 0-20 &" file-print visual-horizon
  file-type "\\\\  forgetspeed & 0-0.005 &" file-print forgetspeed
  file-type "\\\\  undirectedness & 0-45 &" file-print undirectedness
  file-type "\\\\ chance-question & 0-100 &" file-print chance-question
  file-type "\\\\  chance-attack & 0-100 &" file-print chance-attack
  file-type "\\\\  chance-learn-by-neighbour & 0-10 &"  file-print chance-learn-by-neighbour
  file-type "\\\\  chance-learn-by-environment & 0-10 &" file-print chance-learn-by-environment
  file-type "\\\\  chance-mutation & 0-2 &" file-print chance-mutation
  file-type "\\\\  neutral-importance & 0-1 &" file-print neutral-importance
  file-type "\\\\  inconspenalty & 0-1 &" file-print inconspenalty
  file-type "\\\\  attraction & 0-1 &" file-print attraction
  file-type "\\\\  rejection & 0-1 &" file-print rejection
  file-type "\\\\  winthreshold & 0-1 &" file-print winthreshold
  file-print "\\\\ \\hline \\end{tabular}"
  file-close
end

to movie [n]
   setup
   set filenaam user-new-file
   set _recording-save-file-name filenaam
   vid:start-recorder
   repeat n [
     vid:record-view
   go
   ]
   vid:record-interface
vid:save-recording _recording-save-file-name
end
@#$#@#$#@
GRAPHICS-WINDOW
460
10
1060
490
-1
-1
12.1
1
10
1
1
1
0
1
1
1
-24
24
-19
19
1
1
1
ticks
30.0

BUTTON
353
399
408
432
go
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

BUTTON
345
364
422
397
setup
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

SLIDER
8
10
189
43
number-of-agents
number-of-agents
1
100
27.0
1
1
NIL
HORIZONTAL

BUTTON
201
400
316
433
NIL
show-importance
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
200
363
317
396
NIL
show-evidence
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
948
813
1081
846
forgetspeed
forgetspeed
0
0.005
0.00106
0.00001
1
NIL
HORIZONTAL

SLIDER
13
228
187
261
chance-announce
chance-announce
0
100
38.0
1
1
NIL
HORIZONTAL

SLIDER
4
266
96
299
loudness
loudness
0
5
2.5
0.1
1
NIL
HORIZONTAL

SLIDER
408
590
514
623
stepsize
stepsize
0
2
0.8
0.02
1
NIL
HORIZONTAL

SLIDER
195
474
315
507
undirectedness
undirectedness
0
45
26.0
1
1
NIL
HORIZONTAL

SLIDER
195
438
367
471
chance-walk
chance-walk
0
100
48.0
1
1
NIL
HORIZONTAL

SLIDER
807
578
1043
611
chance-learn-by-neighbour
chance-learn-by-neighbour
0
10
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
808
614
1044
647
chance-learn-by-environment
chance-learn-by-environment
0
10
1.0
0.1
1
NIL
HORIZONTAL

PLOT
195
10
455
162
Distribution of Evidence
NIL
NIL
0.0
10.0
0.0
50.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
195
170
456
320
Importance Distribution
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
"default" 1.01 1 -16777216 true "" ""

SLIDER
99
266
191
299
visual-horizon
visual-horizon
1
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
808
812
947
845
chance-mutation
chance-mutation
0
2
0.0
0.02
1
NIL
HORIZONTAL

SLIDER
808
849
1056
882
neutral-importance
neutral-importance
0.01
0.99
0.5
0.01
1
NIL
HORIZONTAL

SWITCH
199
326
327
359
viewmode
viewmode
0
1
-1000

TEXTBOX
333
329
440
359
Evidence     On\nImportance Off
12
0.0
1

SLIDER
8
47
189
80
number-of-propositions
number-of-propositions
1
10
1.0
1
1
NIL
HORIZONTAL

CHOOSER
26
84
181
129
current-proposition
current-proposition
"A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
4

SLIDER
13
422
188
455
chance-question
chance-question
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
13
386
188
419
chance-attack
chance-attack
0
100
12.0
1
1
NIL
HORIZONTAL

SLIDER
9
143
188
176
force-of-argumentation
force-of-argumentation
0
1
1.0
0.01
1
NIL
HORIZONTAL

PLOT
13
528
796
881
High force-of-Arguments and High force-of-Norms
cycles
parameters
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Reputation Distribution" 1.0 0 -16777216 true "" ""
"Spatial Distribution" 1.0 0 -10022847 true "" ""
"Average Belief" 1.0 0 -13345367 true "" ""
"Average Importance" 1.0 0 -10899396 true "" ""
"Importance Distribution" 1.0 0 -2674135 true "" ""
"Belief Distribution" 1.0 0 -1184463 true "" ""

SLIDER
807
540
1043
573
inconspenalty
inconspenalty
0
1
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
809
665
916
698
attraction
attraction
0.0
1
0.47
0.01
1
NIL
HORIZONTAL

SLIDER
931
666
1037
699
rejection
rejection
0.0
1
0.47
0.01
1
NIL
HORIZONTAL

SLIDER
808
773
1045
806
winthreshold
winthreshold
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
10
183
189
216
force-of-norms
force-of-norms
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
11
341
190
374
firmness-of-principle
firmness-of-principle
0
10
2.6
0.1
1
NIL
HORIZONTAL

SLIDER
12
475
187
508
chance-change-strategy
chance-change-strategy
0
10
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
11
301
192
334
announce-threshold
announce-threshold
0
10
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
318
475
453
508
stepsize
stepsize
0
10
0.8
0.1
1
NIL
HORIZONTAL

TEXTBOX
808
521
1016
549
Argumentation & Size related
11
0.0
1

TEXTBOX
815
650
1041
678
Announcement& Opinion related
11
0.0
1

TEXTBOX
817
758
1009
786
Argumentation & Opinion related
11
0.0
1

TEXTBOX
13
458
163
476
Strategy related
11
0.0
1

SLIDER
806
723
994
756
lack-of-princ-penalty
lack-of-princ-penalty
0
1
0.07
0.01
1
NIL
HORIZONTAL

TEXTBOX
817
708
967
726
Opinion & Size related
11
0.0
1

BUTTON
1114
47
1271
80
Create media agents
creat-media-agents
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This model is a demonstration of group formation of communicating agents. The collective memory is implemented in the patches. There is only one proposition. Whoever has evidence in favour of it paints the world from blue to white when she announces her opinion. And who has evidence against it paint their environment from blue to black when revealing her opinion.  
The borders of the agents also represent the evidence in favour or against the proposition.

## HOW IT WORKS

The model starts with a random distribution  agents with random evidence and importance values. The evidence ranges from -1 to 1. 1 means pro a proposition (white); -1 is contra (black) and 0 (blue) is no clue. Importance ranges from 0 to 1
 Turtles move in the direction of a patch that is most similar to their opinion or paint the world. Similarity is defined by the carthesian distance:
 sqrt( (e1 - e2)^2 + (i1 - i2)^2).  
Agents make announcements and the patches in the neighbourhood of the announcers (environment), remember that information for some time.

## HOW TO USE IT

The NUMBER slider sets the number of turtles.   
The LOUDNESS slider determines the distance an announcement  travels away from the announcer.

The SETUP button initializes the model, and GO runs the model.

While running the world only shows the evidence values. To se the importance of the message:  press SHOW-IMPORTANCE to see those values for patches and agents.  
Press SHOW-EVIDENCE toswitch back to the evidence presentation.  
Hint: Not pressing SHOW-EVIDENCE followed by the GO button repaints the patches with the evidence color, but the turtles keep the importance representation.

The ALARM monitor should always be 0. If the value is different then there is code executed, in the announce procedure, that should not have been executed. Maybe this is useful for you too Corinna.

LOUDNESS
        - determines the distance an announcement  travels away from the announcer.

STEPSIZE
        - the maximum distance an agent travels in one round.

VISUAL-MEMORY   - determines how far an agent look foor the best place to go.  
UNDIRECTEDNESS  - the wobling angle, i.e. the inverse of the chance to get to the best place.  
CHANCE-WALK
     - determines the chance an agent makes a step instead of doing something else.

CHANCE-ANNOUNCE - determines the chance an agent makes an announcement. The only other option is to walk in the direction of similar patch.  
CHANCE-LEARN-BY-NEIGHBOUR - the agent copies the evidence and importance values from a neighbour.  
CHANCE-LEARN-BY-MEMORY
    - the agent copies the evidence and importance values from the patch it is standing on.

CHANCE-MUTATION
           - a random change of the evidence and importance values of an agent.

FORGETSPEED
     - decreases the time a patch remembers it's contents.

NEUTRAL-IMPORTANCE - The default background importance of the patches that forgotten all information. If this value is 0, the agent with opposite opinions may prefer each other over a neutral patch because of the distance in importance.

## THINGS TO NOTICE

The formation of groups, tight and loose. There are even wanderers.  
Forgetting, announcing, but with a low voice, are real space makers!  
What we learn from this? A lot about what agents can do without actually reasoning.  
Well reasoning is supposed to make it more intelligent, so this is our point of departure.

## THINGS TO TRY

Slide the sliders. Check importance to see for those values. Try to create the right circumstances for the emergence of tight or loose groups and wandering agents.

## EXTENDING THE MODEL

More propositions.   
Asking.  
Memory (forgetting) for the agents.  
Trust and dialogues.

## RELATED MODELS

GenDrift (P Global)  
Segregation

## CREDITS AND REFERENCES

## AUTHOR

Piter Dykstra
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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 100</exitCondition>
    <metric>clustering</metric>
    <metric>report-LOA</metric>
    <enumeratedValueSet variable="force-of-argumentation">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="varyrationality" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 50</exitCondition>
    <metric>report-groupness</metric>
    <metric>report-authority</metric>
    <metric>report-extremeness</metric>
    <metric>report-pressure</metric>
    <metric>report-importance</metric>
    <metric>report-giniimportance</metric>
    <enumeratedValueSet variable="force-of-norms">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="force-of-argumentation">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="attraction" first="0.2" step="0.2" last="1"/>
    <steppedValueSet variable="rejection" first="0.2" step="0.2" last="1"/>
  </experiment>
  <experiment name="varynorms-argumentation" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 200</exitCondition>
    <metric>clustering</metric>
    <metric>report-authority</metric>
    <metric>report-eopop</metric>
    <metric>report-ginievid</metric>
    <metric>report-iopop</metric>
    <metric>report-giniimp</metric>
    <steppedValueSet variable="force-of-norms" first="0" step="0.05" last="1"/>
    <steppedValueSet variable="force-of-argumentation" first="0" step="0.05" last="1"/>
  </experiment>
  <experiment name="varynorms-norms" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 200</exitCondition>
    <metric>clustering</metric>
    <metric>report-authority</metric>
    <metric>report-eopop</metric>
    <metric>report-ginievid</metric>
    <metric>report-iopop</metric>
    <metric>report-giniimp</metric>
    <steppedValueSet variable="force-of-norms" first="0" step="0.005" last="0.1"/>
    <steppedValueSet variable="force-of-argumentation" first="0" step="0.05" last="1"/>
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
