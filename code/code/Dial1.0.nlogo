extensions [array vid]
breed [medias media]
breed [peoples people]
peoples-own [
   props            ; a list of pairs: < evidence importance >
   init-props       ; a list of pairs: < evidence importance > the initial values
   announcements    ; a list of 4-tuples: < key <evidence importance> ticks trust>
   attacks          ; pairs: < attacking-agent prop >
   questions        ; pairs: < requesting-agent prop >
   profit-strategy  ; list of learned profits of all strategy
   prior-size       ; prior size for profit
 ]
medias-own [
   props            ; a list of pairs: < evidence importance >
   init-props       ; a list of pairs: < evidence importance > the initial values
   announcements    ; a list of 4-tuples: < key <evidence importance> ticks trust>
   prior-size       ; prior size for profit
   reputation       ; determines how likely media agents reach others
 ]
patches-own [ pprops]

globals [delta action-prob-pairs current-prop number-of-props total-odds totalsim totalsize filenaam agentsorderedatstart triangles strategy-shapes plottitle _recording-save-file-name]

; Utility functions
to-report second [l] ; RENAME #################################
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
  if breed = peoples [
   let yesterday ticks - 10
   set announcements filter [ ?1 -> yesterday < item 2  ?1 ]  announcements
  ]
end


; Dialogue Oriented

; Initialization and the Main Loop
;;to-report random-prop ; to create a proposition with random evidence and importance values
                      ; used in setup
;;report list  (random-float 1) (random-float 1)
;;end

; A function that gives us a random number within a certain interval
to-report random-between [ min-num max-num ]
    report random-float (max-num - min-num) + min-num
end


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
  set-default-shape peoples "circle"
  ask patches [
               set pprops[]
    repeat number-of-props [ set pprops fput (list 0.5 neutral-importance) pprops]]
  ask patches [if pycor >= 13 [set pcolor brown]]
  ask patches [if pycor < 13 [set pcolor blue]]
  ;; initialise people in a restricted y-coordinate range:
  create-peoples number-of-people [setxy random-xcor random-between (min-pycor + 0.5) (max-pycor - 8)
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
  create-medias number-of-medias [
              setxy (int (who - number-of-people) * 5 - 20) 16
              set props generateopinionsmedia ;; adjust this function for experiment
              set init-props props
              set announcements [] ;maybe remove ##################################
              set shape "target"
              set color red
              ;set color  scale-color red first (item current-prop props)  1 0
              set label who - number-of-people + 1
              set label-color 66
              set size 4
              set reputation random-float 1; all medias start with the same reputation
  ]
  set totalsize  sum [size] of peoples
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
   ask peoples [act]
   ask medias [act-media]
   ask patches [if pycor < 13  ; to forget
    [set pprops map [ ?1 -> list (forget-pevidence first ?1) (forget-pimportance second ?1) ] pprops ]]


   ask peoples [
       forget-announcements
       answer-questions
       reply-attacks
       ]
   let f totalsize / (totalsize + totalsim)
   ask peoples [set size max (list 0 (size * f))]
;   update-plot ;; !!!!!!
   show-world
   tick
end

to-report similar-attitude [a b]
  report sum (map [ [?1 ?2] -> agreementfactor first ?1 first ?2 ] a b )
end

to act
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

; new added


to act-media
  set prior-size size
  let random_prop_index random number-of-props
  let evidence item random_prop_index props ;props is no longer a tuple for medias, only evidence stored

  ; give the media agent a reach based on their reputation
  ; media-reach determines fraction of people reached
  let media-reach reputation-based-prob reputation
  let number-of-reached-people round(number-of-people * media-reach)
  ; we start at a random index number (so we don't depend on the order of the agentset)
  let startindex random number-of-people
  let peopleaddressed []
  
  ; combine loops into one
  let counter 0
  repeat number-of-reached-people [
    set peopleaddressed fput (startindex mod number-of-people) peopleaddressed
    set startindex startindex + 1
    adjust-people-opinion counter peopleaddressed evidence random_prop_index
    set counter counter + 1
  ]
end

to adjust-people-opinion [cnt pa ev rprop] ; cnt = counter pa = patches addressed ev = evidence rprop = random_prop_index
  let peopleindex item cnt pa
  ask people peopleindex [
    
    let po item 0 item rprop props; po = people opinion
    let old-sublist item rprop props
    
    ; #########
    ; This formula needs to be adjusted by the perceived media bias
    
    set props replace-item rprop props (replace-item 0 old-sublist (po + (ev - po)* media-impact))
  ]
end

; -1 to 1
; -1 = gets influenced in opposite direction
; 1 = influenced in media opinion direction
; 0 = not influenced at all

to init-perceived-media-bias
  
end

to adjust-influence-by-perceived-media-bias
  
end

to-report reputation-based-prob [r] ; r = reputation of media agents
  let half-range 1 - r
  report r + random-float half-range - random-float half-range
end



to announce ;turtle procedure
 if size > announce-threshold [

      let announce-odds sum map second props ; sums up all the importance values, stores it in announce-odds
      ; let announce-odds sum map [ ?1 -> second ?1 ] props; we have some multiplying factor (?1)
      let choice random-float announce-odds ;choose a value between 0 and the sum of all importance values
      let  p 0
      let choice-inc second first props ;importance value of the first item of the props list
      while [choice >= choice-inc] [
        set p p + 1
        set choice-inc choice-inc + second item p props ;adds the importance of the propositions we loop through
      ]
  ; opinions with a higher importance have a highe probability of being chosen (code above)
  let w  who ; returns index of current turtle
  let evidence (first item p props  + firmness-of-principle * first item p init-props) /
               (firmness-of-principle + 1)
  let importance (second item p props  + firmness-of-principle * second item p init-props) /
               (firmness-of-principle + 1)
  let loud random-float loudness * size
  ask other peoples with [distance myself < loud]
         [ update-announcement w p evidence importance]
  ask patches with [distance myself < loud and pycor < 13]
         [ announce-patch myself p evidence importance]
         ;; this is a tricky way to pass a turtle via an ask patches command to a number of patches
   ]
end

to-report find-location [a b]
  if empty? b [report false]
  let fl find-location a but-first b
  ifelse fl [
      ifelse first first b = a  and not fl [report 0][report (1 + fl)]
    ][report false]
end

to update-announcement [w p ev i ] ; w = sender, p = proposition
  if breed = peoples [
  ; update memory
  let key number-of-people * p + w
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
  ]
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
   let candidate one-of other peoples with [distance myself < visual-horizon]
   if candidate != nobody     ; ask a passer-by
       [ask candidate [set questions fput (list myself max-imp-question) questions]]
end

to answer-questions
  if breed = peoples [
  if not empty? questions [
     let q one-of questions
     let ag first q
     let ag-dist distance ag
     let w  who
;     let pps props
     let evidence first (item (second q) props)
     let importance second (item (second q) props)
     ask other peoples with [distance myself <= ag-dist]
       [ update-announcement w (second q)  evidence importance]
;    ask patches with [distance ag < loud ]
;       [ announce-patch ag (second q) evidence importance]
     set questions []
    ]
  ]
end

to-report agrees [v] ; rank the announcements for attack
   let i floor (first v / number-of-people)
   let t  (first v) mod number-of-people
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
 ;      create-link-to turtle (key mod number-of-people) [set color 15]
       ask turtle (key mod number-of-people) [
           set attacks fput (list myself floor (key / number-of-people)) attacks]

       show (word self " attacks " (key mod number-of-people))
     ]
  ]
end

to reply-attacks
  if breed = peoples [
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
                       ask patches with [distance first a <  loudness and pycor < 13] [announce-patch first a p epro ipro]
                     ]
                     [
                       ask patches with [distance myself <  loudness and pycor < 13] [announce-patch myself p eop iop]
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
  ]
 ;  ask my-in-links [die]
end


to walk
    find-direction
    rt random undirectedness - random undirectedness
    if [pcolor] of patch-ahead 2 = brown
      [set heading (180 - heading)]
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
  let nb one-of peoples-on neighbors
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
; nondec -> increasing from first element
; noninc -> decreasing from first element
; ############### RENAME
to-report nondec [n l h] ;number of elements lowest value highest value
  let v []
  repeat n [set v fput (random-float (h - l) + l) v]
  report sort v
end
to-report noninc [n l h] ;number of elements lowest value highest value
  let v []
  repeat n [set v fput (random-float (h - l) + l) v] ;create random number between 0 and h-l. Then, add l.
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

to-report convexlist1 [n] ; ordered (by hill, increasing, or decreasing) list with purely random values
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

; generate opinions for the media agents
to-report generateopinionsmedia
  let evids []
  ;let imps []
  repeat number-of-props [ set evids fput (cap (random-normal media-opinion-mean media-opinion-std) 0 1) evids]
  ;repeat number-of-props [ set imps fput (random-float 1) imps]
  ;report zip evids imps
  report evids
end

to-report cap [n l u] ; number, lower bound, upper bound
  ;randdom-float(max-min)+min
  if n > u [report u]
  if n < l [report l]
  report n
end

; create groups
to createtriangles
  clear-links
  set triangles []
  ask peoples [create-links-with other peoples in-radius visual-horizon
                 [set color blue]]
  ask peoples with [count link-neighbors >= 2]
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
  if breed = peoples [
  let ev map [ ?1 -> first ?1 ] props
  report position (max ev) ev
  ]
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
  ask patches [if pycor < 13 [set pcolor  scale-color green  (second item current-prop pprops) 0 1]]
  ask peoples [if pycor < 13 [set color  scale-color red   (second item current-prop props) 1 -0]]
end

to show-evid       ;; show the evidence mode again
                   ;; show a map of the evidence values black yellow white for turtles
                   ;; black blue white for patches

  ask patches [if pycor < 13 [set pcolor  scale-color blue   (first item current-prop pprops) 0 1]]
    ask peoples [if pycor < 13 [set color  scale-color yellow   (first item current-prop props) 1 -0]]
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
  set-plot-y-range 0 number-of-people
  set-plot-x-range 0 1.01
  set-histogram-num-bars 20
  set-current-plot "Importance Distribution"
  set-plot-x-range 0 1
  set-plot-y-range 0 number-of-people
  set-histogram-num-bars 10
  set-current-plot plottitle
  set-plot-y-range 0 1
end
to setup-plotfile
  set filenaam user-new-file
  file-open filenaam
  set agentsorderedatstart sort-by [ [?1 ?2] -> [first item current-prop props] of ?1 <
                                    [first item current-prop props] of ?2 ] peoples

  set-current-plot "Distribution of Evidence"
  set-plot-y-range 0 number-of-people
  set-plot-x-range 0 1
  set-histogram-num-bars 20
  set-current-plot "Importance Distribution"
  set-plot-x-range 0 1
  set-plot-y-range 0 number-of-people
  set-histogram-num-bars 10
  set-current-plot plottitle
  set-plot-y-range 0 1
end

to update-plot
  let tmp 0
  set-current-plot "Distribution of Evidence"
    histogram [first item current-prop props] of peoples
    set-current-plot "Importance Distribution"
    histogram [second item current-prop props] of peoples
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
    histogram [first item current-prop props] of peoples ; evidence of currently displayed prop
    set-current-plot "Importance Distribution"
    histogram [second item current-prop props] of peoples ;; importance of currently displayed prop
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
  report mean [LOA] of peoples / number-of-props
end

; Lattitude of Non-commitment. Average number of props with 0.2 <= evidence <= 0.8
to-report report-LON
  report mean [LON] of peoples / number-of-props
end


to-report report-giniprefop ;gini of preferred opinion
  report gini ranks number-of-propositions
                    [abs ( 2 * (preferredopinion - number-of-propositions / 2 ) /
                           number-of-propositions) ] of peoples
end

; Average evidence of Preferred Opinion.
to-report report-eopop
  report mean [abs  (2 * first item preferredopinion props - 1)] of peoples
end

to-report report-iopop
  report mean [ second item preferredopinion props] of peoples
end


to-report report-ginievid
  report gini [abs  (2 * first item preferredopinion props - 1)] of peoples
end

to-report report-giniimp
  report gini [ second item preferredopinion props] of peoples
end


to-report report-authority
;   report max [size] of turtles / number-of-people
   report gini  [size] of peoples
end





to-report report-pressure
;;  let tmp (peoples with [ (sign first item current-prop props) = (sign first item current-prop pprops)] )
;;  report (mean [min lput visual-horizon ([distance myself] of same-kind-neighbours in-radius visual-horizon)] of turtles ) / visual-horizon
report gini ranks 10 [abs first item current-prop props] of peoples
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
  ask patches [if pycor < 13[
    set evid first item current-prop pprops
    if min [ (evid - 0.5) * (first item current-prop pprops - 0.5)] of neighbors < 0 [
      set cnt cnt + 1
    ]
  ]]
  report cnt / 1600 ;; count patches
end

to-report clustering1
  createtriangles
  jointriangles
  report mean map [ ?1 -> length ?1 ] triangles / 20
end

to-report clustering
  report 1 - mean [avg-dist] of peoples / visual-horizon
end

to-report avg-dist
  let m mean [distance myself] of peoples in-radius visual-horizon
  ifelse m = 0 [report 1][report m]
end

to-report talking
  report count peoples with [size > 1.3]  / number-of-people
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
