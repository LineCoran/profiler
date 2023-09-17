(defun makeText (textValue textX textY rotate)
  (entmake ( list (cons 0 "text") (cons 7 "Standard") (cons 10 (list textX textY 0.0)) (cons 40 2.5) (cons 50 rotate) (cons 1 textValue)))
  )


(defun endOfEnd(slopeList)
  (setq resultLen (length slopeList))
  (setq resultCounter 0)
  (repeat resultLen
    (setq checklast (+ resultCounter 1))
    (setq heigh1V (cadr (nth resultCounter slopeList)))
    
    (setq heigh2V (nth 2 (nth resultCounter slopeList)))
    
    (setq x1Coord (nth 3 (nth resultCounter slopeList)))
    (setq y1Coord (nth 4 (nth resultCounter slopeList)))
    (setq distV (last (nth resultCounter slopeList)))
    (setq x2Coord (+ (/ distV 5) x1Coord))
    (setq baseY y1Coord) ; base height point for all result
    
    (setq VERTICAL_OFFSET 20)
    (setq SLOPE_HEIGHT 10)
    (setq WORD_OFFSET 2)
    
    (setq SCALE 5)
    (setq SCALE_DISTANCE (/ distV SCALE))
    (setq HEIGHT_VALUE_OFFSET (+ (+ VERTICAL_OFFSET SLOPE_HEIGHT) WORD_OFFSET))
    
  
    (setq slopeValue (car (nth resultCounter slopeList)))
    (setq slopeValueAbs (abs slopeValue))
    

    ; vertiacal line coordinates =======
    (setq verticalLineX x1Coord)
    (setq verticalLineYStart (+ baseY VERTICAL_OFFSET))
    (setq verticalLineYEnd (+ verticalLineYStart SLOPE_HEIGHT))

    (setq lastVerticalLineX (+ x1Coord SCALE_DISTANCE))

    (entmake ( list (cons 0 "line") (cons 10 (list verticalLineX verticalLineYStart 0.0)) (cons 11 (list verticalLineX verticalLineYEnd 0.0))))
    (if (= resultLen checklast)
	(entmake ( list (cons 0 "line") (cons 10 (list lastVerticalLineX verticalLineYStart 0.0)) (cons 11 (list lastVerticalLineX verticalLineYEnd 0.0))))
      )
    ; ==========

    ; ========== height value  ==============
    (setq heightValue1 (rtos heigh1V 2 2))
    (setq heightValue2 (rtos heigh2V 2 3))
    (setq heightValue1X x1Coord)
    (setq heightValue2X (+ x1Coord SCALE_DISTANCE))
    (setq heightValueY ( + baseY HEIGHT_VALUE_OFFSET))

    (makeText heightValue1 heightValue1X heightValueY 1.5708)
     (if (= resultLen checklast)
       (makeText heightValue2 heightValue2X heightValueY 1.5708)
      )
    ; ==============

    ; ======= slope value =========
    (setq TOP_BOTTOM_OFFSET 6 )
    (setq slopeValueX nil)
    (setq slopeValueY (+ (+ baseY VERTICAL_OFFSET) TOP_BOTTOM_OFFSET))
    (if (> heigh1V heigh2V)
	(setq slopeValueX (- (+ x1Coord SCALE_DISTANCE) 6))
	(setq slopeValueX (+ x1Coord WORD_OFFSET))
      )
    (if (= heigh1V heigh2V)
      (setq slopeValueX ( - (+ x1Coord (/ SCALE_DISTANCE 2)) 1))
      )
    (makeText (rtos slopeValueAbs 2 2) slopeValueX slopeValueY 0)

    ; ================================

    ; ====== distance value ==========
    (setq distanceValue distV)
    (setq distX nil)
    (setq distY (+ (+ baseY VERTICAL_OFFSET) WORD_OFFSET))
    (if (> heigh1V heigh2V)
      (setq distX (+ x1Coord WORD_OFFSET))
      (setq distX (- (+ x1Coord SCALE_DISTANCE) 8))
      )
    (if (= heigh1V heigh2V)
      (setq distX (- (+ x1Coord (/ SCALE_DISTANCE 2)) 3))
      )


    (makeText (rtos distanceValue 2 0) distX distY 0)

    ;=================================


    ; ========= slope line ==========
    (setq startSlopeLineX x1Coord)
    (setq endSlopeLineX (+ x1Coord SCALE_DISTANCE))
    (setq startSlopeLineY nil)

    (setq endSlopeLineY nil)

    (if (> heigh1V heigh2V)
      (progn
	(setq startSlopeLineY (+ (+ baseY VERTICAL_OFFSET) SLOPE_HEIGHT))
	(setq endSlopeLineY (+ baseY VERTICAL_OFFSET))
	)
      (progn
	(setq startSlopeLineY (+ baseY VERTICAL_OFFSET))
	(setq endSlopeLineY (+ (+ baseY VERTICAL_OFFSET) SLOPE_HEIGHT))
	)
      )
    
    (if (= heigh1V heigh2V)
      (progn
	(setq startSlopeLineY (+ (+ baseY VERTICAL_OFFSET) (/ SLOPE_HEIGHT 2)))
	(setq endSlopeLineY startSlopeLineY)
	)
      )
    
   (entmake ( list (cons 0 "line") (cons 10 (list startSlopeLineX startSlopeLineY 0.0)) (cons 11 (list endSlopeLineX endSlopeLineY 0.0))))



    ; ==============================

    (drawHelperHeight distV heigh1V slopeValue x1Coord (+ baseY HEIGHT_VALUE_OFFSET))
    (setq resultCounter (+ resultCounter 1))
    )
  )

(defun recursia (slopeList yourLimit)
  
  (setq finalRes slopeList)
  (setq exitFromFun 0)
  (setq count 0)
  (setq secondCount 1)
  (setq myLenght (length slopeList))
  (setq ctrlP1 nil)
  (setq ctrlP2 nil)
  (if (/= myLenght 1)
    (progn
      (repeat myLenght
	(setq slope1 (car (nth count slopeList)))
	(setq slope2 (car (nth secondCount slopeList)))
	(setq dis1Control (last (nth count slopeList)))
	(setq dis2Control (last (nth secondCount slopeList)))
	(if (< dis1Control 199)
	(if slope2
	  (progn


	    (setq height1StartControl (cadr (nth count slopeList)))
	    (setq height2SEndControl (nth 2 (nth secondCount slopeList)))
	    (setq middleHeightControl (cadr (nth secondCount slopeList)))
	    (setq totalDistanceControl (+ dis1Control dis2Control))
	    
	    (setq difList (list ))
	    (setq newSlopeControl (atof (rtos (* (/ (- height2SEndControl height1StartControl) totalDistanceControl ) 1000) 2 1)))
	    (setq controlHeight (atof (rtos (+ ( / (* newSlopeControl  dis1Control) 1000) height1StartControl) 2 2)))

	    (if (> totalDistanceControl 201)
	      (progn
		(setq stepsControl (fix (- (/ totalDistanceControl 100) 1)))
		(setq startLenControl 100)
		(setq newHeightVControl middleHeightControl)
		(setq oldHeightVControl middleHeightControl)
		
		(repeat stepsControl
		  (setq cmControl (/ newSlopeControl 10))
		  (setq oldcmControl (/ slope1 10))
		  (setq newHeightVControl (- cmControl newHeightVControl))
		  (setq oldHeightVControl (- oldcmControl oldHeightVControl))
		  (setq startLenControl (+ startLenControl 100))
		  (setq difList (append difList (list (abs(- newHeightVControl oldHeightVControl)))))
		  )
		)
	      (progn
		(setq difList (append difList (list (abs(- middleHeightControl controlHeight)))))
		)
	      )
	    (setq maxDif (apply 'max difList))
	    (if (<= maxDif yourLimit)
	      (progn
		(setq exitFromFun 1)
		(setq ctrlP1 count)
		(setq ctrlP2 secondCount)
		)
	      )
	    (setq count (+ count 1))
	    (setq secondCount (+ secondCount 1))
	    )
	  )
	  )
	)

      (if (= exitFromFun 1)
	(progn
	  (setq exitFromFun 0)
	  (setq height1Start (atof (rtos (cadr (nth ctrlP1 slopeList)) 2 2)))
	  (setq height2SEnd (atof (rtos (nth 2 (nth ctrlP2 slopeList)) 2 2)))
	  (setq x1 (nth 3 (nth ctrlP1 slopeList)))
	  (setq y1 (nth 4 (nth ctrlP1 slopeList)))
	  (setq dis1 (last (nth ctrlP1 slopeList)))
	  (setq dis2 (last (nth ctrlP2 slopeList)))
	  (setq totalDistance (+ dis1 dis2))
	  (setq newSlope (atof (rtos (* (/ (-  height2SEnd height1Start) totalDistance ) 1000) 2 1)))
	  (setq newSlopeElement (list newSlope height1Start height2SEnd x1 y1 totalDistance))
	  (setq newSlopeList (list ))
	  (setq finalCount 0)
	 
	  (repeat myLenght
	    (if (/= finalCount ctrlP2)
	      (progn
		(if (= finalCount ctrlP1)
		  (progn
		    (setq newSlopeList (append newSlopeList (list newSlopeElement)))
		     
		    )
		  (progn
		    (setq newSlopeList (append newSlopeList (list (nth finalCount slopeList))))
		    )
		  )
		)
	      )
	    (setq finalCount (+ finalCount 1))
	    
	    )
;;;	  (endOfEnd newSlopeList)
;;;	  (alert "NEW STEP")
	  (setq finalRes (recursia newSlopeList yourLimit))
	  )
	
	(progn
	  
	  finalRes
	  )))
    (progn finalRes)))

(defun drawHelperHeight (len height slopeNew x y)
  (if (> len 199)
    (progn
      (setq steps (fix (- (/ len 100) 1)))
      (setq newHeightV height)
      (setq startLen 100)
      (repeat steps
	(setq cm (/ slopeNew 10))
	(setq newHeightV (+ cm newHeightV))
	(setq newX (+ x (/ startLen 5)))
	(makeText (rtos newHeightV 2 2) newX y 1.5708)
	(setq startLen (+ startLen 100))
	)
      )
    )
  )

(defun compare-first (a b)
  (< (car a) (car b))
  )

(defun c:profiler ()
  (setq result (list ))
  (setq points (ssget (list(cons 0 "MTEXT"))))
  (setq filteredList (list ))
  (setq filterCount 0)
  (setq count 0)
  (setq secondCount 1)
  (setq finalConter 0)
  (repeat (sslength points)
    (setq pointFilter(ssname points filterCount))
    (setq pointFilterData (entget pointFilter))
    (setq xFilter (car(cdr (assoc 10 pointFilterData))))
    (setq heightValueFilter (cdr (assoc 1 pointFilterData)))
    (setq filteredList (append filteredList (list (list xFilter pointFilter))))
    (setq filterCount (+ filterCount 1))
    )

  (setq resultFilteredList (vl-sort filteredList 'compare-first))

  (repeat (length resultFilteredList)
    (setq firstPointName (cadr (nth count resultFilteredList)))
    (setq secondPointName (cadr (nth secondCount resultFilteredList)))

    (if secondPointName
      (progn
	(setq firstPointData (entget firstPointName))
	(setq secondPointData (entget secondPointName))
	(setq height1 (cdr (assoc 1 firstPointData)))
	(setq height2 (cdr (assoc 1 secondPointData)))
	(setq x1 (car(cdr (assoc 10 firstPointData))))
	(setq y1 (cadr(cdr (assoc 10 firstPointData))))
	(setq x2 (car(cdr (assoc 10 secondPointData))))
	(setq pDis (* 5 (abs (- x1 x2))))
	(setq num1 (atof height1))
	(setq num2 (atof height2))
	(setq slope (* (/ (- num2 num1) pDis) 1000))
	(setq count (+ count 1))
	(setq secondCount (+ secondCount 1))
	(setq result (append result (list (list slope num1 num2 x1 y1 pDis))))
	)
      )
    )
  (setq yourLimit (/ (atof (itoa (getint "\n Press limit (cm):"))) 100))
  (setq myFinalResult (recursia result yourLimit))
  (endOfEnd myFinalResult)
  
  )