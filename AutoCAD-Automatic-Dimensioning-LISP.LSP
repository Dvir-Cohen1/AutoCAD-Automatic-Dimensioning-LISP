;;; Helper functions for dictionary variable get/set -- must be loaded first

(defun getdictvar (dict var / dict_ename)
  (if (setq dict_ename (cdr (assoc -1 (dictsearch (namedobjdict) dict))))
    (cdr (assoc 1 (dictsearch dict_ename var)))
  )
)

(defun setdictvar (dict var val / dict_ename record)
  (or (setq dict_ename (cdr (assoc -1 (dictsearch (namedobjdict) dict))))
      (setq dict_ename (dictadd (namedobjdict) dict (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))))
  (if (setq record (dictsearch dict_ename var))
    (entmod (subst (cons 1 val) (assoc 1 record) record))
    (dictadd
      dict_ename
      var
      (entmakex
        (list
          '(0 . "DICTIONARYVAR")
          '(100 . "DictionaryVariables")
          '(280 . 0)
          (cons 1 val)
        )
      )
    )
  )
  val
)

;;; Dialog function for dimension options

(defun AD_options (old / a1 a2 b1 b2 dcl dcl_id file r)
  (setq a1 (car old)
        a2 (cadr old)
        dcl (open (setq file (vl-filename-mktemp "AD" (getvar 'dwgprefix) ".dcl")) "w")
  )
  (write-line
    "AD: dialog { label = \"Dimension Polyline Options\" ;
    : boxed_radio_column { label = \"Linear dimension position\" ; key = \"a1\" ;
    : radio_button { label = \"Above line\" ; key = \"b0\" ; }
    : radio_button { label = \"Below line\" ; key = \"b1\" ; } }
    : boxed_radio_column { label = \"Arc dimension position\" ; key = \"a2\" ;
    : radio_button { label = \"Inside arc\" ; key = \"c0\" ; }
    : radio_button { label = \"Outside arc\" ; key = \"c1\" ; }
    : radio_button { label = \"As for lines\" ; key = \"c2\" ; } }
    ok_cancel ; }"
    dcl)
  (close dcl)
  (if (< 0 (setq dcl_id (load_dialog file)))
    (if (new_dialog "AD" dcl_id)
      (progn
        (action_tile "a1" "(setq b1 $value)")
        (action_tile "a2" "(setq b2 $value)")
        (set_tile "a1" (setq b1 a1))
        (set_tile "a2" (setq b2 a2))
        (setq r (start_dialog))
        (unload_dialog dcl_id)
      )
    )
  )
  (if (findfile file) (vl-file-delete file))
  (if (= r 1)
    (list b1 b2)
    old
  )
)

;;; Load or get dimension options helper

(defun load-or-get-dim-options ( / oldOptions r)
  (setq oldOptions (getdictvar "AD_options" "Linear"))
  (if (not oldOptions) (setq oldOptions '("b0" "c0"))) ;; default values
  
  (initget "Options")
  (if (= (getkword "\nPress Enter to continue or [Options]: ") "Options")
    (setq r (AD_options oldOptions))
    (setq r oldOptions)
  )
  r
)

;;; Automatic dimensioning for selected entities

(defun automatic-dimensioning (ss space textHeight dimOptions / i e entObj startParam endParam b isLine u pm sd rad d pc p3)
  (setq i (sslength ss))
  (while (> i 0)
    (setq i (1- i)
          e (ssname ss i)
          entObj (vlax-ename->vla-object e)
          startParam (vlax-curve-getStartParam entObj)
          endParam (vlax-curve-getEndParam entObj)
          b nil
          isLine (wcmatch (vla-get-ObjectName entObj) "AcDbLine,AcDbArc")
    )
    (while (<= (setq b (if isLine (if b (1+ b) endParam) (1+ startParam))) endParam)
      (setq
        p1 (vlax-curve-getPointAtParam entObj startParam)
        p2 (vlax-curve-getPointAtParam entObj b)
        u  (angle p1 p2)
        pm (vlax-curve-getPointAtParam entObj (/ (+ startParam b) 2.0))
        sd (vlax-curve-getSecondDeriv entObj (/ (+ startParam b) 2.0))
        rad (distance '(0 0 0) sd)
        d (cond (isLine) ((not (minusp (vla-getBulge entObj startParam)))))
        pc (mapcar (if d '+ '-) pm sd)
        p3 (cond
             ((or (equal rad 0.0 1e-8) (eq (cadr dimOptions) "c2"))
               (if (eq (car dimOptions) "b0")
                   (polar pm (+ (atan (/ (sin u) (cos u))) (/ pi 2.0)) textHeight)
                   (polar pm (- (atan (/ (sin u) (cos u))) (/ pi 2.0)) (* 1.75 textHeight))
               )
             )
             (t
               (if (eq (cadr dimOptions) "c0")
                   (polar pm (angle pm pc) (if (<= 1e-4 (angle pc pm) pi) (* 1.75 textHeight) textHeight))
                   (polar pm (angle pc pm) (if (<= 1e-4 (angle pc pm) pi) textHeight (* 1.75 textHeight)))
               )
             )
           )
      )
      (if (equal rad 0.0 1e-8)
          (vla-AddDimAligned space (vlax-3d-point p1) (vlax-3d-point p2) (vlax-3d-point p3))
          (vla-AddDimArc space (vlax-3d-point pc) (vlax-3d-point p1) (vlax-3d-point p2) (vlax-3d-point p3))
      )
      (setq startParam (1+ startParam))
    )
  )
  (princ "\nAutomatic dimensioning complete.")
)

;;; Manual dimensioning by user picked points

(defun manual-pick-dimension (space textHeight pickedPoints / p1 p2)
  (princ "\nSelect pairs of points for dimensioning. Press Enter to finish.")
  (while (setq p1 (getpoint "\nSelect first point (or Enter to finish): "))
    (setq p2 (getpoint "\nSelect second point: "))
    (if p2
      (setq pickedPoints (cons (list p1 p2) pickedPoints))
    )
  )
  (foreach pair pickedPoints
    (setq p1 (car pair) p2 (cadr pair))
    (vla-AddDimAligned space 
                      (vlax-3d-point p1) 
                      (vlax-3d-point p2) 
                      (vlax-3d-point (polar p1 (angle p1 p2) textHeight))
    )
  )
  (princ "\nManual dimensioning complete.")
)

;;; Main command: C:AD

(defun C:AD ( / acDoc space dimStyle textHeight userChoice dimOptions ss pickedPoints)
  ;; Get active document and drawing space
  (setq acDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vlax-get acDoc (if (= (getvar 'cvport) 1) "PaperSpace" "ModelSpace")))
  
  ;; Get dimension style and compute scaled text height
  (setq dimStyle (getvar 'dimstyle))
  (setq textHeight
        (* 1.0 (getvar 'dimtxt)
           (if (= 0 (getvar 'dimanno))
               (getvar 'dimscale)
               (/ 1.0 (getvar 'cannoscalevalue)))))
  
  ;; Start undo mark to group actions
  (vla-StartUndoMark acDoc)

  ;; Error handler to catch unexpected errors
  (defun *error* (msg)
    (and msg
         (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
         (princ (strcat "\nADError: " msg)))
    (vla-EndUndoMark acDoc)
    (princ)
  )

  ;; Ask user for dimension mode
  (initget "Original PickPoints")
  (setq userChoice (getkword "\nChoose method: [Original/PickPoints] <Original>: "))

  (cond
    ((or (null userChoice) (equal userChoice "Original"))
      (setq dimOptions (load-or-get-dim-options))
      (if (setq ss (ssget '((0 . "LWPOLYLINE,LINE,ARC"))))
        (automatic-dimensioning ss space textHeight dimOptions)
        (princ "\nNo valid entities selected.")
      )
    )
    ((equal userChoice "PickPoints")
      (setq pickedPoints '())
      (manual-pick-dimension space textHeight pickedPoints)
    )
    (t (princ "\nInvalid choice. Use Original or PickPoints."))
  )

  ;; End undo mark
  (vla-EndUndoMark acDoc)
  (princ)
)
